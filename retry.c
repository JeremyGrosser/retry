/*
 * retry: run a command until it succeeds
 * Copyright (C) 2020 Jeremy Grosser <jeremy@synack.me>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <time.h>

#define MAX_SUCCESS_CODES 16

void help() {
    fprintf(stderr, "retry -h -n NUM_RETRIES -b BACKOFF -m MAX_BACKOFF -s RETURN_CODE <command>\n");
    fprintf(stderr, "    -n number of retries to attempt if the command does not return successfully (default: 10)\n");
    fprintf(stderr, "    -b initial backoff in seconds. Each subsequent failure will double the backoff interval (default: 1.0)\n");
    fprintf(stderr, "    -m maximum backoff in seconds. (default: unlimited)\n");
    fprintf(stderr, "    -s return value indicating success of the command. May be used more than once. (default: 0)\n");
    fprintf(stderr, "    -h this help\n");
}

void dsleep(double seconds) {
    struct timespec ts;
    long ns = (long)(seconds * 1000000.0);
    ts.tv_sec = (time_t)(ns / 1000000);
    ts.tv_nsec = ns % 1000000;
    nanosleep(&ts, NULL);
}

int main(int argc, char *argv[]) {
    int num_retries = 10;
    int attempts = 0;
    double backoff = 1.0;
    double max_backoff = 0.0;
    int sc[MAX_SUCCESS_CODES] = {0};
    int sc_idx = 0;
    char *command;
    int opt;
    int ret;
    int i;

    while((opt = getopt(argc, argv, "hn:b:m:s:")) != -1) {
        switch(opt) {
            case 'h':
                help();
                return 0;
                break;
            case 'n':
                num_retries = strtoul(optarg, NULL, 0);
                if(num_retries <= 0) {
                    fprintf(stderr, "Number of retries must be > 0\n");
                    return -1;
                }
                break;
            case 'b':
                backoff = strtod(optarg, NULL);
                if(backoff <= 0.0) {
                    fprintf(stderr, "Backoff must be > 0.0\n");
                    return -1;
                }
                break;
            case 'm':
                max_backoff = strtod(optarg, NULL);
                if(max_backoff <= 0.0) {
                    fprintf(stderr, "Max backoff must be > 0.0\n");
                    return -1;
                }
            case 's':
                if(sc_idx >= MAX_SUCCESS_CODES) {
                    fprintf(stderr, "Too many success codes (-s) specified!\n");
                    return -1;
                }
                sc[sc_idx++] = strtoul(optarg, NULL, 0);
                break;
            default:
                help();
                return -1;
        }
    }

    if(optind >= argc) {
        help();
        return -1;
    }

    if(sc_idx == 0) {
        sc_idx = 1;
        sc[0] = 0;
    }

    command = argv[optind];

    while(attempts <= num_retries) {
        ret = system(command);
        attempts++;

        if(ret == -1) {
            fprintf(stderr, "Unable to create process: %s\n", strerror(errno));
        }

        if(WIFEXITED(ret)) {
            ret = WEXITSTATUS(ret);
            if(ret == 127) {
                fprintf(stderr, "Unable to execute shell or child returned 127\n");
            }else{
                for(i = 0; i < sc_idx; i++) {
                    if(ret == sc[i]) {
                        return ret;
                    }
                }
            }
        }else if(WIFSIGNALED(ret)) {
            ret = WTERMSIG(ret);
            fprintf(stderr, "Terminated by signal %d\n", ret);
        }

        if(attempts < num_retries) {
            fprintf(stderr, "Attempt %d/%d exited with status: %d, retrying after %.03f seconds\n", attempts, num_retries, ret, backoff);
            dsleep(backoff);
            backoff = backoff * 2.0;
            if(max_backoff > 0.0 && backoff > max_backoff) {
                backoff = max_backoff;
            }
        }else{
            fprintf(stderr, "Attempt %d/%d exited with status: %d, no more retries\n", attempts, num_retries, ret);
            return ret;
        }

    }

    return -1;
}
