#!/bin/sh

exec ghci -iapp:src:lib "$@"
