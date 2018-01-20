#lang info

(define deps
  '("base"
    "bazaar"
    "gui-lib"))

(define build-deps
  '("rackunit-lib"))

(define test-omit-paths
  '("color-flood.rkt"
    "main.rkt"))