(defpackage #:clos-constructor/tests/unsupported-types
  (:use #:cl #:rove #:clos-constructor))

(in-package #:clos-constructor/tests/unsupported-types)

;;; Known unsupported types.
;;;
;;; Each test documents a type specifier that a user might reasonably put
;;; on a CLOS slot but that clos-constructor does not yet handle.  Tests
;;; use rove:skip so they show up as PENDING in the test output — a
;;; visible reminder of the gap.  When support is added, replace the
;;; skip with real assertions and the pending count drops.
;;;
;;; Two failure modes exist today:
;;;   1. Named types (hash-table, symbol, ...) → schema-error
;;;   2. Compound types ((vector ...), (satisfies ...), ...) → silent
;;;      fallback to ir-type-primitive :string, which is worse because
;;;      it produces wrong output instead of an error.

;;; ── Named types: signal schema-error ───────────────────────────────

(deftest type/hash-table
  (skip "hash-table: signals schema-error — no IR mapping"))

(deftest type/symbol
  (skip "symbol: signals schema-error — no IR mapping"))

(deftest type/keyword
  (skip "keyword: signals schema-error — no IR mapping"))

(deftest type/character
  (skip "character: signals schema-error — no IR mapping"))

(deftest type/pathname
  (skip "pathname: signals schema-error — no IR mapping"))

;;; ── Compound types: silent fallback to string ──────────────────────

(deftest type/vector
  (skip "(vector <type>): silently falls back to string — should map to ir-type-list"))

(deftest type/array
  (skip "(array <type>): silently falls back to string — should map to ir-type-list or error"))

(deftest type/unsigned-byte
  (skip "(unsigned-byte N): silently falls back to string — should map to ir-type-primitive :integer"))

(deftest type/satisfies
  (skip "(satisfies <pred>): silently falls back to string — should signal schema-error"))

(deftest type/complex
  (skip "(complex <type>): silently falls back to string — should signal schema-error"))

(deftest type/cons-pair
  (skip "(cons <a> <b>): silently falls back to string — should signal schema-error"))

(deftest type/values
  (skip "(values <types>): silently falls back to string — should signal schema-error"))
