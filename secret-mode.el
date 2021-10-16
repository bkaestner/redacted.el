;;; secret-mode.el --- hide text in buffer           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Benjamin Kästner

;; Author:  Benjamin Kästner <benjamin.kaestner@gmail.com>
;; Keywords: games
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `secret-mode', a buffer-local mode that uses
;; `buffer-display-table' to replace the displayed glyphs with Unicode
;; blocks.

;;; Code:

(eval-and-compile
  (defconst secret-mode--max-letter-char-code
    (eval-when-compile
      (let ((max-code-point #x10FFFF)    ; Max Unicode code point
            (letter-categories '(Lu Ll)) ; Upper and Lower case letters
            result)
        (dotimes (i (1+ max-code-point))
          (when (memq (get-char-code-property i 'general-category)
                      letter-categories)
            (setq result i)))
        result))
    "Maximum Unicode code point that designates a letter.")

  (defun secret-mode--compute-table ()
    "Compute the display table for `secret-mode'."
    (let ((disptbl (make-display-table)))
      (dotimes (i (1+ secret-mode--max-letter-char-code))
        (aset disptbl i
              (pcase (get-char-code-property i 'general-category)
                ((or 'Cc 'Cf 'Zs 'Zl 'Zp) nil)
                ('Ll (vector (make-glyph-code ?▃)))
                (_   (vector (make-glyph-code ?▆))))))
      disptbl)))

(defconst secret-mode-table (eval-when-compile (secret-mode--compute-table))
  "Display table for the command `secret-mode'.")

;;;###autoload
(define-minor-mode secret-mode
  "Hide text."
  :lighter " Secret"
  (if secret-mode
      (setq buffer-display-table secret-mode-table)
    (setq buffer-display-table nil)))

(provide 'secret-mode)
;;; secret-mode.el ends here
