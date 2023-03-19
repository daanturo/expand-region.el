;;; treesit-er-expansions.el ---  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Aleksandar Dimitrov


;; Author: Aleksandar Dimitrov <git@aleks.bg>
;; Created: 2023-03-13
;; Keywords: marking region

;; This file is not part of GNU Emacs.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'expand-region-core)

(defun er/treesit-er--get-node-between (a b)
  "Find the node that sits above any node in the region (A B)."
  (let* ((start (min a b))
         (end (max a b)))
    (treesit-parent-until
     (treesit-node-at start)
     (lambda (node) (< end (treesit-node-end node))))))

;;;###autoload
(defun er/treesit-er-parent-node ()
  "Expand to the node above point, or to the node above the active region."
  (interactive)
  (let ((node
          (if (region-active-p)
              (er/treesit-er--get-node-between (mark) (point))
            (treesit-node-at (point)))))
    (goto-char (treesit-node-start node))
    (set-mark (treesit-node-end node))
    (activate-mark)))

(defun er/treesit--super-ancestor-node-p (ancestor descendant)
  "Test if ANCESTOR node's range includes, and is bigger than DESCENDANT's."
  (let* ((beg0 (treesit-node-start ancestor))
         (end0 (treesit-node-end ancestor))
         (beg1 (treesit-node-start descendant))
         (end1 (treesit-node-end descendant)))
    (and (<= beg0 beg1 end1 end0) (or (< beg0 beg1) (< end1 end0)))))

(defun er/treesit-mark--from-to (from to)
  "Set mark at FROM and go to TO."
  (set-mark from)
  (goto-char to)
  (activate-mark))

(defun er/treesit--find-inside-list (super-node cover-beg cover-end)
  "Return SUPER-NODE's inner list range if any.
A beginning and ending position such their span includes
COVER-BEG, COVER-END and more than that.

Conditions:
- SUPER-NODE has at least 3 children
- SUPER-NODE's first and last child are unnamed
- SUPER-NODE's first named child's beginning (1) is before COVER-BEG or
  SUPER-NODE's last named child's ending (2) is after COVER-END

Then return a list: the lower of (1) and COVER-BEG, and the
greater of (2) and COVER-END."
  (when-let* ((_
               (and (<= 3 (treesit-node-child-count super-node))
                    (not
                     (treesit-node-check
                      (treesit-node-child super-node 0) 'named))
                    (not
                     (treesit-node-check
                      (treesit-node-child super-node -1) 'named))))
              (first-named (treesit-node-child super-node 0 t))
              (last-named (treesit-node-child super-node -1 t))
              (inside-beg (min cover-beg (treesit-node-start first-named)))
              (inside-end (max cover-end (treesit-node-end last-named)))
              (_ (or (< inside-beg cover-beg) (< cover-end inside-end))))
    (list inside-end inside-beg)))

;;;###autoload
(defun er/treesit-mark-bigger-list-or-node ()
  "Mark nodes inside ancestor node or itself on the region, or node at point.
When expanding, prefer marking named nodes inside the
ancestor (usually equivalent to `er/mark-inside-pairs' and
`er/mark-inside-quotes')."
  (interactive)
  (if (not (region-active-p))
      (let* ((node@pt (treesit-node-at (point))))
        (er/treesit-mark--from-to
         (treesit-node-end node@pt) (treesit-node-start node@pt)))
    (let* ((beg0 (region-beginning))
           (end0 (region-end))
           (cover-node (treesit-node-on beg0 end0))
           (cover-beg (treesit-node-start cover-node))
           (cover-end (treesit-node-end cover-node)))
      ;; if found node is bigger than the region, mark it
      (if (or (< cover-beg beg0) (< end0 cover-end))
          (er/treesit-mark--from-to cover-end cover-beg)
        (let* ((super-node
                (treesit-parent-until
                 cover-node
                 (lambda (node)
                   (er/treesit--super-ancestor-node-p node cover-node))))
               (super-beg (treesit-node-start super-node))
               (super-end (treesit-node-end super-node)))
          (if-let* ((inside-positions
                     (er/treesit--find-inside-list super-node beg0 end0)))
            (er/treesit-mark--from-to
             (nth 0 inside-positions) (nth 1 inside-positions))
            (er/treesit-mark--from-to super-end super-beg)))))))

(provide 'treesit-er-expansions)

;;; treesit-er-expansions.el ends here
