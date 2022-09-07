;; org agenda
(define-key global-map "\C-ca" 'org-agenda)

;;archive function
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; fontify code in code blocks
(setq org-src-fontify-natively t)

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

;; line wrap
(setq org-startup-truncated nil)

;; org mobile
(setq org-directory "~/Dropbox/wiki/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/wiki/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; 들여쓰기
;; (add-hook 'org-mode-hook
;;  '(lambda()
;;    (setq org-indent-indentation-per-level 4)
;;    (org-indent-mode t)))

;; 키워드 설정
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE" "CANCELLED")))

;; 완료된 키워드와 헤드라인에 취소선 넣기
(setq org-fontify-done-headline t)
  (custom-set-faces
   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
   '(org-headline-done
     ((((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon" :strike-through t)))))

;; 아젠다 파일목록
(setq org-agenda-files
  (list "~/Dropbox/wiki/org/inbox.org"
        "~/Dropbox/wiki/org/work.org"
        "~/Dropbox/wiki/org/home.org"
        "~/Dropbox/wiki/org/read_later.org"
        "~/Dropbox/wiki/org/book_note.org"
        "~/Dropbox/wiki/org/to_buy.org"
        "~/Dropbox/wiki/org/diary.org"))

;; org-mode 용 다이어리 파일 사용
(setq org-agenda-diary-file "~/Dropbox/wiki/org/diary.org")

;; agenda 에 7 일 표시
(setq org-agenda-ndays 7)

;; 월요일부터 아젠다 표시(0 일 1 월 2 화 ..., nil 이면 오늘부터 표시)
(setq org-agenda-start-on-weekday nil)

;; 데드라인 14일 전부터 경고 표시
(setq org-deadline-warning-days 14)

;; 일정이 없어도 날짜 표시
(setq org-agenda-show-all-dates t)

;; 다이어리 내용을 아젠다에 포함 시키기
(setq org-agenda-include-diary t)

;; 완료된 데드라인 표시안함
(setq org-agenda-skip-deadline-if-done t)

;; 완료된 스케줄 표시안함
(setq org-agenda-skip-scheduled-if-done t)

;; 스케줄 등록된 데드라인 표시안함
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; 아젠다 날짜 표시방식 변경 (참고 : C-h f format-time-string <RET>)
;; (setq org-agenda-format-date
;;   "\n\n%Y-%m-%d (%A) ========================================================================")

;; 태그 표시위치 지정
(setq org-agenda-tags-column 100)

;; 아젠다 아이템 표시방법 변경 (참고 : C-h v org-agenda-prefix-format <RET>)
(setq org-agenda-prefix-format
  '(
    ;;(agenda . "  - %-12:c%?-12t% s")
    (agenda . "  %-12t %s")
    ))

;; 데드라인 아이템 표시방법 변경
(setq org-agenda-deadline-text "D: ")
(setq org-agenda-deadline-relative-text "D (%3d): ")
(setq org-agenda-deadline-leaders '("D: " "D (%3d): "))

;; 스케줄 아이템 표시방법 변경
(setq org-agenda-scheduled-text "S: ")
(setq org-agenda-scheduled-relative-text "S (%3d): ")
(setq org-agenda-scheduled-leaders '("S: " "S (%3d): "))

; Custom Agenda Views
(setq org-agenda-custom-commands
  '(
    ("a" . "커스텀 아젠다 명령 prefix")
    ; ==========================================================================
    ("aw" agenda "할일 목록(일주일)"
     (
       (org-agenda-overriding-header "할일 목록(일주일)")
       (org-agenda-ndays 7)
       (org-agenda-include-diary t)
       (org-deadline-warning-days 0)
      ))
    ; ==========================================================================
    ("am" agenda "할일 목록(한달)"
     (
       (org-agenda-overriding-header "할일 목록(한달)")
       (org-agenda-ndays 28)
       (org-agenda-include-diary t)
       (org-deadline-warning-days 0)
       ;(org-agenda-remove-tags t)
      ))
    ; ==========================================================================
    ("ab" agenda "업무 목록"
     (
       (org-agenda-overriding-header "업무 목록")
       (org-agenda-ndays 7)
       (org-agenda-files '("~/Dropbox/wiki/org/work.org"))
       (org-agenda-include-diary t)
       (org-deadline-warning-days 0)
       (org-agenda-skip-deadline-if-done nil)
       (org-agenda-skip-scheduled-if-done nil)
       (org-agenda-skip-timestamp-if-done nil)
       (org-agenda-remove-tags nil)
      ))
    ; ==========================================================================
    ("ah" agenda "home 목록"
     (
       (org-agenda-overriding-header "home 목록")
       (org-agenda-ndays 7)
       (org-agenda-files '("~/Dropbox/wiki/org/home.org"))
       (org-agenda-include-diary t)
       (org-deadline-warning-days 0)
       (org-agenda-skip-deadline-if-done nil)
       (org-agenda-skip-scheduled-if-done nil)
       (org-agenda-skip-timestamp-if-done nil)
       (org-agenda-remove-tags nil)
      ))
    ))


;;=============================================================================
;; org 추가용 음력 entry 생성
;;=============================================================================

(defun my-org-korean-lunar-anniversary (year month day &optional mark)
  "Korean Lunar Anniversary diary entry with fixed (ISO) order of arguments."
  (let* ((y (calendar-extract-year date))
         (c-cycle (ceiling (/ (+ 2637.0 y) 60)))
         (c-year (+ (mod (+ 2637 (- y 1)) 60) 1))
         (c-date (list c-cycle c-year month day))
         (a-date (calendar-chinese-to-absolute c-date))
         (g-date (calendar-gregorian-from-absolute a-date))     ;; `calendar-date-style' is ignored in cal-china.el. always mm/dd/yyyy
         calendar-date-style european-calendar-style ddate dd mm yy diff)
    (if (not (= y (nth 2 g-date)))
        (progn
          (setq c-cycle (ceiling (/ (+ 2637.0 (- y 1)) 60)))
          (setq c-year (+ (mod (+ 2637 (- y 2)) 60) 1))
          (setq c-date (list c-cycle c-year month day))
          (setq a-date (calendar-chinese-to-absolute c-date))
          (setq g-date (calendar-gregorian-from-absolute a-date))))
    (setq calendar-date-style 'european)
    (setq european-calendar-style t)
    (setq ddate (diary-make-date (nth 1 g-date) (nth 0 g-date) (nth 2 g-date)))
    (setq dd (calendar-extract-day ddate))
    (setq mm (calendar-extract-month ddate))
    (setq yy (calendar-extract-year ddate))
    (setq diff (if yy (- y yy) 100))
    (and (= mm 2) (= dd 29) (not (calendar-leap-year-p y))
         (setq mm 3
               dd 1))
    (and (> diff -1) (calendar-date-equal (list mm dd y) date)
         (cons mark (format entry diff (diary-ordinal-suffix diff))))))


;;=============================================================================
;; org-jekyll 설정
;;=============================================================================

(setq org-publish-project-alist
      '(

  ("org-blog"
          ;; Path to your org files.
          :base-directory "~/Dropbox/wiki/blog/"
          :base-extension "org"

          ;; Path to your Jekyll project.
          :publishing-directory "~/blog/_posts/"
          :recursive t
          :publishing-function org-html-publish-to-html
          :headline-levels 4
          :html-extension "html"
          :body-only t ;; Only export section between <body> </body>
    )


    ;("org-static-blog"
    ;      :base-directory "~/Dropbox/wiki/blog/"
    ;      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
    ;      :publishing-directory "~/blog/"
    ;      :recursive t
    ;      :publishing-function org-publish-attachment)
	;
    ;("blog" :components ("org-blog" "org-static-blog"))

))

(provide 'init-org)
