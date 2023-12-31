(setq mb-js-onscroll
      "
function toggleElementsOnScroll(e) {
  if (this.oldScroll === undefined || this.oldScroll > this.scrollY) {
    // scroll up
    document.getElementById('header').style.height = '3.5rem';
    document.getElementById('scroll-to-top').style.height = '3.5rem';
  } else {
    // scroll down
    console.log('foo');
    document.getElementById('header').style.height = '0';
    document.getElementById('scroll-to-top').style.height = '0';
  }
  if (this.scrollY == 0) {
    document.getElementById('scroll-to-top').style.height = '0';
  }
  this.oldScroll = this.scrollY;
}

window.onscroll = toggleElementsOnScroll;
")

(defun mb-date (path)
  "\"/2022-11-08-.../\" -> \"November 12, 2022\""
  (let ((time (thread-first
                path
                (substring 1 11)
                (concat "T00:00:00")
                date-to-time)))
    (format-time-string "%B %d, %Y" time)))

(defun mb-thumbnail (path)
  "/2022-11-08-.../ -> /img/2022-11-08-thumbnail.webp"
  (concat "/img" (substring path 0 12) "thumbnail.webp"))

(defun mb-is-episode-p (path)
  "Return t if PATH is a valid page path."
  (let ((episode-re ;; /2022-11-08-...
         "\\`/[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}-"))
    (string-match-p episode-re path)))

(setq mb-footer
      '(:div/footer
        (:div
         "Built with " (:a (@ :href "https://one.tonyaldon.com") "one.el")
         " (" (:a (@ :href "https://github.com/tonyaldon/minibuffer.tonyaldon.com") "source") ")")
        (:div
         "Twitter: "(:a (@ :href "https://twitter.com/tonyaldon") "@tonyaldon")
         " | "
         "Email: " (:a (@ :href "mailto:tony@tonyaldon.com") "tony@tonyaldon.com")
         " | "
         "RSS: " (:a (@ :href "/feed.xml") "feed.xml"))
        (:div "© 2023 Tony Aldon.  All rights reserved.")))

(defun mb-item (page &optional is-episode-p)
  "Episode item to be listed either in the home page or in a sidebare."
  (let* ((path (plist-get page :one-path))
         (title (plist-get page :one-title))
         (date (mb-date path)))
    `(:div.item
      (:a (@ :href ,path)
       (:div (@ :class ,(if is-episode-p "thumbnail" "thumbnail-home"))
        (:img (@ :src ,(mb-thumbnail path)
                 :alt ,title
                 ,@(if is-episode-p
                       '(:width "168px" :height "94px")
                     '(:width "640px" :height "360px")))))
       (:div.details (:h4 ,title) (:div.date ,date))))))

(defun mb-shuffle (v)
  "Shuffle vectors using Fisher-Yates algo.

See https://www.reddit.com/r/emacs/comments/16cl3mk/shuffling_vectors_in_emacs_lisp_with_fisheryates/."
  (let ((n (length v)))
    (dotimes (i (1- n) v)
      (cl-rotatef (aref v i) (aref v (+ i (random (- n i))))))))

;; (mb-shuffle '[1 2 3 4 5]) ; [5 4 1 3 2]

(defun mb-one-home (page-tree pages global)
  "Render function of the home page."
  (let* ((title (org-element-property :raw-value page-tree)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html (@ :lang "en")
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:meta (@ :name "description"
                  :content "Learn Emacs Lisp One Sexp At A Time."))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/main.css"))
        (:title ,title))
       (:body
        (:div/content
         (:div/header (:div/minibuffer "minibuffer"))
         (:div/intro "Learn Emacs Lisp One Sexp At A Time.")
         (:a (@ :href "#" :aria-label "scroll to top")
          (:div/scroll-to-top
           (:svg/chevron-top
            (@ :viewBox "0 0 32 32")
            (:path (@ :d "M15.997 13.374l-7.081 7.081L7 18.54l8.997-8.998 9.003 9-1.916 1.916z")))))
         (:div/items-home
          ,(mapcar #'mb-item
                   (nreverse
                    (seq-filter
                     (lambda (page)
                       (mb-is-episode-p (plist-get page :one-path)))
                     pages))))
         ,mb-footer)
        (:script ,mb-js-onscroll))))))

(defun mb-one-episode (page-tree pages global)
  "Render function of the episode pages."
  (let* ((title (org-element-property :raw-value page-tree))
         (path (org-element-property :CUSTOM_ID page-tree))
         (date (mb-date path))
         (youtube-id (thread-first
                       (org-element-property :MINIBUFFER_YOUTUBE_LINK page-tree)
                       (split-string "v=")
                       cadr))
         (youtube-embed-link (concat "https://www.youtube.com/embed/" youtube-id))
         (youtube-iframe
          `(:iframe
            (@
             :src ,(concat youtube-embed-link "?"
                           ;; "autoplay=1"
                           "&fs=1"      ; show the fullscreen button
                           "&modestbranding=1" ; don't show YouTube logo in bottom right
                           "&rel=0" ; show only related video from my channel
                           "&widget_referrer=https://minibuffer.tonyaldon.com" ; analytics
                           )
             :title ,(concat "YouTube video player playing " title)
             :frameborder "0"
             :allow "fullscreen;accelerometer;autoplay;clipboard-write;encrypted-media;gyroscope;picture-in-picture;web-share")))
         (content
          (org-export-data-with-backend
           (org-element-contents page-tree)
           'one-ox nil))
         (headlines (cdr (one-default-list-headlines page-tree)))
         (nav (one-default-nav path pages)))
    (jack-html
     "<!DOCTYPE html>"
     `(:html (@ :lang "en")
       (:head
        (:meta (@ :name "viewport" :content "width=device-width,initial-scale=1"))
        (:meta (@ :name "description"
                  :content "Learn Emacs Lisp One Sexp At A Time."))
        (:link (@ :rel "stylesheet" :type "text/css" :href "/main.css"))
        (:title ,title))
       (:body
        (:div/minibuffer "minibuffer")
        (:div/content
         (:div/header
          (:a (@ :href "/") (:div/minibuffer "minibuffer")))
         (:a (@ :href "#")
          (:div/scroll-to-top
           (:svg/chevron-top
            (@ :viewBox "0 0 32 32")
            (:path (@ :d "M15.997 13.374l-7.081 7.081L7 18.54l8.997-8.998 9.003 9-1.916 1.916z")))))
         (:div/columns
          (:div/primary
           ,youtube-iframe
           (:h1 ,title)
           (:div.date (@ :style "text-align:center; margin-bottom:16px;") ,date)
           ,content
           ,nav
           ,mb-footer)
          (:div/secondary
           (:div/items
            ,(mapcar (lambda (page) (mb-item page 'is-episode-p))
                     (seq-filter
                      (lambda (page)
                        (mb-is-episode-p (plist-get page :one-path)))
                      (mb-shuffle (vconcat pages))))))))
        (:script ,mb-js-onscroll))))))

;;; feed.xml

(defun mb-feed (pages tree global)
  "Produce file ./public/feed.xml"
  (with-temp-file "./public/feed.xml"
    (insert
     (jack-html
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
      `(:feed (@ :xmlns "http://www.w3.org/2005/Atom")
        (:title "minibuffer")
        (:link (@ :href "https://minibuffer.tonyaldon.com"))
        (:id "urn:minibuffer-tonyaldon-com:feed")
        (:updated (format-time-string "%FT%T%z"))
        (:author (:name "Tony Aldon"))
        ,(mapcar
          (lambda (page)
            (let* ((title (plist-get page :one-title))
                   (path (plist-get page :one-path))
                   (link (concat "https://minibuffer.tonyaldon.com" path)))
              (when (not (string= path "/"))
                (let ((date (substring path 1 11)))
                  `(:entry
                    (:title ,title)
                    (:link (@ :href ,link))
                    (:id ,(concat "urn:minibuffer-tonyaldon-com:" date))
                    (:updated ,(concat date "T00:00:00Z")))))))
          pages))))))

(add-hook 'one-hook 'mb-feed)
