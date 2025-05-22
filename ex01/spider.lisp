(in-package :spider)

;;; メインのSpiderクラスと再帰的クローリングロジック

(defclass spider ()
  ((start-url :initarg :start-url :accessor start-url
              :documentation "クロールを開始するURL")
   (recursive :initarg :recursive :accessor recursive
              :initform nil
              :documentation "再帰的にクロールするかどうか")
   (max-depth :initarg :max-depth :accessor max-depth
              :initform 5
              :documentation "再帰的クロールの最大深度")
   (save-path :initarg :save-path :accessor save-path
              :initform "./data/"
              :documentation "画像の保存先パス")
   (visited-urls :initform (make-hash-table :test 'equal) :accessor visited-urls
                 :documentation "訪問済みURLを記録するハッシュテーブル")))

(defun make-spider (url &key recursive max-depth save-path)
  "Spiderクラスのインスタンスを作成する"
  (let ((spider (make-instance 'spider
                               :start-url url
                               :recursive recursive
                               :max-depth (or max-depth 5)
                               :save-path (or save-path "./data/"))))
    
    ;; 保存先ディレクトリの作成
    (ensure-directory-exists (save-path spider))
    
    ;; 絶対パスに変換
    (setf (save-path spider) (truename (save-path spider)))
    
    (info-log "Spider initialized with URL: ~A" url)
    (info-log "Recursive: ~A, Max depth: ~A" recursive (max-depth spider))
    (info-log "Save path: ~A" (save-path spider))
    
    spider))

(defun crawl (spider)
  "クロール処理を開始する"
  (info-log "Starting crawl process...")
  (crawl-url spider (start-url spider) 0)
  (info-log "Crawl process completed"))

(defun crawl-url (spider url depth)
  "指定されたURLをクロールする再帰関数"
  ;; 最大深度チェック
  (when (> depth (max-depth spider))
    (return-from crawl-url))
  
  ;; 既に訪問済みのURLはスキップ
  (when (gethash url (visited-urls spider))
    (return-from crawl-url))
  
  ;; URLを訪問済みとしてマーク
  (setf (gethash url (visited-urls spider)) t)
  
  (info-log "Crawling URL: ~A (depth: ~A)" url depth)
  
  (handler-case
      (progn
        ;; URLからHTMLコンテンツを取得
        (let ((html-content (fetch-url url)))
          (when html-content
            ;; HTMLからリンクと画像URLを抽出
            (multiple-value-bind (links image-urls)
                (parse-html html-content url)
              
              ;; 画像をダウンロード
              (let ((downloaded-count (download-images image-urls (save-path spider))))
                (info-log "Downloaded ~A images from ~A" downloaded-count url))
              
              ;; 再帰的クロールが有効な場合、リンクを辿る
              (when (and (recursive spider) (< depth (max-depth spider)))
                (dolist (link links)
                  ;; 同じドメイン内のリンクのみ辿る
                  (when (same-domain-p url link)
                    (crawl-url spider link (1+ depth)))))))))
    
    (error (e)
      (error-log "Error crawling ~A: ~A" url e))))
