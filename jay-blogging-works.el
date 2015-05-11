

(require 'xml-rpc)
(setq org2blog/wp-blog-alist
      '(
        ("prolific"
         :url "http://prolific.dixit.ca/xmlrpc.php"
         :username "jay"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("random")
         :tags-as-categories t)



        ("gf"
         :url "http://greenfield.dixit.ca/xmlrpc.php"
         :username "jay"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("Found")
         :tags-as-categories t)

        ("jd"
         :url "http://jaydixit.com/wordpress/xmlrpc.php"
         :username "admin"
	 :password "ca9e011jd"
         :default-title "Hello World"
         :default-categories ("Found")
         :tags-as-categories t)

	("newyorkwritersintensive"
         :url "http://www.newyorkwritersintensive.com/xmlrpc.php"
         :username "admin"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("Readings")
         :tags-as-categories t)


        ("prolific"
         :url "http://prolific.dixit.ca/xmlrpc.php"
         :username "jay"
	 :password "resistance/1942/"
         :default-title "Hello World"
         :default-categories ("Found")
         :tags-as-categories t)
	))

