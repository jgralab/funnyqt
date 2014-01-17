(ns funnyqt.bidi-test
  (:require [clojure.test :as test]
            [clojure.core.logic :as ccl]
            [funnyqt.bidi :as bidi]
            [funnyqt.relational :as r]
            [funnyqt.relational.tg :as rtg]
            [funnyqt.relational.emf :as remf]
            [funnyqt.tg :as tg]
            [funnyqt.emf :as emf]
            [funnyqt.visualization :as viz]
            [funnyqt.generic :as p]))

;;# AddressBook to AddressBook

(rtg/generate-schema-relations "test/input/addressbook.tg"
                               test.relational.addressbook.tg ab-tg)
(remf/generate-ecore-model-relations "test/input/AddressBook.ecore"
                                     test.relational.addressbook.emf ab-emf)

;;## Example AddressBook Graph

(defn make-example-addressbook-tg []
  (let [g (tg/new-graph (tg/load-schema "test/input/addressbook.tg"))
        ab (tg/create-vertex! g 'AddressBook :name "MyAddressBook")
        jim (tg/create-vertex! g 'Contact
                               :id (int 1)
                               :firstName "Jim"
                               :lastName "Jones"
                               :email "jim@gmail.com")
        tim (tg/create-vertex! g 'Contact
                               :id (int 2)
                               :firstName "Tim"
                               :lastName "Turner"
                               :email "tim@mozilla.org")
        steve (tg/create-vertex! g 'Contact
                                 :id (int 3)
                                 :firstName "Steve"
                                 :lastName "Stevenson"
                                 :email "steve@ibm.com")
        mozilla (tg/create-vertex! g 'Organization
                                   :id (int 4)
                                   :name "Mozilla Foundation"
                                   :homepage "www.mozilla.org"
                                   :employees [tim])
        ibm (tg/create-vertex! g 'Organization
                               :id (int 5)
                               :name "IBM"
                               :homepage "www.ibm.com"
                               :employees [steve tim])
        cat-work (tg/create-vertex! g 'Category :name "Work"
                                    :addressBook ab
                                    :contacts [steve]
                                    :organizations ibm)
        cat-private (tg/create-vertex! g 'Category :name "Private"
                                       :addressBook ab
                                       :contacts [jim tim]
                                       :organizations [mozilla])]
    g))

;;## Transformation TG <-> TG

(bidi/deftransformation addressbook-tg2addressbook-tg [[l r]]
  (^:top addressbook2addressbook
         :left [(ab-tg/AddressBook l ?addrbook1)
                (ab-tg/name l ?addrbook1 ?n)]
         :right [(ab-tg/AddressBook r ?addrbook2)
                 (ab-tg/name r ?addrbook2 ?n)]
         :where [(category2category :?ab1 ?addrbook1 :?ab2 ?addrbook2)])
  (category2category
   :left [(ab-tg/ContainsCategory l ?cc1 ?ab1 ?cat1)
          (ab-tg/Category l ?cat1)
          (ab-tg/name l ?cat1 ?n)]
   :right [(ab-tg/ContainsCategory r ?cc2 ?ab2 ?cat2)
           (ab-tg/Category r ?cat2)
           (ab-tg/name r ?cat2 ?n)]
   :where [(contact2contact :?cat1 ?cat1 :?cat2 ?cat2)
           (org2org :?cat1 ?cat1 :?cat2 ?cat2)])
  ;; The following 2 relations are of course non-sense.  They only serve to
  ;; check if the (transitive) :extends stuff works.
  (^:abstract have-same-ids3
              :left [(ab-tg/id l ?ex1 ?id)]
              :right [(ab-tg/id r ?ex2 ?id)])
  (^:abstract have-same-ids2
              :left [(ab-tg/id l ?e1 ?id)]
              :right [(ab-tg/id r ?e2 ?id)])
  (^:abstract have-same-ids
              :extends [(have-same-ids2 :?e1 ?entry1 :?e2 ?entry2)
                        (have-same-ids3 :?ex1 ?entry1 :?ex2 ?entry2)]
              :left [(ab-tg/id l ?entry1 ?id)]
              :right [(ab-tg/id r ?entry2 ?id)])
  (contact2contact
   :extends [(have-same-ids :?entry1 ?contact1 :?entry2 ?contact2)]
   :left [(ab-tg/->contacts l ?cat1 ?contact1)
          (ab-tg/Contact l ?contact1)
          (ab-tg/firstName l ?contact1 ?fn)
          (ab-tg/lastName l ?contact1 ?ln)
          (ab-tg/email l ?contact1 ?mail)]
   :right [(ab-tg/->contacts r ?cat2 ?contact2)
           (ab-tg/Contact r ?contact2)
           (ab-tg/firstName r ?contact2 ?fn)
           (ab-tg/lastName r ?contact2 ?ln)
           (ab-tg/email r ?contact2 ?mail)])
  (org2org
   :extends [(have-same-ids :?entry1 ?org1 :?entry2 ?org2)]
   :left [(ab-tg/ContainsOrganization l ?co1 ?cat1 ?org1)
          (ab-tg/Organization l ?org1)
          (ab-tg/homepage l ?org1 ?hp)
          (ab-tg/name l ?org1 ?n)]
   :right [(ab-tg/ContainsOrganization r ?co2 ?cat2 ?org2)
           (ab-tg/Organization r ?org2)
           (ab-tg/homepage r ?org2 ?hp)
           (ab-tg/name r ?org2 ?n)])
  (^:top connect-employees
         :when [(bidi/relateo org2org :?org1 ?org1 :?org2 ?org2)
                (bidi/relateo contact2contact :?contact1 ?contact1 :?contact2 ?contact2)]
         :left [(ab-tg/->employees l ?org1 ?contact1)]
         :right [(ab-tg/->employees r ?org2 ?contact2)]))

(defmacro assert-same-addressbooks-tg-tg [l r]
  `(let [l# ~l, r# ~r]
     (test/is (= (tg/vcount l# 'AddressBook)          (tg/vcount r# 'AddressBook)))
     (test/is (= (tg/vcount l# 'Category)             (tg/vcount r# 'Category)))
     (test/is (= (tg/ecount l# 'ContainsCategory)     (tg/ecount r# 'ContainsCategory)))
     (test/is (= (tg/vcount l# 'Contact)              (tg/vcount r# 'Contact)))
     (test/is (= (tg/ecount l# 'ContainsContact)      (tg/ecount r# 'ContainsContact)))
     (test/is (= (tg/vcount l# 'Organization)         (tg/vcount r# 'Organization)))
     (test/is (= (tg/ecount l# 'ContainsOrganization) (tg/ecount r# 'ContainsOrganization)))
     (test/is (= (tg/ecount l# 'HasEmployee)          (tg/ecount r# 'HasEmployee)))))

(test/deftest test-addressbook-tg2addressbook-tg
  (let [l (make-example-addressbook-tg)
        r (tg/new-graph (tg/load-schema "test/input/addressbook.tg"))]
    ;; Transform l to r
    (print "addressbook-tg2addressbook-tg l -> r (empty)                ")
    (time (addressbook-tg2addressbook-tg l r :right))
    (assert-same-addressbooks-tg-tg l r)
    ;; Do it again.  It shouldn't modify anything.
    (print "addressbook-tg2addressbook-tg l -> r (both already in sync) ")
    (time (addressbook-tg2addressbook-tg l r :right))
    (assert-same-addressbooks-tg-tg l r)
    ;; Do it in the other direction.  Again, it shouldn't modify anything.
    (print "addressbook-tg2addressbook-tg l <- r (both already in sync) ")
    (time (addressbook-tg2addressbook-tg l r :left))
    (assert-same-addressbooks-tg-tg l r)
    ;; Now add a new Contact to the right addressbook and synchronize it to the
    ;; left.
    (print "addressbook-tg2addressbook-tg l <- r (r has a new Contact)  ")
    (let [tim (tg/create-vertex! r 'Contact
                                 :id (int 6)
                                 :firstName "Tim"
                                 :lastName "Taylor"
                                 :email "tim@gmail.com")
          cat-work (first (filter #(= (tg/value % :name) "Work")
                                  (tg/vseq r 'Category)))]
      (p/add-adj! cat-work :contacts tim))
    (time (addressbook-tg2addressbook-tg l r :left))
    (assert-same-addressbooks-tg-tg l r)
    #_(do
        (future (viz/print-model l :gtk))
        (viz/print-model r :gtk))))

;;## Transformation TG <-> EMF

(bidi/deftransformation addressbook-tg2addressbook-emf [[l r]]
  (^:top addressbook2addressbook
         :left [(ab-tg/AddressBook l ?addrbook1)
                (ab-tg/name l ?addrbook1 ?n)]
         :right [(ab-emf/AddressBook r ?addrbook2)
                 (ab-emf/name r ?addrbook2 ?n)]
         :where [(category2category :?ab1 ?addrbook1 :?ab2 ?addrbook2)])
  (category2category
   :left [(ab-tg/ContainsCategory l ?cc ?ab1 ?cat1)
          #_(r/echo [?cc ?ab1 ?cat1])
          (ab-tg/Category l ?cat1)
          (ab-tg/name l ?cat1 ?n)]
   :right [(ab-emf/->categories r ?ab2 ?cat2)
           (ab-emf/Category r ?cat2)
           (ab-emf/name r ?cat2 ?n)]
   :where [(contact2contact :?cat1 ?cat1 :?cat2 ?cat2)
           (org2org :?cat1 ?cat1 :?cat2 ?cat2)])
  ;; The following 2 relations are of course non-sense.  They only serve to
  ;; check if the (transitive) :extends stuff works.
  (^:abstract have-same-ids3
              :left [(ab-tg/id l ?ex1 ?id)]
              :right [(ab-emf/id r ?ex2 ?id)])
  (^:abstract have-same-ids2
              :left [(ab-tg/id l ?e1 ?id)]
              :right [(ab-emf/id r ?e2 ?id)])
  (^:abstract have-same-ids
              :extends [(have-same-ids2 :?e1 ?entry1 :?e2 ?entry2)
                        (have-same-ids3 :?ex1 ?entry1 :?ex2 ?entry2)]
              :left [(ab-tg/id l ?entry1 ?id)]
              :right [(ab-emf/id r ?entry2 ?id)])
  (contact2contact
   :extends [(have-same-ids :?entry1 ?contact1 :?entry2 ?contact2)]
   :left [(ab-tg/->contacts l ?cat1 ?contact1)
          (ab-tg/Contact l ?contact1)
          (ab-tg/firstName l ?contact1 ?fn)
          (ab-tg/lastName l ?contact1 ?ln)
          (ab-tg/email l ?contact1 ?mail)]
   :right [(ab-emf/->entries r ?cat2 ?contact2)
           (ab-emf/Contact r ?contact2)
           (ab-emf/firstName r ?contact2 ?fn)
           (ab-emf/lastName r ?contact2 ?ln)
           (ab-emf/email r ?contact2 ?mail)])
  (org2org
   :extends [(have-same-ids :?entry1 ?org1 :?entry2 ?org2)]
   :left [(ab-tg/->organizations l ?cat1 ?org1)
          (ab-tg/Organization l ?org1)
          (ab-tg/homepage l ?org1 ?hp)
          (ab-tg/name l ?org1 ?n)]
   :right [(ab-emf/->entries r ?cat2 ?org2)
           (ab-emf/Organization r ?org2)
           (ab-emf/homepage r ?org2 ?hp)
           (ab-emf/name r ?org2 ?n)])
  (^:top connect-employees
         :when [(bidi/relateo org2org :?org1 ?org1 :?org2 ?org2)
                (bidi/relateo contact2contact :?contact1 ?contact1 :?contact2 ?contact2)]
         :left [(ab-tg/->employees l ?org1 ?contact1)]
         :right [(ab-emf/->employees r ?org2 ?contact2)]))

(defmacro assert-same-addressbooks-tg-emf [l r]
  `(let [l# ~l, r# ~r]
     (test/is (= (tg/vcount l# 'AddressBook)
                 (count (emf/eallobjects r# 'AddressBook))))
     (test/is (= (tg/vcount l# 'Category)
                 (count (emf/eallobjects r# 'Category))))
     (test/is (= (tg/ecount l# 'ContainsCategory)
                 (count (emf/eallpairs r# :addressBook :categories))))
     (test/is (= (tg/vcount l# 'Contact)
                 (count (emf/eallobjects r# 'Contact))))
     (test/is (= (tg/vcount l# 'Organization)
                 (count (emf/eallobjects r# 'Organization))))
     (test/is (= (+ (tg/ecount l# 'ContainsContact)
                    (tg/ecount l# 'ContainsOrganization))
                 (count (emf/eallpairs r# :category :entries))))
     (test/is (= (tg/ecount l# 'HasEmployee)
                 (count (emf/eallpairs r# :employers :employees))))))

(test/deftest test-addressbook-tg2addressbook-emf
  (let [l (make-example-addressbook-tg)
        r (emf/new-resource)]
    ;; Transform l to r
    (print "addressbook-tg2addressbook-emf l -> r (empty)                ")
    (time (addressbook-tg2addressbook-emf l r :right))
    (assert-same-addressbooks-tg-emf l r)
    ;; Do it again.  It shouldn't modify anything.
    (print "addressbook-tg2addressbook-emf l -> r (both already in sync) ")
    (time (addressbook-tg2addressbook-emf l r :right))
    (assert-same-addressbooks-tg-emf l r)
    ;; Do it in the other direction.  Again, it shouldn't modify anything.
    (print "addressbook-tg2addressbook-emf l <- r (both already in sync) ")
    (time (addressbook-tg2addressbook-emf l r :left))
    (assert-same-addressbooks-tg-emf l r)
    ;; Now add a new Contact to the right addressbook and synchronize it to the
    ;; left.
    (print "addressbook-tg2addressbook-emf l <- r (r has a new Contact)  ")
    (let [tim (emf/ecreate! nil 'Contact
                            :id (int 6)
                            :firstName "Tim"
                            :lastName "Taylor"
                            :email "tim@gmail.com")
          cat-work (first (filter #(= (emf/eget % :name) "Work")
                                  (emf/eallobjects r 'Category)))]
      (p/add-adj! cat-work :entries tim))
    (time (addressbook-tg2addressbook-emf l r :left))
    (assert-same-addressbooks-tg-emf l r)
    #_(do
        (future (viz/print-model l :gtk))
        (viz/print-model r :gtk))))

;;# UML Class Diagram to RDBMS Tables

(remf/generate-ecore-model-relations "test/input/uml-rdbms-bidi/classdiagram.ecore"
                                     test.relational.classdiagram.emf cd)
(remf/generate-ecore-model-relations "test/input/uml-rdbms-bidi/database.ecore"
                                     test.relational.database.emf db)

(def cd1 (emf/load-resource "test/input/uml-rdbms-bidi/m1/classdiagram01.xmi"))
(def db1 (emf/load-resource "test/input/uml-rdbms-bidi/m2/database01.xmi"))

(comment
  (ccl/run* [q]
    (ccl/fresh [c v]
      (cd/is-persistent cd1 c v)
      (ccl/== q [c v]))))

(bidi/deftransformation class-diagram2database-schema
  "Transforms between class diagrams and database schemas."
  [[cd db]]
  (^:top package2schema
         :left [(cd/Package cd ?pkg)
                (cd/name cd ?pkg ?name)]
         :right [(db/Schema db ?schema)
                 (db/name db ?schema ?name)]
         :where [(class2table :?pkg ?pkg :?schema ?schema)])
  (class2table
   :left [(cd/->classifiers cd ?pkg ?class)
          (cd/Class cd ?class)
          (cd/is-persistent cd ?class true)
          (cd/name cd ?class ?name)]
   :right [(db/->tables db ?schema ?table)
           (db/Table db ?table)
           (db/name db ?table ?name)]
   :where [(attribute2column :?class ?class :?table ?table)])
  (attribute2column
   :left [(cd/->attrs cd ?class ?attr)
          (cd/Attribute cd ?attr)
          (cd/name cd ?attr ?name)]
   :right [(db/->cols db ?table ?col)
           (db/Column db ?col)
           (db/name db ?col ?name)]
   :where [(primary2pkey :?attr ?attr :?table ?table :?col ?col)])
  (primary2pkey
   :left [(cd/is-primary cd ?attr true)]
   :right [(db/->pkey db ?table ?col)])
  (^:top super-attribute2column
         :only :right
         :when [(cd/->parent cd ?subclass ?superclass)
                (ccl/conde
                 [(bidi/relateo class2table :?class ?subclass :?table ?table)]
                 [(bidi/relateo super-attribute2column :?superclass ?subclass :?table ?table)])]
         :where [(attribute2column :?class ?superclass :?table ?table)
                 (super-attribute2column :?subclass ?superclass :?table ?table)]))

(test/deftest test-cd2db
  (let [result-db (emf/new-resource)]
    (class-diagram2database-schema cd1 result-db :right)
    (test/is (= 1 (count (emf/eallobjects result-db 'Schema))))
    (test/is (= 2 (count (emf/eallobjects result-db 'Table))))
    (test/is (= 7 (count (emf/eallobjects result-db 'Column))))
    #_(viz/print-model result-db :gtk)))

(test/deftest test-db2cd
  (let [result-cd (emf/new-resource)]
    (class-diagram2database-schema result-cd db1 :left)
    (test/is (= 1 (count (emf/eallobjects result-cd 'Package))))
    (test/is (= 2 (count (emf/eallobjects result-cd 'Class))))
    (test/is (= 8 (count (emf/eallobjects result-cd 'Attribute))))
    #_(viz/print-model result-cd :gtk)))

(bidi/deftransformation ^{:extends [class-diagram2database-schema]}
  class-diagram2database-schema-ext
  "This transformation extends class-diagram2database-schema.  It only
  overrides class2table with the very same definition plus a
  always-succeeding :when clause, and uses different model parameter names."
  [[l r]]
  (class2table
   :when [ccl/succeed]
   :left [(cd/->classifiers l ?pkg ?class)
          (cd/Class l ?class)
          (cd/is-persistent l ?class true)
          (cd/name l ?class ?name)]
   :right [(db/->tables r ?schema ?table)
           (db/Table r ?table)
           (db/name r ?table ?name)]
   :where [(attribute2column :?class ?class :?table ?table)]))

(test/deftest test-cd2db-ext
  (let [result-db (emf/new-resource)]
    (class-diagram2database-schema-ext cd1 result-db :right)
    (test/is (= 1 (count (emf/eallobjects result-db 'Schema))))
    (test/is (= 2 (count (emf/eallobjects result-db 'Table))))
    (test/is (= 7 (count (emf/eallobjects result-db 'Column))))
    #_(viz/print-model result-db :gtk)))

(test/deftest test-db2cd-ext
  (let [result-cd (emf/new-resource)]
    (class-diagram2database-schema-ext result-cd db1 :left)
    (test/is (= 1 (count (emf/eallobjects result-cd 'Package))))
    (test/is (= 2 (count (emf/eallobjects result-cd 'Class))))
    (test/is (= 8 (count (emf/eallobjects result-cd 'Attribute))))
    #_(viz/print-model result-cd :gtk)))
