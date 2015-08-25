(ns funnyqt.bidi-test
  (:require [clojure.core.logic :as ccl]
            [clojure.test :as test]
            [funnyqt
             [bidi :as bidi]
             [emf :as emf]
             [generic :as g]
             [relational :as r]
             [tg :as tg]
             [visualization :as viz]]))

;;# AddressBook to AddressBook

(r/generate-metamodel-relations "test/input/addressbook.tg"
                                test.relational.addressbook.tg ab-tg)
(r/generate-metamodel-relations "test/input/AddressBook.ecore"
                                test.relational.addressbook.emf ab-emf)

(emf/load-ecore-resource "test/input/AddressBook.ecore")

;;## Example AddressBook Graph

(defn make-example-addressbook-tg []
  (let [g (tg/new-graph (tg/load-schema "test/input/addressbook.tg"))
        ab (tg/create-vertex! g 'AddressBook {:name "MyAddressBook"})
        jim (tg/create-vertex! g 'Contact
                               {:id (int 1)
                                :firstName "Jim"
                                :lastName "Jones"
                                :email "jim@gmail.com"})
        tim (tg/create-vertex! g 'Contact
                               {:id (int 2)
                                :firstName "Tim"
                                :lastName "Turner"
                                :email "tim@mozilla.org"})
        steve (tg/create-vertex! g 'Contact
                                 {:id (int 3)
                                  :firstName "Steve"
                                  :lastName "Stevenson"
                                  :email "steve@ibm.com"})
        mozilla (tg/create-vertex! g 'Organization
                                   {:id (int 4)
                                    :name "Mozilla Foundation"
                                    :homepage "www.mozilla.org"
                                    :employees [tim]})
        ibm (tg/create-vertex! g 'Organization
                               {:id (int 5)
                                :name "IBM"
                                :homepage "www.ibm.com"
                                :employees [steve tim]})
        cat-work (tg/create-vertex! g 'Category
                                    {:name "Work"
                                     :addressBook ab
                                     :contacts [steve]
                                     :organizations ibm})
        cat-private (tg/create-vertex! g 'Category
                                       {:name "Private"
                                        :addressBook ab
                                        :contacts [jim tim]
                                        :organizations [mozilla]})]
    g))

;;## Transformation TG <-> TG

(bidi/deftransformation addressbook-tg2addressbook-tg [l r]
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
         :when [(org2org :?org1 ?org1 :?org2 ?org2)
                (contact2contact :?contact1 ?contact1 :?contact2 ?contact2)]
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
    ;; l to r checkonly
    (print "addressbook-tg2addressbook-tg l -> r (empty, checkonly)     ")
    (let [t0 (time (addressbook-tg2addressbook-tg l r :right-checkonly))
          unrelated (:unrelated t0)]
      (test/is (= {} (:related t0)))
      (test/is (= {:addressbook2addressbook
                   #{{:?addrbook1 (tg/first-vertex l #(g/has-type? % 'AddressBook))
                      :?n "MyAddressBook"}}}
                  unrelated)))
    ;; Transform l to r
    (print "addressbook-tg2addressbook-tg l -> r (empty)                ")
    (let [t1 (time (addressbook-tg2addressbook-tg l r :right))
          t11 (addressbook-tg2addressbook-tg l r :right-checkonly)]
      (assert-same-addressbooks-tg-tg l r)
      (test/is (= t1 t11))
      ;; Do it again.  It shouldn't modify anything.
      (print "addressbook-tg2addressbook-tg l -> r (both already in sync) ")
      (let [t2 (time (addressbook-tg2addressbook-tg l r :right))
            t22 (addressbook-tg2addressbook-tg l r :right-checkonly)]
        (assert-same-addressbooks-tg-tg l r)
        (test/is (= t1 t2 t22))))
    ;; Do it in the other direction.  Again, it shouldn't modify anything.
    (print "addressbook-tg2addressbook-tg l <- r (both already in sync) ")
    (let [t3 (time (addressbook-tg2addressbook-tg l r :left))
          t33 (addressbook-tg2addressbook-tg l r :left-checkonly)]
      (test/is (= t3 t33))
      (assert-same-addressbooks-tg-tg l r))
    ;; Now add a new Contact to the right addressbook and synchronize it to the
    ;; left.
    (print "addressbook-tg2addressbook-tg l <- r (r has a new Contact)  ")
    (let [tim (tg/create-vertex! r 'Contact
                                 {:id (int 6)
                                  :firstName "Tim"
                                  :lastName "Taylor"
                                  :email "tim@gmail.com"})
          cat-work (first (filter #(= (tg/value % :name) "Work")
                                  (tg/vseq r 'Category)))]
      (g/add-adj! cat-work :contacts tim))
    (let [t4 (time (addressbook-tg2addressbook-tg l r :left))
          t44 (addressbook-tg2addressbook-tg l r :left-checkonly)]
      (test/is (= t4 t44))
      (assert-same-addressbooks-tg-tg l r))))

;;## Transformation TG <-> EMF

(bidi/deftransformation addressbook-tg2addressbook-emf [l r]
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
         :when [(org2org :?org1 ?org1 :?org2 ?org2)
                (contact2contact :?contact1 ?contact1 :?contact2 ?contact2)]
         :left [(ab-tg/->employees l ?org1 ?contact1)]
         :right [(ab-emf/->employees r ?org2 ?contact2)]))

(defmacro assert-same-addressbooks-tg-emf [l r]
  `(let [l# ~l, r# ~r]
     (test/is (= (tg/vcount l# 'AddressBook)
                 (count (emf/eallcontents r# 'AddressBook))))
     (test/is (= (tg/vcount l# 'Category)
                 (count (emf/eallcontents r# 'Category))))
     (test/is (= (tg/ecount l# 'ContainsCategory)
                 (count (emf/epairs r# :addressBook :categories))))
     (test/is (= (tg/vcount l# 'Contact)
                 (count (emf/eallcontents r# 'Contact))))
     (test/is (= (tg/vcount l# 'Organization)
                 (count (emf/eallcontents r# 'Organization))))
     (test/is (= (+ (tg/ecount l# 'ContainsContact)
                    (tg/ecount l# 'ContainsOrganization))
                 (count (emf/epairs r# :category :entries))))
     (test/is (= (tg/ecount l# 'HasEmployee)
                 (count (emf/epairs r# :employers :employees))))))

(test/deftest test-addressbook-tg2addressbook-emf
  (let [l (make-example-addressbook-tg)
        r (emf/new-resource)]
    ;; Transform l to r
    (print "addressbook-tg2addressbook-emf l -> r (empty)                ")
    (let [t (time (addressbook-tg2addressbook-emf l r :right))
          t1 (addressbook-tg2addressbook-emf l r :right-checkonly)]
      (test/is (= t t1))
      (assert-same-addressbooks-tg-emf l r))
    ;; Do it again.  It shouldn't modify anything.
    (print "addressbook-tg2addressbook-emf l -> r (both already in sync) ")
    (let [t (time (addressbook-tg2addressbook-emf l r :right))
          t1 (addressbook-tg2addressbook-emf l r :right-checkonly)]
      (test/is (= t t1))
      (assert-same-addressbooks-tg-emf l r))
    ;; Do it in the other direction.  Again, it shouldn't modify anything.
    (print "addressbook-tg2addressbook-emf l <- r (both already in sync) ")
    (let [t (time (addressbook-tg2addressbook-emf l r :left))
          t1 (addressbook-tg2addressbook-emf l r :left-checkonly)]
      (test/is (= t t1))
      (assert-same-addressbooks-tg-emf l r))
    ;; Now add a new Contact to the right addressbook and synchronize it to the
    ;; left.
    (print "addressbook-tg2addressbook-emf l <- r (r has a new Contact)  ")
    (let [tim (emf/ecreate! nil 'Contact
                            {:id (int 6)
                             :firstName "Tim"
                             :lastName "Taylor"
                             :email "tim@gmail.com"})
          cat-work (first (filter #(= (emf/eget % :name) "Work")
                                  (emf/eallcontents r 'Category)))]
      (g/add-adj! cat-work :entries tim))
    (let [t (time (addressbook-tg2addressbook-emf l r :left))
          t1 (addressbook-tg2addressbook-emf l r :left-checkonly)]
      (test/is (= t t1))
      (assert-same-addressbooks-tg-emf l r))))

;;## Tests for attribute modifications (TG <-> TG)

(bidi/deftransformation attr-override-contact-tg2contact-tg [l r]
  (^:top contact2contact
         :left [(ab-tg/Contact l ?contact1)
                (ab-tg/firstName l ?contact1 ?fn)
                (ab-tg/lastName l ?contact1 ?ln)
                (ab-tg/email* l ?contact1 ?mail)]
         :right [(ab-tg/Contact r ?contact2)
                 (ab-tg/firstName r ?contact2 ?fn)
                 (ab-tg/lastName r ?contact2 ?ln)
                 (ab-tg/email* r ?contact2 ?mail)]))

(test/deftest test-attr-override-contact-tg2contact-tg
  (let [l (tg/new-graph (tg/load-schema "test/input/addressbook.tg"))
        r (tg/new-graph (tg/load-schema "test/input/addressbook.tg"))
        get-john (fn [g]
                   (first (filter #(and
                                    (= "John" (tg/value % :firstName))
                                    (= "Doe"  (tg/value % :lastName)))
                                  (tg/vseq g 'Contact))))]
    (tg/create-vertex! l 'Contact {:firstName "John" :lastName "Doe"
                                   :email "jdoe@yahoo.com"})
    (attr-override-contact-tg2contact-tg l r :right)
    (test/is (= 1 (tg/vcount r)))
    (let [john (get-john r)]
      (test/is (= "John" (tg/value john :firstName)))
      (test/is (= "Doe" (tg/value john :lastName)))
      (test/is (= "jdoe@yahoo.com" (tg/value john :email)))
      ;; Now change John's email address!
      (tg/set-value! john :email "jdoe@gmail.com"))
    ;; Propagate the changed email address back to the source model l
    (attr-override-contact-tg2contact-tg l r :left)
    ;; No new vertex must have been created.
    (test/is (= 1 (tg/vcount l)))
    (let [john (get-john l)]
      (test/is (= "John" (tg/value john :firstName)))
      (test/is (= "Doe" (tg/value john :lastName)))
      (test/is (= "jdoe@gmail.com" (tg/value john :email))))))

;;## Tests for attribute modifications (EMF <-> EMF)

(bidi/deftransformation attr-override-contact-emf2contact-emf [l r]
  (^:top contact2contact
   :left [(ab-emf/Contact l ?contact1)
          (ab-emf/firstName l ?contact1 ?fn)
          (ab-emf/lastName l ?contact1 ?ln)
          (ab-emf/email* l ?contact1 ?mail)]
   :right [(ab-emf/Contact r ?contact2)
           (ab-emf/firstName r ?contact2 ?fn)
           (ab-emf/lastName r ?contact2 ?ln)
           (ab-emf/email* r ?contact2 ?mail)]))

(test/deftest test-attr-override-contact-emf2contact-emf
  (let [l (emf/new-resource)
        r (emf/new-resource)
        get-john (fn [m]
                   (first (filter #(and
                                    (= "John" (emf/eget % :firstName))
                                    (= "Doe"  (emf/eget % :lastName)))
                                  (emf/eallcontents m 'Contact))))]
    (emf/ecreate! l 'Contact {:firstName "John" :lastName "Doe"
                              :email "jdoe@yahoo.com"})
    (attr-override-contact-emf2contact-emf l r :right)
    (test/is (= 1 (count (emf/eallcontents r))))
    (let [john (get-john r)]
      (test/is (= "John" (emf/eget john :firstName)))
      (test/is (= "Doe" (emf/eget john :lastName)))
      (test/is (= "jdoe@yahoo.com" (emf/eget john :email)))
      ;; Now change John's email address!
      (emf/eset! john :email "jdoe@gmail.com"))
    ;; Propagate the changed email address back to the source model l
    (attr-override-contact-emf2contact-emf l r :left)
    ;; No new vertex must have been created.
    (test/is (= 1 (count (emf/eallcontents l))))
    (let [john (get-john l)]
      (test/is (= "John" (emf/eget john :firstName)))
      (test/is (= "Doe" (emf/eget john :lastName)))
      (test/is (= "jdoe@gmail.com" (emf/eget john :email))))))

;;## Tests for single-valued role modifications (TG <-> TG)

(bidi/deftransformation single-valued-role-override-ab-tg2ab-tg [l r]
  (^:top addressbook2addressbook
         :left [(ab-tg/AddressBook l ?addrbook1)
                (ab-tg/name l ?addrbook1 ?n)]
         :right [(ab-tg/AddressBook r ?addrbook2)
                 (ab-tg/name r ?addrbook2 ?n)]
         :where [(category2category :?ab1 ?addrbook1 :?ab2 ?addrbook2)])
  (category2category
   :left [(ab-tg/Category l ?cat1)
          (ab-tg/->addressBook* l ?cat1 ?ab1)
          (ab-tg/name l ?cat1 ?n)]
   :right [(ab-tg/Category r ?cat2)
           (ab-tg/->addressBook* r ?cat2 ?ab2)
           (ab-tg/name r ?cat2 ?n)]))

(test/deftest test-single-valued-role-override-ab-tg2ab-tg
  (let [l (tg/new-graph (tg/load-schema "test/input/addressbook.tg"))
        r (tg/new-graph (tg/load-schema "test/input/addressbook.tg"))
        get-private (fn [g]
                      (first (filter #(= "Private" (tg/value % :name))
                                     (tg/vseq g 'Category))))]
    (tg/create-vertex! l 'AddressBook
                       {:name "AB1"
                        :categories [(tg/create-vertex! l 'Category {:name "Private"})]})
    (single-valued-role-override-ab-tg2ab-tg l r :right)
    (test/is (= 2 (tg/vcount r)))
    (let [priv (get-private r)]
      (test/is (= "Private" (tg/value priv :name)))
      ;; Now create a new AddressBook AB2 and assign Private there.
      (let [ab2 (tg/create-vertex! r 'AddressBook {:name "AB2"})]
        (tg/unlink! priv)
        (g/add-adj! ab2 :categories priv)))
    ;; Propagate the change back to the left model where Private should be
    ;; reassigned to the AB2 AddressBook.
    (single-valued-role-override-ab-tg2ab-tg l r :left)
    ;;(viz/print-model l "/home/horn/l.pdf")
    ;; Now we have 3 vertices: 2 AddressBooks and one Contact.
    (test/is (= 3 (tg/vcount l)))
    (let [priv (get-private l)]
      (test/is (= "Private" (tg/value priv :name)))
      (test/is (== 1 (count (g/adjs priv :addressBook)))))))

;;## Tests for single-valued role modifications (TG <-> TG)

(bidi/deftransformation single-valued-role-override-ab-emf2ab-emf [l r]
  (^:top addressbook2addressbook
         :left [(ab-emf/AddressBook l ?addrbook1)
                (ab-emf/name l ?addrbook1 ?n)]
         :right [(ab-emf/AddressBook r ?addrbook2)
                 (ab-emf/name r ?addrbook2 ?n)]
         :where [(category2category :?ab1 ?addrbook1 :?ab2 ?addrbook2)])
  (category2category
   :left [(ab-emf/Category l ?cat1)
          (ab-emf/->addressBook* l ?cat1 ?ab1)
          (ab-emf/name l ?cat1 ?n)]
   :right [(ab-emf/Category r ?cat2)
           (ab-emf/->addressBook* r ?cat2 ?ab2)
           (ab-emf/name r ?cat2 ?n)]))

(test/deftest test-single-valued-role-override-ab-emf2ab-emf
  (let [l (emf/new-resource)
        r (emf/new-resource)
        get-private (fn [m]
                      (first (filter #(= "Private" (emf/eget % :name))
                                     (emf/eallcontents m 'Category))))]
    (emf/ecreate! l 'AddressBook
                  {:name "AB1"
                   :categories [(emf/ecreate! l 'Category {:name "Private"})]})
    (single-valued-role-override-ab-emf2ab-emf l r :right)
    (test/is (= 2 (count (emf/eallcontents r))))
    (let [priv (get-private r)]
      (test/is (= "Private" (emf/eget priv :name)))
      ;; Now create a new AddressBook AB2 and assign Private there.
      (let [ab2 (emf/ecreate! r 'AddressBook {:name "AB2"})]
        (emf/eset! ab2 :categories [priv])))
    ;; Propagate the change back to the left model where Private should be
    ;; reassigned to the AB2 AddressBook.
    (single-valued-role-override-ab-emf2ab-emf l r :left)
    ;; Now we have 3 objs: 2 AddressBooks and one Contact.
    (test/is (= 3 (count (emf/eallcontents l))))
    (let [priv (get-private l)]
      (test/is (= "Private" (emf/eget priv :name)))
      (test/is (== 1 (count (g/adjs priv :addressBook)))))))


;;# UML Class Diagram to RDBMS Tables

(r/generate-metamodel-relations "test/input/uml-rdbms-bidi/classdiagram.ecore"
                                test.relational.classdiagram.emf cd)
(r/generate-metamodel-relations "test/input/uml-rdbms-bidi/database.ecore"
                                test.relational.database.emf db)

(emf/load-ecore-resource "test/input/uml-rdbms-bidi/classdiagram.ecore")
(emf/load-ecore-resource "test/input/uml-rdbms-bidi/database.ecore")

(def cd1 (emf/load-resource "test/input/uml-rdbms-bidi/m1/classdiagram01.xmi"))
(def db1 (emf/load-resource "test/input/uml-rdbms-bidi/m2/database01.xmi"))

(comment
  (ccl/run* [q]
    (ccl/fresh [c v]
      (cd/is-persistent cd1 c v)
      (ccl/== q [c v]))))

(bidi/deftransformation class-diagram2database-schema
  "Transforms between class diagrams and database schemas."
  [cd db]
  (^:top package2schema
         :left [(cd/Package cd ?pkg)
                (cd/name cd ?pkg ?name)]
         :right [(db/Schema db ?schema)
                 (db/name db ?schema ?name)]
         :where [(class2table :?pkg ?pkg :?schema ?schema)])
  (this-is-just-a-normal-relation [a b c]
                                  (== a b c)
                                  (== 1 b))
  (and-another-one [a b c]
                   (== a b c)
                   (== 1 b))
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
         :when [(cd/->parent cd ?subclass ?superclass)
                (ccl/conde
                 [(class2table :?class ?subclass :?table ?table)]
                 [(super-attribute2column :?superclass ?subclass :?table ?table)])]
         :where [(attribute2column :?class ?superclass :?table ?table)
                 (super-attribute2column :?subclass ?superclass :?table ?table)]))

(test/deftest test-cd2db
  (let [result-db (emf/new-resource)
        t0 (class-diagram2database-schema cd1 result-db :right)
        t1 (class-diagram2database-schema cd1 result-db :right-checkonly)
        ;; FIXME: The back transformation should not copy the flattened,
        ;; inherited attributes...
        ;;t2 (class-diagram2database-schema cd1 result-db :left)
        ]
    (test/is (= t0 t1))
    (test/is (= 1 (count (emf/eallcontents result-db 'Schema))))
    (test/is (= 2 (count (emf/eallcontents result-db 'Table))))
    (test/is (= 7 (count (emf/eallcontents result-db 'Column))))
    #_(viz/print-model cd1 :gtk)
    #_(viz/print-model result-db :gtk)))

(test/deftest test-db2cd
  (let [result-cd (emf/new-resource)
        t0 (class-diagram2database-schema result-cd db1 :left)
        t1 (class-diagram2database-schema result-cd db1 :left-checkonly)]
    (test/is (= t0 t1))
    (test/is (= 1 (count (emf/eallcontents result-cd 'Package))))
    (test/is (= 2 (count (emf/eallcontents result-cd 'Class))))
    (test/is (= 8 (count (emf/eallcontents result-cd 'Attribute))))
    #_(viz/print-model result-cd :gtk)))

(bidi/deftransformation class-diagram2database-schema-ext
  "This transformation extends class-diagram2database-schema.  It only
  overrides class2table with the very same definition plus a
  always-succeeding :when clause, and uses different model parameter names."
  [l r]
  :extends [class-diagram2database-schema]
  (this-is-just-a-normal-relation [a b c]
                                  (== a "I overwrite the inherited one"))
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
  (let [result-db (emf/new-resource)
        t0 (class-diagram2database-schema-ext cd1 result-db :right)
        t1 (class-diagram2database-schema-ext cd1 result-db :right-checkonly)]
    (test/is (= t0 t1))
    (test/is (= 1 (count (emf/eallcontents result-db 'Schema))))
    (test/is (= 2 (count (emf/eallcontents result-db 'Table))))
    (test/is (= 7 (count (emf/eallcontents result-db 'Column))))
    #_(viz/print-model result-db :gtk)))

(test/deftest test-db2cd-ext
  (let [result-cd (emf/new-resource)
        t0 (class-diagram2database-schema-ext result-cd db1 :left)
        t1 (class-diagram2database-schema-ext result-cd db1 :left-checkonly)]
    (test/is (= t0 t1))
    (test/is (= 1 (count (emf/eallcontents result-cd 'Package))))
    (test/is (= 2 (count (emf/eallcontents result-cd 'Class))))
    (test/is (= 8 (count (emf/eallcontents result-cd 'Attribute))))
    #_(viz/print-model result-cd :gtk)))

;;* Simple Class Diagram to Simple Database Schema

(r/generate-metamodel-relations "test/input/cd2db-simple/cd-schema.tg"
                                test.relational.simple-cd.tg scd)
(r/generate-metamodel-relations "test/input/cd2db-simple/db-schema.tg"
                                test.relational.simple-db.tg sdb)

(defn gen-simple-class-diagram []
  (let [g (tg/new-graph (tg/load-schema "test/input/cd2db-simple/cd-schema.tg"))
        cls-a (tg/create-vertex! g 'Class {:name "A"})
        attr-a (tg/create-vertex! g 'Attribute {:name "a", :class cls-a,
                                                :type (tg/enum-constant g 'AttributeTypes.STRING)})
        cls-a-sub (tg/create-vertex! g 'Class {:name "ASub", :superclass cls-a})
        attr-a-sub (tg/create-vertex! g 'Attribute {:name "asub", :class cls-a-sub,
                                                    :type (tg/enum-constant g 'AttributeTypes.LONG)})
        cls-b (tg/create-vertex! g 'Class {:name "B"})
        attr-b (tg/create-vertex! g 'Attribute {:name "b", :class cls-b,
                                                :type (tg/enum-constant g 'AttributeTypes.FLOAT)})
        assoc-a2b (tg/create-vertex! g 'Association {:name "A2B", :src cls-a, :trg cls-b})]
    ;;(viz/print-model g :gtk)
    g))

(bidi/deftransformation class-diagram2database-schema-simple [l r]
  (^:top class2table
         :left [(scd/Class l ?cls)
                (scd/name l ?cls ?name)]
         :right [(sdb/Table r ?table)
                 (sdb/name r ?table ?name)
                 (sdb/->cols r ?table ?col)
                 (sdb/name r ?col "ID")
                 (sdb/primary r ?col true)
                 (r/enum-constanto r 'ColumnTypes.INTEGER ?enum-const)
                 (sdb/type r ?col ?enum-const)]
         :where [(generalization2foreign-key :?supercls ?cls :?table ?table :?col ?col)
                 (attribute2column :?cls ?cls :?table ?table)])
  (generalization2foreign-key
   :when  [(class2table :?cls ?supercls :?table ?table :?col ?col)
           (class2table :?cls ?subcls :?table ?subtable :?col ?subcol)]
   :left  [(scd/->superclass l ?subcls ?supercls)]
   :right [(sdb/->pkey r ?subcol ?col)])
  (cd-type2db-type [cdt dbt]
    (ccl/fresh [cdn dbn]
      (ccl/conde
       [(ccl/all (ccl/== cdn 'AttributeTypes.BOOLEAN) (ccl/== dbn 'ColumnTypes.BOOLEAN))]
       ;; We prefer LONG and not INT for INTEGER
       [(ccl/all (ccl/== cdn 'AttributeTypes.LONG)    (ccl/== dbn 'ColumnTypes.INTEGER))]
       [(ccl/all (ccl/== cdn 'AttributeTypes.INT)     (ccl/== dbn 'ColumnTypes.INTEGER))]
       [(ccl/all (ccl/== cdn 'AttributeTypes.FLOAT)   (ccl/== dbn 'ColumnTypes.REAL))]
       [(ccl/all (ccl/== cdn 'AttributeTypes.DOUBLE)  (ccl/== dbn 'ColumnTypes.DOUBLE))]
       [(ccl/all (ccl/== cdn 'AttributeTypes.STRING)  (ccl/== dbn 'ColumnTypes.TEXT))])
      (r/enum-constanto l cdn cdt)
      (r/enum-constanto r dbn dbt)))
  (attribute2column
   :when [(ccl/!= ?name "ID")
          (ccl/condu [(cd-type2db-type ?atype ?ctype)])]
   :left [(scd/->attrs l ?cls ?attr)
          (scd/name l ?attr ?name)
          (scd/type l ?attr ?atype)]
   :right [(sdb/->cols r ?table ?col)
           (sdb/name r ?col ?name)
           (sdb/type r ?col ?ctype)])
  (^:top association2table
         :when [(class2table :?cls ?src :?col ?src-pkey)
                (class2table :?cls ?trg :?col ?trg-pkey)]
         :left [(scd/Association l ?assoc)
                (scd/name l ?assoc ?name)
                (scd/->src l ?assoc ?src)
                (scd/->trg l ?assoc ?trg)]
         :right [(sdb/Table r ?table)
                 (sdb/name r ?table ?name)
                 (sdb/->cols r ?table ?src-col)
                 (sdb/name r ?src-col "SRC")
                 (sdb/->cols r ?table ?trg-col)
                 (sdb/name r ?trg-col "TRG")
                 (sdb/->pkey r ?src-col ?src-pkey)
                 (sdb/->pkey r ?trg-col ?trg-pkey)]))

(test/deftest test-class-diagram2database-schema-simple
  (let [cd (gen-simple-class-diagram)
        db (tg/new-graph (tg/load-schema "test/input/cd2db-simple/db-schema.tg"))
        cd-new (tg/new-graph (tg/load-schema "test/input/cd2db-simple/cd-schema.tg"))]
    ;; New DBS from given CD
    (class-diagram2database-schema-simple cd db :right)
    (test/is (= 4 (tg/vcount db 'Table)))
    (test/is (= 8 (tg/vcount db 'Column)))
    ;;(viz/print-model db :gtk)
    ;; New CD from result DBS
    (class-diagram2database-schema-simple cd-new db :left)
    (test/is (g/equal-models? cd cd-new))
    ;;(viz/print-model cd-new :gtk)
    ;; :right again shouldn't change anything
    (class-diagram2database-schema-simple cd db :right)
    (test/is (= 4 (tg/vcount db 'Table)))
    (test/is (= 8 (tg/vcount db 'Column)))
    ;; :left again shouldn't change anything, too
    (class-diagram2database-schema-simple cd-new db :left)
    (test/is (g/equal-models? cd cd-new))))
