(ns funnyqt.bidi-test
  (:use funnyqt.bidi)
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [funnyqt.relational.tg :as rtg]
            [funnyqt.relational :as r]
            [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.tg :as tg]
            [funnyqt.visualization :as viz]
            [clojure.test :as test]
            [funnyqt.protocols :as p]))

(defn print-counts [g]
  (println "Edges")
  (println "=====")
  (doseq [ec (.getEdgeClasses (.getGraphClass (tg/schema g)))
          :let [ecsym (funnyqt.protocols/qname ec)]]
    (println ecsym "->" (tg/ecount g (symbol (str ecsym "!")))))
  (println "Vertices")
  (println "========")
  (doseq [ec (.getVertexClasses (.getGraphClass (tg/schema g)))
          :let [ecsym (funnyqt.protocols/qname ec)]]
    (println ecsym "->" (tg/vcount g (symbol (str ecsym "!"))))))

(defn make-example-addressbook []
  (let [g (tg/create-graph (tg/load-schema "test/input/addressbook.tg"))
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

(rtg/generate-schema-relations "test/input/addressbook.tg" ab)

(deftransformation addressbook2addressbook [l r]
  (^:top addressbook2addressbook
         :left [(ab/+AddressBook l ?addrbook1)
                (ab/+name l ?addrbook1 ?n)]
         :right [(ab/+AddressBook r ?addrbook2)
                 (ab/+name r ?addrbook2 ?n)]
         :where [(category2category :?ab1 ?addrbook1 :?ab2 ?addrbook2)])
  (category2category
   :left [(ab/+ContainsCategory l ?cc1 ?ab1 ?cat1)
          (ab/+Category l ?cat1)
          (ab/+name l ?cat1 ?n)]
   :right [(ab/+ContainsCategory r ?cc2 ?ab2 ?cat2)
           (ab/+Category r ?cat2)
           (ab/+name r ?cat2 ?n)]
   :where [(contact2contact :?cat1 ?cat1 :?cat2 ?cat2)
           (org2org :?cat1 ?cat1 :?cat2 ?cat2)])
  (have-same-ids
   :left [(ab/+id l ?entry1 ?id)]
   :right [(ab/+id r ?entry2 ?id)])
  (contact2contact
   :extends [(:have-same-ids ?entry1 ?contact1
                             ?entry2 ?contact2)]
   :left [(ab/+->contacts l ?cat1 ?contact1)
          (ab/+Contact l ?contact1)
          (ab/+firstName l ?contact1 ?fn)
          (ab/+lastName l ?contact1 ?ln)
          (ab/+email l ?contact1 ?mail)]
   :right [(ab/+->contacts r ?cat2 ?contact2)
           (ab/+Contact r ?contact2)
           (ab/+firstName r ?contact2 ?fn)
           (ab/+lastName r ?contact2 ?ln)
           (ab/+email r ?contact2 ?mail)])
  (org2org
   :extends [(:have-same-ids ?entry1 ?org1
                             ?entry2 ?org2)]
   :left [(ab/+ContainsOrganization l ?co1 ?cat1 ?org1)
          (ab/+Organization l ?org1)
          (ab/+homepage l ?org1 ?hp)
          (ab/+name l ?org1 ?n)]
   :right [(ab/+ContainsOrganization r ?co2 ?cat2 ?org2)
           (ab/+Organization r ?org2)
           (ab/+homepage r ?org2 ?hp)
           (ab/+name r ?org2 ?n)])
  (^:top connect-employees
   :when [(relateo :org2org :?org1 ?org1 :?org2 ?org2)
          (relateo :contact2contact :?contact1 ?contact1 :?contact2 ?contact2)]
   :left [(ab/+->employees l ?org1 ?contact1)]
   :right [(ab/+->employees r ?org2 ?contact2)]))

(defn assert-same-addressbooks [l r]
  (test/is (= (tg/vcount l 'AddressBook)          (tg/vcount r 'AddressBook)))
  (test/is (= (tg/vcount l 'Category)             (tg/vcount r 'Category)))
  (test/is (= (tg/ecount l 'ContainsCategory)     (tg/ecount r 'ContainsCategory)))
  (test/is (= (tg/vcount l 'Contact)              (tg/vcount r 'Contact)))
  (test/is (= (tg/ecount l 'ContainsContact)      (tg/ecount r 'ContainsContact)))
  (test/is (= (tg/vcount l 'Organization)         (tg/vcount r 'Organization)))
  (test/is (= (tg/ecount l 'ContainsOrganization) (tg/ecount r 'ContainsOrganization)))
  (test/is (= (tg/ecount l 'HasEmployee)          (tg/ecount r 'HasEmployee))))

(test/deftest test-addressbook2addressbook
  (let [l (make-example-addressbook)
        r (tg/create-graph (tg/load-schema "test/input/addressbook.tg"))]
    ;; Transform l to r
    (println "addressbook2addressbook l -> r (empty)")
    (addressbook2addressbook l r :right)
    (assert-same-addressbooks l r)
    ;; Do it again.  It shouldn't modify anything.
    (println "addressbook2addressbook l -> r (both already in sync)")
    (addressbook2addressbook l r :right)
    (assert-same-addressbooks l r)
    ;; Do it in the other direction.  Again, it shouldn't modify anything.
    (println "addressbook2addressbook l <- r (both already in sync)")
    (addressbook2addressbook l r :left)
    (assert-same-addressbooks l r)
    ;; Now add a new Contact to the right addressbook and synchronize it to the
    ;; left.
    (println "addressbook2addressbook l <- r (r has a new Contact)")
    (let [tim (tg/create-vertex! r 'Contact
                                 :id (int 6)
                                 :firstName "Tim"
                                 :lastName "Taylor"
                                 :email "tim@gmail.com")
          cat-work (first (filter #(= (tg/value % :name) "Work")
                                  (tg/vseq r 'Category)))]
      (p/add-adj! cat-work :contacts tim))
    (addressbook2addressbook l r :left)
    (assert-same-addressbooks l r)
    #_(viz/print-model r :gtk)))
