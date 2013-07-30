(ns funnyqt.bidi-test
  (:use funnyqt.bidi)
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [funnyqt.relational.tg :as rtg]
            [funnyqt.relational :as r]
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
        cat-work (tg/create-vertex! g 'Category :name "Work")
        cat-private (tg/create-vertex! g 'Category :name "Private")
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
                                   :homepage "www.mozilla.org")
        ibm (tg/create-vertex! g 'Organization
                               :id (int 5)
                               :name "IBM"
                               :homepage "www.ibm.com")]
    (p/add-adjs! ab :categories [cat-work cat-private])
    (p/add-adj! cat-work :contacts steve)
    (p/add-adj! cat-work :organizations ibm)
    (p/add-adjs! cat-private :contacts [jim tim])
    (p/add-adj! cat-private :organizations mozilla)
    (p/add-adjs! ibm :employees [steve tim])
    (p/add-adj! mozilla :employees tim)
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
   :where [(contact2contact :?cat1 ?cat1 :?cat2 ?cat2)])
  (contact2contact
   :left [(ab/+->contacts l ?cat1 ?contact1)
          (ab/+Contact l ?contact1)
          (ab/+firstName l ?contact1 ?fn)
          (ab/+lastName l ?contact1 ?ln)
          (ab/+email l ?contact1 ?mail)
          (ab/+id l ?contact1 ?id)]
   :right [(ab/+->contacts r ?cat2 ?contact2)
           (ab/+Contact r ?contact2)
           (ab/+firstName r ?contact2 ?fn)
           (ab/+lastName r ?contact2 ?ln)
           (ab/+email r ?contact2 ?mail)
           (ab/+id r ?contact2 ?id)]))

(test/deftest test-addressbook2addressbook-l2r
  (let [l (make-example-addressbook)
        r (tg/create-graph (tg/load-schema "test/input/addressbook.tg"))]
    (addressbook2addressbook l r :right)
    (test/is (= 1
                (tg/vcount l 'AddressBook)
                (tg/vcount r 'AddressBook)))
    (test/is (= 2
                (tg/vcount l 'Category)
                (tg/vcount r 'Category)))
    (test/is (= 2
                (tg/ecount l 'ContainsCategory)
                (tg/ecount r 'ContainsCategory)))
    (viz/print-model r :gtk)
    ))
