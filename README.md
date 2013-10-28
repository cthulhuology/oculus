# Oculus

An Experimental Full Text Search Engine in Erlang

In the Beginning
================

I originally started Oculus to experiment with search engine ideas that kept popping up as I built ever larger Solr deployments.  As the number of Solr cores running in a single cluster started to hit the magic 256 mark, a number of limitations of it's clustering became more and more painful.  The other thing that became double painful with Solr and Lucene was keeping both the real-time in memory search engine running along side of the more traditional document store.  As changes were made to the Solr schema, it became non-trivial to determine how best to replicate functionality between the two engines.  As more and more unstructured data was entered into the system, the lack of flexibility on the schema side broke the camel's back metaphorically.


The Design of Oculus
--------------------

Oculus uses a simple varint scheme for storing index data in lists of integers.  It is fairly space efficient, and with index compression on disk is very cost efficient.  Each document is parsed into a list of tokens, and these tokens are stored in an inverted index.  By storing the index locations for each document, we can do proximity search and phrase search.  Oculus is also designed to support multiple indexes so that each user / application can maintain it's own versioning. Oculus supports versioning of the tokenizer and index storage format as well, so that multiple revisions can coexist on the same instance.

The query interface for Oculus is largely non-existent.  Right now I merely use list comprehensions in Erlang to iterate over sets of words.  My original intent was to build a full UI on top of a REST interface for adhoc queries, and add a full streaming engine by making Oculus a filter on an AMQP bus.  As I've been toying with the AMQP bus filter concept, the idea took on legs of it's own, and spawned a ton of other projects.  Future development of Oculus will follow this idiom as a stream filtering engine.


Getting Started
---------------

You can build Oculus using rebar and then use the components in an erl console.


TODO
----

* Finish the AMQP integration
* Add a nicer query interface
* Update custering & configuration to use Mnesia
* Add some sort of auth integration
* Hook up some sort of stats tracking to Mnesia

LICENSE
-------

Oculus is free softare and is distributed under the GNU Affero GPL v3 or greater.

See LICENSE file for full text.

Contact
-------

If you would like to discuss Oculus, my plans for the engine, or just by me coffee:

	Dave Goehrig <dave@dloh.org>

