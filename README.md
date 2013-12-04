Mustached-Octo-Happiness
===============

Mustached Octo Happiness (moh), a basic calendar system.

Instance: (http://mustached-octo-happiness.functor.paivola.fi/)
Haddock: (http://animu.paivola.fi/dump/mustached-octo-happiness/)

see
---

- calTargetForm @Handler.Calendar



---

OLD
========

## Calendar
   - Take and reperesent models from CalDAV (RFC4791): *Core objects*, *Events*,
     *To-do's*, *Journal entries*.
   - Support most important fields: timestamps, titles, identifiers, repeats
     (every week, day, month..), etc. etc.
   - **Support plain .ics export and import**.
   - RFC4791 compliant as much as possible.

## Users
   * Multiple users, private and public calendars and events.
   * Collaboration: comments, notes or similar on calendars/events.

## Web interface

* Create, delete, edit, import, export calendars and events via HTTP.
* A web application client which exposes aferomentioned methods, of course with
  a slick UI.

## *Extra:* CalDAV interface
   * Sync support for (awful) clients (like android). (via CalDAV sync app).


RFC4791 (CalDAV): http://tools.ietf.org/html/rfc4791#section-3

