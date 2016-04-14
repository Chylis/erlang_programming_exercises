-module(foobar).
-export([is_person_record_named_joe/1]).
-record(person, {name,age,phone}).


is_person_record_named_joe(P) when P#person.name == "Joe" ->
  true.
