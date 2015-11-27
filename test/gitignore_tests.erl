-module(gitignore_tests).

-include_lib("eunit/include/eunit.hrl").

gitignore_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_default())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_default() ->
  {ok, Pid} = gitignore:compile(string:tokens(eutils:to_string(gitignore()), "\n\r")),
  ?assert(gitignore:accepts(Pid, "src/gitignore.erl")),
  ?assert(gitignore:accepts(Pid, "nonexistent/foo/accepts.txt")),
  ?assert(gitignore:accepts(Pid, "othernonexistent/toto/what/foo/accepts.txt")),
  ?assert(gitignore:accepts(Pid, "othernonexistent/titi/what/foo/accepts.txt")),
  ?assert(gitignore:accepts(Pid, "foo/accepts.txt")),
  ?assert(gitignore:accepts(Pid, "accepts.baz")),
  ?assert(gitignore:accepts(Pid, "othernonexistent/toto/what/accepts.h")),
  ?assert(gitignore:accepts(Pid, "othernonexistent/toto/what/accepts.c")),
  ?assert(gitignore:accepts(Pid, "accepts/othernonexistent/accepts.c")),
  ?assert(gitignore:denies(Pid, "othernonexistent/toto/what/accepts.ch")),
  ?assert(gitignore:denies(Pid, "othernonexistent/toto/what/accepts.hc")),
  ?assert(gitignore:denies(Pid, "denies.log")),
  ?assert(gitignore:denies(Pid, "foo/denies.log")),
  ?assert(gitignore:denies(Pid, "bar/denies.log")),
  ?assert(gitignore:denies(Pid, "baz/denies.log")),
  ?assert(gitignore:denies(Pid, "nonexistent/denies.log")),
  ?assert(gitignore:denies(Pid, "foo/denies.wat")),
  ?assert(gitignore:denies(Pid, "othernonexistent/toto/what/denies.wat")),
  ?assert(gitignore:denies(Pid, "othernonexistent/titi/what/denies.wat")).

gitignore() ->
  <<"# This is a comment in a .gitignore file!
/node_modules
*.log

# Ignore this nonexistent file
/nonexistent

# Do not ignore this file
!/nonexistent/foo

# Ignore some files

/baz

/foo/*.wat

# Ignore some deep sub folders
/othernonexistent/**/what

# Unignore some other sub folders
!/othernonexistent/**/what/*.[ch]
!/othernonexistent/**/what/foo


*.swp">>.
