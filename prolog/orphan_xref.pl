% ============================================================================
% ORPHAN XREF — reproducible export-vs-caller orphan census (OQ-38)
% ============================================================================
% Why: ISSUES.md's "217-candidate orphan upper bound" came from an uncommitted
% ad-hoc grep sweep (2026-05-31 wiring_gap_census.md), hand-transcribed and not
% reproducible. The 217 conflates genuinely-dead predicates with
% over-exported-but-internally-called ones; separating them needs
% clause-head-vs-body parsing per predicate. library(prolog_xref) is exactly
% that separator — it sees defined heads, exported heads, and called goals
% (module-qualified) per source. This file mirrors check_stack.pl's shape: a
% library-driven, load-path-independent diagnostic, NOT a pipeline gate (its
% output is an upper bound on a new axis — see [EDGE] below — so it is a
% diagnostic the human reads, not an automatic stripper).
%
% Run:
%   cd prolog && swipl -l orphan_xref.pl -g "run_orphan_xref, halt" -t "halt(1)"
%   cd prolog && swipl -l orphan_xref.pl -g "run_orphan_xref('../outputs/oq38_orphan_xref.tsv'), halt" -t "halt(1)"
%
% [EDGE] xref is STATIC; the codebase is not. prolog_xref sees clause head vs
% body only. It does NOT see (a) goals built in Python and shipped to swipl as
% strings (run_pipeline.py `-g "<goal>, halt."`), nor (b) computed meta-calls
% (Goal =.. [Pred,...], call(Goal); maplist/foldl/findall with the predicate as
% functor). So a STATIC_ORPHAN here is "statically uncalled", which is an UPPER
% BOUND on "dead" — the Python driver (oq38_orphan_sweep.py) subtracts the
% dynamic surface to get the real list. Do not strip on this file alone.
%
% Caller matching is by Name/Arity GLOBALLY (module qualifier stripped). This is
% deliberately CONSERVATIVE: if two modules define the same Name/Arity and only
% one is called, BOTH read LIVE. That biases toward LIVE (fewer orphans), which
% is the SAFE direction for a tool whose only dangerous error is a false orphan.
% ============================================================================

:- module(orphan_xref,
          [ run_orphan_xref/0,
            run_orphan_xref/1,
            orphan_row/6           % File, Name, Arity, Exported, Class, Callers
          ]).

:- use_module(library(prolog_xref)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

% ---- source enumeration -----------------------------------------------------
% Every module-level source in prolog/. Self and any scratch _probe files are
% excluded so the census never reports the tool's own machinery.
orphan_source_files(Files) :-
    expand_file_name('*.pl', All),
    exclude(excluded_source, All, Files0),
    sort(Files0, Files).

excluded_source(F) :-
    file_base_name(F, B),
    ( B == 'orphan_xref.pl'
    ; atom_concat('_probe', _, B)
    ).

xref_all(Files) :-
    forall(member(F, Files),
           catch(xref_source(F, [silent(true)]),
                 E,
                 ( message_to_codes(prolog, E, _),
                   format(user_error, '[orphan_xref] xref failed on ~w~n', [F]) ))).

% ---- defined / exported predicates ------------------------------------------
% Locally defined head in File: any xref_defined whose How is not imported/_.
% (local(Line), or a dynamic/thread_local/multifile declaration in the file.)
defined_pred(File, Name, Arity) :-
    xref_defined(File, Goal, How),
    How \= imported(_),
    callable(Goal),
    functor(Goal, Name, Arity).

exported_pred(File, Name, Arity) :-
    xref_exported(File, Head),
    callable(Head),
    functor(Head, Name, Arity).

% ---- caller resolution ------------------------------------------------------
% Strip a (possibly module-qualified) called goal to Name/Arity.
called_pi(M:G, Name, Arity) :- atom(M), !, called_pi(G, Name, Arity).
called_pi(G, Name, Arity) :- callable(G), functor(G, Name, Arity).

% A static caller of Name/Arity: some source where a goal resolving to
% Name/Arity is called by a head whose own predicate is NOT Name/Arity
% (self-recursion does not keep a predicate alive — nothing enters it).
% Caller identity rendered "file:By" for the witness column.
static_caller(Name/Arity, CallerTag) :-
    xref_called(Src, Called, By),
    called_pi(Called, Name, Arity),
    by_pi(By, BName, BArity),
    \+ ( BName == Name, BArity == Arity ),
    file_base_name(Src, SrcB),
    format(atom(CallerTag), '~w:~w/~w', [SrcB, BName, BArity]).

% By is normally the caller's head term; directive callers come through as
% '<directive>'(Line) or similar — keep them (they DO keep a predicate alive).
by_pi(M:G, Name, Arity) :- atom(M), !, by_pi(G, Name, Arity).
by_pi(By, Name, Arity) :- callable(By), !, functor(By, Name, Arity).
by_pi(_, '<unknown>', 0).

% ---- classification ---------------------------------------------------------
% LIVE          : >=1 static (non-self) caller
% ENTRYPOINT_CLI: arity 0 with no static caller (run-goal candidate; resolved
%                 against the Python dynamic surface, never stripped here)
% STATIC_ORPHAN : arity > 0 with no static caller
classify(Name, Arity, Class, Callers) :-
    ( setof(C, static_caller(Name/Arity, C), Callers0)
    -> Callers = Callers0
    ;  Callers = []
    ),
    ( Callers \= []        -> Class = 'LIVE'
    ; Arity =:= 0          -> Class = 'ENTRYPOINT_CLI'
    ;                         Class = 'STATIC_ORPHAN'
    ).

% ---- public: one row per defined predicate ----------------------------------
orphan_row(File, Name, Arity, Exported, Class, Callers) :-
    setof(F-N-A, defined_pred(F, N, A), Defs),
    member(File-Name-Arity, Defs),
    ( exported_pred(File, Name, Arity) -> Exported = 1 ; Exported = 0 ),
    classify(Name, Arity, Class, Callers).

% ---- drivers ----------------------------------------------------------------
run_orphan_xref :-
    run_orphan_xref('../outputs/oq38_orphan_xref.tsv').

run_orphan_xref(OutFile) :-
    orphan_source_files(Files),
    length(Files, NFiles),
    format(user_error, '[orphan_xref] xref-ing ~w sources...~n', [NFiles]),
    xref_all(Files),
    findall(row(File,Name,Arity,Exp,Class,Callers),
            orphan_row(File,Name,Arity,Exp,Class,Callers),
            Rows),
    setup_call_cleanup(
        open(OutFile, write, S),
        write_rows(S, Rows),
        close(S)),
    summarize(Rows, NFiles),
    format(user_error, '[orphan_xref] wrote ~w~n', [OutFile]).

write_rows(S, Rows) :-
    format(S, 'file\tname\tarity\texported\tclass\tn_callers\tcallers~n', []),
    forall(member(row(File,Name,Arity,Exp,Class,Callers), Rows),
           ( file_base_name(File, B),
             length(Callers, NC),
             atomic_list_concat(Callers, ';', CallerStr),
             format(S, '~w\t~w\t~w\t~w\t~w\t~w\t~w~n',
                    [B, Name, Arity, Exp, Class, NC, CallerStr]) )).

% ---- summary (the tool-native funnel head) ----------------------------------
summarize(Rows, NFiles) :-
    length(Rows, NDef),
    include([row(_,_,_,1,_,_)]>>true, Rows, Exported),
    length(Exported, NExp),
    include([row(_,_,_,_,'STATIC_ORPHAN',_)]>>true, Rows, Orphans),
    length(Orphans, NOrphan),
    include([row(_,_,_,1,'STATIC_ORPHAN',_)]>>true, Rows, ExpOrphans),
    length(ExpOrphans, NExpOrphan),
    include([row(_,_,_,_,'ENTRYPOINT_CLI',_)]>>true, Rows, Entry),
    length(Entry, NEntry),
    include([row(_,_,_,_,'LIVE',_)]>>true, Rows, Live),
    length(Live, NLive),
    format(user_error, '~n[orphan_xref] ===== TOOL-NATIVE CENSUS =====~n', []),
    format(user_error, '  sources xref-ed     : ~w~n', [NFiles]),
    format(user_error, '  predicates defined  : ~w~n', [NDef]),
    format(user_error, '  exported            : ~w~n', [NExp]),
    format(user_error, '  LIVE (>=1 caller)   : ~w~n', [NLive]),
    format(user_error, '  ENTRYPOINT_CLI (/0) : ~w~n', [NEntry]),
    format(user_error, '  STATIC_ORPHAN (all) : ~w~n', [NOrphan]),
    format(user_error, '    of which exported : ~w~n', [NExpOrphan]).
