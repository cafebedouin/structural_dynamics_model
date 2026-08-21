% ============================================================================
% CORPUS LOADER — Centralized Testset Loading
% ============================================================================
% Single module for bulk-loading testset .pl files from testsets/ directory.
% Provides a guard flag (corpus_loaded/0) to prevent redundant re-loading
% when multiple analysis modules run in sequence.
%
% Usage:
%   :- use_module(corpus_loader).
%   ...
%   corpus_loader:ensure_corpus_loaded,
%   <your analysis here>
%
% All modules should call ensure_corpus_loaded/0 (or load_all_testsets/0)
% rather than maintaining their own loading code.
% ============================================================================

:- module(corpus_loader, [
    load_all_testsets/0,
    ensure_corpus_loaded/0,
    corpus_constraint/1,
    corpus_story/1,        % OQ-306: membership as a CHECKED FACT, not a filename convention.
    corpus_member_kind/2,  % OQ-306: total kinding over corpus_constraint/1.
    resolve_corpus_dir/2,
    corpus_loaded/0        % OQ-68: load-completion witness. Part of the contract, not an
                           % internal — callers legitimately branch on "already loaded?"
                           % (ensure_corpus_loaded/0 is guarded by exactly this fact), and
                           % probes need it to fail closed rather than silently load 0.
]).

:- use_module(config).
:- use_module(config_validation).

:- dynamic corpus_loaded/0.

%% corpus_constraint(?Id)
%  Registry of constraint ids loaded by THIS loader, asserted one per
%  successfully consulted testset file (id = file base name; 1:1 witnessed
%  2026-06-04, OQ-70 Probe 0). This is the authoritative corpus-membership
%  fact: exporters and probes should enumerate corpus_constraint/1 rather
%  than "anything with a constraint_metric or a constraint_classification" —
%  the latter picks up engine-resident demo constraints from
%  constraint_instances.pl (catholic_church_1200), the historical source of
%  the per_constraint=1107 vs n_constraints=1106 denominator drift.
:- dynamic corpus_constraint/1.

% ---------------------------------------------------------------------------
% STORY MEMBERSHIP AS A CHECKED FACT (OQ-306)
% ---------------------------------------------------------------------------
% corpus_constraint/1 answers "is this a corpus MEMBER" by filename. It does not
% answer "is this a STORY", and 27 of the members are *_contradictions.pl axiom
% meta-files carrying no story content — so every rate computed over the member
% count is off by that stratum's share. The defect is the GROWTH, not the
% presence: the stratum went 9 -> 26 on disk (live census 2026-07-02 -> filing
% 2026-08-17; 27 today) with nothing going red, which silently rewrites a time
% series rather than biasing it by a constant. COUNT IT LIVE, NEVER FROM GIT —
% git tracked 4 of those 9, so its tracking series understates (OQ-306 close).
%
% These predicates make the distinction queryable. They REPORT; they do not
% enforce. Enforcement lives at run_pipeline.py's n_unclassified refusal and
% corpus_census_check.py's totality arm (OQ-306 C2/C4).
%
% NOTE (by-plan severance, not an unfinished wire): at C1 these predicates have
% no emit-side consumer — json_report.pl's per-entry `member_kind` and the
% manifest census land in C2, which is blocked on pending operator rulings.
% Build-discipline Pattern 1 says a producer is not done until something
% consumes it; this severance is deliberate and dated, marked here at the site
% so it reads as staged rather than abandoned.

%% corpus_story(?Id)
%  A corpus member that carries STORY content. Pure fact-family test: a member
%  is a story iff it authors any constraint_metric/3. Deliberately NOT a
%  filename convention — the filename convention is what failed.
corpus_story(C) :-
    corpus_constraint(C),
    has_story_facts(C).

%% has_story_facts(+Id)
%  Metric name deliberately UNBOUND: binding it from config would make corpus
%  membership config-sensitive. Semi-deterministic by construction — without
%  once/1 a failed contradiction scan backtracks in here and re-runs the
%  (unmemoized) clause enumeration once per authored metric per member.
has_story_facts(C) :-
    once(narrative_ontology:constraint_metric(C, _, _)).

%% contributes_axiom_contradiction(+Id)
%  True when some cs_axiom_contradiction/2 clause was consulted from <Id>.pl.
%  Both arguments of that predicate are AXIOM ids — never the kernel or file id
%  (verified 2026-08-20, re-verified 2026-08-21 across all 27 files) — so
%  attribution has to go through clause provenance. The residual filename
%  dependency is inherited from corpus_constraint/1 itself, not introduced here.
%
%  clause/3 is legal despite the predicate being static-multifile, and
%  source/1 and file/1 return identical paths on consulted testsets (witnessed
%  2026-08-21: no throw, 92 clauses over 27 distinct source basenames). `source`
%  is chosen for the include/1 case; if testsets ever arrive via include/1 the
%  two diverge and this is the line to revisit.
%
%  NOT memoized — plain enumeration. If its cost ever matters, measure it and
%  report the measurement rather than adding a cache to guess at.
contributes_axiom_contradiction(C) :-
    once(( clause(narrative_ontology:cs_axiom_contradiction(_, _), true, Ref),
           clause_property(Ref, source(File)),
           file_base_name(File, Base),
           file_name_extension(C, pl, Base) )).

%% corpus_member_kind(?Id, ?Kind)
%  TOTAL over corpus_constraint/1. Kind in
%  {story, axiom_contradiction, dual_family, unknown}.
%
%  `dual_family`, NOT `ambiguous`: `ambiguous` is already a live engine token
%  (constraint_signature(C, ambiguous)) carrying its own misreading history, and
%  minting a colliding second one repeats the four-purity-banders class.
%
%  Both fail-closed kinds are DISCOVERIES, never defaults. A member satisfying
%  BOTH fact families is `dual_family` — the disjointness test runs FIRST so it
%  can never be silently absorbed into `story`. A member satisfying neither is
%  `unknown`. Zero of each is expected on every live leg; a nonzero count is a
%  finding about the corpus, not a bug in this predicate.
%
%  Head carries a fresh variable and EVERY kind atom is bound AFTER the cut (the
%  dr_type/3 immune idiom, KNOWN_STATE 2026-08-17). A caller may therefore bind
%  Kind without the bound-probe hazard of build-discipline Pattern 7 — including
%  the catch-all clause, whose head must not carry a bare atom either.
%
% corpus_member_kind/2 is deliberately NOT registered in reading_registry.pl
% (OQ-306 R-H, ruled 2026-08-21). Membership predicate, not a reading — it
% determines what gets read, upstream of the registry's domain. Totality is
% structural: member_kind_/2's final clause is a fresh-variable catch-all and
% cannot fail, so the OQ-137 gate would check a by-construction property
% against a population expected to hold zero unknowns. Enforcement lives at
% D2's n_unclassified refusal (run_pipeline.py) and corpus_census_check.py's
% totality arm, whose planted unknown-shape selftest exercises the catch-all
% against an input the live corpus does not supply. Do not re-register
% without first retiring those.
corpus_member_kind(C, Kind) :-
    corpus_constraint(C),
    member_kind_(C, K),
    Kind = K.

member_kind_(C, K) :-                        % disjointness FIRST — a member in
    has_story_facts(C),                      % both families is a discovery and
    contributes_axiom_contradiction(C), !,   % must not be absorbed into `story`
    K = dual_family.
member_kind_(C, K) :- has_story_facts(C), !, K = story.
member_kind_(C, K) :- contributes_axiom_contradiction(C), !, K = axiom_contradiction.
member_kind_(_, K) :- K = unknown.

%% report_member_census(+Loaded)
%  Prints the kind breakdown BEFORE the final `[corpus] Loaded N` line, so
%  last-line consumers are undisturbed. (Enumerated 2026-08-21: four consumers
%  of `[corpus]`-prefixed stderr, all human-progress echo filters that parse no
%  value; the only `tail -1` in the repo is scripts/gate.sh over checker output.
%  load_warning_gate.py sees neither line — it runs `[stack]` without loading
%  the corpus, and its regex is `^(Warning|ERROR):`.)
%
%  ZERO-GUARD with two INDEPENDENT derivations: `Loaded` is the load loop's own
%  per-file success counter, `Sum` is a fresh enumeration of corpus_member_kind/2
%  over the corpus_constraint/1 registry. A load-loop/registry divergence is
%  exactly what the mismatch branch catches, and it is reachable — retract one
%  corpus_constraint/1 fact and call this with the original Loaded to see it
%  print. (NStory + NNon + NOther =:= Sum is true BY CONSTRUCTION and is
%  deliberately NOT the check — that would be a total recomputed from its own
%  parts.)
%
%  A 0/0/0 census on a corpus loaded WITHOUT allow_empty_corpus is a
%  guard-ordering bug, never an empty corpus: load_all_testsets/0 throws
%  corpus_empty first. Under the escape hatch 0/0/0 is legitimate — check which
%  regime you are in before diagnosing.
report_member_census(Loaded) :-
    findall(K, (corpus_constraint(C), corpus_member_kind(C, K)), Kinds),
    length(Kinds, Sum),
    count_kind(Kinds, story, NStory),
    count_kind(Kinds, axiom_contradiction, NNon),
    include(other_member_kind, Kinds, Others),
    length(Others, NOther),
    (   Sum =:= Loaded
    ->  format(user_error,
               '[corpus] census: ~w stories, ~w non-story, ~w other.~n',
               [NStory, NNon, NOther])
    ;   format(user_error,
               '[corpus] CENSUS DISCREPANCY: ~w members kinded but ~w files loaded — \c
                the membership registry and the load loop disagree; every rate over \c
                this corpus is unsafe until reconciled.~n',
               [Sum, Loaded])
    ).

count_kind(Kinds, Kind, N) :-
    include(==(Kind), Kinds, Selected),
    length(Selected, N).

other_member_kind(K) :-
    \+ memberchk(K, [story, axiom_contradiction]).

%% loader_directory(-Dir)
%  Source anchor captured at load time: the directory THIS file was loaded
%  from (= prolog/). Makes corpus_path resolution independent of the
%  process working directory — running swipl from the repo root previously
%  globbed 0 files silently. Overlay note: a RELATIVE corpus_path overlay
%  (e.g. testsets_3000) now always resolves against prolog/, which is
%  identical to prior behavior under the cd-prolog convention.
:- dynamic loader_directory/1.
:- prolog_load_context(directory, D),
   retractall(loader_directory(_)),
   assertz(loader_directory(D)).

%% ensure_corpus_loaded
%  Loads all testsets if not already loaded. Preferred entry point.
ensure_corpus_loaded :-
    load_all_testsets.

%% load_all_testsets
%  Bulk-loads all .pl files from testsets/ directory.
%  Guarded by corpus_loaded/0 to prevent redundant loading.
load_all_testsets :-
    (   corpus_loaded
    ->  true
    ;   config:param(corpus_path, Dir),
        resolve_corpus_dir(Dir, AbsDir),
        atom_concat(AbsDir, '/*.pl', Pattern),
        expand_file_name(Pattern, Files),
        length(Files, N),
        % Fail-closed on absence (build_discipline Pattern 5): a 0-file glob
        % previously proceeded silently and every downstream gate "passed" on
        % an empty corpus. Escape hatch for deliberately-empty runs:
        % assert config:param(allow_empty_corpus, true) before loading.
        (   N =:= 0,
            \+ catch(config:param(allow_empty_corpus, true), _, fail)
        ->  throw(error(corpus_empty(Pattern),
                        corpus_loader_glob_matched_zero_files))
        ;   true
        ),
        retractall(corpus_constraint(_)),
        format(user_error, '[corpus] Loading ~w testset files...~n', [N]),
        load_testset_list(Files, 0, Loaded),
        % OQ-306: kind breakdown BEFORE the final line (last-line parsers undisturbed).
        report_member_census(Loaded),
        format(user_error, '[corpus] Loaded ~w testsets successfully.~n', [Loaded]),
        assertz(corpus_loaded),
        % DP-001 / OQ-25: ε-coherence seal and chimera detection. CS predicates are
        % query-time only — none fire during consult — so this guard runs after the
        % full corpus is in the DB and before any analysis call. See docs/cs_load_discipline.md.
        config_validation:validate_config_postcorpus
    ).

%% load_testset_list(+Files, +Acc, -Total)
%  Tail-recursive loader with error tolerance and diagnostics.
load_testset_list([], N, N).
load_testset_list([F|Fs], Acc, N) :-
    (   catch(
            user:consult(F),
            Error,
            (   format(user_error, '[corpus] WARNING: Failed to load ~w: ~w~n', [F, Error]),
                fail
            )
        )
    ->  Acc1 is Acc + 1,
        register_corpus_constraint(F)
    ;   format(user_error, '[corpus] SKIPPED: ~w~n', [F]),
        Acc1 = Acc
    ),
    load_testset_list(Fs, Acc1, N).

%% resolve_corpus_dir(+Dir, -AbsDir)
%  Anchors a relative corpus_path against the loader's own source directory
%  (prolog/), making corpus loading cwd-independent. Absolute paths pass
%  through unchanged (testsets_3000 retrospective overlays may use either).
resolve_corpus_dir(Dir, AbsDir) :-
    (   is_absolute_file_name(Dir)
    ->  AbsDir = Dir
    ;   loader_directory(LD),
        atomic_list_concat([LD, '/', Dir], AbsDir)
    ).

%% register_corpus_constraint(+File)
%  Asserts the corpus-membership fact for a successfully loaded testset.
%  Id = file base name (1:1 with files, witnessed 2026-06-04).
register_corpus_constraint(File) :-
    file_base_name(File, Base),
    file_name_extension(Id, pl, Base),
    (   corpus_constraint(Id)
    ->  true
    ;   assertz(corpus_constraint(Id))
    ).
