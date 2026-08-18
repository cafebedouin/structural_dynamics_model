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
