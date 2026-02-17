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
    ensure_corpus_loaded/0
]).

:- dynamic corpus_loaded/0.

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
    ;   expand_file_name('testsets/*.pl', Files),
        length(Files, N),
        format(user_error, '[corpus] Loading ~w testset files...~n', [N]),
        load_testset_list(Files, 0, Loaded),
        format(user_error, '[corpus] Loaded ~w testsets successfully.~n', [Loaded]),
        assertz(corpus_loaded)
    ).

%% load_testset_list(+Files, +Acc, -Total)
%  Tail-recursive loader with error tolerance.
load_testset_list([], N, N).
load_testset_list([F|Fs], Acc, N) :-
    (   catch(user:consult(F), _, true)
    ->  Acc1 is Acc + 1
    ;   Acc1 = Acc
    ),
    load_testset_list(Fs, Acc1, N).
