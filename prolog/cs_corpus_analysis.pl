% ============================================================================
% CS CORPUS ANALYSIS
% ============================================================================
% Pattern distribution across CS-bearing constraints.
% Flags empty attractor regions and masking/cover-story instances.
%
% Usage (from prolog/ directory, testsets already loaded):
%   ?- cs_corpus_distribution.
%
% Standalone run (loads testsets/*.pl automatically):
%   swipl -g "[cs_corpus_analysis], run_cs_corpus_analysis, halt" \
%         -t "halt(1)"
% ============================================================================

:- module(cs_corpus_analysis, [
    cs_corpus_distribution/0,
    cs_corpus_distribution/1,
    run_cs_corpus_analysis/0
]).

:- use_module(cs_pattern_detection).
:- use_module(narrative_ontology).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

all_cs_patterns([marked_revision, interpretive_accretion, diffuse_reconstruction,
                 implicit_practice, anchored_fixity_with_accretion,
                 anchored_fixity_brittle, natural_law_constraint, epistemic_consensus,
                 no_pattern_match]).

%% cs_corpus_distribution(-Dist)
%  Dist = list of N-Pattern pairs sorted by count descending.
%  Operates on the currently loaded KB — caller must have loaded testsets.
%  Uses narrative_ontology:cs_kernel_codification/2 directly to avoid the
%  cut in cs_has_fields/1, which prevents enumeration when C is unbound.
cs_corpus_distribution(Dist) :-
    findall(C, narrative_ontology:cs_kernel_codification(C, _), Cs0),
    sort(Cs0, Cs),
    findall(Pat, (member(C, Cs), once(cs_pattern(C, Pat, _))), Pats),
    sort(Pats, Unique),
    findall(N-P,
            (member(P, Unique),
             include(=(P), Pats, Ms),
             length(Ms, N)),
            Pairs),
    sort(0, @>=, Pairs, Dist).

%% cs_corpus_distribution/0
%  Prints a formatted distribution table to stdout.
cs_corpus_distribution :-
    findall(C, narrative_ontology:cs_kernel_codification(C, _), Cs0),
    sort(Cs0, Cs),
    length(Cs, Total),
    cs_corpus_distribution(Dist),
    format("~n=== CS Pattern Distribution (~w constraints with CS fields) ===~n~n",
           [Total]),
    forall(member(N-P, Dist),
           (Pct is (N * 100) // max(Total, 1),
            format("  ~30|~w  ~3|~w  (~w%)~n", [P, N, Pct]))),
    nl,
    all_cs_patterns(AllPats),
    ( forall(member(AP, AllPats), member(_-AP, Dist))
    -> format("  No empty attractor slots.~n")
    ;  format("  Empty attractor slots (no corpus instances):~n"),
       forall((member(AP, AllPats), \+ member(_-AP, Dist)),
              format("    ~w~n", [AP]))
    ).

%% run_cs_corpus_analysis/0
%  Loads all testsets from testsets/ (relative to CWD), then reports
%  pattern distribution plus masking and cover-story diagnostics.
run_cs_corpus_analysis :-
    expand_file_name('testsets/*.pl', Files),
    length(Files, NFiles),
    format("Loading ~w testset files...~n", [NFiles]),
    maplist([F]>>(catch(user:consult(F), _, true)), Files),
    cs_corpus_distribution,
    nl,
    format("=== Authority Masking (cs_authority_masking/3) ===~n~n"),
    findall(C-Sig-AG, cs_authority_masking(C, Sig, AG), Masked),
    ( Masked = []
    -> format("  None found.~n")
    ;  length(Masked, NM), format("  ~w instances:~n", [NM]),
       forall(member(C-Sig-AG, Masked),
              format("    ~w  sig=~w  ag=~w~n", [C, Sig, AG]))
    ),
    nl,
    format("=== Cover Story Active (cs_cover_story_active/2) ===~n~n"),
    findall(C-V, cs_cover_story_active(C, V), Covers),
    ( Covers = []
    -> format("  None found.~n")
    ;  length(Covers, NC), format("  ~w instances:~n", [NC]),
       forall(member(C-V, Covers),
              format("    ~w  verdict=~w~n", [C, V]))
    ),
    nl,
    format("=== Displaced Beneficiary (cs_displaced_beneficiary/1) ===~n~n"),
    findall(C, cs_displaced_beneficiary(C), Disps),
    ( Disps = []
    -> format("  None found.~n")
    ;  length(Disps, ND), format("  ~w instances:~n", [ND]),
       forall(member(C, Disps),
              format("    ~w~n", [C]))
    ).
