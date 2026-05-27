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
    cs_trifurcation_profile/0,
    run_cs_corpus_analysis/0
]).

:- use_module(cs_pattern_detection).
:- use_module(cs_drift_engine).
:- use_module(cs_axiom_engine).
:- use_module(cs_kernel_registry).
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

/* ================================================================
   TRIFURCATION PROFILE
   Reports axiom-conflict rates, drift trajectory terminal distribution,
   kernel-divergence counts, cs_drift_unacknowledged and cs_axiom_foreclosed.
   Testsets must already be loaded.
   ================================================================ */

%% cs_trifurcation_profile/0
%  Reports the trifurcation diagnostic profile to stdout.
cs_trifurcation_profile :-
    format("~n=== Trifurcation Profile ===~n~n"),

    % 1. Drift trajectory terminal distribution
    format("-- Drift Trajectory Terminal Distribution --~n~n"),
    findall(UID-Terminal,
            (narrative_ontology:cs_story_uid(C, UID), \+ is_list(C),
             cs_drift_engine:cs_drift_trajectory(UID, _, Terminal)),
            TrajRaw),
    sort(TrajRaw, TrajUniq),
    findall(T, member(_-T, TrajUniq), TerminalsRaw),
    sort(TerminalsRaw, UniqueTerminals),
    length(TrajUniq, TrajTotal),
    forall(member(Term, UniqueTerminals),
           (include([_-T]>>(T == Term), TrajUniq, Matches),
            length(Matches, Count),
            format("  ~w: ~w~n", [Term, Count]))),
    format("  (total CS-bearing with trajectory: ~w)~n~n", [TrajTotal]),

    % 2. Kernel divergence count
    % K must be bound before calling cs_kernel_divergence — the predicate uses
    % cs_readings_for_kernel/2 which calls findall internally, causing K to be
    % copied rather than bound. Enumerate kernel atoms first.
    format("-- Kernel Divergence Count --~n~n"),
    findall(K0, (narrative_ontology:cs_kernel_id(_, K0), atom(K0)), AllKRaw),
    sort(AllKRaw, AllKernels),
    findall(K-C1-C2,
            (member(K, AllKernels),
             cs_kernel_registry:cs_kernel_divergence(K, _, C1, C2)),
            DivRaw),
    sort(DivRaw, DivUniq),
    length(DivUniq, NDivPairs),
    findall(K2, (member(K2-_-_, DivUniq)), DivKRaw),
    sort(DivKRaw, UniqDivKernels),
    length(UniqDivKernels, NKernelsWithDiv),
    format("  Distinct reading-pair divergences: ~w~n", [NDivPairs]),
    format("  Kernels with at least one divergence: ~w~n~n", [NKernelsWithDiv]),

    % 3. Axiom conflict rates (closure vs licensed plurality)
    % K must be bound before cs_kernel_axiom_conflict — same issue as divergence above.
    format("-- Axiom Conflict Rates --~n~n"),
    findall(K-(UID1-C1n)-(UID2-C2n),
            (member(K, AllKernels),
             cs_axiom_engine:cs_kernel_axiom_conflict(K, UID1-C1n, UID2-C2n, _)),
            ConflictRaw),
    sort(ConflictRaw, ConflictUniq),
    length(ConflictUniq, NConflict),
    findall(K-(UID1-C1n)-(UID2-C2n),
            (member(K-(UID1-C1n)-(UID2-C2n), ConflictUniq),
             once((narrative_ontology:cs_reading_relation(UID1, C2n, forecloses)
                  ;narrative_ontology:cs_reading_relation(UID2, C1n, forecloses)))),
            ClosureRaw),
    sort(ClosureRaw, ClosureUniq),
    length(ClosureUniq, NClosure),
    findall(K-(UID1p-C1np)-(UID2p-C2np),
            (member(K-(UID1p-C1np)-(UID2p-C2np), ConflictUniq),
             once((narrative_ontology:cs_reading_relation(UID1p, C2np, coexists_with)
                  ;narrative_ontology:cs_reading_relation(UID2p, C1np, coexists_with)))),
            PluralityRaw),
    sort(PluralityRaw, PluralityUniq),
    length(PluralityUniq, NPlurality),
    % "neither" = no forecloses AND no coexists_with edge
    findall(K-(UID1q-C1nq)-(UID2q-C2nq),
            (member(K-(UID1q-C1nq)-(UID2q-C2nq), ConflictUniq),
             \+ (narrative_ontology:cs_reading_relation(UID1q, C2nq, forecloses)
                ;narrative_ontology:cs_reading_relation(UID2q, C1nq, forecloses)),
             \+ (narrative_ontology:cs_reading_relation(UID1q, C2nq, coexists_with)
                ;narrative_ontology:cs_reading_relation(UID2q, C1nq, coexists_with))),
            NeitherRaw),
    sort(NeitherRaw, NeitherUniq),
    length(NeitherUniq, NNeither),
    format("  Total cross-reading axiom conflicts: ~w~n", [NConflict]),
    format("  Real closure (forecloses edge):       ~w~n", [NClosure]),
    format("  Licensed plurality (coexists_with):   ~w~n", [NPlurality]),
    format("  No typed edge (structural only):      ~w~n~n", [NNeither]),

    % 4. cs_drift_unacknowledged instances
    format("-- cs_drift_unacknowledged Instances --~n~n"),
    findall(UID-Gap,
            (narrative_ontology:cs_story_uid(C, UID), \+ is_list(C),
             cs_pattern_detection:cs_drift_unacknowledged(UID, Gap)),
            UnackRaw),
    sort(UnackRaw, UnackUniq),
    length(UnackUniq, NUnack),
    (NUnack =:= 0
    ->  format("  None found.~n~n")
    ;   format("  ~w instances:~n", [NUnack]),
        forall(member(UID-Gap, UnackUniq),
               format("    ~w  ~w~n", [UID, Gap])),
        nl
    ),

    % 5. cs_axiom_foreclosed instances
    format("-- cs_axiom_foreclosed Instances --~n~n"),
    findall(UID-Atom,
            (narrative_ontology:cs_story_uid(C, UID), \+ is_list(C),
             cs_axiom_engine:cs_axiom_foreclosed(UID, Atom)),
            ForeclosedRaw),
    sort(ForeclosedRaw, ForeclosedUniq),
    length(ForeclosedUniq, NForeclosed),
    (NForeclosed =:= 0
    ->  format("  None found.~n~n")
    ;   format("  ~w instances:~n", [NForeclosed]),
        forall(member(C-Atom, ForeclosedUniq),
               format("    ~w: ~w~n", [C, Atom])),
        nl
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
    ),
    cs_trifurcation_profile.
