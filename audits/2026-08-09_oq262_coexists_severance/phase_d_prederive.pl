% ============================================================================
% OQ-262 Phase D pre-derivation probe (READ-ONLY — runs BEFORE any edit)
% ============================================================================
% Computes, over the live corpus, BOTH the current (raw-match) and the routed
% (cs_edge_target_member/4) semantics for the two consumers being routed:
%   1. cs_pattern_detection:cs_displaced_beneficiary/1
%   2. cs_corpus_analysis closure/plurality/neither split (:131-149)
% The outputs ARE the pre-derived prediction: after the routing edits, the
% edited code's firings must equal the ROUTED rows here, over an md5-identical
% corpus. Also predicts the pipeline diff: byte-identical per_constraint
% (neither predicate has a pipeline_output.json surface — verified by grep).
% Run from prolog/:
%   swipl -l ../audits/2026-08-09_oq262_coexists_severance/phase_d_prederive.pl \
%         -g "prederive, halt" -t "halt(1)"
% ============================================================================
:- [stack].
:- corpus_loader:load_all_testsets.

% --- displaced-beneficiary gates (shared) -----------------------------------
db_gates(C, UID) :-
    cs_pattern_detection:cs_kernel_codification(C, _),
    cs_pattern_detection:cs_authority_grounding(C, AG),
    memberchk(AG, [self_enforcing, lineage, practice, expertise, diffuse_epistemic]),
    signature_detection:constraint_signature(C, Sig),
    \+ memberchk(Sig, [natural_law, coupling_invariant_rope, coordination_scaffold]),
    narrative_ontology:cs_story_uid(C, UID).

% CURRENT semantics (raw target match) — mirrors cs_pattern_detection.pl:348-357
db_current(C) :-
    db_gates(C, UID),
    narrative_ontology:cs_reading_relation(UID, Sibling, forecloses),
    cs_pattern_detection:cs_has_fields(Sibling),
    cs_pattern_detection:cs_authority_grounding(Sibling, extraction).

% ROUTED semantics (resolver when kernel-registered; raw fallback otherwise)
resolve_target(C, T, Sibling) :-
    ( narrative_ontology:cs_kernel_id(C, K)
    -> cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
       once(cs_kernel_registry:cs_edge_target_member(K, T, Pairs, Sibling))
    ;  Sibling = T ).
db_routed(C) :-
    db_gates(C, UID),
    narrative_ontology:cs_reading_relation(UID, T, forecloses),
    resolve_target(C, T, Sibling),
    cs_pattern_detection:cs_has_fields(Sibling),
    cs_pattern_detection:cs_authority_grounding(Sibling, extraction).

% --- cs_corpus_analysis closure/plurality split -----------------------------
all_kernels(Ks) :- setof(K, C^(narrative_ontology:cs_kernel_id(C, K)), Ks).

conflicts(ConflictUniq) :-
    all_kernels(AllKernels),
    findall(K-(U1-C1)-(U2-C2),
            ( member(K, AllKernels),
              cs_axiom_engine:cs_kernel_axiom_conflict(K, U1-C1, U2-C2, _) ),
            Raw),
    sort(Raw, ConflictUniq).

% raw edge test (current :131-149 shape)
edge_raw(U1, C2n, U2, C1n, Rel) :-
    ( narrative_ontology:cs_reading_relation(U1, C2n, Rel)
    ; narrative_ontology:cs_reading_relation(U2, C1n, Rel) ).
% routed edge test
edge_routed(K, U1, C2n, U2, C1n, Rel) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    ( narrative_ontology:cs_reading_relation(U1, T1, Rel),
      cs_kernel_registry:cs_edge_target_member(K, T1, Pairs, C2n)
    ; narrative_ontology:cs_reading_relation(U2, T2, Rel),
      cs_kernel_registry:cs_edge_target_member(K, T2, Pairs, C1n) ).

split(Name, EdgeClosure, EdgePlural) :-
    conflicts(Conflicts),
    length(Conflicts, NC),
    findall(x, ( member(K-(U1-C1)-(U2-C2), Conflicts),
                 once(call(EdgeClosure, K, U1, C2, U2, C1)) ), Cl),
    length(Cl, NClosure),
    findall(x, ( member(K-(U1-C1)-(U2-C2), Conflicts),
                 once(call(EdgePlural, K, U1, C2, U2, C1)) ), Pl),
    length(Pl, NPlurality),
    findall(x, ( member(K-(U1-C1)-(U2-C2), Conflicts),
                 \+ call(EdgeClosure, K, U1, C2, U2, C1),
                 \+ call(EdgePlural, K, U1, C2, U2, C1) ), Ne),
    length(Ne, NNeither),
    format("  ~w: conflicts=~w closure=~w plurality=~w neither=~w~n",
           [Name, NC, NClosure, NPlurality, NNeither]).

edge_raw_k(_K, U1, C2, U2, C1, Rel) :- edge_raw(U1, C2, U2, C1, Rel).
raw_closure(K, U1, C2, U2, C1)    :- edge_raw_k(K, U1, C2, U2, C1, forecloses).
raw_plural(K, U1, C2, U2, C1)     :- edge_raw_k(K, U1, C2, U2, C1, coexists_with).
routed_closure(K, U1, C2, U2, C1) :- edge_routed(K, U1, C2, U2, C1, forecloses).
routed_plural(K, U1, C2, U2, C1)  :- edge_routed(K, U1, C2, U2, C1, coexists_with).

prederive :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("corpus: ~w constraints~n", [NC]),
    format("-- 1. cs_displaced_beneficiary --~n"),
    findall(C, db_current(C), Cur0), sort(Cur0, Cur),
    format("  CURRENT (raw) firings: ~w ~w~n", [Cur, '']),
    findall(C, db_routed(C), Rt0), sort(Rt0, Rt),
    format("  ROUTED firings: ~w~n", [Rt]),
    subtract(Rt, Cur, Gained), subtract(Cur, Rt, Lost),
    format("  predicted movement: gained=~w lost=~w~n", [Gained, Lost]),
    format("-- 2. cs_corpus_analysis conflict split --~n"),
    split(current_raw, raw_closure, raw_plural),
    split(routed,      routed_closure, routed_plural),
    format("-- 3. pipeline prediction: per_constraint byte-identical (no surface consumes either predicate)~n").
