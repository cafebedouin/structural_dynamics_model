% ============================================================================
% READING REGISTRY (OQ-137) — the aggregatable-reading totality registry
% ============================================================================
% One queryable fact per "aggregatable reading": a verdict/reading predicate an
% aggregate (census, report table, sweep) could consume as a measurement over a
% corpus-enumerable key. The OQ-121 typed-absence convention
% (docs/design/design_discipline.md §5) binds exactly these predicates: on its
% declared domain the reading must return an EXPLICIT token (out-of-domain /
% absence / measured), never fail silently — a bare failure collapses
% didn't-apply / measured-clear / didn't-look into one missing token at every
% read site (Build Discipline Pattern 6 at the source).
%
% The registry is the machine-checkable half of that convention:
% tests/test_reading_totality.pl walks every total_on_domain entry and proves
% exactly-one-solution over the entry's declared domain (totality AND
% determinism — a nondeterministic reading over-counts in an aggregate exactly
% the way a partial one under-counts).
%
%   aggregatable_reading(Module:Pred/Arity, DomainSpec, Class)
%     DomainSpec ∈ {corpus_constraint, seat}   (extend reading_domain_key/2)
%     Class      ∈ {total_on_domain, partial_by_design(Reason)}
%   The KEY args are the FIRST len(DomainSpec-key) arguments of the predicate;
%   remaining arguments are outputs.
%
%   partial_by_design(Reason): genuinely relational predicates whose off-domain
%   silence is correct (e.g. a per-seat lookup keyed on an existing seat).
%   Recorded so "not total" is a DECLARED class, not an unexamined absence; the
%   totality suite does not run them. A predicate that fails ON its own declared
%   domain is neither class — it is a defect: fix it to a typed token (§5),
%   then register it total_on_domain.
%
% ANTI-FORK BRIDGE (Pattern 2): census_source_backing/2 maps every
% commentary_census source to the registry entry backing its commentary_cell/3
% hook. test_reading_totality.pl proves forall commentary_source(S) a backing
% entry exists and is registered total_on_domain — so a census source can never
% be absent from the totality registry (the census's Σ==n_corpus invariant
% presumes exactly the property this registry tests).
%
% LOADING: not loaded by stack.pl (mirrors commentary_census.pl) — load
% alongside: swipl -l stack.pl -l reading_registry.pl ... . Module-qualified
% calls into corpus_loader/narrative_ontology bind at call time, so this file
% loads cleanly on its own; the predicates must be resident when the domain
% enumeration runs (they are, under [stack]).
%
% REGISTRATION IS OPT-IN (named residual risk, OQ-137 close): a new reading
% predicate that never registers escapes the totality suite entirely. When you
% ADD an aggregatable reading, add its entry here in the same change.
% ============================================================================

:- module(reading_registry, [
    aggregatable_reading/3,
    census_source_backing/2,
    reading_domain_key/2
]).

:- use_module(corpus_loader).

% Extension points are multifile so a future module can register its own
% readings without editing this file.
:- multifile aggregatable_reading/3.
:- multifile census_source_backing/2.

% ----------------------------------------------------------------------------
% DOMAIN ENUMERATION  reading_domain_key(+DomainSpec, -KeyArgs)
%   One solution per domain member; KeyArgs is the list of key arguments in
%   predicate-argument order. Corpus membership is corpus_constraint/1 — the
%   authoritative denominator (never constraint_metric unions).
% ----------------------------------------------------------------------------
reading_domain_key(corpus_constraint, [C]) :-
    corpus_loader:corpus_constraint(C).
reading_domain_key(seat, [C, Name]) :-
    corpus_loader:corpus_constraint(C),
    narrative_ontology:constraint_stakeholder(C, Name, _, _, _, _, _).
% (C, its computed signature) — registering a reading here proves every
% signature the corpus computes has that reading (e.g. an explanation clause);
% a new signature atom without one becomes a suite failure, not a silent hole.
reading_domain_key(constraint_signature_pair, [C, Sig]) :-
    corpus_loader:corpus_constraint(C),
    signature_detection:constraint_signature(C, Sig).
% distinct kernels of the loaded corpus
% NOTE the parens around the ^-scoped goals: `C^m:g(...)` parses as
% `(C^m):g(...)` (`:` is priority 600 > `^` 200) and breaks setof silently.
reading_domain_key(kernel, [K]) :-
    setof(K0, C^(narrative_ontology:cs_kernel_id(C, K0)), Ks),
    member(K, Ks).
% all story-UIDs of the loaded corpus (cs_story_uid/2 surrogate identity)
reading_domain_key(story_uid, [UID]) :-
    setof(U, C^(narrative_ontology:cs_story_uid(C, U)), Us),
    member(UID, Us).
% story-UIDs that author a cs_drift_state/3 (the drift engine's declared domain)
reading_domain_key(drift_story, [UID]) :-
    setof(U, C^G^( narrative_ontology:cs_story_uid(C, U),
                   narrative_ontology:cs_drift_state(U, _, G) ), Us),
    member(UID, Us).

% ----------------------------------------------------------------------------
% REGISTRY — seeded with the proven-total family (OQ-121 hand-fixes + the two
% never-fail templates). Exactly-one over the live corpus witnessed 2026-07-02
% (N=119 constraints, 661 seats, 0 violations on every entry below).
% ----------------------------------------------------------------------------
aggregatable_reading(stakeholder_seats:q6_crosscheck/3,        corpus_constraint, total_on_domain).
aggregatable_reading(stakeholder_seats:extraction_state/2,     corpus_constraint, total_on_domain).
aggregatable_reading(stakeholder_seats:consensus_provenance/2, corpus_constraint, total_on_domain).
aggregatable_reading(stakeholder_seats:seat_perceived_vs_real/4, seat,            total_on_domain).
aggregatable_reading(signature_detection:constraint_signature/2, corpus_constraint, total_on_domain).

% Phase-5 sweep additions (audits/2026-07-02_oq137_reading_totality/ — the
% classification_table.md row is the evidence for every entry below).
aggregatable_reading(stakeholder_seats:stakeholder_context/3,  seat, total_on_domain).
aggregatable_reading(stakeholder_seats:derive_directionality_for_stakeholder/3, seat, total_on_domain).
aggregatable_reading(stakeholder_seats:power_witness_map/2,    corpus_constraint, total_on_domain).
aggregatable_reading(signature_detection:signature_confidence/3, constraint_signature_pair, total_on_domain).
aggregatable_reading(signature_detection:explain_signature/3,    constraint_signature_pair, total_on_domain).
aggregatable_reading(signature_detection:structural_purity/2,    corpus_constraint, total_on_domain).
aggregatable_reading(signature_detection:has_viable_alternatives/2, corpus_constraint, total_on_domain).
aggregatable_reading(cs_pattern_detection:cs_pattern/3,          corpus_constraint, total_on_domain).
aggregatable_reading(cs_kernel_registry:cs_kernel_coverage/2,    kernel, total_on_domain).
aggregatable_reading(cs_kernel_registry:cs_kernel_obstruction_status/2, kernel, total_on_domain).
aggregatable_reading(cs_drift_engine:cs_drift_trajectory/3,      drift_story, total_on_domain).

% Partial-by-design (relational; off-domain silence is the domain, not a
% defect). The doc-named case from OQ-137's scope discriminator:
aggregatable_reading(stakeholder_seats:in_contention/3, corpus_constraint,
    partial_by_design('relation between seats, not a per-constraint verdict; holds only where a beneficiary-side/payer pair shares a power atom')).
aggregatable_reading(stakeholder_seats:dr_type_for_stakeholder/3, seat,
    partial_by_design('raw per-seat computation, can fail by design; totalized wrapper = seat_perceived_vs_real/4 (Computed=untyped)')).
aggregatable_reading(stakeholder_seats:chi_for_stakeholder/3, seat,
    partial_by_design('raw per-seat chi, same shape as dr_type_for_stakeholder/3; no totalized wrapper yet — first candidate if an aggregate ever consumes per-seat chi')).
aggregatable_reading(stakeholder_seats:power_witness_count/3, corpus_constraint,
    partial_by_design('per-power-atom expansion (6 solutions per C); power_witness_map/2 is the exactly-one surface')).
aggregatable_reading(stakeholder_seats:extraction_reading/2, corpus_constraint,
    partial_by_design('fires exactly on extraction_fired; total surface = extraction_state/2')).
aggregatable_reading(signature_detection:false_natural_law/2, corpus_constraint,
    partial_by_design('detection verdict; total surface = constraint_signature/2')).
aggregatable_reading(signature_detection:false_summit_mountain/2, corpus_constraint,
    partial_by_design('detection verdict; total surface = constraint_signature/2')).
aggregatable_reading(signature_detection:coupling_invariant_rope/2, corpus_constraint,
    partial_by_design('one solution per coupling witness (multi); consumers must once/1 it')).
aggregatable_reading(signature_detection:false_ci_rope/2, corpus_constraint,
    partial_by_design('detection verdict; total surface = constraint_signature/2')).
aggregatable_reading(signature_detection:has_metric_perspectival_variance/1, corpus_constraint,
    partial_by_design('boolean; consumers treat failure as false (guarded). CAVEAT: failure collapses no-authored-perspective-metrics with authored-equal-metrics')).
aggregatable_reading(signature_detection:level_gradient_divergence/2, corpus_constraint,
    partial_by_design('OQ-93 Stage D detection; consumer degrades gracefully to the grid-less question')).
aggregatable_reading(cs_pattern_detection:cs_has_fields/1, corpus_constraint,
    partial_by_design('domain gate; the typed absence is cs_pattern/3''s cs_fields_absent')).
aggregatable_reading(cs_pattern_detection:cs_verdict/2, corpus_constraint,
    partial_by_design('enumerates FIRED verdicts (several can co-fire); didn''t-look is carried by cs_pattern/3')).
aggregatable_reading(cs_pattern_detection:cs_naturalized_mountain/1, corpus_constraint,
    partial_by_design('detection; dark on the live corpus')).
aggregatable_reading(cs_pattern_detection:cs_authority_masking/3, corpus_constraint,
    partial_by_design('detection with evidence args')).
aggregatable_reading(cs_pattern_detection:cs_cover_story_active/2, corpus_constraint,
    partial_by_design('detection')).
aggregatable_reading(cs_pattern_detection:cs_displaced_beneficiary/1, corpus_constraint,
    partial_by_design('detection; dark on the live corpus')).
aggregatable_reading(cs_pattern_detection:cs_grounding_mismatch/3, corpus_constraint,
    partial_by_design('detection')).
aggregatable_reading(cs_axiom_engine:cs_has_axioms/1, story_uid,
    partial_by_design('UID-keyed field gate (NOT constraint-name — silent wrong-key trap, doc fixed 2026-07-02); no consumers yet')).
aggregatable_reading(cs_axiom_engine:cs_axiom_inconsistent/2, story_uid,
    partial_by_design('UID-keyed detection (same wrong-key trap); requires authored cs_axiom_contradiction/2')).
aggregatable_reading(cs_axiom_engine:cs_axiom_foreclosed/2, story_uid,
    partial_by_design('UID-keyed detection; one solution per foreclosed atom')).
aggregatable_reading(cs_pattern_detection:cs_drift_unacknowledged/2, story_uid,
    partial_by_design('UID-keyed detection')).
aggregatable_reading(cs_trifurcation:cs_reading_trifurcation/3, kernel,
    partial_by_design('fails on singleton kernels by contract ("not contested, no verdict"); typed surface for singletons = cs_kernel_obstruction_status/2')).
aggregatable_reading(cs_kernel_registry:cs_kernel_divergence/4, kernel,
    partial_by_design('relational enumeration of divergence pairs')).

% ----------------------------------------------------------------------------
% ANTI-FORK BRIDGE  census_source_backing(?Source, ?Entry)
%   Every commentary_census:commentary_source/1 must appear here, and its Entry
%   must be a total_on_domain registry entry (enforced by
%   tests/test_reading_totality.pl:census_sources_all_registered).
% ----------------------------------------------------------------------------
census_source_backing(q6,                 stakeholder_seats:q6_crosscheck/3).
census_source_backing(extraction_reading, stakeholder_seats:extraction_state/2).
census_source_backing(consensus,          stakeholder_seats:consensus_provenance/2).
