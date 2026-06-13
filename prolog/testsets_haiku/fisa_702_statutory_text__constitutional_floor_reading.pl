% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Warrant Requirement as Constitutional Floor for 702 Queries
 *   domain: constitutional_law/national_security
 *
 * SUMMARY:
 *   This constraint instantiates the constitutional-floor reading of Section
 *   702 FISA authority: regardless of the statute's foreign intelligence
 *   purpose language, the Fourth Amendment requires individualized probable
 *   cause warrant review for any government search of U.S. person
 *   communications content retained in the 702 database. The reading reframes
 *   the legal question from 'does statutory authorization justify
 *   collection?' to 'does constitutional search doctrine apply to access?'.
 *   Under this reading, 702 is not an exception to Fourth Amendment limits
 *   but a source of data whose access remains constitutionally gated. This is
 *   ONE READING of the contested kernel: fisa_702_statutory_text. The other
 *   readings (foreign_target_strict, incidental_collection) instantiate
 *   alternative positions on whether statutory foreign intelligence purpose
 *   creates a sufficient justification for unwarranted query access.
 *
 * KEY AGENTS:
 *   - u_s_persons_subject_to_query: Powerless individuals whose communications are incidentally retained; under this reading they gain constitutional protection.
 *   - fisa_court: Institutional agenda-setter; must apply Fourth Amendment probable cause standard rather than deferential foreign intelligence review.
 *   - executive_intelligence_agencies: Institutional payer; lose warrantless query access and must satisfy heightened burden for domestic-side queries.
 *   - congress: Institutional observer; statutory authority is not superseded but is read subject to constitutional floor.
 *   - civil_rights_organizations: Organized beneficiary; achieve constitutional framing of privacy protection.
 *   - supreme_court: Institutional observer (analytical seat); final arbiter of whether this reading becomes binding doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.15).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Warrant Requirement as Constitutional Floor for 702 Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security").

domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31').
narrative_ontology:cs_kernel_codification('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', fixed_text).
narrative_ontology:cs_authority_grounding('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', lineage).
narrative_ontology:cs_interpretation_layer_present('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31').
narrative_ontology:cs_reading_relation('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_axiom('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', foundational, fourth_amendment_applies_to_database_queries).
narrative_ontology:cs_axiom_status(fourth_amendment_applies_to_database_queries, holdable).
narrative_ontology:cs_axiom_grounding('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', fourth_amendment_applies_to_database_queries, deontological).
narrative_ontology:cs_axiom('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', foundational, warrant_requirement_universal_to_searches).
narrative_ontology:cs_axiom_status(warrant_requirement_universal_to_searches, holdable).
narrative_ontology:cs_axiom_grounding('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', warrant_requirement_universal_to_searches, deontological).
narrative_ontology:cs_reference_frame('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', fourth_amendment_core_doctrine).
narrative_ontology:cs_drift_state('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', post_snowden_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fc0bec2d-4f6c-4b4a-ab08-96c8a48b3f31', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_subject_to_query).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, ExtMetricName, E),
    domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.25—substantially lower than the statutory foreign intelligence reading (incidental_collection ≈ 0.80) because the constraint's core claim is that constitutional limits apply regardless of efficiency preferences. The executive's extraction (speed/secrecy advantage) is real under current law but diminishes under this reading's adoption. The measurement series (2008–2026) models the trajectory: high extractiveness (0.88) at 702 inception when warrantless access was unquestioned, declining to 0.25 as constitutional scrutiny (Snowden disclosure, Carpenter decision, civil society pressure) establishes probable cause as a realistic requirement. Theater ratio remains very low (0.05) throughout because the constraint is presented as a straightforward constitutional principle, not as a constructed doctrine hiding extraction. Suppression requirement is low (0.15) because the constraint gains legitimacy from constitutional text and grows through judicial reasoning, not through coercive enforcement of a contentious rule.
 *
 * PERSPECTIVAL GAP:
 *   The executive intelligence agencies and the u_s_persons_subject_to_query seats experience this constraint in opposite frames. From the agency position, the constraint extracts operational efficiency and forces legal overhead. From the u_s_person position, the constraint provides constitutional protection and assurance of judicial review. The FISA Court, as institutional agenda-setter, sits between: they must apply the constitutional standard, which increases their workload but grants them authority over a previously executive-dominated domain. The engine computes directionality from the structural data: executive agencies are targets (constrained exit, powerful but now facing legal limits); u_s_persons are beneficiaries (powerless but protected); civil rights organizations are beneficiaries (organized, mobile exit, gain constitutional vindication). This divergence should produce different per-seat classifications: executive agencies compute as facing extractive burden, u_s_persons compute as gaining protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: u_s_persons_subject_to_query (powerless, trapped exit, gain constitutional warrant protection), civil_rights_organizations (organized, mobile exit, achieve constitutional framing). Victims/Payers: executive_intelligence_agencies (institutional power, constrained exit, lose warrantless access speed). The FISA Court and Congress are institutional observers/allocators rather than direct payers or beneficiaries—their roles are to implement and referee, not to collect extraction or bear its cost directly. Directionality for executive agencies: d approaches 1.0 (full target) because they lose a legal freedom they previously held. Directionality for u_s_persons: d approaches 0.0 (full beneficiary) because they gain protection without bearing cost directly. No overrides are needed—the derivation chain from beneficiary/victim declarations and exit options produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Amendment protection of communications privacy) remains live as a constitutional matter independent of surveillance technology. This reading asserts that the problem has NOT been solved by statutory foreign intelligence authorization—that statutory authorizations cannot override constitutional requirements. The disappearance verdict (world_rearranges) confirms that if the constraint vanished, U.S. persons would lose Fourth Amendment protection for their communications in foreign intelligence databases, and institutional architecture would collapse to efficiency-driven executive discretion. This prevents mandatrophy: the founding problem and the constraint's utility remain aligned. The constraint is not a vestigial institutional artifact but an active doctrinal requirement. Mandatrophy is foreclosed by the founding problem's continued salience and the constraint's role in vindicating the Fourth Amendment itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_statutory_reading_contest,
    'Does the Fourth Amendment doctrine of ''searches'' encompass database queries into incidentally retained communications, or do foreign intelligence statutory authorizations create a carve-out from Fourth Amendment scope?',
    'Supreme Court determination in a case presenting the precise issue: whether querying Section 702 data for U.S. person information constitutes a Fourth Amendment search requiring warrant/probable cause.',
    'If Fourth Amendment applies to 702 queries, this reading becomes doctrine and ε drops to ~0.25 (extraction is now constitutional compliance cost, not operational efficiency gain). If foreign intelligence exception holds, sibling incidental_collection_reading becomes binding and ε rises to ~0.80.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_statutory_reading_contest, conceptual, 'Whether Fourth Amendment searches doctrine applies to Section 702 database queries.').

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is this constraint a discovered constitutional floor (natural law of the republic''s founding document), or a constructed constitutional interpretation that benefits civil rights advocates and burdens executive efficiency?',
    'Originalist historical analysis of Fourth Amendment text and ratification context; comparison of this reading''s derivation to other constitutional interpretation methodologies (living Constitution, purposivism); examination of whether early Fourth Amendment doctrine anticipated digital communications and database query scenarios.',
    'If this constraint emerges naturally from constitutional text and history independent of contemporary policy preferences, it is Mountain-grade. If the reading selectively emphasizes certain historical elements and downplays executive necessity, some extraction is hidden in the interpretive choice. FSM may reclassify to tangled_rope if beneficiary presence (civil rights organizations, privacy advocates) is shown to have driven the reading''s prominence over alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Whether the constitutional floor reading is a natural derivation or a constructed interpretation serving identifiable beneficiaries.').

omega_variable(
    warrant_necessity_empirical,
    'Does individualized probable cause review of 702 queries materially degrade foreign intelligence collection effectiveness, or is the assertion of operational cost exaggerated?',
    'Comparative analysis of intelligence effectiveness metrics before and after warrant requirement implementation; FISA Court caseload and approval rate analysis; testimony from intelligence professionals on actual operational impacts vs. theoretical speed loss.',
    'If warrant requirements significantly degrade intelligence capability, the extraction (measured as executive agency speed/secrecy loss) may be reconceptualized as necessary cost rather than extraction per se, shifting ε downward further or validating the mountain classification. If operational impact is minimal, the constraint appears as pure constitutional enforcement with negligible extraction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrant_necessity_empirical, empirical, 'Whether warrant requirements materially degrade foreign intelligence effectiveness.').

omega_variable(
    incidental_vs_targeted_boundary,
    'Can Section 702 collection be cleanly divided into foreign-targeted communications (no Fourth Amendment issue) and incidentally collected U.S. person communications (warrant-protected), or are the two categories entangled such that the boundary is unenforceable?',
    'Technical analysis of Section 702 collection and query systems; examination of whether NSA/CIA systems can segregate incidental from primary data; FISA Court experience with distinguishing queries by target category.',
    'If the boundary is clean, this reading''s warrant requirement is administratively feasible and ε remains ~0.25. If entangled, the warrant requirement becomes a structural impossibility, and the reading forecloses in practice (even if the constitutional principle survives). This resolves toward the foreign_target_strict_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_vs_targeted_boundary, empirical, 'Whether incidental and targeted 702 data can be administratively distinguished for warrant purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(id_702_theater_t2008_inauguration, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.02).
narrative_ontology:measurement(id_702_theater_t2013_snowden, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2013, 0.03).
narrative_ontology:measurement(id_702_theater_t2018_carpenter, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2018, 0.04).
narrative_ontology:measurement(id_702_theater_t2023_judicial_scrutiny, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2023, 0.05).
narrative_ontology:measurement(id_702_theater_t2026_constitutional_floor, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(id_702_extractiveness_t2008_inauguration, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.88).
narrative_ontology:measurement(id_702_extractiveness_t2013_snowden, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2013, 0.85).
narrative_ontology:measurement(id_702_extractiveness_t2018_carpenter, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2018, 0.72).
narrative_ontology:measurement(id_702_extractiveness_t2023_judicial_scrutiny, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(id_702_extractiveness_t2026_constitutional_floor, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2026, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(id_702_suppression_t2008_inauguration, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.05).
narrative_ontology:measurement(id_702_suppression_t2013_snowden, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2013, 0.12).
narrative_ontology:measurement(id_702_suppression_t2018_carpenter, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2018, 0.14).
narrative_ontology:measurement(id_702_suppression_t2023_judicial_scrutiny, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2023, 0.15).
narrative_ontology:measurement(id_702_suppression_t2026_constitutional_floor, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2026, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__constitutional_floor_reading, 0.18).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_digital_communications_scope).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_court_authority_constitutional_limits).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested fisa_702_statutory_text kernel. The sibling readings (foreign_target_strict_reading, incidental_collection_reading) are separate constraint stories with different ε values and structural data. The kernel contest is the constitutional/statutory framing of Section 702 authority. All three readings must be linked via network.affects_constraints to enable the kernel-tracking infrastructure to identify their common genealogy and measure inter-reading coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
