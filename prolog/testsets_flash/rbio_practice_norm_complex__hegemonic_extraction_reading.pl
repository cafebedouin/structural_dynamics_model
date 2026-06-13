% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Norms: Hegemonic Extraction Reading
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint describes the RBIO (Rules-Based International Order)
 *   norms as a mechanism for hegemonic extraction, focusing on their
 *   practical un-amendability and selective enforcement. While formally
 *   presented as a 'rope' for global coordination, this reading argues the
 *   system functions as a 'snare' for Global South states and populations,
 *   benefiting P5 states and associated capital. The P5 veto power and
 *   institutional path-dependency prevent genuine revision, and enforcement
 *   is applied selectively to maintain a favorable economic and political
 *   order.
 *
 * KEY AGENTS:
 *   - us_european_capital: Primary beneficiary (institutional/arbitrage) — benefits from facilitated capital flows and market access.
 *   - p5_states: Agenda setter (institutional/constrained) — wield veto power, selectively enforce norms.
 *   - global_south_states: Primary target (powerless/trapped) — subjected to conditionalities, lack power to amend norms.
 *   - global_south_populations: Primary target (powerless/identity_locked) — bear social/economic costs, identity tied to national self-determination.
 *   - liberal_institutionalists: Analytical observer (analytical/analytical) — interpret norms as universal and consent-based.
 *   - sovereignty_maximalists: Excluded voice (organized/constrained) — advocate for absolute sovereignty, marginalized in formal discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.85).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.9).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Norms: Hegemonic Extraction Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '2187f10b-d160-48cb-a697-e9160bdfac4b').
narrative_ontology:cs_kernel_codification('2187f10b-d160-48cb-a697-e9160bdfac4b', formalized).
narrative_ontology:cs_authority_grounding('2187f10b-d160-48cb-a697-e9160bdfac4b', extraction).
narrative_ontology:cs_interpretation_layer_present('2187f10b-d160-48cb-a697-e9160bdfac4b').
narrative_ontology:cs_reading_relation('2187f10b-d160-48cb-a697-e9160bdfac4b', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('2187f10b-d160-48cb-a697-e9160bdfac4b', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('2187f10b-d160-48cb-a697-e9160bdfac4b', foundational, intervention_without_consent_illegitimate).
narrative_ontology:cs_axiom_status(intervention_without_consent_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2187f10b-d160-48cb-a697-e9160bdfac4b', intervention_without_consent_illegitimate, deontological).
narrative_ontology:cs_axiom('2187f10b-d160-48cb-a697-e9160bdfac4b', foundational, conditionality_as_coerced_contract).
narrative_ontology:cs_axiom_status(conditionality_as_coerced_contract, holdable).
narrative_ontology:cs_axiom_grounding('2187f10b-d160-48cb-a697-e9160bdfac4b', conditionality_as_coerced_contract, empirically_contingent).
narrative_ontology:cs_reference_frame('2187f10b-d160-48cb-a697-e9160bdfac4b', post_wwii_multilateralism).
narrative_ontology:cs_drift_state('2187f10b-d160-48cb-a697-e9160bdfac4b', contemporary_multipolar_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2187f10b-d160-48cb-a697-e9160bdfac4b', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability and enforceability of RBIO norms that facilitate global capital flows, resource extraction, and market access, often through conditionalities that open up economies. Can leverage the P5 veto power to protect its interests.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital, beneficiary,
    institutional, generational, arbitrage, global).

% As permanent members of the UN Security Council, they hold veto power over formal amendments to RBIO norms, effectively freezing the normative framework. They selectively enforce norms to align with their strategic and economic interests, often benefiting their domestic capital.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_states, agenda_setter,
    institutional, generational, constrained, global).

% Subjected to conditionalities and interventions justified by RBIO norms, which often lead to structural adjustments that benefit external capital. They lack the power to amend the norms or resist enforcement without severe economic or political repercussions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    powerless, generational, trapped, global).

% Bear the social and economic costs of structural adjustments and interventions, experiencing reduced sovereignty over their resources and development paths. Their identity is often tied to national self-determination, making compliance with externally imposed norms a form of identity-lock.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations, payer,
    powerless, biographical, identity_locked, global).

% Analyze RBIO norms as a system of universal, consent-based rules, emphasizing their potential for cooperation and mutual benefit. They tend to attribute enforcement selectivity to capacity issues or political will rather than structural extraction.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutionalists, observer,
    analytical, generational, analytical, global).

% Advocate for absolute state sovereignty and view RBIO norms as legitimate only when they protect national self-determination. They often interpret humanitarian interventions as pretexts for regime change and economic conditionalities as coercive contracts, but their views are marginalized in dominant international forums.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_maximalists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international cooperation, trade, and security, ostensibly to prevent conflict and facilitate global governance.
% TRANSFER_FUNCTION: Transfers economic resources, political autonomy, and decision-making power from Global South states and populations to P5 states and associated capital interests, under the guise of 'good governance' or 'stability'.
% ABSENT_VOICES: Sovereignty maximalists and anti-imperialist movements from the Global South are largely excluded from the formal amendment and enforcement mechanisms, despite being directly impacted. Their objections to the extractive nature of the norms are dismissed as obstructionist or illegitimate.
% DISAPPEARANCE_RATIONALE: If RBIO norms vanished, the existing global economic and political order would face immediate and profound disruption. Capital flows would be re-routed, interventions would lose their legal pretexts, and Global South states would reclaim significant policy space, leading to a fundamental rearrangement of power dynamics.
% FOUNDING_PROBLEM: The need for a stable, predictable international order to prevent large-scale conflict and facilitate global economic interaction after World War II.
% FOUNDING_PROBLEM_CORROBORATION: P5 states and liberal institutionalists assert the founding problem of global instability and economic fragmentation remains live. Global South states and critical scholars, from outside the benefiting parties, argue that while stability is still desired, the norms have been co-opted to serve hegemonic interests, making the original problem a cover for ongoing extraction.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the norms enable significant wealth transfer and policy capture. Suppression is very high (0.90) due to the P5 veto, institutional path-dependency, and the severe consequences for states that defy the norms. Theater ratio is high (0.60) as the rhetoric of universalism and consent-based governance increasingly masks the underlying extractive function and selective enforcement. The temporal measurements show a clear trend of increasing extractiveness, suppression, and theatricality since 1945, reflecting the hardening of the hegemonic project.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of P5 states and global capital, RBIO norms are a legitimate and necessary framework for global stability and prosperity (a 'rope' or 'mountain'). From the perspective of Global South states and populations, the same norms operate as a coercive mechanism for resource and policy extraction (a 'snare'). The engine's classification will highlight this divergence by computing a snare from the authored metrics, despite the claimed type being 'rope' by the agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states and US/European capital are clear beneficiaries (d near 0.0) as they shape and benefit from the system. Global South states and populations are clear targets (d near 1.0) as they bear the costs of conditionalities and interventions with limited exit options. Liberal institutionalists are analytical observers (d=0.5). Sovereignty maximalists are excluded, their d is high due to their structural marginalization and the costs of non-compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (global stability, cooperation) has been co-opted. While the original problem of preventing global conflict remains, the mechanisms designed to address it have become self-serving for the powerful. The P5 veto and path-dependency prevent the system from adapting to new global power distributions or addressing the concerns of the Global South, indicating a severe case of mandatrophy where the original function is now a cover for extraction. The high theater ratio and increasing extractiveness over time are key indicators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p5_veto_vs_amendability,
    'Is the P5 veto a necessary stability mechanism or a structural barrier to equitable norm revision?',
    'Analysis of counterfactual scenarios where the veto is absent, or empirical study of norm evolution in international bodies without veto powers.',
    'If a necessary stability mechanism, the un-amendability is a feature of a ''mountain'' of international politics; if a structural barrier, it reinforces the ''snare'' classification by demonstrating active suppression of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p5_veto_vs_amendability, conceptual, 'Role of P5 veto in RBIO norm revision.').

omega_variable(
    conditionality_coercion_boundary,
    'At what point does ''conditionality'' in RBIO norms (e.g., IMF loans) transition from legitimate policy guidance to coerced contract?',
    'Legal analysis of consent under duress in international law, and empirical studies of policy autonomy loss in recipient states.',
    'If primarily coercive, it strengthens the ''snare'' classification by highlighting the lack of genuine consent; if primarily guidance, it leans towards a ''tangled_rope'' or ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_coercion_boundary, empirical, 'Distinguishing legitimate conditionality from coercion.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''hegemonic_extraction_reading'' of the RBIO practice/norm complex, or does it conflate elements of other readings?',
    'Detailed textual analysis of critical international relations scholarship and Global South policy statements to ensure the core premises align exclusively with this reading''s structural delta.',
    'If conflated, the classification of this specific constraint would be less precise, potentially diluting the signal of extraction. If distinct, it validates the decomposition of the RBIO kernel into separate, structurally unique constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring the distinctness of this kernel reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, global_financial_governance_norms).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, international_humanitarian_law_application).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the RBIO practice/norm complex kernel. This 'hegemonic extraction' reading focuses on the extractive and un-amendable nature of the norms, contrasting with the 'liberal institutional' and 'sovereignty maximalist' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
