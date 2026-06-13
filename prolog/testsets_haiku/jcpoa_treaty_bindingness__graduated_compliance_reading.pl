% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Commitment Framework
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action, 2015) represents a
 *   reciprocal commitment framework where Iran restricts uranium enrichment
 *   and submits to intrusive IAEA inspections in exchange for graduated
 *   sanctions relief. Under the graduated compliance reading, enforcement is
 *   calibrated to violation severity: minor enrichment increases trigger
 *   proportional sanction adjustments rather than comprehensive reimposition.
 *   This reading interprets the treaty as a workable mechanism for managing
 *   partial engagement under uncertainty, prioritizing de-escalation and
 *   reciprocal adjustment over binary outcomes (war/capitulation). The
 *   constraint is CLAIMED as tangled_rope (coordination of enrichment limits
 *   + asymmetric enforcement on Iran) while metrics describe substantial
 *   extractiveness (0.58) and moderate suppression (0.42) — the gap reflects
 *   contest over whether the constraint's persistence rests on genuine mutual
 *   benefit or coercive extraction disguised as reciprocity.
 *
 * KEY AGENTS:
 *   - Joint Commission (institutional agenda-setter): sets enforcement thresholds, assesses compliance proportionality, triggers sanction adjustments
 *   - Iran (payer): restricts enrichment, admits inspections, receives conditional sanctions relief
 *   - European economic actors (beneficiary+payer): benefit from partial sanctions relief, pay political cost of escalation
 *   - Pragmatic diplomacy advocates (beneficiary): benefit from de-escalation framing, maintain soft-power investment
 *   - US maximalist actors (excluded): would escalate to binary sanctions/military; structurally outside the graduated framework
 *   - IAEA (observer): reports technical compliance metrics underlying dispute resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.58).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Commitment Framework").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222').
narrative_ontology:cs_kernel_codification('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', fixed_text).
narrative_ontology:cs_authority_grounding('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', extraction).
narrative_ontology:cs_interpretation_layer_present('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222').
narrative_ontology:cs_reading_relation('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', foundational, proportional_enforcement_reciprocal_commitment).
narrative_ontology:cs_axiom_status(proportional_enforcement_reciprocal_commitment, holdable).
narrative_ontology:cs_axiom_grounding('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', proportional_enforcement_reciprocal_commitment, instrumental).
narrative_ontology:cs_axiom('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', secondary, multilateral_consensus_modification_requirement).
narrative_ontology:cs_axiom_status(multilateral_consensus_modification_requirement, holdable).
narrative_ontology:cs_axiom_grounding('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', multilateral_consensus_modification_requirement, conventional).
narrative_ontology:cs_reference_frame('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', reciprocal_graduated_enforcement).
narrative_ontology:cs_drift_state('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', contemporary_unilateral_escalation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8a0d37b-7ffa-417c-b8c3-6a4b25e6a222', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, european_economic_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, global_energy_markets).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_economic_interests).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, usmca_states_under_secondary_sanctions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, non_alignment_movement_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 2015 (0.35, low initial cooperation) to 2019 (0.58, peak US withdrawal + escalation) and remains elevated through stabilization at 2024 (0.58). Theater peaks in 2021 (0.61) when Iran increases enrichment as compliance leverage falls, then drops to 0.48 as both sides restore graduated enforcement. The constraint is extractive rather than purely coordinating because: (1) Iran's enrichment restrictions are asymmetrically monitored and reversible only through costly escalation; (2) sanctions relief is conditional on Joint Commission assessment, not automatic; (3) US actors can withdraw unilaterally, making Iran's reciprocal investment perpetually at risk. Suppression requirement follows a similar arc: low in 2015 (mutual trust in negotiation), high in 2019 (US withdrawal forced Iran to demonstrate commitment through enrichment escalation, simultaneously proving the constraint lacked real binding power), then stabilizes at 0.42 as graduated logic reasserts. The measurement series shares one time grid (2015, 2017, 2019, 2021, 2024, 2026) across all three metrics, enabling lifecycle analysis. Theater's rise-and-fall pattern reflects the constraint's shift from genuine coordination mechanism (2015–2017) to crisis theater (2019–2021) to partial restoration (2024–2026).
 *
 * PERSPECTIVAL GAP:
 *   The Joint Commission and Iran's compliance authority see the graduated framework as workable reciprocity calibrated to practical verification limits. US maximalist and Israeli security audiences read the same constraint as a legitimized appeasement mechanism that extracts Iranian compliance while preserving Iran's enrichment knowledge base (a zombie nuclear capability). European economic actors occupy the middle: they benefit from partial sanctions relief but are perpetually vulnerable to US escalation that reimposes secondary sanctions. The engine should compute markedly different types from each seat: the Joint Commission and Iran experience a binding reciprocal constraint (tangled_rope or even rope from the perspective of mutual gain); US/Israeli audiences compute snare (Iran is trapped, concessions are temporary, enrichment restrictions are extractive cover for knowledge preservation). The perspectival gap reflects the reading itself: graduated compliance is tenable ONLY if all signatories commit to proportionality; if any party pivots to maximalism (binary enforcement), the graduated logic fails and Iran experiences pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran (moderate power, constrained exit, identity-locked to non-alignment framing) is the structural payer: its enrichment is restricted, inspections are intrusive, and sanction relief is conditional. Iranian directionality should compute high (near 1.0 = full target). European economic actors (powerful, moderate time horizon, constrained exit due to US geopolitical pressure) experience moderate extraction: they benefit from partial sanctions relief but pay political cost and secondary-sanction risk. Pragmatic diplomacy advocates (organized, mobile exit, analytical time horizon) are beneficiaries: they benefit from the framing of reciprocal governance without bearing enrichment costs. Global energy markets (organized, immediate time horizon, mobile exit via diversification) benefit from supply stability without formal commitment. US maximalist actors are excluded entirely, so directionality is not applicable; however, their exclusion is precisely what makes Iran's exit option 'constrained' — Iran cannot negotiate directly with actors who reject the graduated reading. Directionality overrides may be needed for the Joint Commission (institutional power, trapped exit, agenda-setter role) — the automatic derivation might place it near symmetric (d ≈ 0.5), but structurally it acts as an enforcement authority subordinate to P5+1 political will, so d should be pushed toward 0.6–0.7 (beneficiary-of-institutional-power rather than neutral arbiter).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophy-resolved. The founding problem (how to link nuclear non-proliferation assurance to reciprocal economic engagement) remains contested but demonstrably LIVE: IAEA data show enrichment restraint when sanctions relief is active and rapid acceleration when it is withdrawn, confirming the graduated incentive structure operates. However, the constraint exhibits signs of mandate-drift: the founding problem was nuclear non-proliferation + economic development; the persistent function is now nuclear non-proliferation + preservation of US geopolitical leverage. If US actors permanently escalate to maximalism, the constraint could shift from tangled_rope (with real reciprocal function) to piton (with performative compliance theater masking terminal deterioration). This risk is documented in the theater metric's 2019–2021 spike (0.55→0.61) when the constraint's actual binding power broke and Iran compensated with enrichment escalation to signal credibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocal_extraction_boundary,
    'At what level of Iranian enrichment increase does proportional sanctions relief withdrawal transform from calibrated enforcement into pure extraction leverage?',
    'Post-facto analysis comparing Joint Commission thresholds for relief withdrawal against IAEA technical assessments of weapons-capability proximity. If thresholds are symmetric across signatories (relief-withdrawal-for-enrichment-above-X matched by enrichment-decrease-for-relief-grant-below-Y), reciprocity holds; asymmetric thresholds indicate extraction.',
    'Symmetric thresholds support tangled_rope (real reciprocal function); asymmetric thresholds reclassify to snare (Iran is trapped, relief is extractive conditioning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_extraction_boundary, empirical, 'Whether the graduated enforcement framework is structurally reciprocal or asymmetrically extractive.').

omega_variable(
    reading_stability_unilateral_escalation,
    'Does the graduated compliance reading survive if any single signatory (particularly the US) pivots to maximalist unilateral enforcement?',
    'Temporal analysis: if US/other signatories escalate enforcement beyond graduated proportionality (e.g., comprehensive re-sanctions despite minimal enrichment excess), does Iran''s enrichment accelerate beyond the agreed cap? If yes, the reading''s binding function dissolves.',
    'If unilateral escalation causes reading collapse, reclassify to snare (Iran is trapped; graduated framing is cover for coercive extraction). If graduated logic persists despite escalation attempts, reading is stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability_unilateral_escalation, empirical, 'Structural robustness of the graduated compliance reading against maximalist unilateral enforcement.').

omega_variable(
    founding_problem_live_vs_dead,
    'Is the founding problem (verifiable non-proliferation + reciprocal sanctions relief) still live, or has the constraint become a zombie mechanism preserving enrichment knowledge while extracting compliance theater?',
    'Measure Iranian breakout timeline to weapons-grade enrichment under full JCPOA compliance vs. post-withdrawal scenarios. If JCPOA compliance extends breakout time materially, the founding problem is live. If enrichment knowledge is preserved in either scenario, the constraint may be extractive theater rather than true non-proliferation assurance.',
    'If founding problem is dead, reclassify to piton (performative non-proliferation compliance, actual function is geopolitical signaling). If founding problem is live, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_vs_dead, empirical, 'Whether the constraint''s original non-proliferation assurance function remains operant or has atrophied into theater.').

omega_variable(
    us_maximalist_reading_coercion,
    'Does the excluded US maximalist reading reflect genuine foreign-policy preference or institutional/ideological coercion preventing pragmatic compliance?',
    'Historical analysis of US policy reversals (2015 negotiation support → 2018 withdrawal → 2021 negotiation resumption) against Congressional testimony, expert consensus, and cost-benefit calculations. If reversals correlate with domestic political pressure rather than Iran policy changes, coercion is present.',
    'If coercion is high, the exclusion of maximalist voices is unjust and the graduated reading is fragile (depends on US domestic-political stability, not on genuine reciprocal commitment). If coercion is low, the reading reflects genuine policy choice and is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_maximalist_reading_coercion, preference, 'Whether the US maximalist reading is excluded due to incoherent preferences or genuine value divergence.').

omega_variable(
    graduated_vs_binary_enforcement_dynamics,
    'Does proportional enforcement (graduated sanctions response) actually reduce Iranian enrichment escalation, or does it function identically to binary enforcement in practice?',
    'Comparative temporal analysis: correlate IAEA enrichment measurements against Joint Commission enforcement actions (relief adjustments) vs. unilateral US escalation periods. If enrichment accelerates identically under both graduated and binary enforcement, the distinction is theater.',
    'If graduated enforcement reduces escalation cycles, tangled_rope is correct. If no difference, the constraint is snare or piton (graduated framing masks binary coercion or performative theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(graduated_vs_binary_enforcement_dynamics, empirical, 'Functional difference between graduated and binary enforcement responses on Iranian compliance behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 2015, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2019, 0.55).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2021, 0.61).
narrative_ontology:measurement(jcpo_tr_t2024, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement(jcpo_tr_t2026, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(jcpo_be_t2024, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement(jcpo_be_t2026, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2017, 0.32).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2019, 0.62).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(jcpo_su_t2024, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement(jcpo_su_t2026, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, us_iran_sanctions_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_framework).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, non_proliferation_treaty_compliance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested jcpoa_treaty_bindingness kernel. The binding_multilateral_reading treats the JCPOA as requiring consensus modification; the transactional_provisional_reading treats it as voidable unilaterally. This graduated_compliance_reading sits structurally between: it acknowledges binding intent but makes enforcement proportional to compliance assessment, creating a scalable rather than binary commitment. All three readings share the same legal text (the kernel) but derive different structural constraints from it due to different framings of authority, reciprocity, and modification procedures. The constraint family should be analyzed as a unified set with cross-reading influence edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
