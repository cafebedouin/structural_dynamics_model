% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public Health Primacy Vaccine Mandate
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   vaccine_mandate_balance. The public_health_primary reading holds that
 *   collective protection supersedes individual consent when voluntary
 *   compliance fails to achieve herd immunity and vulnerable populations face
 *   lethal exposure. It justifies state coercion to override bodily autonomy,
 *   treating the unvaccinated-coerced as subjects of necessity rather than
 *   victims. The kernel's other readings (bodily_autonomy_primary,
 *   proportionality_reading) are structurally distinct constraints and are
 *   not described here per the Îµ-invariance rule.
 *
 * KEY AGENTS:
 *   - public_health_authority: Agenda setter (institutional/constrained) â designs and enforces mandate policy
 *   - vulnerable_populations: Primary beneficiary (powerless/trapped) â medically dependent on community suppression
 *   - mandate_targets: Structural payer (powerless/trapped) â bears coerced compliance and autonomy costs
 *   - civil_liberties_advocates: Excluded voice (organized/analytical) â object but are overridden by necessity framing
 *   - epidemiological_community: Analytical observer (institutional/analytical) â corroborates or contests scientific premises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.78).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.85).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primacy Vaccine Mandate").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'a5adb926-a755-4687-bb6d-ff0c87d3152f').
narrative_ontology:cs_kernel_codification('a5adb926-a755-4687-bb6d-ff0c87d3152f', formalized).
narrative_ontology:cs_authority_grounding('a5adb926-a755-4687-bb6d-ff0c87d3152f', expertise).
narrative_ontology:cs_interpretation_layer_present('a5adb926-a755-4687-bb6d-ff0c87d3152f').
narrative_ontology:cs_reading_relation('a5adb926-a755-4687-bb6d-ff0c87d3152f', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('a5adb926-a755-4687-bb6d-ff0c87d3152f', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('a5adb926-a755-4687-bb6d-ff0c87d3152f', foundational, collective_protection_supersedes_consent).
narrative_ontology:cs_axiom_status(collective_protection_supersedes_consent, holdable).
narrative_ontology:cs_axiom_grounding('a5adb926-a755-4687-bb6d-ff0c87d3152f', collective_protection_supersedes_consent, deontological).
narrative_ontology:cs_reference_frame('a5adb926-a755-4687-bb6d-ff0c87d3152f', public_health_necessity_framework).
narrative_ontology:cs_drift_state('a5adb926-a755-4687-bb6d-ff0c87d3152f', endemic_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5adb926-a755-4687-bb6d-ff0c87d3152f', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, mandate_targets).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, herd_immunity_collective_good).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, state_police_power_public_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccination mandate policy, defines herd immunity thresholds, and enforces compliance through employment restrictions, school exclusions, and civil penalties. Justifies coercion as epidemiologically necessary when voluntary coverage is insufficient to protect vulnerable populations.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Immunocompromised individuals and others with medical contraindications who cannot mount protective immunity. They depend on community transmission suppression for survival and benefit from reduced exposure risk, but cannot exit their biological vulnerability or the mandate regime.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, national).

% Individuals who would not voluntarily vaccinate but are compelled by mandate to accept medical intervention or face loss of livelihood, education, and public participation. They bear the direct cost of coerced bodily intrusion and autonomy override, even though this reading subordinates their consent claims to collective necessity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, mandate_targets, payer,
    powerless, immediate, trapped, national).

% Legal and advocacy organizations arguing for bodily integrity and informed consent as inviolable rights. Their objections are overridden by the public health necessity framing and they are structurally excluded from the policy table when this reading dominates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% Provides empirical estimates of herd immunity thresholds, transmission dynamics, and vaccine effectiveness. External to the state enforcement apparatus, they can corroborate or contest whether the scientific premises for coercion remain valid.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, epidemiological_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Overcoming free-rider problems in vaccination to achieve herd immunity thresholds sufficient to break transmission chains and protect individuals who cannot mount protective immune responses.
% TRANSFER_FUNCTION: Moves bodily autonomy and compliance costs from mandate-targeted individuals to the collective protection pool; moves enforcement authority and political legitimacy to public health institutions.
% ABSENT_VOICES: Bodily autonomy advocates, vaccine-hesitant communities, and informed consent absolutists are structurally excluded from the decision frame when public health necessity is declared paramount.
% DISAPPEARANCE_RATIONALE: If the mandate principle vanished overnight, vaccination rates among the reluctant would drop, transmission would rise, and vulnerable populations would face renewed lethal exposure; public health institutions would lose a primary outbreak-containment tool and the political economy of vaccination would shift toward purely voluntary regimes with coverage gaps.
% FOUNDING_PROBLEM: Infectious disease outbreaks that kill or disable vulnerable populations because voluntary vaccination coverage falls below herd immunity thresholds due to free-riding, misinformation, or complacency.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and global health institutions outside the immediate enforcement state attest that voluntary coverage gaps endanger vulnerable populations. Civil liberties scholars and some medical ethicists contest that the problem's severity justifies overriding informed consent, arguing that the founding problem is mismanaged rather than mandate-insoluble.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint overrides bodily autonomy and compels medical intervention, a severe cost to the payer seat. Suppression is higher (0.88) because persistence depends on active state enforcementâexclusions, penalties, and employment sanctionsârather than voluntary adherence. Theater ratio is moderate (0.40): enforcement is largely functional, but a growing share of activity is performative compliance theater as the emergency rationale weakens. Accessibility collapse (0.70) reflects the dismissal of alternatives such as targeted protection or voluntary regimes as epidemiologically insufficient. Resistance (0.75) is substantial due to organized anti-mandate movements and legal challenges. The temporal series use one shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat experiences the constraint as legitimate coordination defending the vulnerable; the payer seat experiences it as state extraction of bodily compliance. The engine computes this divergence from structural dataâthe reading's own denial of victimhood to the coerced does not eliminate their payer role.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are structural beneficiaries (low d): the constraint subsidizes their survival risk. Mandate targets are structural payers (high d): their autonomy is the resource extracted. Public health authority sits near the beneficiary end because it gains enforcement capacity and political legitimacy from the arrangement. Civil liberties advocates are excluded, receiving no directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy only so long as the founding problemâlethal exposure risk to vulnerable populations from insufficient voluntary coverageâis credibly live. If endemic transition or therapeutic advance renders the problem obsolete, the constraint threatens to become a piton (enforcement maintained by inertia) or snare (coercion persists without protective function). The contested founding_problem_status flags this transition risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the public_health_primary reading''s subordination of consent collapse into pure extraction when voluntary compliance thresholds are actually achievable without coercion?',
    'Comparative analysis of jurisdictions with similar demographics but different mandate policies; if herd immunity is achieved voluntarily, the extraction component becomes disproportionate.',
    'Would shift classification toward snare if the coordination function is unnecessary, or toward scaffold if the problem is transient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the public health necessity reading survives when voluntary alternatives suffice.').

omega_variable(
    coercion_cost_bearing,
    'Are individuals subjected to mandate enforcement structurally paying costs (psychological harm, trust erosion, bodily intrusion) that this reading''s necessity framing backgrounds?',
    'Post-mandate outcome studies measuring medical system disengagement, psychological distress, and trust erosion among coerced populations.',
    'If costs are substantiated, the tangled_rope asymmetry is confirmed; if negligible, the constraint approaches rope with low extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_cost_bearing, empirical, 'Empirical test of whether coercion imposes hidden costs denied by the reading.').

omega_variable(
    herd_immunity_threshold_obsolescence,
    'Has the founding problem become obsolete due to endemic transition, therapeutic advances, or population immunity?',
    'Surveillance data on pathogen circulation, hospitalization rates among vulnerable populations, and vaccine coverage in fully voluntary regimes.',
    'If dead, the constraint persists as piton or snare; if live, it remains tangled_rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_threshold_obsolescence, empirical, 'Whether the mandate''s founding rationale has expired.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__public_health_primary, theater_ratio, 6, 0.22).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.28).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_balance__public_health_primary, theater_ratio, 18, 0.32).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__public_health_primary, theater_ratio, 24, 0.36).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__public_health_primary, theater_ratio, 30, 0.38).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_balance__public_health_primary, theater_ratio, 36, 0.4).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__public_health_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_balance__public_health_primary, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__public_health_primary, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__public_health_primary, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_balance__public_health_primary, base_extractiveness, 36, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__public_health_primary, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_balance__public_health_primary, suppression_requirement, 18, 0.8).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__public_health_primary, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__public_health_primary, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_balance__public_health_primary, suppression_requirement, 36, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
