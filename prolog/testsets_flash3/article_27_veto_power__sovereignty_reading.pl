% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: UNSC Article 27 Veto Power (Sovereignty Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint models the UN Security Council's Article 27 veto power as
 *   an instantiation of the Westphalian sovereignty principle, specifically
 *   applied to great powers with global-reach enforcement capacity. From this
 *   'sovereignty reading,' the veto is a structural inevitability, reflecting
 *   the physical reality of power distribution in the international system,
 *   particularly the existence of nuclear weapons. It is classified as a
 *   Mountain because any global institution empowered to compel great-power
 *   action would face the same coordination failure, making the veto a
 *   necessary feature rather than a chosen policy. Extraction is near-zero
 *   because it's seen as an irreducible cost of maintaining international
 *   stability, not a rent. There are no 'victims' in this framing, as the
 *   constraint derives from the physical reality of power, not from a
 *   human-designed extractive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.95).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "UNSC Article 27 Veto Power (Sovereignty Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'bc4e6b0e-09ba-40fa-876d-bd991c787f11').
narrative_ontology:cs_kernel_codification('bc4e6b0e-09ba-40fa-876d-bd991c787f11', fixed_text).
narrative_ontology:cs_authority_grounding('bc4e6b0e-09ba-40fa-876d-bd991c787f11', lineage).
narrative_ontology:cs_interpretation_layer_present('bc4e6b0e-09ba-40fa-876d-bd991c787f11').
narrative_ontology:cs_reading_relation('bc4e6b0e-09ba-40fa-876d-bd991c787f11', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc4e6b0e-09ba-40fa-876d-bd991c787f11', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('bc4e6b0e-09ba-40fa-876d-bd991c787f11', foundational, great_power_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(great_power_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('bc4e6b0e-09ba-40fa-876d-bd991c787f11', great_power_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('bc4e6b0e-09ba-40fa-876d-bd991c787f11', foundational, international_law_requires_consent_of_powerful).
narrative_ontology:cs_axiom_status(international_law_requires_consent_of_powerful, holdable).
narrative_ontology:cs_axiom_grounding('bc4e6b0e-09ba-40fa-876d-bd991c787f11', international_law_requires_consent_of_powerful, conventional).
narrative_ontology:cs_reference_frame('bc4e6b0e-09ba-40fa-876d-bd991c787f11', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('bc4e6b0e-09ba-40fa-876d-bd991c787f11', contemporary_global_governance_debates, gap(stable, minor, false)).
narrative_ontology:cs_created_at('bc4e6b0e-09ba-40fa-876d-bd991c787f11', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, p5_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_27_veto_power__sovereignty_reading, non_p5_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As permanent members of the UN Security Council, these states possess the veto power, which ensures no resolution can be passed against their vital national interests. This power is seen as a reflection of their sovereign right not to be bound by international law without consent, given their global enforcement capacity.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, p5_states, beneficiary,
    institutional, generational, arbitrage, global).

% These states are subject to Security Council resolutions but lack the power to block them. They experience the veto as a structural limit on the Council's ability to act decisively, particularly when P5 interests diverge from collective security. Their options are to lobby P5 states or pursue alternative, less effective, international mechanisms.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_p5_states, payer,
    moderate, biographical, constrained, global).

% Analyze the legal and political implications of the veto power, often debating its consistency with principles of collective security and sovereign equality. From this reading, they see the veto as a necessary, if imperfect, reflection of the distribution of power in the international system.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that no international body can compel a great power to act against its perceived vital interests, thereby preventing direct military confrontation among nuclear-armed states and preserving the stability of the international system by reflecting the underlying distribution of power.
% TRANSFER_FUNCTION: The veto transfers the ultimate authority over international security decisions from the collective will of the UN Security Council to the individual P5 states, effectively granting them immunity from enforcement actions they oppose.
% ABSENT_VOICES: States advocating for a more equitable distribution of power in international institutions, or those who believe in the supremacy of collective security over individual state sovereignty, are structurally marginalized in the current UN framework. They would argue for reforms to the veto power.
% DISAPPEARANCE_RATIONALE: If the P5 veto power vanished overnight, the international system would undergo a fundamental rearrangement. Great powers would likely withdraw from the UN Security Council or disregard its resolutions, leading to a collapse of the collective security framework and a return to more overt power politics, as the underlying power realities would reassert themselves outside the institutional structure.
% FOUNDING_PROBLEM: The problem of how to create an international organization capable of maintaining peace and security without compelling great powers into conflicts that could escalate to global war, given their sovereign right to self-defense and their capacity to resist unwanted interventions.
% FOUNDING_PROBLEM_CORROBORATION: P5 states consistently attest that the founding problem remains live, citing the ongoing need to prevent great-power conflict. Many international relations realists and some legal scholars, from outside the direct beneficiary set, corroborate this view, arguing that the veto reflects an enduring structural reality of international power distribution.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and high suppression (0.95) reflect the view that the veto is an unchangeable feature of the international system, a 'natural law' of great power relations. It 'suppresses' alternative forms of international governance that would bind great powers without their consent, not through coercion, but through the sheer impossibility of enforcing such rules against them. The theater ratio is low (0.05) because the veto is seen as a direct, functional expression of power, not a performance. Accessibility collapse is high (0.98) because alternatives to this power distribution are considered structurally impossible without a fundamental change in global power dynamics.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (coordination, oligopoly) would classify the veto differently, seeing it as a chosen mechanism or an extractive tool. This sovereignty reading, however, asserts its inevitability, leading to a Mountain classification where other readings might see a Rope or Snare. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states are beneficiaries in the sense that the veto protects their sovereign interests, but this reading frames it as a reflection of their inherent power, not a privilege granted by the system. Non-P5 states are 'payers' in that they bear the cost of the Council's inaction when P5 interests diverge, but this is seen as an unavoidable consequence of the international power structure. The analytical observer (international law scholars) views the constraint as a given, a structural reality to be understood.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the P5 veto a genuine natural law reflecting an immutable distribution of power, or a constructed constraint that benefits identifiable agents (P5 states)?',
    'Counterfactual analysis: if a global institution could compel great-power action without triggering global conflict, the veto would be revealed as a constructed constraint. Empirical observation of whether P5 states would genuinely accept binding resolutions against their vital interests if the veto were removed.',
    'If revealed as constructed, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope or Snare), and the P5 states would be re-evaluated as beneficiaries of an extractive mechanism rather than merely reflecting a structural reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Ambiguity between structural inevitability and institutional design choice.').

omega_variable(
    power_distribution_immutability,
    'Is the current distribution of global power, which the veto reflects, truly immutable, or could it be altered through political and economic shifts, thereby changing the ''natural'' basis of the veto?',
    'Longitudinal study of global power shifts and their impact on international institutional design. Analysis of historical precedents where ''great powers'' lost their status and their privileges.',
    'If power distribution is mutable, the veto''s ''Mountain'' classification would be challenged, as its foundation in natural law would weaken. This would open pathways for reclassification towards a more contingent, human-designed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_distribution_immutability, empirical, 'Whether the underlying power dynamics are fixed or subject to change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__sovereignty_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__sovereignty_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__sovereignty_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(arti_tr_t79, article_27_veto_power__sovereignty_reading, theater_ratio, 79, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__sovereignty_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__sovereignty_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__sovereignty_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(arti_be_t79, article_27_veto_power__sovereignty_reading, base_extractiveness, 79, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__sovereignty_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__sovereignty_reading, suppression_requirement, 20, 0.95).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__sovereignty_reading, suppression_requirement, 40, 0.95).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__sovereignty_reading, suppression_requirement, 60, 0.95).
narrative_ontology:measurement(arti_su_t79, article_27_veto_power__sovereignty_reading, suppression_requirement, 79, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UN Security Council's Article 27 veto power. This 'sovereignty reading' frames the veto as a structural inevitability reflecting great-power sovereignty, distinct from the 'coordination reading' (veto as conflict prevention) and the 'oligopoly reading' (veto as power entrenchment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
