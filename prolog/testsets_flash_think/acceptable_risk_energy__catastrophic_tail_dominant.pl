% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Policy
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint describes a dominant risk assessment philosophy in energy
 *   policy that prioritizes avoiding low-probability, high-impact
 *   catastrophic outcomes (e.g., nuclear accidents) even if it means
 *   accepting higher expected aggregate harm from other sources (e.g., fossil
 *   fuel pollution). This reading of 'acceptable risk' leads to the
 *   suppression of certain energy pathways and the implicit discounting of
 *   other, more diffuse harms. The constraint is actively enforced through
 *   regulatory frameworks and public discourse.
 *
 * KEY AGENTS:
 *   - catastrophe_averse_public: Primary beneficiary (moderate/constrained) — protected from perceived catastrophe.
 *   - anti_nuclear_advocates: Agenda setter/beneficiary (organized/mobile) — actively shapes policy to suppress nuclear.
 *   - nuclear_energy_proponents: Primary payer (organized/constrained) — bears costs of suppressed pathway.
 *   - fossil_fuel_impacted_communities: Primary victim (powerless/trapped) — bears discounted aggregate harm.
 *   - energy_regulators: Agenda setter (institutional/constrained) — implements and enforces the policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.65).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.75).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic Tail Risk Dominance in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '4f04c8d4-0119-46bd-b47f-36f7be51ff42').
narrative_ontology:cs_kernel_codification('4f04c8d4-0119-46bd-b47f-36f7be51ff42', implicit).
narrative_ontology:cs_authority_grounding('4f04c8d4-0119-46bd-b47f-36f7be51ff42', practice).
narrative_ontology:cs_interpretation_layer_present('4f04c8d4-0119-46bd-b47f-36f7be51ff42').
narrative_ontology:cs_reading_relation('4f04c8d4-0119-46bd-b47f-36f7be51ff42', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('4f04c8d4-0119-46bd-b47f-36f7be51ff42', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('4f04c8d4-0119-46bd-b47f-36f7be51ff42', foundational, catastrophic_tail_risk_intolerable).
narrative_ontology:cs_axiom_status(catastrophic_tail_risk_intolerable, holdable).
narrative_ontology:cs_axiom_grounding('4f04c8d4-0119-46bd-b47f-36f7be51ff42', catastrophic_tail_risk_intolerable, deontological).
narrative_ontology:cs_axiom('4f04c8d4-0119-46bd-b47f-36f7be51ff42', secondary, distributed_aggregate_harm_tolerable).
narrative_ontology:cs_axiom_status(distributed_aggregate_harm_tolerable, holdable).
narrative_ontology:cs_axiom_grounding('4f04c8d4-0119-46bd-b47f-36f7be51ff42', distributed_aggregate_harm_tolerable, conventional).
narrative_ontology:cs_reference_frame('4f04c8d4-0119-46bd-b47f-36f7be51ff42', post_chernobyl_risk_aversion).
narrative_ontology:cs_drift_state('4f04c8d4-0119-46bd-b47f-36f7be51ff42', contemporary_climate_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4f04c8d4-0119-46bd-b47f-36f7be51ff42', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, catastrophe_averse_public).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_energy_proponents).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_impacted_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from policies that prioritize avoiding low-probability, high-impact events like nuclear meltdowns, even if it means accepting higher aggregate harm from other sources. Their aversion to highly visible, catastrophic risks shapes policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, catastrophe_averse_public, beneficiary,
    moderate, biographical, constrained, global).

% Actively promote policies that suppress nuclear energy development due to perceived catastrophic risks. They benefit from the policy framework aligning with their advocacy goals and often influence regulatory bodies.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocates, beneficiary).

% Bear the costs of heightened regulatory hurdles, public opposition, and suppressed investment in nuclear energy, despite arguing for its benefits in terms of climate change mitigation and energy security. Their preferred pathway is actively constrained.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_energy_proponents, payer,
    organized, generational, constrained, national).

% Suffer disproportionately from the 'higher expected aggregate harm' (e.g., air pollution, health issues) that is implicitly accepted or discounted when catastrophic tail risks are prioritized. They have limited power to influence policy or exit their situation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_impacted_communities, payer,
    powerless, immediate, trapped, local).

% Analyze energy policy through the lens of minimizing aggregate expected harm, often finding that the catastrophic-tail-dominant approach leads to suboptimal outcomes. They provide alternative analytical frameworks but do not directly set policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, expected_value_economists, observer,
    analytical, biographical, analytical, global).

% Advocate for maintaining diverse energy pathways to preserve flexibility under deep uncertainty, critiquing policies that foreclose options based on a single risk metric. They offer a strategic perspective on risk management.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, option_value_theorists, observer,
    analytical, generational, analytical, global).

% Implement and enforce policies that reflect the catastrophic-tail-dominant risk philosophy, often balancing public perception, political pressure, and scientific advice. They are responsible for the practical application of this constraint.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal and political efforts to manage energy risks by establishing a shared (though contested) priority: avoiding low-probability, high-impact catastrophic events, particularly in the nuclear sector.
% TRANSFER_FUNCTION: Transfers the burden of diffuse, higher-probability harms (e.g., from fossil fuel emissions, mining impacts) onto certain populations and future generations, while protecting others from the perceived threat of catastrophic, low-probability events. It also transfers investment and policy focus away from suppressed energy pathways.
% ABSENT_VOICES: Future generations who might benefit from low-carbon nuclear energy, or communities disproportionately affected by the 'aggregate harm' that is discounted. Their long-term interests or immediate suffering are often not adequately represented in the policy-making process driven by catastrophic risk aversion.
% DISAPPEARANCE_RATIONALE: If this risk philosophy vanished overnight, energy policy would immediately shift to other frameworks (e.g., expected value, option value), leading to different investment, regulatory, and public acceptance landscapes for various energy sources. Nuclear energy would likely see a resurgence, while fossil fuel impacts would be re-evaluated.
% FOUNDING_PROBLEM: The constraint was built to address the profound public fear and political fallout associated with highly visible, low-probability, high-impact technological failures, particularly after major nuclear accidents like Chernobyl (1986) and Fukushima (2011).
% FOUNDING_PROBLEM_CORROBORATION: Public opinion polls, media coverage patterns, and political discourse consistently show a strong aversion to catastrophic risks, even if scientific bodies or economists present different aggregate risk profiles. This public sentiment continues to exert significant pressure on energy policy, corroborating the ongoing 'liveness' of the founding problem from a political and social perspective.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the policy choice imposes significant costs on suppressed energy sectors and on communities bearing diffuse harms, without a proportional benefit to those specific groups. Suppression is very high, particularly after events like Fukushima, as regulatory and political mechanisms actively block or disincentivize nuclear development. Theater ratio is moderate, as some genuine risk assessment and safety measures are in place, but a significant portion of the activity serves to maintain the political narrative of catastrophic risk aversion rather than purely functional safety. The measurements show a spike in extractiveness and suppression after Fukushima (2011), followed by a slight decrease as climate change concerns bring some re-evaluation.
 *
 * PERSPECTIVAL GAP:
 *   The 'catastrophe_averse_public' and 'anti_nuclear_advocates' experience this as a necessary and beneficial coordination mechanism for public safety. In contrast, 'nuclear_energy_proponents' experience it as an extractive snare that unfairly targets their industry, while 'fossil_fuel_impacted_communities' bear the unacknowledged costs of this risk prioritization. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'catastrophe_averse_public' and 'anti_nuclear_advocates' are beneficiaries, as the constraint aligns with their risk preferences and advocacy goals. 'Nuclear_energy_proponents' and 'fossil_fuel_impacted_communities' are victims, bearing the direct and indirect costs of the policy. Energy regulators are agenda-setters, implementing the policy, and their directionality is shaped by the political and social pressures they face.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting against catastrophic risk) remains 'live' in public perception, preventing a clear mandatrophy resolution. However, the increasing awareness of climate change and the need for low-carbon energy sources creates tension, suggesting that the 'founding problem' may be evolving or contested in its current form. The persistence of high suppression and extractiveness, despite the evolving energy landscape, indicates a potential for the constraint to become a 'tangled rope' or 'snare' if its coordination function atrophies relative to its extractive effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_perception_vs_objective_risk,
    'To what extent does the prioritization of catastrophic tail risk reflect objective scientific risk assessment versus public and political risk perception?',
    'Comparative analysis of expert elicitation on objective risk probabilities and consequences versus public opinion surveys and media framing of energy risks. If a significant divergence exists, the constraint''s ''naturalness'' is undermined.',
    'If primarily driven by perception, the constraint''s ''emerges_naturally'' claim (if any) would be weakened, and its classification might shift towards a more constructed type (e.g., Snare or Tangled Rope) due to the political leverage of fear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(risk_perception_vs_objective_risk, empirical, 'Ambiguity between perceived and objective risk in policy decisions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of nuclear pathways structural (e.g., regulatory hurdles, high insurance costs) or internalized (e.g., public fear, political stigma)?',
    'Post-policy-change trajectory: if public and political aversion to nuclear persists even after structural barriers are removed or reduced, reclassify as partially internalized. Conversely, if removal of structural barriers leads to rapid resurgence, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the aversion persists even without explicit policy enforcement. This would make exit options for nuclear proponents more ''identity_locked'' or ''trapped''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for nuclear energy.').

omega_variable(
    discounting_of_diffuse_harm_justification,
    'Is the discounting of diffuse, aggregate harms (e.g., from fossil fuels) an explicit analytical choice within this risk framework, or an implicit political convenience?',
    'Analysis of policy documents and expert testimony: if the discounting is explicitly justified with a coherent, stated methodology, it''s an analytical choice. If it''s absent from formal analysis but present in outcomes, it''s a political convenience.',
    'If an implicit political convenience, the ''extractiveness'' metric is more robustly supported as a feature of the constraint, rather than a consequence of a defensible (if contested) analytical framework. This would strengthen the Snare/Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discounting_of_diffuse_harm_justification, conceptual, 'Justification for discounting diffuse aggregate harms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(acce_tr_t1996, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(acce_tr_t2006, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(acce_tr_t2011, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(acce_tr_t2018, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(acce_tr_t2024, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(acce_be_t1996, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 1996, 0.6).
narrative_ontology:measurement(acce_be_t2006, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2006, 0.62).
narrative_ontology:measurement(acce_be_t2011, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2011, 0.7).
narrative_ontology:measurement(acce_be_t2018, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(acce_be_t2024, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.65).
narrative_ontology:measurement(acce_su_t1996, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 1996, 0.7).
narrative_ontology:measurement(acce_su_t2006, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2006, 0.72).
narrative_ontology:measurement(acce_su_t2011, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2011, 0.85).
narrative_ontology:measurement(acce_su_t2018, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2018, 0.8).
narrative_ontology:measurement(acce_su_t2024, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'acceptable_risk_energy' kernel. Each reading represents a different structural approach to risk management in energy policy, with different beneficiaries, victims, and operational metrics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
