% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Sovereignty as Conditional Responsibility (R2P)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint represents the 'conditional responsibility' reading of
 *   Westphalian sovereignty, often associated with the Responsibility to
 *   Protect (R2P) doctrine. It posits that a state's sovereignty is not
 *   absolute but conditional on its capacity and willingness to protect its
 *   own population from mass atrocities. Failure to do so forfeits its
 *   territorial inviolability, legitimizing external intervention. This
 *   reading emerged in response to the international community's failures to
 *   prevent genocide and mass killings, challenging the traditional 'absolute
 *   non-intervention' principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.75).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.8).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Sovereignty as Conditional Responsibility (R2P)").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'd6e24424-7fa3-49d3-8ef2-c7d8757fcb0a').
narrative_ontology:cs_kernel_codification('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', formalized).
narrative_ontology:cs_authority_grounding('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', lineage).
narrative_ontology:cs_interpretation_layer_present('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a').
narrative_ontology:cs_reading_relation('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', foundational, sovereignty_is_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_is_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', sovereignty_is_responsibility, deontological).
narrative_ontology:cs_axiom('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', secondary, international_community_has_right_to_intervene).
narrative_ontology:cs_axiom_status(international_community_has_right_to_intervene, holdable).
narrative_ontology:cs_axiom_grounding('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', international_community_has_right_to_intervene, conventional).
narrative_ontology:cs_reference_frame('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', post_r2p_consensus).
narrative_ontology:cs_drift_state('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', contemporary_geopolitical_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d6e24424-7fa3-49d3-8ef2-c7d8757fcb0a', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, human_security_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and legitimizes the norm that sovereignty entails a responsibility to protect populations, and that failure to do so can justify external intervention. This collective body, often represented by the UN Security Council, holds the power to authorize or condemn interventions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_community, agenda_setter,
    institutional, generational, constrained, global).

% These are states or alliances that gain legitimacy and a legal basis for intervening in other states' affairs when mass atrocities are occurring. They benefit from the expanded scope of action and the moral authority derived from the R2P norm.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    organized, biographical, mobile, global).

% Institutions like the United Nations, International Criminal Court, and various human rights bodies see their mandates and authority enhanced by the conditional sovereignty norm, as it provides a framework for addressing human rights violations across borders.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, constrained, global).

% These are the states whose traditional right to territorial inviolability is forfeited under the conditional sovereignty norm. They bear the cost of potential external intervention, loss of autonomy, and international condemnation. Their options are to comply with international demands or face military action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect, payer,
    powerful, immediate, trapped, national).

% While the ultimate beneficiaries of protection, these populations often bear the immediate costs of the intervention itself, including increased conflict, displacement, and humanitarian crises. Their state's sovereignty is overridden on their behalf, but without their direct consent to the intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer,
    powerless, immediate, trapped, local).

% States or blocs that adhere to a strict interpretation of non-intervention and view conditional sovereignty as an illegitimate erosion of state rights. They are often excluded from the decision-making processes regarding R2P interventions and their arguments are marginalized in the dominant discourse.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, absolute_sovereignty_advocates, excluded,
    organized, generational, constrained, global).

% Academics and legal experts who analyze the evolution, application, and implications of the conditional sovereignty norm. They provide critical commentary and contribute to the conceptual debate, but do not directly participate in its enforcement or suffer its direct consequences.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international action to address mass atrocities within sovereign states, providing a framework for collective response when states fail to protect their own populations.
% TRANSFER_FUNCTION: Transfers the right to non-intervention from states failing to protect their populations to the international community, enabling the deployment of military, diplomatic, and economic resources for intervention.
% ABSENT_VOICES: Advocates for absolute state sovereignty, particularly from states wary of external interference, are often sidelined in discussions about intervention. They would argue that the norm is a dangerous precedent for geopolitical power projection.
% DISAPPEARANCE_RATIONALE: If the norm of conditional sovereignty vanished, states would likely revert to a stricter interpretation of non-intervention, making it significantly harder for the international community to respond to mass atrocities. This would fundamentally alter the landscape of international relations and human rights enforcement.
% FOUNDING_PROBLEM: The failure of the international community to prevent or halt mass atrocities (e.g., Rwanda, Srebrenica) due to strict adherence to the principle of non-intervention in domestic affairs.
% FOUNDING_PROBLEM_CORROBORATION: Reports from the UN, human rights organizations (e.g., Human Rights Watch, Amnesty International), and independent commissions consistently document ongoing mass atrocities and the challenges of intervention, corroborating the continued relevance of the founding problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely aims to coordinate international action for humanitarian protection (a coordination function), but it simultaneously involves significant extraction of sovereign rights from target states and active suppression of their traditional autonomy. Extractiveness is high (0.75) as it imposes a severe cost (loss of inviolability) on states. Suppression is also high (0.80) due to the active enforcement mechanisms (military intervention, sanctions) required to implement the norm. Theater ratio is moderate (0.45) because while humanitarian motives are real, interventions are often influenced by geopolitical interests, leading to selective application and accusations of pretext. Resistance is high (0.85) from states fearing intervention and from those targeted, as it challenges a foundational principle of international law.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of interventionist states and global governance institutions, this constraint is a necessary evolution of international law, a 'Rope' or 'Scaffold' for collective security. From the perspective of states fearing intervention or those targeted, it is a 'Snare' or 'Tangled Rope' that undermines state autonomy and can be selectively applied for geopolitical gain. The engine's classification as Tangled Rope reflects this inherent tension and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanitarian intervention coalitions and global governance institutions are beneficiaries, gaining legitimacy and authority to act. States failing to protect are payers, losing their traditional sovereign rights and facing intervention. Populations under atrocity regimes are also payers in the immediate term, bearing the costs of conflict, even if the intervention is ultimately for their protection. Absolute sovereignty advocates are excluded, as their core premise is directly challenged by this norm.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this constraint as a pure 'Rope' (ignoring the extraction of sovereignty) or a pure 'Snare' (ignoring the genuine coordination function of protecting populations). It highlights that while there is a legitimate coordination problem (mass atrocities), the solution involves significant, often contested, extraction of traditional state rights, requiring active enforcement and facing substantial resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_pretext,
    'Is the conditional sovereignty norm primarily a genuine humanitarian imperative, or does it serve as a pretext for powerful states to pursue geopolitical interests through selective intervention?',
    'Systematic analysis of intervention patterns, including cases where intervention was justified but not undertaken, and cases where it occurred with ambiguous humanitarian outcomes or clear geopolitical gains for intervening powers.',
    'If primarily a pretext, the effective extractiveness and theater_ratio of the constraint are higher than currently measured, pushing it closer to a Snare. If genuinely humanitarian, the coordination function is stronger, supporting the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_pretext, empirical, 'Ambiguity regarding the true motives behind the application of conditional sovereignty.').

omega_variable(
    intervention_effectiveness,
    'Does external intervention, enabled by conditional sovereignty, consistently achieve its goal of protecting populations from mass atrocities without exacerbating conflict or creating new harms?',
    'Longitudinal studies and meta-analyses of post-intervention outcomes, assessing civilian protection, state stability, and human rights records in intervened states compared to non-intervened control cases.',
    'If interventions are consistently ineffective or harmful, the coordination function of the constraint is undermined, increasing its effective extractiveness and potentially reclassifying it towards a Snare. If effective, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_effectiveness, empirical, 'Uncertainty about the actual protective efficacy of interventions under conditional sovereignty.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of state sovereignty primarily structural (international legal framework, military power) or internalized (states self-censor to avoid intervention)?',
    'Analysis of state behavior in anticipation of intervention: if states proactively adjust policies to avoid triggering R2P, it suggests internalization. If only direct threats or interventions alter behavior, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as states carry the suppression with them. If purely structural, the suppression is externally imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2000, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(west_tr_t2010, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(west_tr_t2015, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(west_tr_t2020, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(west_tr_t2030, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2030, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t2000, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(west_be_t2010, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(west_be_t2015, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(west_be_t2020, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(west_be_t2030, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2030, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2000, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(west_su_t2010, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(west_su_t2015, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(west_su_t2020, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(west_su_t2030, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2030, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, un_security_council_veto_power).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
