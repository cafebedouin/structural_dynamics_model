% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: Responsibility to Protect (R2P)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'conditional sovereignty' reading of the
 *   Westphalian principle, often articulated as the Responsibility to Protect
 *   (R2P). It posits that a state's sovereignty is not absolute but entails a
 *   responsibility to protect its own population from mass atrocities.
 *   Failure to do so can trigger legitimate external intervention. This
 *   reading emerged in response to the international community's failures to
 *   prevent genocides in the late 20th century. While it provides a framework
 *   for intervention, its application remains highly contested and often
 *   inconsistent due to geopolitical realities.
 *
 * KEY AGENTS:
 *   - international_intervention_advocates: Agenda setter (institutional/mobile) — pushes for intervention
 *   - human_rights_organizations: Beneficiary (organized/constrained) — benefits from legitimacy
 *   - sovereign_states_committing_violations: Payer (powerful/trapped) — bears direct costs of intervention
 *   - states_prioritizing_absolute_sovereignty: Payer (institutional/constrained) — resists the principle
 *   - victims_of_human_rights_violations: Beneficiary (powerless/trapped) — ultimate intended beneficiaries
 *   - united_nations_security_council: Agenda setter (institutional/constrained) — authorizes interventions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.4).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.6).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: Responsibility to Protect (R2P)").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '512026e5-bdd7-4a1a-9630-bc68e39e08e8').
narrative_ontology:cs_kernel_codification('512026e5-bdd7-4a1a-9630-bc68e39e08e8', formalized).
narrative_ontology:cs_authority_grounding('512026e5-bdd7-4a1a-9630-bc68e39e08e8', lineage).
narrative_ontology:cs_interpretation_layer_present('512026e5-bdd7-4a1a-9630-bc68e39e08e8').
narrative_ontology:cs_reading_relation('512026e5-bdd7-4a1a-9630-bc68e39e08e8', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('512026e5-bdd7-4a1a-9630-bc68e39e08e8', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('512026e5-bdd7-4a1a-9630-bc68e39e08e8', foundational, sovereignty_entails_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('512026e5-bdd7-4a1a-9630-bc68e39e08e8', sovereignty_entails_responsibility, deontological).
narrative_ontology:cs_axiom('512026e5-bdd7-4a1a-9630-bc68e39e08e8', secondary, international_community_has_right_to_intervene).
narrative_ontology:cs_axiom_status(international_community_has_right_to_intervene, holdable).
narrative_ontology:cs_axiom_grounding('512026e5-bdd7-4a1a-9630-bc68e39e08e8', international_community_has_right_to_intervene, conventional).
narrative_ontology:cs_reference_frame('512026e5-bdd7-4a1a-9630-bc68e39e08e8', post_cold_war_humanitarian_intervention_consensus).
narrative_ontology:cs_drift_state('512026e5-bdd7-4a1a-9630-bc68e39e08e8', contemporary_multipolar_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('512026e5-bdd7-4a1a-9630-bc68e39e08e8', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, human_rights_organizations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, sovereign_states_committing_violations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, states_prioritizing_absolute_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, victims_of_human_rights_violations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the principle that sovereignty is conditional on a state's adherence to human rights. They push for international mechanisms to authorize and conduct interventions when states fail to protect their populations. They benefit from the legitimacy this constraint provides for their actions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from the legal and moral framework that conditional sovereignty provides, enabling them to advocate for victims and pressure states. Their work gains greater traction when the international community acknowledges a right to intervene.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, human_rights_organizations, beneficiary,
    organized, generational, constrained, global).

% Bear the direct costs of this constraint, facing potential external intervention, sanctions, or loss of international legitimacy if they engage in systematic human rights violations. Their autonomy is directly curtailed.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, sovereign_states_committing_violations, payer,
    powerful, immediate, trapped, national).

% Resist the conditional sovereignty principle, viewing it as an infringement on state autonomy and a potential pretext for intervention. They bear the cost of having their preferred international order challenged and potentially undermined.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, states_prioritizing_absolute_sovereignty, payer,
    institutional, generational, constrained, global).

% Are the ultimate intended beneficiaries, as the constraint aims to protect them from state-sponsored atrocities. However, their agency in triggering or directing intervention is minimal, and the protection is often reactive rather than preventative.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, victims_of_human_rights_violations, beneficiary,
    powerless, immediate, trapped, local).

% Holds the primary authority to authorize external intervention under international law. Its permanent members' veto power means that political considerations often override the strict application of conditional sovereignty, leading to inconsistent enforcement.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, united_nations_security_council, agenda_setter,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate international action to prevent or halt mass atrocities when individual states fail to protect their own populations, establishing a shared understanding of when non-interference can be overridden.
% TRANSFER_FUNCTION: Transfers a portion of state autonomy and non-interference guarantees from states engaged in systematic human rights violations to the international community, particularly to those advocating for and capable of intervention.
% ABSENT_VOICES: Populations within states that fear intervention as a form of neo-colonialism or a threat to their self-determination, even if their governments are abusive. Their concerns are often marginalized in the intervention debate.
% DISAPPEARANCE_RATIONALE: If the principle of conditional sovereignty vanished, states would revert to a more absolute interpretation of non-interference, potentially leading to more unchecked human rights abuses without the threat of external accountability. The international human rights regime would lose a key enforcement mechanism.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to genocides and mass atrocities in the late 20th century, such as in Rwanda and Srebrenica, due to strict interpretations of state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal scholars, and many UN member states (excluding those with strong absolute sovereignty stances) corroborate that the problem of mass atrocities and the need for a framework to address them remains live. Reports from UN special rapporteurs and various NGOs provide ongoing evidence.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the significant but not absolute curtailment of state autonomy. Suppression (0.6) is substantial, as the principle requires active enforcement and challenges the deeply entrenched norm of non-interference. Theater ratio (0.2) is relatively low, indicating that while there's some performative aspect to international declarations, the threat of intervention is often real, even if inconsistently applied. Resistance (0.7) is high, as many states actively oppose or seek to limit the scope of this principle. Accessibility collapse (0.4) is moderate, as states still have options to resist or deflect intervention, but their absolute right to non-interference is diminished.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intervention advocates and human rights organizations, this is a necessary 'rope' for global governance, coordinating action to prevent atrocities. From the perspective of states prioritizing absolute sovereignty, it is a 'snare' that undermines the foundational principle of international law and can be selectively applied for geopolitical ends. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   International intervention advocates and human rights organizations are beneficiaries (low d) as the constraint legitimizes their goals. States committing violations and those prioritizing absolute sovereignty are targets (high d) as their autonomy is curtailed or challenged. The UNSC, while an agenda-setter, has constrained exit due to its internal political dynamics, leading to a more symmetric d than a pure beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing mass atrocities) remains live, so mandatrophy is not resolved. The classification as a Tangled Rope acknowledges both its genuine coordination function (mobilizing international response) and its asymmetric extraction (curtailing the autonomy of targeted states). This prevents mislabeling it as a pure Snare, which would ignore its humanitarian intent, or a pure Rope, which would ignore its coercive and contested nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_selectivity_bias,
    'Is the application of conditional sovereignty and subsequent intervention genuinely driven by human rights concerns, or is it selectively applied based on geopolitical interests?',
    'Systematic analysis of all cases of mass atrocities against intervention decisions, controlling for geopolitical factors, resource interests, and alliance structures.',
    'If highly selective, the constraint''s effective extractiveness and theater ratio would be higher for targeted states, and its coordination function would be undermined by its use as a tool of power politics, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_selectivity_bias, empirical, 'Assesses whether interventions are consistently applied or driven by ulterior motives.').

omega_variable(
    effectiveness_of_intervention,
    'Do external interventions authorized under conditional sovereignty consistently improve human rights outcomes and long-term stability, or do they often exacerbate conflict and instability?',
    'Longitudinal studies comparing human rights trajectories and state stability in intervened vs. non-intervened cases of mass atrocities, controlling for pre-existing conditions.',
    'If interventions consistently fail or worsen outcomes, the legitimacy of the constraint''s coordination function would be severely undermined, increasing its perceived extractiveness and potentially shifting its classification towards a Piton (ineffective but maintained for theatrical reasons) or a Snare (if the negative outcomes are seen as a feature, not a bug).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_intervention, empirical, 'Evaluates the actual impact of interventions on human rights and stability.').

omega_variable(
    sovereignty_definition_ambiguity,
    'Is ''sovereignty'' fundamentally an absolute right of non-interference, or a conditional responsibility to protect?',
    'Conceptual analysis of international legal philosophy and state practice, examining the historical evolution of sovereignty norms and their underlying justifications.',
    'If resolved towards absolute sovereignty, this reading would be foreclosed, and the international system would revert to a non-interventionist stance. If resolved towards conditional responsibility, the legitimacy of intervention would be strengthened, potentially reducing resistance and increasing the constraint''s effectiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'The core conceptual disagreement over the nature of state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(west_tr_t1995, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(west_tr_t2000, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(west_tr_t2010, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(west_be_t1995, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(west_be_t2000, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(west_be_t2010, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(west_su_t1995, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(west_su_t2000, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(west_su_t2010, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, un_peacekeeping_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. It is linked to other readings (absolute_sovereignty, graduated_sovereignty) which represent alternative interpretations of state sovereignty and its limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
