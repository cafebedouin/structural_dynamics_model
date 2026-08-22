% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty: Responsibility to Protect and Forfeiture of Territorial Inviolability
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the conditional_responsibility reading of
 *   the contested kernel westphalia_sovereignty. Under the 'Responsibility to
 *   Protect' (R2P) doctrine, sovereignty is reconceptualized as contingent on
 *   a state's willingness and capacity to shield its population from mass
 *   atrocities. When states fail this responsibility, they forfeit the
 *   normative shield of territorial inviolability, and adjudicative authority
 *   passes to the UN Security Council and allied humanitarian coalitions.
 *   This reading competes with absolute_non_intervention
 *   (post-colonial/Global South) and graded_sovereignty (capacity-based
 *   scalar models). The kernel is contested because the UN Charter text
 *   simultaneously enshrines sovereignty and human rights, supporting
 *   divergent interpretive frameworks.
 *
 * KEY AGENTS:
 *   - global_governance_institutions (agenda_setter, institutional/constrained) â adjudicate forfeiture and authorize enforcement
 *   - humanitarian_intervention_coalitions (beneficiary, powerful/constrained) â gain legitimization for military operations
 *   - populations_under_atrocity_regimes (payer, powerless/trapped) â bear the costs of both atrocity and intervention
 *   - targeted_states (payer, moderate/constrained) â lose sovereign protection and face sanctions or intervention
 *   - global_south_advocates (excluded, organized/constrained) â contest the doctrine as neo-imperial but are marginalized
 *   - international_legal_scholars (observer, analytical/analytical) â trace doctrinal genealogy and selective application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.72).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty: Responsibility to Protect and Forfeiture of Territorial Inviolability").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '3c238174-bf29-4488-a534-0e3ab330923c').
narrative_ontology:cs_kernel_codification('3c238174-bf29-4488-a534-0e3ab330923c', formalized).
narrative_ontology:cs_authority_grounding('3c238174-bf29-4488-a534-0e3ab330923c', lineage).
narrative_ontology:cs_interpretation_layer_present('3c238174-bf29-4488-a534-0e3ab330923c').
narrative_ontology:cs_reading_relation('3c238174-bf29-4488-a534-0e3ab330923c', westphalia_sovereignty__absolute_non_intervention, coexists_with).
narrative_ontology:cs_reading_relation('3c238174-bf29-4488-a534-0e3ab330923c', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('3c238174-bf29-4488-a534-0e3ab330923c', foundational, sovereignty_conditional_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('3c238174-bf29-4488-a534-0e3ab330923c', sovereignty_conditional_on_protection, conventional).
narrative_ontology:cs_axiom('3c238174-bf29-4488-a534-0e3ab330923c', foundational, international_adjudication_legitimate).
narrative_ontology:cs_axiom_status(international_adjudication_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3c238174-bf29-4488-a534-0e3ab330923c', international_adjudication_legitimate, conventional).
narrative_ontology:cs_reference_frame('3c238174-bf29-4488-a534-0e3ab330923c', sovereignty_as_responsibility_framework).
narrative_ontology:cs_drift_state('3c238174-bf29-4488-a534-0e3ab330923c', post_libya_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c238174-bf29-4488-a534-0e3ab330923c', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, targeted_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the normative framework through the UN Security Council, General Assembly, and the International Criminal Court, adjudicating when a state has failed its responsibility to protect. Drafts resolutions authorizing intervention and maintains the institutional architecture that translates the conditional doctrine into international law and practice.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Gain legal and moral legitimacy to conduct military operations under the auspices of protecting civilian populations. Their interventions are framed as humanitarian but often align with geopolitical interests, and they rely on the doctrine to secure multilateral backing and access to conflict zones.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    powerful, biographical, constrained, global).

% Live under governments committing or permitting mass atrocities. They are nominally the beneficiaries of the protection doctrine, but in practice frequently suffer the direct harms of military intervention, collateral damage, and continued abandonment when geopolitical interest is absent.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer,
    powerless, immediate, trapped, national).

% States accused of failing to protect populations from mass atrocities. They lose the traditional protection of territorial inviolability and non-intervention, becoming subject to sanctions, ICC referral, or military action authorized by the international community.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, targeted_states, payer,
    moderate, biographical, constrained, national).

% Represent post-colonial and non-aligned states that view conditional sovereignty as a neo-imperial mechanism enabling selective intervention by powerful states. They are systematically marginalized in UNSC deliberations and international legal forums where the doctrine is interpreted and applied.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_south_advocates, excluded,
    organized, generational, constrained, global).

% Analyze and debate the doctrinal coherence of responsibility-to-protect frameworks, tracing the legal genealogy from the UN Charter through the ICISS report to contemporary practice. They document selective application and contest the empirical claims underlying intervention justifications.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a normative framework that determines when military intervention to halt mass atrocities is legitimate, preventing both unchecked unilateral intervention and total non-intervention deadlock in the face of genocide, war crimes, ethnic cleansing, and crimes against humanity.
% TRANSFER_FUNCTION: Transfers adjudicative authority over domestic conduct and territorial inviolability from the target state to the international community, specifically the UN Security Council and humanitarian coalitions, while transferring the human and material costs of enforcement to populations within the target state.
% ABSENT_VOICES: Populations under atrocity regimes are nominally protected but structurally excluded from the decision to intervene; global south states and anti-imperial critics who view the doctrine as neo-imperialism are marginalized in the adjudicative forums where conditionality is interpreted.
% DISAPPEARANCE_RATIONALE: Without the conditional responsibility doctrine, the post-2001 intervention landscape loses its primary legitimizing framework; unilateral intervention would proliferate without normative guardrails, and the institutional authority of the UN Security Council and the ICC would attenuate significantly as states revert to absolute non-intervention defenses.
% FOUNDING_PROBLEM: The UN Charter's absolute sovereignty and non-intervention norms left the international community without a lawful mechanism to halt mass atrocities witnessed in Rwanda and Srebrenica, creating a persistent gap between legal prohibition and moral imperative.
% FOUNDING_PROBLEM_CORROBORATION: Independent international commissions such as ICISS (2001) and some global south legal scholars attest the founding problem was real; however, many post-colonial states and critical international lawyers outside the beneficiary coalition argue the problem was constructed to license great-power intervention, and the continued occurrence of atrocities without consistent protection demonstrates the arrangement serves selective enforcement rather than civilian protection.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the doctrine licenses military intervention that imposes severe costs on civilian populations and strips sovereignty from targeted states, while its selective application (Libya invoked, Syria/Yemen ignored) reveals a decoupling from protection efficacy. Suppression (0.68) reflects the active discursive and institutional effort to marginalize absolute non-intervention alternatives and to enforce the adjudicative authority of the UNSC. Theater_ratio (0.45) captures the performative humanitarian rhetoric that masks geopolitical selection criteria. The temporal series show extraction and theater rising sharply after the 2011 Libya intervention, when the gap between protection claims and regime-change outcomes became visible. Resistance (0.62) is substantial, driven by Russia, China, and Global South coalitions that reject conditional sovereignty as a cover for interventionism.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (UN institutions, NATO coalitions) experience the constraint as necessary coordination to prevent Rwanda-like failures; the payer seats (targeted states, victim populations) experience it as the loss of sovereign shelter and exposure to military violence. The engine computes this divergence from the structural data: the same UNSC resolution reads as legitimate law enforcement to the beneficiary seat and as coercive regime change to the payer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Global governance institutions and humanitarian intervention coalitions are structural beneficiaries: they gain adjudicative authority and intervention legitimacy (low directionality). Targeted states and atrocity-affected populations are structural targets: they lose sovereign protection and bear the direct costs of enforcement (high directionality). Global South advocates are excluded from the interpretive process entirely, which further amplifies the asymmetric extraction because their resistance is structurally suppressed rather than incorporated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmass atrocities in the face of legal paralysisâwas genuine, but the constraint's selective enforcement record demonstrates partial mandatrophy. The coordination function (preventing unilateral free-for-all and establishing atrocity-response norms) retains real value, which prevents full snare classification. However, the arrangement's uneven application suggests the extraction component (authority transfer to Western-led institutions, regime-change licensing) has layered onto the coordination function. Thus the constraint sits in the tangled_rope category: genuine coordination problem solved, but asymmetric extraction routed through the same structure and maintained by active enforcement against excluded alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_pretext_ambiguity,
    'To what extent does the conditional responsibility doctrine serve as a pretext for great-power intervention rather than a genuine protection mechanism for atrocity-affected populations?',
    'Comparative case analysis of interventions invoked under R2P versus non-invoked crises with similar severity profiles; examine correlation between intervention likelihood and geopolitical interest.',
    'High pretext ratio would shift classification toward snare; low ratio would strengthen the coordination half of the tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_pretext_ambiguity, empirical, 'Whether R2P is primarily protection or pretext').

omega_variable(
    kernel_framing_alternative,
    'Does the authority of this reading rest on the UN Charter text reinterpreted, or on a supra-constitutional moral principle that overrides the text?',
    'Trace the legal genealogy of the ICISS report and World Summit Outcome Document to determine whether the doctrine is framed as interpretation of existing Charter law or as new customary law.',
    'If grounded in text-interpretation, the reading coexists with absolute non-intervention as a hermeneutic dispute; if grounded in overriding moral principle, it tends toward foreclosing the absolute reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether authority derives from Charter text or overriding principle').

omega_variable(
    global_south_consent,
    'Would the conditional responsibility norm have been adopted if post-colonial states had equal procedural weight in its formulation?',
    'Examine voting records and negotiating histories of the 2005 World Summit; compare with Global South alternative drafts and subsequent resistance.',
    'If the norm was formulated without equal consent, its coordination claim is weakened and its extraction component (authority transfer to Western-led institutions) is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_consent, empirical, 'Whether unequal consent in norm formation biases classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.25).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.35).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__conditional_responsibility, theater_ratio, 15, 0.55).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__conditional_responsibility, theater_ratio, 20, 0.5).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__conditional_responsibility, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 25, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'westphalia_sovereignty'. The three readings (absolute_non_intervention, conditional_responsibility, graded_sovereignty) decompose the colloquial label 'Westphalian sovereignty' into structurally distinct claims with different beneficiary/victim structures and intervention thresholds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
