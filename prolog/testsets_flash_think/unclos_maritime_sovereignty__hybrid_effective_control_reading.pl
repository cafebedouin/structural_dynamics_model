% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid effective control' reading of
 *   UNCLOS maritime sovereignty, which posits that natural features generate
 *   full EEZ/territorial sea, while artificial features generate limited
 *   safety zones (500m) but may mature into territorial claims through
 *   prolonged effective control absent challenge. This reading attempts to
 *   balance strict geographic principles with the realities of state practice
 *   and power projection, resulting in a graduated system of sovereignty. It
 *   is a contested interpretation within the broader UNCLOS framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.7).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '239c6677-fac1-468d-857b-ef46151a0b77').
narrative_ontology:cs_kernel_codification('239c6677-fac1-468d-857b-ef46151a0b77', formalized).
narrative_ontology:cs_authority_grounding('239c6677-fac1-468d-857b-ef46151a0b77', lineage).
narrative_ontology:cs_interpretation_layer_present('239c6677-fac1-468d-857b-ef46151a0b77').
narrative_ontology:cs_reading_relation('239c6677-fac1-468d-857b-ef46151a0b77', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('239c6677-fac1-468d-857b-ef46151a0b77', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('239c6677-fac1-468d-857b-ef46151a0b77', foundational, graduated_sovereignty_principle).
narrative_ontology:cs_axiom_status(graduated_sovereignty_principle, holdable).
narrative_ontology:cs_axiom_grounding('239c6677-fac1-468d-857b-ef46151a0b77', graduated_sovereignty_principle, conventional).
narrative_ontology:cs_axiom('239c6677-fac1-468d-857b-ef46151a0b77', foundational, effective_control_legitimacy).
narrative_ontology:cs_axiom_status(effective_control_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('239c6677-fac1-468d-857b-ef46151a0b77', effective_control_legitimacy, conventional).
narrative_ontology:cs_reference_frame('239c6677-fac1-468d-857b-ef46151a0b77', unclos_treaty_framework).
narrative_ontology:cs_drift_state('239c6677-fac1-468d-857b-ef46151a0b77', contemporary_maritime_disputes, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('239c6677-fac1-468d-857b-ef46151a0b77', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity_and_regional_power).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_established_natural_features).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, effective_occupation_principle).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, graduated_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states possess the technological and military capacity to construct artificial features and maintain prolonged effective control over them, thereby maturing limited safety zones into broader territorial claims. They actively shape the interpretation of UNCLOS to legitimize their actions while maintaining a semblance of international law adherence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity_and_regional_power, agenda_setter,
    institutional, generational, constrained, global).

% These states lack the capacity to challenge the prolonged effective control of powerful states over artificial features, or to construct their own. They bear the cost of diminished maritime space, lost resource access, and increased geopolitical vulnerability, often having no recourse but diplomatic protest.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    powerless, biographical, trapped, regional).

% These bodies interpret UNCLOS and adjudicate disputes, but their authority is often limited by the willingness of powerful states to submit to their jurisdiction or enforce their rulings. They observe the drift in state practice and attempt to reconcile it with treaty text.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_tribunals, observer,
    institutional, civilizational, analytical, global).

% These states benefit from the clear recognition of full EEZ and territorial sea generated by their natural features, which is a core tenet of this reading. They generally support interpretations that uphold the primacy of natural geography while tolerating some aspects of effective control for artificial features.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_established_natural_features, beneficiary,
    powerful, generational, mobile, global).

% These states are unable to leverage the 'effective control' aspect of this reading to expand their maritime claims. While not as powerless as militarily weaker claimants, they still face disadvantages in contested areas and may see their potential maritime zones encroached upon by more capable actors.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_without_construction_capacity, payer,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity_and_regional_power).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for resolving maritime boundary disputes and allocating resource rights, balancing traditional geographic claims with the realities of state practice and effective control over artificial features.
% TRANSFER_FUNCTION: Transfers potential territorial claims and resource access from areas that might otherwise be international waters or contested zones to states capable of establishing and maintaining effective control over artificial features, while upholding natural feature claims.
% ABSENT_VOICES: States lacking naval power or construction capacity, indigenous communities with traditional maritime claims not recognized by UNCLOS, and environmental advocacy groups concerned about artificial island construction. Their objections are often marginalized in favor of state-centric interpretations.
% DISAPPEARANCE_RATIONALE: If this hybrid interpretation vanished overnight, there would be increased ambiguity and conflict over maritime claims, particularly regarding artificial features and contested zones. States would revert to more aggressive unilateral claims or strict interpretations, leading to greater instability and potential for military confrontation, as the compromise framework would be gone.
% FOUNDING_PROBLEM: How to reconcile traditional international law principles of maritime sovereignty (based on natural geography) with the increasing capacity of states to create artificial features and project power, while preventing unchecked expansionism and managing resource competition.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UNCLOS signatory states (especially those with moderate power projection), and some international organizations attest to the ongoing challenge of balancing these principles. The tension between natural rights and effective control remains a central issue in contemporary maritime disputes, corroborated by ongoing diplomatic and legal debates.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.58) is intermediate, reflecting the compromise nature: it allows some states to gain claims through artificial features and effective control, but not as expansively as other readings. Suppression (0.70) is high because the 'absent challenge' clause implies that militarily weaker states are effectively suppressed from contesting claims. Theater ratio (0.20) is low because the 'effective control' aspect requires genuine, sustained presence and administration, not just performative gestures. Accessibility collapse (0.60) is moderate, as some avenues for claims exist, but not for all actors. Resistance (0.50) is moderate, as this reading is a compromise that still faces objections from both stricter and more expansive interpretations.
 *
 * PERSPECTIVAL GAP:
 *   States with significant construction and naval capabilities view this reading as a pragmatic and legitimate evolution of international law, allowing for the protection of their investments and interests. Militarily weaker states, however, perceive it as a mechanism that legitimizes the expansionist tendencies of powerful actors, effectively extracting maritime space and resources from them through a 'might makes right' principle disguised as legal interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction capacity and regional power are primary beneficiaries and agenda-setters, as this reading provides a pathway for them to expand their maritime claims. States with established natural features also benefit from the clear recognition of their existing rights. Militarily weaker claimants and states without construction capacity are victims, as they bear the costs of diminished maritime space and resource access without the means to leverage the 'effective control' clause. International tribunals act as observers, attempting to apply the law amidst these competing interpretations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_unclos_maritime_sovereignty,
    'Is this constraint a genuine interpretation of UNCLOS, or a policy preference disguised as legal reading?',
    'Analysis of state practice, ICJ/ITLOS jurisprudence, and scholarly consensus over time. If the interpretation gains widespread acceptance and is consistently applied by international bodies, it strengthens its claim as a genuine reading.',
    'If a genuine reading, its legitimacy is higher, and its classification as a Tangled Rope reflects a complex, evolving legal reality. If a policy preference, its extractive elements are more clearly exposed as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_unclos_maritime_sovereignty, conceptual, 'This constraint is one reading of the `unclos_maritime_sovereignty` kernel.').

omega_variable(
    strict_geographic_reading_delta,
    'How would the constraint''s structure change if the ''strict geographic'' reading of UNCLOS prevailed?',
    'Counterfactual analysis: if only natural features generated full rights, artificial features would yield only safety zones, and ''effective control'' would be irrelevant for territorial claims. This would reduce extractiveness and suppression for weaker states.',
    'The constraint would shift towards a Rope or even a Mountain (for natural features), with significantly lower extractiveness and suppression, as the pathway for power-based claims would be closed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_geographic_reading_delta, conceptual, 'Impact of the ''strict geographic'' sibling reading.').

omega_variable(
    expansive_construction_reading_delta,
    'How would the constraint''s structure change if the ''expansive construction'' reading of UNCLOS prevailed?',
    'Counterfactual analysis: if artificial features generated full territorial rights through effective occupation, extractiveness and suppression would increase dramatically for weaker states, and the distinction between natural/artificial features would largely disappear for claim purposes.',
    'The constraint would shift towards a Snare, with significantly higher extractiveness and suppression, as powerful states could unilaterally create and claim vast maritime zones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expansive_construction_reading_delta, conceptual, 'Impact of the ''expansive construction'' sibling reading.').

omega_variable(
    effective_control_ambiguity,
    'What constitutes ''prolonged effective control absent challenge,'' and how is it measured or adjudicated?',
    'Development of clearer international legal standards, specific case law from international tribunals, or a new UNCLOS protocol defining these terms. Currently, it relies heavily on state practice and the absence of effective counter-challenge.',
    'Lack of clear definition allows powerful states to unilaterally define ''effective control,'' amplifying extraction. Clearer definitions would reduce ambiguity and potentially limit the scope for power-based claims, shifting the constraint towards a more balanced Tangled Rope or even a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_ambiguity, empirical, 'Ambiguity of ''prolonged effective control absent challenge''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1982, 2032).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1982, 0.18).
narrative_ontology:measurement(uncl_tr_t1992, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1992, 0.19).
narrative_ontology:measurement(uncl_tr_t2002, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement(uncl_tr_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2012, 0.21).
narrative_ontology:measurement(uncl_tr_t2022, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2022, 0.21).
narrative_ontology:measurement(uncl_tr_t2032, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2032, 0.22).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(uncl_be_t1992, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1992, 0.57).
narrative_ontology:measurement(uncl_be_t2002, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2002, 0.59).
narrative_ontology:measurement(uncl_be_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2012, 0.6).
narrative_ontology:measurement(uncl_be_t2022, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement(uncl_be_t2032, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2032, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(uncl_su_t1992, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1992, 0.67).
narrative_ontology:measurement(uncl_su_t2002, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2002, 0.69).
narrative_ontology:measurement(uncl_su_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(uncl_su_t2022, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement(uncl_su_t2032, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2032, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'unclos_maritime_sovereignty' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
