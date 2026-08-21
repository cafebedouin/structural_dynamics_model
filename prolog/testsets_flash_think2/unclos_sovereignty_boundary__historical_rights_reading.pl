% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Usage and Occupation Override UNCLOS EEZ
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint asserts that historical usage and occupation create
 *   sovereign rights that predate and override the Exclusive Economic Zone
 *   (EEZ) provisions of the United Nations Convention on the Law of the Sea
 *   (UNCLOS). It is a reading of the 'unclos_sovereignty_boundary' kernel,
 *   specifically the 'historical_rights_reading'. This reading is actively
 *   promoted by certain states to justify expansive maritime claims, often
 *   leading to disputes with coastal states adhering to UNCLOS. The
 *   constraint is claimed as a Tangled Rope because it coordinates the
 *   actions of claimant states while extracting from others through active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.8).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Usage and Occupation Override UNCLOS EEZ").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '3638a0bd-a3f6-44ff-be50-89ea62c34d89').
narrative_ontology:cs_kernel_codification('3638a0bd-a3f6-44ff-be50-89ea62c34d89', formalized).
narrative_ontology:cs_authority_grounding('3638a0bd-a3f6-44ff-be50-89ea62c34d89', extraction).
narrative_ontology:cs_interpretation_layer_present('3638a0bd-a3f6-44ff-be50-89ea62c34d89').
narrative_ontology:cs_reading_relation('3638a0bd-a3f6-44ff-be50-89ea62c34d89', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('3638a0bd-a3f6-44ff-be50-89ea62c34d89', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('3638a0bd-a3f6-44ff-be50-89ea62c34d89', foundational, historical_sovereignty_precedence).
narrative_ontology:cs_axiom_status(historical_sovereignty_precedence, holdable).
narrative_ontology:cs_axiom_grounding('3638a0bd-a3f6-44ff-be50-89ea62c34d89', historical_sovereignty_precedence, conventional).
narrative_ontology:cs_axiom('3638a0bd-a3f6-44ff-be50-89ea62c34d89', secondary, effective_control_as_source_of_right).
narrative_ontology:cs_axiom_status(effective_control_as_source_of_right, holdable).
narrative_ontology:cs_axiom_grounding('3638a0bd-a3f6-44ff-be50-89ea62c34d89', effective_control_as_source_of_right, empirically_contingent).
narrative_ontology:cs_reference_frame('3638a0bd-a3f6-44ff-be50-89ea62c34d89', pre_unclos_customary_maritime_law).
narrative_ontology:cs_drift_state('3638a0bd-a3f6-44ff-be50-89ea62c34d89', contemporary_maritime_disputes, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('3638a0bd-a3f6-44ff-be50-89ea62c34d89', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historical_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, effective_occupation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that assert historical rights and usage as a basis for sovereign claims over maritime areas, often overlapping with other states' UNCLOS-defined Exclusive Economic Zones. They actively enforce these claims through naval patrols, administrative decrees, and diplomatic pressure, benefiting from potential resource control and strategic advantage.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, beneficiary).

% States whose UNCLOS-defined Exclusive Economic Zones are challenged by historical claims. They bear the costs of lost resource control, increased security risks, and diplomatic friction. Their options are limited to diplomatic protest, legal challenges, or increased naval presence to defend their EEZ rights.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, excluded).

% Commercial shipping, fishing fleets, and military vessels that operate in contested maritime areas. They face increased uncertainty, potential harassment, and higher insurance costs due to overlapping claims and assertive enforcement. Their freedom of navigation is constrained by the assertion of historical rights.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors, payer,
    organized, immediate, constrained, global).

% The majority of states that have ratified UNCLOS and adhere to its framework for maritime governance. They observe the challenges to UNCLOS, often issuing diplomatic statements upholding the treaty, but may not be directly involved in enforcement or dispute resolution for specific claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_signatories, observer,
    institutional, generational, analytical, global).

% International courts and arbitration bodies (e.g., ITLOS, PCA) that adjudicate maritime disputes. They provide legal interpretations of UNCLOS and customary international law, but their jurisdiction often depends on the consent of disputing parties, which claimant states may withhold.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of expansive claimant states by providing a shared legal and historical narrative to legitimize their territorial expansion into maritime areas, thereby reducing internal friction among these states regarding their shared objectives.
% TRANSFER_FUNCTION: Transfers potential resource exploitation rights, strategic control, and sovereign authority over contested maritime zones from states adhering strictly to UNCLOS EEZ provisions to states asserting historical claims.
% ABSENT_VOICES: Small island developing states and landlocked states, whose maritime rights and access are almost entirely dependent on the UNCLOS framework, would strongly object to any erosion of its authority. Their voices are often marginalized in disputes dominated by powerful claimant states.
% DISAPPEARANCE_RATIONALE: If the principle that historical usage overrides UNCLOS EEZ provisions vanished overnight, the legal basis for numerous ongoing maritime disputes would collapse. Claimant states would lose their primary justification for expansive claims, leading to a re-stabilization of UNCLOS-based boundaries, but also potentially new forms of contestation or a power vacuum in previously disputed areas.
% FOUNDING_PROBLEM: To provide a legal and historical justification for states to assert sovereign rights over maritime areas beyond the limits defined by modern international treaties like UNCLOS, particularly where historical claims predate or conflict with these treaties.
% FOUNDING_PROBLEM_CORROBORATION: Expansive claimant states and their national legal scholars attest that the founding problem (legitimizing historical claims) is live and ongoing, citing historical maps, ancient texts, and traditional fishing grounds. However, the vast majority of UNCLOS signatories and international legal scholars outside these claimant states dispute the legal validity of these claims overriding UNCLOS, viewing the founding problem as an attempt to circumvent established international law.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because this reading directly challenges and seeks to diminish the established rights of coastal states under UNCLOS, transferring potential control and resources. Suppression is also high (0.75) as the persistence of these claims relies on active enforcement (naval presence, diplomatic pressure) and the suppression of alternative interpretations of international law. The theater ratio is moderate (0.4) because while there is a performative aspect to asserting historical narratives, there are also real, tangible enforcement actions. Accessibility collapse is significant (0.65) for EEZ-holding states, as their alternatives to contest these claims are limited. Resistance is high (0.8) due to widespread international opposition and active diplomatic and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of expansive claimant states (agenda_setter/beneficiary), this constraint is a legitimate assertion of long-standing sovereign rights, a 'natural' extension of their historical presence. From the perspective of EEZ-holding coastal states and navigational actors (payers/victims), it is an extractive and coercive challenge to established international law, undermining stability and creating conflict. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are clear beneficiaries and agenda-setters, as the constraint directly serves their geopolitical and economic interests. EEZ-holding coastal states and navigational actors are targets/victims, bearing the costs of diminished control and increased operational risk. UNCLOS signatories and international tribunals act as observers, analyzing and commenting on the constraint's operation without directly benefiting or paying in the same way.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_sufficiency,
    'Is the historical evidence presented by claimant states sufficiently robust and continuous to establish sovereign rights that genuinely predate and override modern treaty law?',
    'Independent, multi-disciplinary historical and archaeological review, coupled with international legal arbitration that rigorously assesses the evidentiary standards for ''effective occupation'' and ''historical usage'' in maritime contexts.',
    'If the evidence is found insufficient, the constraint''s legitimacy would collapse, reclassifying it closer to a Snare. If deemed sufficient, it would strengthen the ''conventional'' grounding of the axioms, potentially shifting its classification towards a more legitimate (though still extractive) Tangled Rope or even a contested Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_sufficiency, empirical, 'Ambiguity regarding the factual basis and legal interpretation of historical claims.').

omega_variable(
    customary_law_evolution,
    'To what extent has UNCLOS, through widespread ratification and state practice, superseded or modified prior customary international law regarding maritime sovereignty, rendering historical claims based on older customs invalid?',
    'A definitive ruling by the International Court of Justice or a similar body on the relationship between UNCLOS and pre-existing customary law in specific contested areas, considering the principle of ''lex posterior derogat legi priori'' (later law repeals earlier law).',
    'If UNCLOS is found to have superseded prior custom, the constraint''s legal foundation would be severely weakened, increasing its effective extractiveness and suppression. If prior custom is found to retain significant force, it would lend more legitimacy to the claimant states'' position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_evolution, conceptual, 'Uncertainty about the hierarchical relationship between treaty law and evolving customary international law.').

omega_variable(
    enforcement_legitimacy_threshold,
    'At what point does the ''active enforcement'' of historical claims by claimant states cross the threshold from legitimate assertion of perceived rights to coercive action that violates the sovereignty or rights of other states?',
    'Establishment of clear international guidelines or precedents for ''peaceful assertion'' versus ''coercive action'' in disputed maritime zones, potentially through UN Security Council resolutions or regional security frameworks.',
    'A lower threshold for ''coercive action'' would increase the perceived suppression and extractiveness of the constraint, pushing its classification closer to a Snare. A higher threshold would allow more aggressive assertion without immediate reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_threshold, preference, 'Ambiguity in the international community''s tolerance for unilateral enforcement of contested claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1970, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(uncl_tr_t1985, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(uncl_tr_t2000, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(uncl_tr_t2010, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(uncl_tr_t2025, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1970, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(uncl_be_t1985, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(uncl_be_t2000, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(uncl_be_t2010, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement(uncl_be_t2025, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1970, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(uncl_su_t1985, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(uncl_su_t2000, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(uncl_su_t2010, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(uncl_su_t2025, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_sovereignty_boundary' kernel. This 'historical_rights_reading' directly challenges the 'strict_eez_reading' and influences the operational context of the 'non_ratifier_enforcement_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
