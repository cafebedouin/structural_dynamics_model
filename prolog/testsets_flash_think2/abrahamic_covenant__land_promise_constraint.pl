% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant: Land Promise (Territorial Claim Reading)
 *   domain: religious_studies/geopolitical/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Abrahamic covenant,
 *   focusing on the territorial grant of the Land of Canaan as an
 *   unconditional and exclusive divine promise. This reading serves as a
 *   foundational justification for modern geopolitical claims and actions,
 *   particularly in the Israeli-Palestinian conflict. The constraint is
 *   classified as a Snare due to its high extractiveness and suppression,
 *   which disproportionately impact displaced populations and non-Jewish
 *   residents. The claimed type (Snare) reflects the structural reality of
 *   its operation, while the narrative context acknowledges its origin in a
 *   religious text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.85).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.9).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.85).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant: Land Promise (Territorial Claim Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/geopolitical/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '95f79a42-3d97-4085-a8da-6a6c11695411').
narrative_ontology:cs_kernel_codification('95f79a42-3d97-4085-a8da-6a6c11695411', fixed_text).
narrative_ontology:cs_authority_grounding('95f79a42-3d97-4085-a8da-6a6c11695411', extraction).
narrative_ontology:cs_interpretation_layer_present('95f79a42-3d97-4085-a8da-6a6c11695411').
narrative_ontology:cs_reading_relation('95f79a42-3d97-4085-a8da-6a6c11695411', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('95f79a42-3d97-4085-a8da-6a6c11695411', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('95f79a42-3d97-4085-a8da-6a6c11695411', foundational, divine_unconditional_land_grant).
narrative_ontology:cs_axiom_status(divine_unconditional_land_grant, holdable).
narrative_ontology:cs_axiom_grounding('95f79a42-3d97-4085-a8da-6a6c11695411', divine_unconditional_land_grant, theological).
narrative_ontology:cs_axiom('95f79a42-3d97-4085-a8da-6a6c11695411', foundational, exclusive_inheritance_through_isaac).
narrative_ontology:cs_axiom_status(exclusive_inheritance_through_isaac, holdable).
narrative_ontology:cs_axiom_grounding('95f79a42-3d97-4085-a8da-6a6c11695411', exclusive_inheritance_through_isaac, theological).
narrative_ontology:cs_reference_frame('95f79a42-3d97-4085-a8da-6a6c11695411', unconditional_divine_mandate).
narrative_ontology:cs_drift_state('95f79a42-3d97-4085-a8da-6a6c11695411', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('95f79a42-3d97-4085-a8da-6a6c11695411', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, state_actors_leveraging_covenant).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_scholars_and_interpreters).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinian_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, non_jewish_residents_of_canaan).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_right_to_land).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, exclusive_inheritance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforce territorial claims and policies based on this reading of the covenant, using legal, military, and diplomatic means. They benefit from the perceived legitimacy and territorial control derived from the covenant.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, state_actors_leveraging_covenant, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct and intergenerational costs of territorial claims, including loss of land, homes, and self-determination. Their ability to return or establish sovereignty is severely constrained by the enforcement of this covenant reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinian_populations, payer,
    powerless, generational, trapped, regional).

% Live under the territorial claims derived from this covenant reading, facing restrictions on land use, building, and movement. They experience systemic discrimination and the constant threat of displacement.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, non_jewish_residents_of_canaan, payer,
    powerless, biographical, constrained, local).

% Their interpretations provide theological grounding and moral justification for the territorial claims, enhancing their influence and authority within their communities and in political discourse. They benefit from the perpetuation of this narrative.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_scholars_and_interpreters, beneficiary,
    organized, generational, mobile, global).

% Analyze the conflict and territorial claims through the lens of international law, human rights, and self-determination. Their findings often contradict the claims derived from this covenant reading, but they lack direct enforcement power.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, state_actors_leveraging_covenant).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and beliefs of those who accept this reading, providing a unified theological and historical basis for territorial claims and settlement activities in the Land of Canaan.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from displaced and non-Jewish populations to state actors and settlers who claim a divine mandate based on this covenant reading.
% ABSENT_VOICES: Indigenous populations and alternative theological interpretations that emphasize conditional promises, universal justice, or non-exclusive inheritance are structurally excluded or marginalized. They would argue for shared stewardship, human rights, and a non-territorial understanding of the covenant.
% DISAPPEARANCE_RATIONALE: If this specific reading of the Abrahamic covenant as an unconditional, exclusive territorial grant vanished overnight, a primary ideological and historical justification for certain state policies and settlement activities would collapse. This would fundamentally alter the geopolitical landscape of the Israeli-Palestinian conflict, leading to a re-evaluation of land claims and rights.
% FOUNDING_PROBLEM: To establish a divine mandate and an enduring claim for a specific people to a specific territory (the Land of Canaan), ensuring their presence and sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Beneficiary state actors and religious scholars attest that the founding problem (securing the land for the Jewish people) is still live and ongoing, citing religious texts and historical narratives. International legal bodies, human rights organizations, and Palestinian historians attest that the 'problem' has been transformed into a tool for dispossession, and that the original theological intent is either fulfilled, conditional, or superseded by modern international law; their corroboration comes from legal analysis, demographic data, and historical records outside the benefiting parties.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading directly underpins policies leading to land confiscation, displacement, and denial of self-determination. Suppression is very high (0.90) as the territorial claims are enforced through military occupation, legal frameworks, and diplomatic pressure, actively suppressing resistance and alternative narratives. The theater ratio is moderate (0.40): while there is genuine religious belief in the divine promise, a significant portion of the justification and enforcement serves to maintain political and territorial control, often obscuring the extractive mechanisms. Accessibility collapse is high for victims, as their alternatives (e.g., return, statehood) are systematically denied. Resistance is also high, reflecting the ongoing conflict.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state actors and religious beneficiaries, this constraint is a divinely ordained right, a fulfillment of prophecy, and a necessary act of self-determination. From the perspective of the victims, it is a mechanism of dispossession, occupation, and ethnic cleansing, cloaked in religious justification. The engine's classification as a Snare reflects the latter, structurally derived perspective, while acknowledging the former as the claimed justification.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors leveraging this covenant reading are primary beneficiaries (d near 0.0), as they gain territorial legitimacy and control. Religious scholars also benefit from the validation of their interpretations. Displaced Palestinian populations and non-Jewish residents are clear targets (d near 1.0), bearing the direct costs of land loss, restricted rights, and violence. International legal bodies act as analytical observers, assessing the constraint's operation against universal legal principles.
 *
 * MANDATROPHY ANALYSIS:
 *   The original 'founding problem' of securing a land for a people, while still 'live' for beneficiaries, is 'contested' by victims and observers who argue that the problem has either been fulfilled or that the constraint's function has drifted. The persistence of this constraint, despite its contested status and high extraction, indicates a potential mandatrophy where the original mandate (divine promise) is leveraged to maintain an extractive political structure. The high suppression and extractiveness suggest it is not merely inertial (Piton) but actively maintained for concentrated benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_unconditional_promise,
    'Is the Abrahamic land promise conditional on obedience and righteous conduct, or is it an unconditional, eternal grant?',
    'Theological re-interpretation within the tradition that gains widespread acceptance, or a shift in political discourse to prioritize conditional interpretations.',
    'If conditional, the legitimacy of current territorial claims could be challenged based on contemporary conduct, potentially reducing extractiveness. If unconditional, the claims remain robust within this theological framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_vs_unconditional_promise, conceptual, 'Ambiguity regarding the conditional nature of the divine land promise.').

omega_variable(
    fulfillment_status_of_promise,
    'Has the Abrahamic land promise already been fulfilled in historical events (e.g., ancient Israelite kingdoms, modern statehood), or is it an ongoing, unfulfilled promise?',
    'A consensus among theological and historical scholars that the promise has been fulfilled, or a political settlement that redefines the terms of fulfillment.',
    'If fulfilled, the ongoing territorial claims lose their theological urgency, potentially reducing suppression and extractiveness. If ongoing, it continues to justify expansionist policies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_status_of_promise, empirical, 'Disagreement on whether the land promise is historically fulfilled or still active.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (military, legal, economic barriers) or internalized (ideological conviction, historical trauma, identity fusion)?',
    'Post-conflict analysis of displaced populations: if resistance and claims for return persist strongly after structural barriers are removed, internalized suppression is lower than structural measures suggest.',
    'If suppression is primarily structural, removing external barriers would significantly reduce its effective force. If internalized, the constraint''s effective suppression is higher than structural measures suggest, as the target carries the suppression with them after exit, making resolution more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of territorial claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(abra_tr_t1987, abrahamic_covenant__land_promise_constraint, theater_ratio, 1987, 0.38).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(abra_tr_t2023, abrahamic_covenant__land_promise_constraint, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(abra_be_t1987, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1987, 0.82).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement(abra_be_t2023, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement(abra_su_t1987, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1987, 0.87).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.91).
narrative_ontology:measurement(abra_su_t2023, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2023, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Abrahamic covenant kernel. Its structural properties and metrics differ significantly from sibling readings, which focus on different aspects of the covenant (e.g., lineage, prophetic succession) and have different beneficiaries/victims. All readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
