% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra: Reformist Contextual Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the 'reformist contextual' reading of the
 *   Dharmasastra corpus, which interprets the texts as reflecting historical
 *   social conditions and seeks to separate an eternal ethical core (dharma
 *   as righteous conduct) from time-bound caste prescriptions. It aims to
 *   preserve the authority of the texts while adapting them to modern ethical
 *   sensibilities. The classification as a Tangled Rope reflects its genuine
 *   coordination function (ethical guidance) alongside persistent, albeit
 *   reinterpreted, asymmetric extraction (residual caste influence).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.52).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.58).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.52).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra: Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, '7eb6515d-18b1-41be-b430-46f30fb652d3').
narrative_ontology:cs_kernel_codification('7eb6515d-18b1-41be-b430-46f30fb652d3', fixed_text).
narrative_ontology:cs_authority_grounding('7eb6515d-18b1-41be-b430-46f30fb652d3', lineage).
narrative_ontology:cs_interpretation_layer_present('7eb6515d-18b1-41be-b430-46f30fb652d3').
narrative_ontology:cs_reading_relation('7eb6515d-18b1-41be-b430-46f30fb652d3', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('7eb6515d-18b1-41be-b430-46f30fb652d3', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('7eb6515d-18b1-41be-b430-46f30fb652d3', foundational, dharma_is_universal_ethical_principle).
narrative_ontology:cs_axiom_status(dharma_is_universal_ethical_principle, holdable).
narrative_ontology:cs_axiom_grounding('7eb6515d-18b1-41be-b430-46f30fb652d3', dharma_is_universal_ethical_principle, deontological).
narrative_ontology:cs_axiom('7eb6515d-18b1-41be-b430-46f30fb652d3', foundational, caste_prescriptions_are_historical_contingent).
narrative_ontology:cs_axiom_status(caste_prescriptions_are_historical_contingent, holdable).
narrative_ontology:cs_axiom_grounding('7eb6515d-18b1-41be-b430-46f30fb652d3', caste_prescriptions_are_historical_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('7eb6515d-18b1-41be-b430-46f30fb652d3', ethical_dharma_as_universal).
narrative_ontology:cs_drift_state('7eb6515d-18b1-41be-b430-46f30fb652d3', contemporary_ethical_discourse, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('7eb6515d-18b1-41be-b430-46f30fb652d3', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_scholars).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, general_hindu_community).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, dalits_and_lower_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, orthodox_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, general_hindu_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively reinterpret Dharmasastra texts to align with modern ethical standards, emphasizing the universal ethical core (dharma) while contextualizing or discarding caste-based prescriptions. They gain moral authority and influence within the community by offering a path to reconcile tradition with modernity.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from a modernized ethical framework that allows engagement with tradition without endorsing historical injustices. They bear the social friction and internal debate that comes with reinterpretation, and some may still experience residual social pressures from traditional hierarchies.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, general_hindu_community, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, general_hindu_community, payer).

% While the reformist reading aims to mitigate caste discrimination, they still bear the residual social stigma and disadvantage from historical caste structures, even if reinterpreted as spiritual stages. Their identity is often deeply intertwined with their social position, making exit from the system difficult.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dalits_and_lower_castes, payer,
    powerless, generational, identity_locked, local).

% Their literalist interpretation of Dharmasastra, particularly regarding varna/jati hierarchy, is challenged and often marginalized by the reformist reading. They bear the cost of losing traditional authority and influence, and are often excluded from mainstream reformist discourse, though they maintain their own interpretive traditions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_interpreters, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, orthodox_interpreters, excluded).

% Observe the ongoing debates and social impact of Dharmasastra interpretations. They often advocate for complete abolition of caste-based discrimination and may view even reformist interpretations as insufficient or perpetuating the problem. They are largely outside the internal religious interpretive framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, secular_activists, observer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, secular_activists, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for righteous conduct (dharma) and social order that is adaptable to changing historical and ethical contexts, allowing adherents to maintain continuity with tradition while addressing historical injustices.
% TRANSFER_FUNCTION: Transfers moral authority from rigid, literal textual interpretation to dynamic, ethically-informed scholarship; shifts social obligations from strict caste adherence to broader ethical principles; and aims to diffuse traditional social privileges more equitably.
% ABSENT_VOICES: Those who advocate for the complete abolition of Dharmasastra's authority due to its historical association with oppression (abolitionist_rejection reading) are largely excluded from the internal reformist debate, as their position rejects the premise of textual authority that reformists seek to preserve.
% DISAPPEARANCE_RATIONALE: If the reformist contextual reading vanished, the Hindu community would face a profound crisis of reconciling tradition with modern ethics. Either a return to more literalist interpretations (re-entrenching historical injustices) or a complete abandonment of Dharmasastra's authority would occur, fundamentally reorganizing religious and social life.
% FOUNDING_PROBLEM: To provide a comprehensive and authoritative guide for righteous living (dharma) and social order across various aspects of human life in ancient Indian society.
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars and social historians corroborate that the ethical core of dharma remains a live problem, but the specific social prescriptions (especially caste) are dead or require reinterpretation due to historical contingency and modern ethical standards. Orthodox religious leaders and traditional communities dispute this, asserting the eternal validity of all prescriptions.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.52, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.52) is moderate, reflecting the ongoing effort to reduce the burden of caste while still operating within a framework that historically legitimized it. Suppression (0.58) is present as reformist interpretations must actively counter orthodox resistance and guide social practice. The theater ratio (0.28) is relatively low, as the ethical core remains functional, but some reinterpretation of caste may involve symbolic rather than fully transformative changes. Accessibility collapse (0.45) is moderate, as alternatives (secularism, other religious paths) exist, but exiting the traditional framework carries social costs. Resistance (0.55) is significant due to ongoing debates with orthodox factions and social movements for caste abolition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformist scholars, this reading is a necessary and beneficial adaptation, preserving tradition while promoting justice. From the perspective of Dalits and lower castes, the 'softened' extraction may still be a significant burden, and the pace of change too slow. Orthodox interpreters view it as an illegitimate dilution of sacred texts. The engine's classification captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars are agenda-setters and beneficiaries, shaping the interpretation and gaining moral authority. The general Hindu community benefits from ethical guidance but also bears the social costs of internal conflict. Dalits and lower castes remain victims, bearing the residual social stigma and disadvantage, even if softened. Orthodox interpreters are victims of the reformist reading, as their traditional authority is challenged. Secular activists are observers, outside the interpretive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dharmasastra_reading_identity,
    'Is this constraint a genuine ''reformist contextual'' reading, or is it a ''tangled rope'' that merely re-frames existing extraction under a new narrative?',
    'Longitudinal study of social mobility and discrimination rates among lower castes in communities adopting this reading, compared to those adhering to orthodox or abolitionist views. If material conditions for lower castes do not significantly improve, the ''reformist'' label may be largely performative.',
    'If largely performative, the effective extractiveness and theater_ratio would be higher, pushing the classification closer to a Snare or a more extractive Tangled Rope. If genuine, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharmasastra_reading_identity, empirical, 'Whether the reformist reading genuinely reduces extraction or merely re-frames it.').

omega_variable(
    ethical_core_boundary_ambiguity,
    'Where precisely does the ''ethical core'' of Dharmasastra end, and the ''time-bound caste prescriptions'' begin? Is this boundary consistently applied across different reformist interpretations?',
    'Comparative textual analysis of prominent reformist commentaries and their application in specific social contexts. Divergent or inconsistent boundaries would indicate a conceptual ambiguity that allows for selective application.',
    'If the boundary is ambiguous or inconsistently applied, it creates a loophole for residual extraction to persist under the guise of ''ethical core,'' increasing effective extractiveness. If clear and consistent, it strengthens the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_core_boundary_ambiguity, conceptual, 'Ambiguity in separating ethical core from time-bound prescriptions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58) primarily structural (e.g., social pressure, economic dependency) or internalized (e.g., self-identification with caste, belief in karma-based hierarchy)?',
    'Post-exit suppression trajectory: if individuals who formally exit caste-based communities (e.g., through conversion or migration) continue to experience internalized stigma or self-limiting beliefs, it suggests a significant internalized component. If suppression dissipates quickly upon structural exit, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient and harder to dismantle. If structural, external interventions are more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in caste-related social dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1950, dharmasastra_corpus__reformist_contextual, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(dhar_tr_t1965, dharmasastra_corpus__reformist_contextual, theater_ratio, 1965, 0.23).
narrative_ontology:measurement(dhar_tr_t1980, dharmasastra_corpus__reformist_contextual, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(dhar_tr_t1995, dharmasastra_corpus__reformist_contextual, theater_ratio, 1995, 0.27).
narrative_ontology:measurement(dhar_tr_t2010, dharmasastra_corpus__reformist_contextual, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(dhar_tr_t2020, dharmasastra_corpus__reformist_contextual, theater_ratio, 2020, 0.28).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1950, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(dhar_be_t1965, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(dhar_be_t1980, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(dhar_be_t1995, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1995, 0.51).
narrative_ontology:measurement(dhar_be_t2010, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(dhar_be_t2020, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2020, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1950, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(dhar_su_t1965, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement(dhar_su_t1980, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1980, 0.59).
narrative_ontology:measurement(dhar_su_t1995, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1995, 0.57).
narrative_ontology:measurement(dhar_su_t2010, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(dhar_su_t2020, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2020, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Dharmasastra corpus kernel, alongside 'orthodox_literalist' and 'abolitionist_rejection'. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
