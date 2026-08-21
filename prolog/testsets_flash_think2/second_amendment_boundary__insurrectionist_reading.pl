% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment: Insurrectionist Reading (Right to Resist Tyranny)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'insurrectionist_reading' of the Second
 *   Amendment, asserting an individual right to possess arms, including
 *   military-grade, for the purpose of resisting a tyrannical government. It
 *   views individual possession as instrumental to potential overthrow. This
 *   reading differs from others by explicitly extending protection to arms
 *   for potential overthrow and treating state disarmament as a precursor to
 *   tyranny. The claim/metric gap is deliberate: the constraint is CLAIMED as
 *   a tangled_rope (acknowledging coordination for armed citizens and
 *   extraction from the state) while the authored metrics describe its
 *   substantially extractive and suppressive operation from the perspective
 *   of the state and civilians.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.85).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.9).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment: Insurrectionist Reading (Right to Resist Tyranny)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'e3d81014-daf1-45f2-85ef-17730c9797b6').
narrative_ontology:cs_kernel_codification('e3d81014-daf1-45f2-85ef-17730c9797b6', fixed_text).
narrative_ontology:cs_authority_grounding('e3d81014-daf1-45f2-85ef-17730c9797b6', lineage).
narrative_ontology:cs_interpretation_layer_present('e3d81014-daf1-45f2-85ef-17730c9797b6').
narrative_ontology:cs_reading_relation('e3d81014-daf1-45f2-85ef-17730c9797b6', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3d81014-daf1-45f2-85ef-17730c9797b6', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('e3d81014-daf1-45f2-85ef-17730c9797b6', foundational, individual_right_to_resist_tyranny).
narrative_ontology:cs_axiom_status(individual_right_to_resist_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('e3d81014-daf1-45f2-85ef-17730c9797b6', individual_right_to_resist_tyranny, deontological).
narrative_ontology:cs_axiom('e3d81014-daf1-45f2-85ef-17730c9797b6', secondary, arms_possession_as_deterrent_against_state).
narrative_ontology:cs_axiom_status(arms_possession_as_deterrent_against_state, holdable).
narrative_ontology:cs_axiom_grounding('e3d81014-daf1-45f2-85ef-17730c9797b6', arms_possession_as_deterrent_against_state, instrumental).
narrative_ontology:cs_reference_frame('e3d81014-daf1-45f2-85ef-17730c9797b6', founding_era_right_of_revolution).
narrative_ontology:cs_drift_state('e3d81014-daf1-45f2-85ef-17730c9797b6', contemporary_political_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3d81014-daf1-45f2-85ef-17730c9797b6', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_prioritizing_peace_and_safety).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, right_of_revolution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Believe they are upholding a constitutional check on government, maintaining a deterrent against tyranny, and are prepared to act if the government becomes tyrannical. They benefit from the perceived legitimacy of their armed status and the legal protection of their arms.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    organized, generational, identity_locked, national).

% Faces a challenge to its monopoly on force, increased risk in law enforcement operations, and the potential for armed conflict. It bears the costs of heightened security, training, and the erosion of its authority over arms regulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, biographical, constrained, national).

% Bear the risk of armed conflict, increased gun violence, and live under the threat of societal breakdown. They pay with their sense of security and potential physical harm, with limited ability to opt out of these societal risks.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_prioritizing_peace_and_safety, payer,
    powerless, immediate, trapped, local).

% Seek to uphold the state's legitimate authority to regulate arms and maintain public order, but are challenged by this interpretation. Their efforts to enact stricter firearms policies are often legally and politically constrained by this reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, government_officials_claiming_monopoly_on_force, agenda_setter,
    institutional, biographical, constrained, national).

% Analyze the historical, legal, and theoretical underpinnings of the Second Amendment and its various interpretations, without direct involvement in enforcement or resistance. They provide critical analysis of the reading's implications.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_scholars_analytical, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate armed citizens in preserving a collective capacity for resistance against a potentially tyrannical government, ensuring a popular check on state power.
% TRANSFER_FUNCTION: Transfers a degree of authority and legitimacy over the use of force from the state to armed citizens; transfers the risk and potential costs of armed conflict to the state and civilians.
% ABSENT_VOICES: Government officials and international bodies advocating for state monopoly on force, as well as civilians prioritizing peace and public safety above the right to armed resistance, are often excluded from the discourse that legitimizes this reading.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, the state would likely move swiftly to assert a stronger monopoly on force, leading to significant changes in firearms policy, law enforcement tactics, and the political landscape surrounding civil-military relations. Armed citizen groups would lose their primary legal justification.
% FOUNDING_PROBLEM: The founding problem was the fear of a tyrannical government, either foreign or domestic, and the perceived need for the populace to retain the capacity for armed resistance as a final check on state power.
% FOUNDING_PROBLEM_CORROBORATION: Historical writings of some Founding Fathers (e.g., Jefferson, Mason) and contemporary anti-tyranny movements corroborate the original intent to provide a check on government. Mainstream legal scholarship and state authorities often contest this as the primary or currently relevant interpretation, arguing the founding problem is largely 'dead' in the modern context.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading challenges the state's monopoly on force and imposes significant societal costs and risks. Suppression is also high (0.90) as it actively suppresses the state's ability to regulate firearms and maintain public order. Theater ratio is low (0.10) because proponents of this reading generally hold it with strong conviction, viewing it as a serious, functional right rather than a performative one. Accessibility collapse for state control is high (0.80) as this interpretation severely limits legislative options for firearms regulation. Resistance is high (0.75) from both state authorities (through legislative and legal challenges) and segments of the civilian population (advocating for stricter gun control). The measurement series reflect a gradual increase in the perceived extractiveness and suppression as this reading gains prominence and faces opposition.
 *
 * PERSPECTIVAL GAP:
 *   The armed citizens (beneficiaries) experience this as a vital constitutional protection and a coordination mechanism for collective security against potential tyranny. In contrast, the state security apparatus and civilians (payers/victims) experience it as a highly extractive and suppressive force that undermines public safety and state authority. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens are the primary beneficiaries (d near 0.0) as the constraint legitimizes their armed status and potential for resistance. The state security apparatus and civilians are the primary targets (d near 1.0), bearing the costs of challenged authority, increased risk, and potential conflict. Government officials, while agenda-setters, are constrained by this reading, placing them closer to the target end than a pure beneficiary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tyranny_definition_ambiguity,
    'What specific conditions or actions constitute ''tyrannical government'' sufficient to trigger the right to armed resistance under this reading?',
    'Legal precedent from hypothetical or actual cases of armed resistance, or a clear, widely accepted political theory of legitimate rebellion.',
    'A narrow definition would limit the scope of the right and reduce its perceived extractiveness from the state; a broad or ambiguous definition amplifies the extractiveness and societal risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tyranny_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''tyranny'' that justifies armed resistance.').

omega_variable(
    effectiveness_of_deterrence,
    'Does the widespread individual possession of military-grade arms genuinely deter governmental overreach, or does it primarily increase societal violence and instability?',
    'Empirical studies comparing political outcomes and violence levels in jurisdictions with and without this interpretation''s practical effects, or historical analysis of similar contexts.',
    'If deterrence is proven ineffective or counterproductive, the instrumental justification for the right weakens, potentially reclassifying it as a snare for civilians; if effective, it strengthens the coordination aspect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_deterrence, empirical, 'The actual impact of armed resistance capacity on government behavior and societal stability.').

omega_variable(
    structural_delta_military_arms,
    'To what extent does this reading''s protection of military-grade arms fundamentally alter the balance of power between citizens and the state, compared to other readings?',
    'Comparative legal analysis across different Second Amendment interpretations and their practical effects on arms availability and state regulatory capacity.',
    'If military-grade arms are deemed essential for the ''insurrectionist'' purpose, it significantly increases the extractiveness from the state and the risk to civilians. If not, the reading''s unique structural delta is diminished.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_military_arms, conceptual, 'The specific impact of military-grade arms on the power balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1980, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(seco_tr_t2020, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2020, 0.09).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1980, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(seco_be_t1990, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(seco_be_t2020, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1980, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(seco_su_t1990, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(seco_su_t2020, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_boundary' kernel. Each reading has a unique structural interpretation and ε value, and they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
