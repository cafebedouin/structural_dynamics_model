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
 *   human_readable: Second Amendment Boundary (Insurrectionist Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'insurrectionist' reading of the Second
 *   Amendment, where the right to bear arms is primarily understood as a
 *   means for individuals to resist a tyrannical government. This
 *   interpretation extends protection to military-grade weaponry and views
 *   state disarmament efforts as precursors to tyranny. It is one reading of
 *   the 'second_amendment_boundary' kernel, distinct from individual
 *   self-defense or militia-conditioned interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.65).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.7).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Boundary (Insurrectionist Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'beaf4d32-bfd0-4d1f-a050-9c7600aec6d8').
narrative_ontology:cs_kernel_codification('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', fixed_text).
narrative_ontology:cs_authority_grounding('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', lineage).
narrative_ontology:cs_interpretation_layer_present('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8').
narrative_ontology:cs_reading_relation('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', second_amendment_boundary__individual_right_reading, influences).
narrative_ontology:cs_reading_relation('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', foundational, armed_populace_as_tyranny_deterrent).
narrative_ontology:cs_axiom_status(armed_populace_as_tyranny_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', armed_populace_as_tyranny_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', foundational, individual_possession_instrumental_to_overthrow).
narrative_ontology:cs_axiom_status(individual_possession_instrumental_to_overthrow, holdable).
narrative_ontology:cs_axiom_grounding('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', individual_possession_instrumental_to_overthrow, empirically_contingent).
narrative_ontology:cs_reference_frame('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', founding_era_anti_tyranny_ethos).
narrative_ontology:cs_drift_state('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', contemporary_military_technologies_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('beaf4d32-bfd0-4d1f-a050-9c7600aec6d8', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, right_to_revolution_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, tyranny_prevention_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens believe their armed status deters government overreach and preserves liberty. They benefit from the interpretation that legitimizes their possession of military-grade arms. Their identity is often fused with this right, making exit (disarmament) unthinkable.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    organized, generational, identity_locked, national).

% Faces a constant threat of armed resistance, complicating law enforcement and national security. They bear the costs of potential conflict and the erosion of state monopoly on force. Their options are to enforce disarmament (risking conflict) or concede legitimacy to armed groups.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, biographical, constrained, national).

% Are caught in the crossfire of any actualized armed conflict between citizens and the state. They bear the direct costs of violence, displacement, and loss of life. They have no exit options once conflict begins.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Are tasked with interpreting and applying the Second Amendment. This reading constrains their ability to enact gun control measures, particularly those targeting military-style weapons, under threat of being labeled tyrannical. They navigate public opinion and legal challenges.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, legislators_and_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% While supporting broad gun rights, they may not fully endorse the insurrectionist premise, focusing instead on self-defense. They observe the debate and strategically align with or distance themselves from the insurrectionist reading based on political expediency.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, individual_right_advocates, observer,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectation that citizens possess the means to resist perceived tyranny, theoretically deterring government overreach and preserving a balance of power between the state and the populace.
% TRANSFER_FUNCTION: Transfers a portion of the state's monopoly on force and legitimacy to armed citizens, and transfers the risk of armed conflict from the state to civilians caught in potential uprisings.
% ABSENT_VOICES: Victims of gun violence and proponents of stricter gun control are often marginalized in discussions framed by this reading, as their concerns are dismissed as undermining a fundamental check on government power. They would argue for a state monopoly on force to ensure public safety.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the state's monopoly on force would be reasserted, leading to significant shifts in firearms policy, potentially disarming many citizens. The political landscape regarding civil-military relations would fundamentally alter, and the perceived legitimacy of armed citizen groups would collapse.
% FOUNDING_PROBLEM: The historical fear of tyrannical government and the need for citizens to retain the ultimate means of self-defense against state oppression, as articulated by some Enlightenment thinkers and early American revolutionaries.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including some constitutional scholars and firearms rights organizations, attest that the threat of government overreach remains live. Critics, including most mainstream political scientists and legal scholars, argue that the modern state's power and military capabilities render armed citizen resistance futile and dangerous, making the founding problem effectively dead or transformed beyond the scope of individual arms.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.65) because this reading imposes significant costs on the state's ability to maintain order and on civilians caught in potential conflicts. Suppression (0.7) is also high, as the state must actively suppress the actualization of armed resistance, while simultaneously being suppressed in its ability to regulate arms. The theater ratio (0.4) reflects that while the 'deterrent' function is often performative, the threat of armed conflict is real enough to influence policy and public discourse. Resistance (0.8) is high due to active opposition from both armed citizens against state control and from the state against armed groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of armed citizens, this is a vital 'rope' or even 'mountain' protecting liberty. From the state's perspective, it is a 'snare' that undermines its authority and endangers citizens. Civilians caught in conflict experience it as a 'snare' with no exit. The engine's classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens claiming deterrent legitimacy are beneficiaries (d=0.0-0.2) as this reading legitimizes their power. The state security apparatus and civilians in conflict zones are victims (d=0.8-1.0) as they bear the direct costs and risks. Legislators and the judiciary are agenda-setters (d=0.5-0.7) who must navigate the implications of this reading, facing constraints on their legislative power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tyranny_definition_ambiguity,
    'What constitutes ''tyrannical government'' sufficient to justify armed resistance, and who adjudicates this definition?',
    'A clear, widely accepted legal or philosophical framework for defining governmental tyranny that is independent of the armed groups themselves.',
    'If the definition is subjective or self-serving, the constraint functions as a snare, legitimizing private violence. If an objective standard exists, it could function as a highly conditional rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tyranny_definition_ambiguity, conceptual, 'Ambiguity in the trigger condition for armed resistance.').

omega_variable(
    efficacy_of_armed_resistance,
    'Is individual or collective armed resistance a genuinely effective deterrent or overthrow mechanism against a modern state''s military and intelligence capabilities?',
    'Empirical analysis of historical and contemporary armed insurrections against modern states, assessing their success rates and societal costs.',
    'If ineffective, the ''deterrent'' function is pure theater, increasing the constraint''s theater_ratio and extractiveness (from the costs of futile conflict). If effective, it supports the coordination function, lowering extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_armed_resistance, empirical, 'The actual effectiveness of armed resistance against a modern state.').

omega_variable(
    scope_of_protected_arms,
    'Does the ''right to bear arms'' under this reading extend to all military-grade weaponry (e.g., automatic weapons, explosives), or are there inherent limits based on destructive capacity or proportionality?',
    'Judicial rulings or legislative consensus establishing clear boundaries on the types of arms protected under this specific interpretation.',
    'If unlimited, the extractiveness and suppression on the state and civilians increase dramatically. If limited, the constraint''s scope and severity are reduced, potentially shifting it towards a more constrained tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_protected_arms, preference, 'The specific types of arms protected by this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1980, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(seco_tr_t2020, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t1980, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(seco_be_t1990, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(seco_be_t2020, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1980, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(seco_su_t1990, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(seco_su_t2020, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, state_monopoly_on_force).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
