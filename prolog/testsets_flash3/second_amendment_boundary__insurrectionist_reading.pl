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
 *   human_readable: Second Amendment (Insurrectionist Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story analyzes the 'insurrectionist reading' of the
 *   Second Amendment, which posits that the right to bear arms, including
 *   military-grade weapons, is primarily for preserving the capacity for
 *   armed resistance against a tyrannical government. This reading expands
 *   the scope of protected arms and views state disarmament efforts as
 *   precursors to tyranny. It is one of several contested interpretations of
 *   the Second Amendment, with significant implications for firearms policy
 *   and the balance of power between citizens and the state.
 *
 * KEY AGENTS:
 *   - armed_citizens_claiming_deterrent_legitimacy: Primary beneficiary (organized/identity_locked) — benefits from perceived legitimacy and freedom from disarmament.
 *   - state_security_apparatus: Primary payer (institutional/constrained) — bears the cost of operating in an environment of potential armed challenge.
 *   - civilians_caught_in_hypothetical_armed_conflict: Primary victim (powerless/trapped) — bears the direct costs of any actual conflict.
 *   - government_officials: Agenda setter (institutional/constrained) — constrained in policy options by this reading.
 *   - gun_control_advocates: Excluded (organized/constrained) — their policy arguments are systematically undermined.
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
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, snare).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment (Insurrectionist Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'f83b6bca-f072-4279-ad35-f9917bf667e9').
narrative_ontology:cs_kernel_codification('f83b6bca-f072-4279-ad35-f9917bf667e9', fixed_text).
narrative_ontology:cs_authority_grounding('f83b6bca-f072-4279-ad35-f9917bf667e9', lineage).
narrative_ontology:cs_interpretation_layer_present('f83b6bca-f072-4279-ad35-f9917bf667e9').
narrative_ontology:cs_reading_relation('f83b6bca-f072-4279-ad35-f9917bf667e9', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('f83b6bca-f072-4279-ad35-f9917bf667e9', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('f83b6bca-f072-4279-ad35-f9917bf667e9', foundational, armed_populace_as_tyranny_deterrent).
narrative_ontology:cs_axiom_status(armed_populace_as_tyranny_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('f83b6bca-f072-4279-ad35-f9917bf667e9', armed_populace_as_tyranny_deterrent, deontological).
narrative_ontology:cs_axiom('f83b6bca-f072-4279-ad35-f9917bf667e9', secondary, military_grade_arms_are_protected).
narrative_ontology:cs_axiom_status(military_grade_arms_are_protected, holdable).
narrative_ontology:cs_axiom_grounding('f83b6bca-f072-4279-ad35-f9917bf667e9', military_grade_arms_are_protected, conventional).
narrative_ontology:cs_reference_frame('f83b6bca-f072-4279-ad35-f9917bf667e9', founding_era_insurrectionary_potential).
narrative_ontology:cs_drift_state('f83b6bca-f072-4279-ad35-f9917bf667e9', contemporary_state_power_asymmetry, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f83b6bca-f072-4279-ad35-f9917bf667e9', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_caught_in_hypothetical_armed_conflict).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, tyranny_prevention_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, popular_sovereignty_through_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens believe their right to bear arms, including military-grade weapons, is essential for deterring or overthrowing a tyrannical government. They benefit from the perceived legitimacy of this stance and the freedom from state disarmament efforts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    organized, generational, identity_locked, national).

% The state's law enforcement and military bodies are positioned as potential targets of armed resistance under this reading. They bear the cost of operating in an environment where their authority can be challenged by armed citizens, leading to increased risk and operational complexity.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, immediate, constrained, national).

% Ordinary citizens who are not part of the armed resistance or state apparatus would be victims of any actual armed conflict, bearing the costs of violence, instability, and loss of life. They have no exit options from such a scenario.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_caught_in_hypothetical_armed_conflict, payer,
    powerless, immediate, trapped, local).

% Officials responsible for public safety and governance face a constant challenge in balancing constitutional rights with the need for order. Under this reading, their efforts to regulate firearms are often framed as tyrannical precursors, limiting their policy options.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, government_officials, agenda_setter,
    institutional, biographical, constrained, national).

% Advocates for stricter firearms regulation find their arguments systematically undermined by this reading, which prioritizes armed resistance over public safety concerns. Their policy proposals are often dismissed as infringing on fundamental rights.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_control_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the actions of armed citizens by providing a shared ideological framework and justification for maintaining armed capacity, ostensibly to deter or resist government overreach.
% TRANSFER_FUNCTION: Transfers the burden of potential armed conflict and the erosion of state authority from armed citizens to the state security apparatus and the general civilian population, in exchange for the perceived deterrent effect against tyranny.
% ABSENT_VOICES: Civilians who would be caught in hypothetical armed conflict, and those who prioritize collective safety over individual armed resistance, are often marginalized in the discourse shaped by this reading. Their concerns about widespread violence and instability are not adequately addressed.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal and political landscape around firearms would fundamentally shift. State disarmament efforts would gain legitimacy, the perceived threat of armed citizen resistance would diminish, and the balance of power between citizens and the state would be reconfigured, leading to significant societal reorganization.
% FOUNDING_PROBLEM: The founding problem this reading addresses is the potential for government tyranny and the need for citizens to retain the ultimate means of self-defense against an oppressive state.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including various citizen militias and some political theorists, attest that the threat of tyranny is ever-present and the founding problem remains live. However, external corroboration from independent political scientists or historians, outside of groups benefiting from this interpretation, is contested and often absent, with many arguing that modern state power renders such resistance futile or counterproductive.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.85) is high because this reading imposes significant costs on the state and general populace (potential for armed conflict, erosion of state authority) for the benefit of a specific group's perceived right to resist. Suppression (0.9) is also high, as any attempt by the state to regulate or disarm is met with strong ideological and sometimes physical resistance, framed as a tyrannical act. The theater ratio is low (0.1) because the threat of armed resistance, while often rhetorical, is taken seriously by both proponents and opponents, leading to real-world policy and security implications. Accessibility collapse (0.7) is moderate-high, as the premise of armed resistance limits the perceived legitimacy of alternative, non-violent means of political change. Resistance (0.8) is high, reflecting the active and often confrontational opposition to any measures seen as infringing on this right.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of armed citizens, this is a fundamental right protecting liberty, with minimal extraction. From the perspective of the state and civilians, it is a highly extractive and suppressive constraint that imposes significant risks and costs on society. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens are beneficiaries (d=0.0-0.1) as the constraint legitimizes their armed status and deters state action against them. The state security apparatus and civilians are targets (d=0.9-1.0) as they bear the direct and indirect costs of potential conflict and eroded state authority. Government officials are also targets (d=0.7-0.8) as their policy options are constrained. Gun control advocates are excluded, meaning their directionality is not directly computed but their interests are clearly opposed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by highlighting the active, ideological contestation. It is not a Piton, as there are clear beneficiaries actively maintaining it and identifiable victims bearing costs. It is not a Rope, as the benefits are highly asymmetric and coercive. The high extractiveness and suppression, coupled with clear victims, point towards a Snare, where the coordination story (deterrence of tyranny) serves as cover for the imposition of costs on others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_insurrection,
    'Is armed insurrection against the state a legitimate constitutional mechanism, or an extra-constitutional act of rebellion?',
    'Historical precedent, legal scholarship on the limits of popular sovereignty, and the outcomes of actual attempts at armed resistance.',
    'If legitimate, the constraint''s claimed purpose is validated, potentially lowering its perceived extractiveness for beneficiaries. If illegitimate, the constraint''s foundational premise is undermined, reclassifying it as pure extraction (Snare) for all parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_insurrection, conceptual, 'The fundamental question of whether the ''right to resist'' is a constitutional right or a revolutionary act.').

omega_variable(
    efficacy_of_armed_resistance,
    'In the modern era, can armed citizen resistance realistically deter or overthrow a tyrannical government with advanced military capabilities?',
    'Empirical analysis of historical and contemporary conflicts between state forces and non-state actors, and military-strategic assessments.',
    'If ineffective, the coordination function (deterrence) is theatrical, increasing the theater_ratio and potentially reclassifying to Piton or a more severe Snare. If effective, the constraint''s justification gains empirical weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_of_armed_resistance, empirical, 'The practical viability of armed citizen resistance against a modern state.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, state power) or internalized (ideological commitment, identity fusion)?',
    'Post-exit suppression trajectory: if ideological commitment to armed resistance persists after legal barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making it harder to disarm or regulate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for armed citizens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(seco_be_t1985, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(seco_su_t1985, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1985, 0.78).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, firearms_regulation_policy).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, state_monopoly_on_force).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_boundary' kernel. Each reading has a different ε value and structural profile, necessitating separate constraint stories. This reading focuses on the insurrectionist purpose, which significantly increases extractiveness and suppression compared to other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
