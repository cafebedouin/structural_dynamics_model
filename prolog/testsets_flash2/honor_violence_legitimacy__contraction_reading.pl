% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the historical process by which the concept of
 *   'honor' itself underwent a fundamental redefinition, structurally
 *   excluding violence (like dueling) as a legitimate means of its defense.
 *   This 'contraction reading' posits that dueling became unthinkable not
 *   primarily due to external costs or legal prohibitions, but because the
 *   very conceptual space of honor contracted to exclude it. It is claimed as
 *   a Mountain because the redefinition became an unchangeable feature of the
 *   social landscape, persisting regardless of individual enforcement. The
 *   metrics reflect a low and decreasing extractiveness and suppression, as
 *   the constraint became internalized and self-enforcing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.15).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.05).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '6a9de6fa-2465-4bb5-9e21-94aba971417e').
narrative_ontology:cs_kernel_codification('6a9de6fa-2465-4bb5-9e21-94aba971417e', implicit).
narrative_ontology:cs_authority_grounding('6a9de6fa-2465-4bb5-9e21-94aba971417e', practice).
narrative_ontology:cs_interpretation_layer_present('6a9de6fa-2465-4bb5-9e21-94aba971417e').
narrative_ontology:cs_reading_relation('6a9de6fa-2465-4bb5-9e21-94aba971417e', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('6a9de6fa-2465-4bb5-9e21-94aba971417e', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('6a9de6fa-2465-4bb5-9e21-94aba971417e', foundational, honor_is_non_violent_reputation).
narrative_ontology:cs_axiom_status(honor_is_non_violent_reputation, holdable).
narrative_ontology:cs_axiom_grounding('6a9de6fa-2465-4bb5-9e21-94aba971417e', honor_is_non_violent_reputation, conventional).
narrative_ontology:cs_axiom('6a9de6fa-2465-4bb5-9e21-94aba971417e', secondary, private_violence_is_dishonorable).
narrative_ontology:cs_axiom_status(private_violence_is_dishonorable, holdable).
narrative_ontology:cs_axiom_grounding('6a9de6fa-2465-4bb5-9e21-94aba971417e', private_violence_is_dishonorable, deontological).
narrative_ontology:cs_reference_frame('6a9de6fa-2465-4bb5-9e21-94aba971417e', honor_as_non_violent_civic_virtue).
narrative_ontology:cs_drift_state('6a9de6fa-2465-4bb5-9e21-94aba971417e', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6a9de6fa-2465-4bb5-9e21-94aba971417e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, civil_society).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, former_duelists).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, non_violence_as_honor).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reduction of private violence and the redefinition of honor towards civic virtues. Actively promotes and reinforces the new understanding of honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, civil_society, beneficiary,
    organized, generational, analytical, national).

% Benefits from the increased legitimacy of its monopoly on violence and the reduced need to prosecute dueling. Its legal framework aligns with the new honor code, reinforcing it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Individuals who previously saw dueling as a legitimate means of defending honor. They now face social ostracization or legal consequences for engaging in duels, as their understanding of honor is no longer recognized. Their identity is tied to a superseded code.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, former_duelists, payer,
    moderate, biographical, identity_locked, local).

% Academics and intellectuals who study the evolution of honor codes and their relationship to violence. They analyze the conceptual shift and its societal implications.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social expectations around honor, shifting the definition away from violent confrontation towards non-violent means of reputation management and conflict resolution, thereby reducing social friction and private violence.
% TRANSFER_FUNCTION: Transfers the social cost of private violence (deaths, injuries, feuds) from individuals and society to a conceptual space where such violence is no longer considered honorable, effectively 'transferring' the legitimacy of violence to the state.
% ABSENT_VOICES: Those who clung to the older, more violent honor code were gradually marginalized and silenced by the evolving social consensus and legal enforcement. Their voices are absent from the dominant narrative of honor's evolution.
% DISAPPEARANCE_RATIONALE: If the redefinition of honor to exclude violence vanished overnight, it would not revert society to a dueling culture. The conceptual shift is deeply embedded in social norms and legal structures; its 'disappearance' would mean a fundamental change in how society understands itself, rather than a return to a prior state.
% FOUNDING_PROBLEM: The problem of unchecked private violence and the instability it introduced into social and political life, where personal honor could demand lethal confrontation.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal codes, and philosophical treatises from the period corroborate the widespread societal concern over dueling and private violence. Contemporary historians and sociologists attest that the problem of dueling as a legitimate social practice is dead, having been superseded by new norms and state authority.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low and decreases over time because the constraint primarily redefines a conceptual space, rather than imposing direct costs. The 'cost' is borne by those whose identity was tied to the old honor code, but this is a conceptual rather than material extraction. Suppression is also low because the constraint's persistence relies on internalized norms and conceptual shifts, not active coercion. Accessibility collapse is high (0.9) because the redefinition made dueling conceptually impossible as an honorable act. Resistance is low (0.05) because the shift was largely accepted and internalized by society over time. The decreasing values reflect the deepening entrenchment of the new honor code.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil society and the state, this redefinition is a positive evolution towards a more civilized order. From the perspective of former duelists, it represents a loss of a legitimate means of self-defense and a challenge to their identity. The engine's classification will reflect this divergence, with beneficiaries experiencing a Mountain-like constraint and payers experiencing a more Snare-like or Tangled Rope-like constraint due to their identity lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society and the state legal apparatus are beneficiaries, as they gain from reduced violence and increased social order. Former duelists are payers, as their identity and social standing are challenged by the new definition of honor, leading to an 'identity_locked' exit option where their self-concept is tied to a superseded code. Honor theorists are observers, analyzing the conceptual shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reducing private violence by redefining honor) is not only resolved but has become a foundational aspect of modern social order. The 'founding_problem_status' is 'dead' because dueling is no longer a live social problem, yet the constraint (the redefinition of honor) persists as a fundamental social norm. This prevents mislabeling it as a Snare, as its persistence is not due to active extraction but to its deep conceptual embedding. The low extractiveness and suppression, combined with high accessibility collapse, indicate a genuine conceptual shift rather than an extractive cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_material_causation,
    'To what extent was the decline of dueling primarily driven by this conceptual redefinition of honor, versus material factors like legal prohibitions or economic costs?',
    'Comparative historical analysis across different societies with varying legal and economic contexts but similar conceptual shifts in honor, or counterfactual historical modeling.',
    'If material factors were dominant, this constraint''s ''mountain'' classification might be overstated, and it might be reclassified as a Rope or Tangled Rope that coordinated responses to external costs. If conceptual redefinition was primary, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_material_causation, empirical, 'Distinguishing conceptual shifts from material causes in historical change.').

omega_variable(
    identity_lock_internalization,
    'For ''former_duelists'', is the ''identity_locked'' exit option a result of internalized conceptual shift or external social pressure?',
    'Analysis of personal memoirs, diaries, and cultural artifacts from the period to gauge individual psychological responses versus overt social sanctions.',
    'If primarily internalized, the constraint''s ''mountain'' aspect is stronger, as the redefinition became part of individual self-concept. If primarily external, the ''suppression'' metric might be understated, and the constraint might lean more towards a Snare for those individuals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Structural vs. internalized suppression mechanism for identity-locked agents.').

omega_variable(
    framing_underdetermination_honor_decline,
    'Is the ''contraction_reading'' the most defensible framing for the decline of dueling, or do the ''drop_reading'' or ''composite_reading'' offer a more complete or accurate account?',
    'Further historical and sociological research, including analysis of the relative explanatory power of each reading''s causal mechanisms and their empirical support. The choice depends on which set of signals (conceptual shifts vs. external costs) is prioritized.',
    'If the ''drop_reading'' were adopted, the constraint would likely be classified as a Rope or Tangled Rope, with higher extractiveness (from legal/economic costs) and suppression. If the ''composite_reading'' were adopted, the constraint would likely be a more complex Tangled Rope, reflecting both conceptual and material dynamics. This ''contraction_reading'' emphasizes the conceptual shift, leading to a Mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_honor_decline, conceptual, 'Alternative framings for the decline of dueling and their classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.25).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.08).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.06).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.04).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_violence_legitimacy' kernel. The 'contraction_reading' focuses on the redefinition of honor itself, while the 'drop_reading' emphasizes external costs, and the 'composite_reading' combines both. Each represents a distinct structural claim about the decline of dueling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
