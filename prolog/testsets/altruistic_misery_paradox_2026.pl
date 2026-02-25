% ============================================================================
% CONSTRAINT STORY: altruistic_misery_paradox_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_altruistic_misery_paradox_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: altruistic_misery_paradox_2026
 *   human_readable: The Paradox of Altruistic Misery
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the 'Paradox of Altruistic Misery,' a social norm
 *   that pressures individuals to prioritize the happiness of others through
 *   self-sacrifice. While fostering community cohesion (a coordination
 *   function), this pressure can lead to burnout, resentment, and diminished
 *   well-being for the altruist (an extraction function). The constraint's
 *   structure is a classic example of a Tangled Rope, where a socially
 *   valuable behavior is maintained through mechanisms that create a distinct
 *   class of victims.
 *
 * KEY AGENTS:
 *   - Compulsive Altruists: Primary victims (powerless/trapped) — bear the emotional and material costs of the norm.
 *   - Recipients of Aid: Primary beneficiaries (powerful/arbitrage) — receive support and view the norm as a pure good.
 *   - The Social Group: Secondary beneficiary and enforcer (organized/mobile) — benefits from social cohesion and actively maintains the norm through praise and censure.
 *   - The Analytical Observer: Sees the full, mixed nature of the constraint (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(altruistic_misery_paradox_2026, 0.55).
domain_priors:suppression_score(altruistic_misery_paradox_2026, 0.65).
domain_priors:theater_ratio(altruistic_misery_paradox_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(altruistic_misery_paradox_2026, tangled_rope).
narrative_ontology:human_readable(altruistic_misery_paradox_2026, "The Paradox of Altruistic Misery").
narrative_ontology:topic_domain(altruistic_misery_paradox_2026, "social/psychological").

domain_priors:requires_active_enforcement(altruistic_misery_paradox_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(altruistic_misery_paradox_2026, recipients_of_aid).
narrative_ontology:constraint_beneficiary(altruistic_misery_paradox_2026, social_group_cohesion).
narrative_ontology:constraint_victim(altruistic_misery_paradox_2026, compulsive_altruists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMPULSIVE ALTRUIST (SNARE) — Trapped by internalized guilt and social expectations, the individual cannot exit the cycle of self-sacrifice without significant social or psychological cost. The constant extraction of their time, energy, and well-being feels like a trap. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.62. With suppression at 0.65, this is on the threshold of a Snare, reflecting the coercive nature of the social pressure.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RECIPIENT (ROPE) — From the perspective of someone receiving help, the altruist's actions are a pure coordination good. They benefit directly with minimal cost, experiencing the social norm as a reliable support system. As a beneficiary with arbitrage options, their effective extraction is negative. d≈0.15, f(d)≈-0.01, σ=0.8 → χ≈-0.004.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SOCIAL GROUP (ROPE) — The community as a whole perceives the norm of altruism as a highly effective coordination mechanism that fosters cohesion, trust, and mutual support. The costs borne by a few over-taxed individuals are seen as minor compared to the collective benefit. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.32. This low effective extraction classifies it as a Rope, a tool for collective well-being.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE BURNED-OUT ALTRUIST (PITON) — After years of self-sacrifice, the altruist's actions become performative. They go through the motions to maintain their social role, but the genuine desire or ability to help has been exhausted. The function (providing effective aid) has atrophied, but the theater remains. The high theater_ratio of 0.75 meets the Piton classification gate.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The psychologist or sociologist sees the full picture: a system that provides genuine coordination benefits (social cohesion) but achieves this through asymmetric extraction from a specific subgroup (the altruists). It requires active social enforcement to maintain. This dual nature is the hallmark of a Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(altruistic_misery_paradox_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(altruistic_misery_paradox_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(altruistic_misery_paradox_2026, TR),
    TR >= 0.70.

:- end_tests(altruistic_misery_paradox_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): Represents the significant, non-financial cost (time, emotional energy, opportunity cost) imposed on the altruist. Suppression (0.65): High, reflecting the power of social norms, guilt, and fear of being labeled 'selfish' which prevent individuals from easily opting out. Theater Ratio (0.75): High, as social pressure incentivizes performative acts of giving to gain approval, which can become more prevalent as genuine motivation wanes due to burnout.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The altruist, trapped by expectation, experiences a Snare. The recipient, benefiting from aid, sees a pure Rope. The wider community also sees a Rope, valuing the collective benefit of the norm over the individual cost. The burned-out individual experiences a Piton, where the function of helping is gone but the performance remains. Only the analytical observer perceives the complete structure as a Tangled Rope, acknowledging both the genuine coordination and the harmful extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the clear roles. 'Compulsive altruists' are victims with trapped exit, leading to a high 'd' value (~0.95) and a Snare classification. 'Recipients of aid' are beneficiaries with arbitrage exit, leading to a very low 'd' (~0.15) and a Rope classification. The 'Social Group' is a beneficiary but also self-constraining, leading to a moderate 'd' (~0.50) that still classifies as Rope due to the immense perceived coordination benefit. The analytical view uses the canonical 'd' for that power atom, revealing the Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that a social norm is not monolithically 'good' or 'bad'. Labeling it a pure Rope (the community view) ignores the victims. Labeling it a pure Snare (the victim's view) ignores the real coordination benefits it provides. The Tangled Rope classification from the analytical perspective correctly identifies the dual nature of the phenomenon, preventing the mislabeling of coercive social pressure as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    motivation_purity,
    'Is the altruist''s motivation primarily selfless concern for others, or a selfish desire for social approval and the ''warm glow'' of giving?',
    'Neuro-imaging studies correlating brain reward centers with altruistic acts under varying conditions of social visibility and internal vs. external pressure.',
    'If primarily selfish/performative, the constraint is more of a Snare from more perspectives, as it''s a system for extracting social status. If primarily selfless, it''s a more tragic Tangled Rope where a virtue is exploited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(motivation_purity, empirical, 'Distinguishing selfless motivation from selfish desire for social approval in altruism.').

omega_variable(
    pathology_threshold,
    'At what point does the cost to the altruist''s well-being (burnout, resentment, depression) outweigh the benefit to the recipient and the group?',
    'Longitudinal studies tracking mental and physical health outcomes of individuals with varying levels of altruistic behavior against the perceived benefits to their communities.',
    'Defines the boundary where the constraint flips from a beneficial Rope to a harmful Tangled Rope or Snare. This threshold is likely culturally and individually variable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pathology_threshold, conceptual, 'Defining the threshold between healthy altruism and pathological self-sacrifice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(altruistic_misery_paradox_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altr_tr_t0, altruistic_misery_paradox_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(altr_tr_t10, altruistic_misery_paradox_2026, theater_ratio, 10, 0.6).
narrative_ontology:measurement(altr_tr_t20, altruistic_misery_paradox_2026, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(altr_be_t0, altruistic_misery_paradox_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(altr_be_t10, altruistic_misery_paradox_2026, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(altr_be_t20, altruistic_misery_paradox_2026, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(altruistic_misery_paradox_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
