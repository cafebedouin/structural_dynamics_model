% [RESOLVED MANDATROPHY] High extraction is definitional to the constraint's philosophical mandate, not a sign of decay or policy failure.
% ============================================================================
% CONSTRAINT STORY: self_surpassing
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_self_surpassing, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: self_surpassing
 * human_readable: The Rearing of the Superman (Übermensch)
 * domain: philosophical/social
 * * SUMMARY:
 * This constraint represents the imperative to overcome the "All-too-human"
 * state of the "Last Man" following the "Death of God." It functions as an
 * existential demand that requires the total transvaluation of existing
 * "slave" moral values into "master" values of power and creation. It is
 * claimed to be a "Rope" to a higher state of being, but its high demands
 * create significant perspectival gaps.
 * * KEY AGENTS:
 * - The Last Man: Subject (Powerless), finds happiness in static comfort and equality.
 * - The State ("New Idol"): Beneficiary (Institutional), seeks to replace God with a secular order.
 * - Zarathustra: Architect (Analytical), the prophet who defines the path between beast and Superman.
 * - The Higher Man: Victim (Powerful), the failed bridge-builder who suffers under modern nihilism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extractiveness: High (0.8). The rearing of the Superman extracts everything
% from the individual—comfort, safety, and traditional morality.
domain_priors:base_extractiveness(self_surpassing, 0.8).

% Suppression score: High (0.7). Traditional "Good and Just" values actively
% suppress the creative will that seeks to break old tables of values.
domain_priors:suppression_score(self_surpassing, 0.7).

% Theater ratio: Low (0.1). The demand is existential and functional, not performative.
domain_priors:theater_ratio(self_surpassing, 0.1).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(self_surpassing, extractiveness, 0.8).
narrative_ontology:constraint_metric(self_surpassing, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(self_surpassing, theater_ratio, 0.1).

% Constraint self-claim (what Zarathustra claims it to be)
narrative_ontology:constraint_claim(self_surpassing, tangled_rope).
narrative_ontology:human_readable(self_surpassing, "The Rearing of the Superman (Übermensch)").

% Binary flags
domain_priors:requires_active_enforcement(self_surpassing). % Requires active will-to-power.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(self_surpassing, posterity).
narrative_ontology:constraint_beneficiary(self_surpassing, the_superman).
narrative_ontology:constraint_victim(self_surpassing, the_last_man).
narrative_ontology:constraint_victim(self_surpassing, the_higher_man).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE LAST MAN (SUBJECT)
% To the "hop-flea" of the earth who wants only comfort, the demand for
% greatness is an incomprehensible, immutable feature of a world he rejects.
% It is a Mountain whose heights he has no desire to scale.
constraint_indexing:constraint_classification(self_surpassing, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE STATE / "NEW IDOL" (BENEFICIARY)
% The institutional powers that replace God see Zarathustra's project as a
% rival value system. They would view it as a Rope they could potentially
% co-opt to coordinate their own populace, stripping it of its radical
% individualism.
constraint_indexing:constraint_classification(self_surpassing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ZARATHUSTRA (ANALYTICAL OBSERVER)
% The analytical view must account for the high extraction (E=0.8), high
% suppression (S=0.7), a clear coordination function (beneficiary exists),
% asymmetric extraction (victim exists), and active enforcement. This is the
% canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(self_surpassing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE HIGHER MAN (VICTIM)
% For the Higher Men (artists, kings, thinkers), traditional morality is a
% Snare. It extracts their power through pity and resentment. The demand to
% overcome it without sufficient strength becomes another Snare of inadequacy.
constraint_indexing:constraint_classification(self_surpassing, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_surpassing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(self_surpassing, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(self_surpassing, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(self_surpassing, tangled_rope, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(self_surpassing, snare, context(agent_power(powerful), _, _, _)).

test(tangled_rope_detection) :-
    % The analytical view must be Tangled Rope due to high extraction + coordination function.
    constraint_indexing:constraint_classification(self_surpassing, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(self_surpassing, Score),
    Score >= 0.46.

:- end_tests(self_surpassing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The central tension of Zarathustra is the "type-shift" of constraints
 * based on the agent's degree of Will to Power. The original file's
 * classification of the analytical view as a 'Rope' was inconsistent with the
 * base extraction of 0.8. A pure Rope must have extraction <= 0.15.
 *
 * This version corrects the analytical classification to 'Tangled Rope', which
 * accurately reflects a system with a genuine coordination function (creating
 * the Superman) that also imposes immense costs and has clear victims (the
 * Last Man, the Higher Man).
 *
 * * PERSPECTIVAL GAP:
 * - The Last Man (powerless) sees a MOUNTAIN: the demand is incomprehensible.
 * - The Higher Man (powerful but trapped) sees a SNARE: a demand that extracts his potential.
 * - The State (institutional) sees a ROPE: a tool for social coordination it can co-opt.
 * - The Analyst (Zarathustra) sees a TANGLED ROPE: a dangerous but necessary path with both victims and beneficiaries.
 *
 * * MANDATROPHY ANALYSIS:
 * Mandatrophy is resolved because the high extraction is not a bug or a
 * sign of decay; it is the central, explicitly stated feature of the demand
 * for self-overcoming. The constraint is functioning as designed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_eternal_recurrence,
    'Is the "Eternal Recurrence" a physical Mountain (law of physics) or a Rope (psychological tool)?',
    'Empirical validation of cosmological models vs. analysis of its function in Nietzsche''s text.',
    'If Mountain, choice is an illusion. If Rope, it is the ultimate test of Yea-saying.',
    confidence_without_resolution(low)
).

omega_variable(
    omega_superman_feasibility,
    'Is the Superman a real biological possibility or a poetic phantom?',
    'Long-term observation of human evolutionary trends and transhumanist technological outcomes.',
    'If phantom, the entire project becomes a Snare of despair. If real, it is a valid goal.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_surpassing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The philosophical demand is modeled as timeless and stable across the interval.
% Extraction is consistently high, and theatricality is consistently low.
%
% Theater ratio over time:
narrative_ontology:measurement(self_surpassing_tr_t0, self_surpassing, theater_ratio, 0, 0.1).
narrative_ontology:measurement(self_surpassing_tr_t5, self_surpassing, theater_ratio, 5, 0.1).
narrative_ontology:measurement(self_surpassing_tr_t10, self_surpassing, theater_ratio, 10, 0.1).

% Extraction over time:
narrative_ontology:measurement(self_surpassing_ex_t0, self_surpassing, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(self_surpassing_ex_t5, self_surpassing, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(self_surpassing_ex_t10, self_surpassing, base_extractiveness, 10, 0.8).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The philosophical nature of this constraint makes it a meta-constraint on
% value systems. It could be considered an enforcement mechanism for a new
% table of values.
narrative_ontology:coordination_type(self_surpassing, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */