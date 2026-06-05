% ============================================================================
% CONSTRAINT STORY: belief_argument_conclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_belief_argument_conclusion, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: belief_argument_conclusion
 *   human_readable: The Futility of Arguing Against Instinctive Belief
 *   domain: social/philosophical
 *
 * SUMMARY:
 *   This constraint models the futility of using rational argumentation to
 *   change beliefs rooted in instinct, emotion, or group identity. Cognitive
 *   mechanisms like confirmation bias, belief perseverance, and motivated
 *   reasoning act as powerful enforcement, making the believer's worldview
 *   resistant to external evidence. The act of arguing becomes a social
 *   dynamic with different functions and costs depending on one's structural
 *   position relative to the belief in question.
 *
 * KEY AGENTS:
 *   - Rational Arguer: Primary victim (powerless/trapped) — invests time and energy into a futile process.
 *   - Instinctive Believer: Primary beneficiary (moderate/mobile) — their cognitive comfort and identity are protected from challenge.
 *   - Social Group: Institutional beneficiary (organized/arbitrage) — uses the belief's resilience to maintain group cohesion and identity.
 *   - Public Debater: Performative actor (powerful/mobile) — uses the argument as a stage for signaling rather than persuasion.
 *   - Analytical Observer: Sees the full, mixed structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(belief_argument_conclusion, 0.65).
domain_priors:suppression_score(belief_argument_conclusion, 0.8).
domain_priors:theater_ratio(belief_argument_conclusion, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(belief_argument_conclusion, extractiveness, 0.65).
narrative_ontology:constraint_metric(belief_argument_conclusion, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(belief_argument_conclusion, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(belief_argument_conclusion, tangled_rope).
narrative_ontology:human_readable(belief_argument_conclusion, "The Futility of Arguing Against Instinctive Belief").
narrative_ontology:topic_domain(belief_argument_conclusion, "social/philosophical").

domain_priors:requires_active_enforcement(belief_argument_conclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(belief_argument_conclusion, instinct_holder).
narrative_ontology:constraint_beneficiary(belief_argument_conclusion, social_group).
narrative_ontology:constraint_victim(belief_argument_conclusion, rational_arguer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONAL ARGUER (SNARE) — Trapped in a futile exchange, their time, energy, and reason are extracted with no return. The cognitive mechanisms of the believer create a high-suppression environment where logical alternatives are inaccessible. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.74. This high effective extraction classifies the constraint as a Snare.
constraint_indexing:constraint_classification(belief_argument_conclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INSTINCTIVE BELIEVER (MOUNTAIN) — Experiences their belief as an immutable, natural law. The argument is not a threat but a confirmation of this 'law's' strength. The constraint feels unchangeable and fundamental. This is a 'false summit' perspective; the engine will flag it because the base properties (ε=0.65) violate Mountain criteria (ε≤0.25).
constraint_indexing:constraint_classification(belief_argument_conclusion, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: SOCIAL GROUP (ROPE) — For the group sharing the belief, the futility of outside arguments is a pure coordination mechanism. It reinforces in-group identity, strengthens social bonds, and prevents fragmentation. d≈0.15, f(d)≈-0.01, σ=0.9 → χ≈-0.006. The negative extraction indicates a net benefit.
constraint_indexing:constraint_classification(belief_argument_conclusion, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PUBLIC DEBATER (PITON) — Engages in the argument as a performance. The original function (persuasion) has atrophied, replaced by the theatrical function of signaling intelligence, tribal affiliation, or moral conviction. The high theater_ratio (0.75) satisfies the Piton gate (≥0.70).
constraint_indexing:constraint_classification(belief_argument_conclusion, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full structure: a genuine coordination function (social cohesion for the believers) combined with asymmetric extraction (wasted effort from the arguers). This dual nature is the hallmark of a Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(belief_argument_conclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(belief_argument_conclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(belief_argument_conclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(belief_argument_conclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(belief_argument_conclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(belief_argument_conclusion, TR),
    TR >= 0.70.

:- end_tests(belief_argument_conclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65): High. Represents the significant, unrecoverable cost in time, cognitive effort, and emotional energy expended by the arguer for zero persuasive return. Suppression (0.80): Very High. The internal cognitive biases of the believer strongly suppress the alternative outcome (i.e., changing one's mind). It is extremely difficult to bypass these defenses with logic alone. Theater Ratio (0.75): High. As persuasion fails, the argument increasingly becomes a performative ritual of identity defense (for the believer) and intellectual signaling (for the arguer), satisfying the Piton gate.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The Arguer experiences a Snare, a trap that extracts their resources. The Believer experiences a Mountain, perceiving their belief as an unchangeable law of nature. Their Social Group sees a Rope, a tool for coordination and unity. The Public Debater sees a Piton, a hollowed-out ritual. The Analytical Observer synthesizes these views, identifying a Tangled Rope: a structure that simultaneously coordinates one group while extracting from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the Instinctive Believer and their Social Group, as the constraint shields their worldview and reinforces social bonds. This leads to low 'd' values and low/negative effective extraction (χ). The victim is the Rational Arguer, whose efforts are consumed by the constraint, leading to a high 'd' value and a high χ that classifies their experience as a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a strong resolver of mandatrophy. It demonstrates that a single phenomenon is not monolithically a 'Snare' or a 'Rope'. The classification is an indexical property of the observer's relationship to the constraint. The system correctly identifies that the same base metrics can produce Snare, Rope, Mountain, Piton, and Tangled Rope classifications simultaneously, depending on the (P,T,E,S) tuple, without contradiction. The mandatrophy is resolved by moving from a single 'correct' classification to a presheaf of classifications over different observer sites.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    belief_malleability,
    'Are these ''instinctive'' beliefs truly immutable (Mountain-like), or are they deeply conditioned but ultimately malleable under specific circumstances?',
    'Longitudinal studies on belief change; effectiveness trials of de-radicalization programs or therapeutic interventions like Street Epistemology.',
    'If beliefs are found to be malleable, the constraint is confirmed as a Tangled Rope/Snare. If they are truly immutable, it would lend credence to the Believer''s Mountain perspective, suggesting a biological or cognitive limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(belief_malleability, empirical, 'Whether instinctive beliefs are immutable or merely deeply conditioned.').

omega_variable(
    arguer_secondary_gain,
    'Does the ''rational arguer'' receive non-obvious benefits, such as social status, self-satisfaction, or reinforcement of their own identity as a rational person?',
    'Psychological analysis of arguers'' motivations and the social rewards for performing rationality.',
    'If significant secondary gains exist, the ''extraction'' is lower than modeled, potentially shifting the Arguer''s perspective from Snare to Tangled Rope. If gains are negligible, the Snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arguer_secondary_gain, conceptual, 'Whether the arguer receives secondary gains that offset the futility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(belief_argument_conclusion, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beli_tr_t0, belief_argument_conclusion, theater_ratio, 0, 0.6).
narrative_ontology:measurement(beli_tr_t10, belief_argument_conclusion, theater_ratio, 10, 0.68).
narrative_ontology:measurement(beli_tr_t20, belief_argument_conclusion, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(beli_be_t0, belief_argument_conclusion, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(beli_be_t10, belief_argument_conclusion, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(beli_be_t20, belief_argument_conclusion, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(belief_argument_conclusion, enforcement_mechanism).
narrative_ontology:affects_constraint(belief_argument_conclusion, political_polarization).
narrative_ontology:affects_constraint(belief_argument_conclusion, epistemic_closure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
