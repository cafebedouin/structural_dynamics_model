% ============================================================================
% CONSTRAINT STORY: adversarial_truth_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adversarial_truth_decay, []).

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
 *   constraint_id: adversarial_truth_decay
 *   human_readable: The Epistemic Siege
 *   domain: social/technological/political
 *
 * SUMMARY:
 *   The 'Epistemic Siege' describes a structural reality where the cost to
 *   generate high-fidelity, synthetic misinformation has fallen below the
 *   cost to verify information. This fundamental economic asymmetry creates a
 *   powerful new vector for social manipulation, eroding the shared factual
 *   basis required for functional democracies and social trust. The
 *   constraint is not the AI technology itself, but the cost imbalance it
 *   creates, which can be weaponized at scale.
 *
 * KEY AGENTS:
 *   - Disinformation Creators: Primary beneficiaries (organized/arbitrage) - Exploit the low cost to generate influence.
 *   - General Public: Primary victims (powerless/trapped) - Bear the cognitive and social costs of a polluted information environment.
 *   - Social Media Platforms: Institutional actors (institutional/arbitrage) - Maintain a performative moderation system while benefiting from engagement.
 *   - Fact-Checkers & Journalists: Secondary victims (moderate/constrained) - Face unsustainable costs in their mission to verify truth.
 *   - AI Tool Creators: Institutional beneficiaries (institutional/arbitrage) - Frame the crisis as a temporary, solvable problem (a scaffold).
 *   - Democratic Institutions: Abstract victim (powerless/trapped) - The epistemic commons on which they depend is degraded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adversarial_truth_decay, 0.75).
domain_priors:suppression_score(adversarial_truth_decay, 0.8).
domain_priors:theater_ratio(adversarial_truth_decay, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adversarial_truth_decay, extractiveness, 0.75).
narrative_ontology:constraint_metric(adversarial_truth_decay, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(adversarial_truth_decay, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adversarial_truth_decay, tangled_rope).
narrative_ontology:human_readable(adversarial_truth_decay, "The Epistemic Siege").
narrative_ontology:topic_domain(adversarial_truth_decay, "social/technological/political").

domain_priors:requires_active_enforcement(adversarial_truth_decay).
narrative_ontology:has_sunset_clause(adversarial_truth_decay).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adversarial_truth_decay, disinformation_creators).
narrative_ontology:constraint_beneficiary(adversarial_truth_decay, platform_shareholders).
narrative_ontology:constraint_victim(adversarial_truth_decay, general_public).
narrative_ontology:constraint_victim(adversarial_truth_decay, democratic_institutions).
narrative_ontology:constraint_victim(adversarial_truth_decay, professional_journalists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PUBLIC (SNARE) — Trapped in an information ecosystem where the cost of verification is offloaded onto them. The sheer volume of high-fidelity falsehoods acts as a coercive force, eroding trust and the ability to make informed decisions. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.28 (capped at 1.0). This is a maximal extraction scenario.
constraint_indexing:constraint_classification(adversarial_truth_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISINFORMATION CREATOR (ROPE) — For malicious actors, generative AI is a pure coordination tool. It dramatically lowers the cost of influence operations, enabling them to achieve political or financial goals with unprecedented efficiency. They experience no extraction. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01. Negative effective extraction signifies a net subsidy.
constraint_indexing:constraint_classification(adversarial_truth_decay, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE FACT-CHECKER (TANGLED ROPE) — Experiences the dual nature of the technology. AI tools can aid in research (coordination), but the firehose of AI-generated falsehoods makes their primary job nearly impossible (extraction). They are constrained to operate within this hostile environment. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.86. This high chi value reflects the severe extractive pressure on their work.
constraint_indexing:constraint_classification(adversarial_truth_decay, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE AI DEVELOPER (SCAFFOLD) — Views the current epistemic crisis as a temporary, transitional phase. They argue that better detection tools and new social norms will eventually emerge, creating a 'sunset' for the current problem. This perspective frames the technology as a scaffold for a new, more advanced information infrastructure. The `has_sunset_clause` is based on this claimed technological and social evolution.
constraint_indexing:constraint_classification(adversarial_truth_decay, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE PLATFORM (PITON) — The platform's content moderation and AI detection systems are largely performative. Given the cost asymmetry, they cannot possibly vet all content. Their efforts persist due to regulatory pressure and PR concerns, not because they solve the underlying problem. The high theater_ratio (0.75) triggers the Piton classification, reflecting a system whose primary function (ensuring information quality) has atrophied.
constraint_indexing:constraint_classification(adversarial_truth_decay, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE DETERMINIST (MOUNTAIN) — Argues that major technological shifts (like the printing press) inevitably cause periods of epistemic chaos. This view naturalizes the crisis as an unchangeable law of technological progress. The engine will identify this as a false summit, as the high ε, high suppression, and active enforcement are hallmarks of a contingent, artificial system, not a natural law.
constraint_indexing:constraint_classification(adversarial_truth_decay, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adversarial_truth_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adversarial_truth_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_truth_decay, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adversarial_truth_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(adversarial_truth_decay, TR),
    TR >= 0.70.

:- end_tests(adversarial_truth_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75) is very high, representing the massive offloading of verification costs onto society and individuals. Suppression (0.80) is also very high; the sheer volume of falsehoods effectively suppresses truth through noise, a 'firehose of falsehood' strategy that makes finding reliable information exceptionally difficult. Theater Ratio (0.75) is high because platform-level interventions (content moderation, AI detectors) are largely performative; they address symptoms at the margins without altering the core cost asymmetry, thus creating an illusion of control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a diagnostic exemplar, producing all six classifications from a single set of base properties. For the powerless public, it's a Snare. For the malicious creator, it's a Rope. For the overwhelmed fact-checker, it's a Tangled Rope. For the optimistic AI developer, it's a Scaffold. For the platform maintaining appearances, it's a Piton. For the determinist who naturalizes the chaos, it's a Mountain. The gap reveals that the 'nature' of the constraint is entirely dependent on one's structural relationship to the underlying cost asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (disinformation creators) have arbitrage exit and low structural costs, leading to a negative chi (Rope). Primary victims (the public) are trapped with no exit, leading to a maximal chi (Snare). Constrained actors (journalists) face high costs but have some agency, resulting in a high-but-not-maximal chi (Tangled Rope). The classification for institutional actors like platforms and AI developers is determined by other gates: the high theater_ratio for platforms makes it a Piton, while the claimed sunset clause for developers makes it a Scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating that a single, severe constraint (ε=0.75) is not monolithically a 'Snare'. Its classification is a function of the observer's index. The analytical error is to pick one perspective (usually the victim's Snare or the beneficiary's Rope) and declare it the 'true' type. The Deferential Realism model shows that all six perspectives are structurally valid readings of the same underlying reality. The complete description is the full set of indexed classifications, not any single one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_scaling_parity,
    'Can AI-driven verification and detection tools ever achieve cost and scale parity with AI-driven generation tools?',
    'Economic and performance analysis of red-teaming vs. blue-teaming AI models; tracking the marginal cost of detection vs. generation over time.',
    'If parity is possible, the constraint could evolve into a Rope (an arms race with symmetric costs). If not, it remains a Snare for the public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_scaling_parity, empirical, 'Whether detection can ever scale as cheaply as generation').

omega_variable(
    cognitive_adaptation_rate,
    'Will human populations develop cognitive ''antibodies'' and new media literacy heuristics faster than generative models can adapt to circumvent them?',
    'Longitudinal studies of media consumption habits, belief formation, and susceptibility to misinformation in high-exposure populations.',
    'Rapid adaptation would lower the effective extractiveness (ε). Slow or no adaptation means ε remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_adaptation_rate, empirical, 'The rate of human cognitive adaptation to AI misinformation').

omega_variable(
    regulatory_effectiveness,
    'What level of platform regulation (e.g., watermarking, liability shifts) can effectively mitigate harm without causing unacceptable censorship or chilling effects?',
    'Comparative policy analysis of different regulatory regimes implemented globally.',
    'Effective regulation could increase suppression for malicious actors and lower it for the public, fundamentally altering the constraint''s structure. Ineffective regulation could increase theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness, preference, 'The impact and trade-offs of different regulatory interventions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adversarial_truth_decay, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adve_tr_t0, adversarial_truth_decay, theater_ratio, 0, 0.4).
narrative_ontology:measurement(adve_tr_t5, adversarial_truth_decay, theater_ratio, 5, 0.6).
narrative_ontology:measurement(adve_tr_t10, adversarial_truth_decay, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(adve_be_t0, adversarial_truth_decay, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(adve_be_t5, adversarial_truth_decay, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(adve_be_t10, adversarial_truth_decay, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adversarial_truth_decay, information_standard).
narrative_ontology:affects_constraint(adversarial_truth_decay, electoral_integrity).
narrative_ontology:affects_constraint(adversarial_truth_decay, public_health_compliance).
narrative_ontology:affects_constraint(adversarial_truth_decay, financial_market_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
