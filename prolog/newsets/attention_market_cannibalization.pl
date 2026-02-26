% ============================================================================
% CONSTRAINT STORY: attention_market_cannibalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_market_cannibalization, []).

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
 *   constraint_id: attention_market_cannibalization
 *   human_readable: The Cognitive Exhaustion Loop
 *   domain: economic/psychological/technological
 *
 * SUMMARY:
 *   The Cognitive Exhaustion Loop describes a systemic process where digital
 *   platforms, competing in a finite attention market, progressively
 *   'cannibalize' the cognitive resources of their users. This is driven by
 *   business models that equate engagement with revenue. The result is a
 *   negative feedback loop: as users become more cognitively depleted, their
 *   ability to self-regulate diminishes, making them more susceptible to the
 *   platforms' engagement tactics, thus deepening the exhaustion. This
 *   constraint story models the structural reality of this loop, which
 *   manifests as all six constraint types depending on the observer's
 *   position.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — their finite attention is the resource being extracted.
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — design and control the system to maximize engagement and revenue.
 *   - Advertisers: Secondary beneficiaries (organized/mobile) — purchase access to the extracted attention.
 *   - Digital Wellness Advocates: Organized resistance (organized/constrained) — attempt to create exit ramps through regulation and ethical design advocacy.
 *   - Legacy Media Institutions: Constrained participants (institutional/constrained) — forced to adopt the system's extractive logic to survive, degrading their original function.
 *   - Analytical Observer: The systemic view (analytical/analytical) — recognizes both the coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_market_cannibalization, 0.65).
domain_priors:suppression_score(attention_market_cannibalization, 0.75).
domain_priors:theater_ratio(attention_market_cannibalization, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_market_cannibalization, extractiveness, 0.65).
narrative_ontology:constraint_metric(attention_market_cannibalization, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(attention_market_cannibalization, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_market_cannibalization, tangled_rope).
narrative_ontology:human_readable(attention_market_cannibalization, "The Cognitive Exhaustion Loop").
narrative_ontology:topic_domain(attention_market_cannibalization, "economic/psychological/technological").

domain_priors:requires_active_enforcement(attention_market_cannibalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_market_cannibalization, platform_operators).
narrative_ontology:constraint_beneficiary(attention_market_cannibalization, advertisers).
narrative_ontology:constraint_victim(attention_market_cannibalization, individual_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Trapped by network effects and addictive design. Experiences the system as pure extraction of cognitive resources, leading to burnout and diminished agency. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11.
constraint_indexing:constraint_classification(attention_market_cannibalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM OPERATOR (ROPE) — Experiences the system as a pure coordination mechanism, connecting users, content creators, and advertisers. Extraction is framed as the price of service. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(attention_market_cannibalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function and the severe, asymmetric extraction of attentional resources. This is the canonical classification. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(attention_market_cannibalization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL WELLNESS ADVOCATE (SCAFFOLD) — Views the current system as a temporary, harmful state. Works to build alternatives through regulation and ethical design, creating a sunset clause for the extractive model. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(attention_market_cannibalization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY MEDIA INSTITUTION (PITON) — Forced to participate in the attention market, adopting clickbait tactics that degrade its core mission. Its engagement strategies are largely performative, chasing metrics while its original function atrophies. The high theater_ratio (0.75) triggers the Piton classification.
constraint_indexing:constraint_classification(attention_market_cannibalization, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LIBERTARIAN TECHNOLOGIST (MOUNTAIN) — Frames the attention economy as an immutable, emergent law of free information exchange. Any attempt at regulation is seen as futile. The engine will flag this as a 'false summit' as the base properties (high ε, high suppression) contradict a Mountain classification.
constraint_indexing:constraint_classification(attention_market_cannibalization, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_market_cannibalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_market_cannibalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_market_cannibalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_market_cannibalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_market_cannibalization, TR),
    TR >= 0.70.

:- end_tests(attention_market_cannibalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Represents the quantified cost of lost productivity, increased mental health burdens (anxiety, depression, ADHD-like symptoms), and degraded decision-making capacity imposed on users. Suppression (0.75): High. Network effects, social pressures, and intentionally addictive design patterns (e.g., infinite scroll, variable rewards) make opting out extremely costly. Theater Ratio (0.75): High. Reflects the rise of performative 'digital well-being' features (e.g., screen time reports) that signal concern without altering the core extractive business model. These features serve as public relations theater rather than functional solutions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a diagnostic exemplar, showing how a single set of metrics produces all six classifications. For the user, it's a Snare. For the platform that built it, it's a Rope for coordinating advertisers and users. For advocates, it's a temporary Scaffold to be dismantled by regulation. For legacy media caught in the loop, it's a Piton—a degraded version of their original purpose. For a technologist who sees it as inevitable, it's a Mountain. The analytical observer, seeing all parts, classifies it as a Tangled Rope. The 'truth' of the constraint is the complete set of these perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is starkly asymmetric. Beneficiaries (platform_operators, advertisers) have arbitrage and mobility, leading to low or negative effective extraction (χ). They experience the system as coordination. The primary victims (individual_users) are trapped, leading to a maximally amplified extraction factor (d≈0.95, f(d)≈1.42), classifying their experience as a Snare. Organized agents (advocates) have more agency, which lowers their effective extraction and allows for a Scaffold classification, as they are actively building an exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating that the conflict between 'is it coordination?' (Rope) and 'is it extraction?' (Snare) is not a contradiction but a perspectival gap. The system is structurally both. Mandatrophy arises when one perspective (typically the beneficiary's Rope) is used to describe the entire system, erasing the victim's experience of it as a Snare. The Deferential Realism framework makes this gap explicit and measurable, showing that the full characterization of the constraint requires acknowledging all valid perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    design_vs_necessity,
    'Is cognitive exhaustion an inevitable consequence of information abundance (a Mountain), or a direct result of specific, profit-driven design choices (a Snare)?',
    'Comparative analysis of platforms with non-extractive business models (e.g., subscription-based, public-funded) versus ad-based models.',
    'If exhaustion persists even with ethical design, the constraint is more Mountain-like. If it abates, the Snare/Tangled Rope classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(design_vs_necessity, empirical, 'Distinguishing between inherent information overload and engineered addiction.').

omega_variable(
    regulatory_effectiveness,
    'Can top-down regulation (e.g., a fiduciary duty for platforms to protect user attention) effectively curb extraction without destroying the coordination benefits?',
    'Policy experiments and analysis of jurisdictions implementing digital wellness regulations (e.g., EU''s Digital Services Act).',
    'Effective regulation would confirm the Scaffold perspective, providing a viable sunset clause. Ineffective regulation would reinforce the Snare/Tangled Rope view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness, empirical, 'Assessing the viability of regulatory solutions to attention extraction.').

omega_variable(
    collapse_threshold,
    'At what point does the cannibalization of cognitive resources lead to a systemic collapse in user productivity and engagement, making the system unprofitable?',
    'Longitudinal economic modeling correlating population-level mental health and productivity metrics with platform revenue.',
    'Identifying a threshold would imply a natural limit to extraction, potentially a self-correcting mechanism. The absence of one suggests the system can remain a stable Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_threshold, conceptual, 'Determining the systemic failure point of the attention economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_market_cannibalization, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atte_tr_t2010, attention_market_cannibalization, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(atte_tr_t2017, attention_market_cannibalization, theater_ratio, 2017, 0.4).
narrative_ontology:measurement(atte_tr_t2024, attention_market_cannibalization, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(atte_be_t2010, attention_market_cannibalization, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(atte_be_t2017, attention_market_cannibalization, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(atte_be_t2024, attention_market_cannibalization, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_market_cannibalization, information_standard).
narrative_ontology:affects_constraint(attention_market_cannibalization, public_discourse_integrity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
