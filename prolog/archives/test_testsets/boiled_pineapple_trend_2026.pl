% ============================================================================
% CONSTRAINT STORY: boiled_pineapple_trend_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boiled_pineapple_trend_2026, []).

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
 *   constraint_id: boiled_pineapple_trend_2026
 *   human_readable: The Bromelain Denaturing Piton (Viral Pineapple Tea)
 *   domain: social/wellness/technological
 *
 * SUMMARY:
 *   This constraint analyzes the viral wellness trend of boiling pineapple
 *   peels and cores to create a 'tea' purported to have anti-inflammatory
 *   benefits from the enzyme bromelain. However, bromelain is a protein that
 *   is denatured (structurally destroyed and rendered inactive) by boiling.
 *   The constraint is therefore a classic Piton: a practice whose claimed
 *   primary function has been nullified by its own process, but which
 *   persists due to social proof, ritual, and misinformation. The activity is
 *   almost entirely performative (theater), with negligible biochemical
 *   function.
 *
 * KEY AGENTS:
 *   - Trend Followers: Primary victims (powerless/trapped) — invest time and belief for a biochemically nullified benefit.
 *   - Wellness Influencers: Primary beneficiaries (powerful/arbitrage) — gain engagement, followers, and revenue by promoting the trend.
 *   - Content Platforms: Institutional beneficiaries (institutional/arbitrage) — profit from the viral engagement loop, regardless of content validity.
 *   - Analytical Observers (e.g., scientists, debunkers): Identify the functional degradation and classify the constraint as a Piton.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boiled_pineapple_trend_2026, 0.15).
domain_priors:suppression_score(boiled_pineapple_trend_2026, 0.2).
domain_priors:theater_ratio(boiled_pineapple_trend_2026, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boiled_pineapple_trend_2026, extractiveness, 0.15).
narrative_ontology:constraint_metric(boiled_pineapple_trend_2026, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(boiled_pineapple_trend_2026, theater_ratio, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boiled_pineapple_trend_2026, piton).
narrative_ontology:human_readable(boiled_pineapple_trend_2026, "The Bromelain Denaturing Piton (Viral Pineapple Tea)").
narrative_ontology:topic_domain(boiled_pineapple_trend_2026, "social/wellness/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boiled_pineapple_trend_2026, wellness_influencers).
narrative_ontology:constraint_beneficiary(boiled_pineapple_trend_2026, content_platforms).
narrative_ontology:constraint_victim(boiled_pineapple_trend_2026, trend_followers).
narrative_ontology:constraint_victim(boiled_pineapple_trend_2026, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL (PITON) — The core claimed function (active bromelain) is destroyed by the preparation method (boiling). The ritual persists due to social inertia and misinformation, detached from its original purpose. theater_ratio=0.95 >> 0.70, triggering the Piton classification. This is the ground-truth perspective.
constraint_indexing:constraint_classification(boiled_pineapple_trend_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: TREND FOLLOWER (ROPE) — Experiences the trend as a low-cost, beneficial coordination activity for health. They are unaware of the denaturation, so from their view, there is no theater. As a victim with trapped exit (in the info-bubble), d is high (≈0.95), but base ε is so low (0.15) that effective extraction χ remains below the Rope threshold (χ ≈ 0.26).
constraint_indexing:constraint_classification(boiled_pineapple_trend_2026, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: WELLNESS INFLUENCER (ROPE) — Experiences the trend as a pure coordination mechanism to generate engagement and ad revenue. As a beneficiary with arbitrage exit (can pivot to the next trend instantly), d is very low (≈0.15), resulting in negative effective extraction (net benefit).
constraint_indexing:constraint_classification(boiled_pineapple_trend_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTENT PLATFORM (ROPE) — Structurally indifferent to the content's validity, the platform sees a pure coordination signal that drives user engagement. The trend is a low-cost mechanism for capturing attention. As an institutional beneficiary, its effective extraction is negative.
constraint_indexing:constraint_classification(boiled_pineapple_trend_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boiled_pineapple_trend_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boiled_pineapple_trend_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boiled_pineapple_trend_2026, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(boiled_pineapple_trend_2026, TR),
    TR >= 0.70.

:- end_tests(boiled_pineapple_trend_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15) is very low; the primary cost to victims is wasted time and pineapple scraps, not significant financial loss. Suppression (0.20) is also low; alternatives are readily available, but are suppressed by the social proof of the viral trend. The defining metric is the Theater Ratio (0.95), which is extremely high because the claimed benefit is entirely negated by the preparation method. The activity's persistence is purely a function of social performance and belief, not biochemical efficacy. This high theater ratio is the key signal for the Piton classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. From the analytical perspective of a biochemist, the constraint is a Piton—a hollowed-out ritual with no function. From the perspective of a trend follower or influencer, it is a Rope—a beneficial coordination game. They are either unaware of or indifferent to the denaturation of bromelain, and thus perceive the activity's function as intact. The disagreement is not about the cost (which all agree is low), but about the existence of the function itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (influencers, platforms) have arbitrage exit and benefit from the engagement, leading to a low 'd' value and negative effective extraction (they see a pure coordination good). Victims (trend followers) are trapped in an information bubble and bear the opportunity cost, leading to a high 'd' value. However, because base extractiveness (ε) is so low, even for victims the effective extraction (χ) does not cross the threshold into Tangled Rope or Snare territory, hence their Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case correctly uses the Piton classification to identify a low-harm, high-theater phenomenon. A naive analysis might misclassify it as a Snare based on the misinformation aspect, but the low extraction and suppression metrics prevent this. It could also be misclassified as a simple Rope if one ignores the scientific reality and only looks at the social coordination function. The Piton type precisely captures the critical feature: the decoupling of a ritual from its original, now-degraded, function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    placebo_effect_magnitude,
    'Does the ritual of preparing and consuming the tea provide a significant placebo effect, thus creating a non-biochemical function?',
    'A randomized controlled trial comparing boiled pineapple tea against a similarly prepared placebo beverage and a no-intervention group, measuring self-reported wellness outcomes.',
    'A strong placebo effect would mean the constraint provides a genuine (though misattributed) coordination benefit, potentially shifting the analytical classification from Piton to a low-extraction Rope, as the ''function'' would no longer be zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placebo_effect_magnitude, empirical, 'Quantifying the placebo effect of the pineapple tea ritual').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boiled_pineapple_trend_2026, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boil_tr_t0, boiled_pineapple_trend_2026, theater_ratio, 0, 0.5).
narrative_ontology:measurement(boil_tr_t12, boiled_pineapple_trend_2026, theater_ratio, 12, 0.8).
narrative_ontology:measurement(boil_tr_t24, boiled_pineapple_trend_2026, theater_ratio, 24, 0.95).

% Extraction over time
narrative_ontology:measurement(boil_be_t0, boiled_pineapple_trend_2026, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(boil_be_t12, boiled_pineapple_trend_2026, base_extractiveness, 12, 0.15).
narrative_ontology:measurement(boil_be_t24, boiled_pineapple_trend_2026, base_extractiveness, 24, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boiled_pineapple_trend_2026, information_standard).
narrative_ontology:affects_constraint(boiled_pineapple_trend_2026, wellness_misinformation_ecosystem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
