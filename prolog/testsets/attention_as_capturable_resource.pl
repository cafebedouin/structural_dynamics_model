% ============================================================================
% CONSTRAINT STORY: attention_as_capturable_resource
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_as_capturable_resource, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: attention_as_capturable_resource
 *   human_readable: Attention as Capturable Resource
 *   domain: technology_governance/behavioral_psychology/social_infrastructure
 *
 * SUMMARY:
 *   The constraint 'attention as capturable resource' refers to the
 *   structural property that human attention responds predictably to certain
 *   stimulus patterns (novelty, variable reinforcement, social validation) in
 *   ways that can be systematically exploited by optimized information
 *   systems. This constraint is claimed as a mountain — an immutable property
 *   of human cognitive architecture — rather than a contingent feature of
 *   current platform design. The key observable is the gap between stated
 *   preferences (53% of Americans report wanting to reduce social media
 *   usage) and revealed preferences (average usage increased from 2h54m in
 *   2022 to 4h25m in 2026). This gap persists across all power levels and
 *   time horizons, suggesting that the constraint is not a matter of
 *   insufficient willpower, inadequate regulation, or temporary market
 *   failure, but rather a structural feature of how attention-processing
 *   systems interact with optimized stimulus streams. The constraint exhibits
 *   minimal extraction (0.18) because the psychological vulnerability
 *   pre-exists platform design — platforms exploit but do not create the
 *   capturable nature of attention. Suppression is near-zero (0.03) because
 *   no external force prevents users from reducing usage; the binding
 *   mechanism is internal (the psychological architecture itself). Theater
 *   ratio is very low (0.08) because the mechanisms are functional, not
 *   performative — variable ratio reinforcement schedules, novelty detection,
 *   and social validation loops genuinely capture attention rather than
 *   merely appearing to do so.
 *
 * KEY AGENTS:
 *   - Individual User: Experiences immediate capture (powerless/trapped at immediate horizon); recognizes pattern but cannot escape it (moderate/constrained at biographical horizon)
 *   - Platform Designer: Experiences constraint as design invariant (institutional/arbitrage); can exit industry but cannot change the underlying psychological architecture
 *   - Digital Wellness Coalition: Organized advocacy with policy influence (organized/mobile); sees regulation as addressing symptoms rather than root cause
 *   - High-Agency User: Can construct protective environments (powerful/mobile); recognizes that positional advantage does not eliminate underlying vulnerability
 *   - Analytical Observer: Sees constraint as universal property of bounded-rational information-processing systems (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_as_capturable_resource, 0.18).
domain_priors:suppression_score(attention_as_capturable_resource, 0.03).
domain_priors:theater_ratio(attention_as_capturable_resource, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_as_capturable_resource, extractiveness, 0.18).
narrative_ontology:constraint_metric(attention_as_capturable_resource, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(attention_as_capturable_resource, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(attention_as_capturable_resource, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(attention_as_capturable_resource, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_as_capturable_resource, mountain).
narrative_ontology:human_readable(attention_as_capturable_resource, "Attention as Capturable Resource").
narrative_ontology:topic_domain(attention_as_capturable_resource, "technology_governance/behavioral_psychology/social_infrastructure").

domain_priors:emerges_naturally(attention_as_capturable_resource).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER / IMMEDIATE (MOUNTAIN) — In the moment of engagement, the user experiences the attention capture mechanism as irresistible. The psychological architecture (variable ratio reinforcement, novelty bias, social validation loops) operates below the threshold of conscious control. The user may intellectually desire less usage but cannot translate that preference into action within the immediate context. The constraint appears as a fixed property of their own psychology.
constraint_indexing:constraint_classification(attention_as_capturable_resource, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SELF-AWARE USER / BIOGRAPHICAL (MOUNTAIN) — Over biographical time, users recognize the pattern and attempt interventions (app timers, grayscale mode, digital detox). These interventions fail systematically because they address symptoms rather than the underlying psychological architecture. The constraint is not the platform's design choices (which are contingent) but the exploitability of human attention itself. The user sees the mountain: their attention is capturable by any sufficiently optimized stimulus stream.
constraint_indexing:constraint_classification(attention_as_capturable_resource, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM DESIGNER (MOUNTAIN) — Platform designers experience the constraint as a design invariant. Any platform that does NOT optimize for engagement loses users to competitors that do. The competitive landscape enforces convergence on attention-capture mechanisms regardless of designer intent. Designers with ethical qualms can exit to other industries (arbitrage), but the constraint itself — that human attention responds predictably to certain stimulus patterns — is immutable. The designer sees the mountain from the supply side: attention is capturable, and market dynamics punish platforms that fail to capture it.
constraint_indexing:constraint_classification(attention_as_capturable_resource, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL WELLNESS COALITION (MOUNTAIN) — Organized advocacy groups (Center for Humane Technology, digital wellness nonprofits) recognize that individual-level interventions fail because the constraint is structural. They advocate for regulatory guardrails, design standards, and platform accountability. But even from this organized position with policy influence, the coalition sees the underlying constraint as immutable: human attention has exploitable vulnerabilities (novelty bias, social comparison, variable reinforcement) that any sufficiently optimized system will trigger. Regulation can constrain platform behavior, but it cannot change the psychological architecture that makes attention capturable.
constraint_indexing:constraint_classification(attention_as_capturable_resource, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical position, the constraint is a natural law of information-processing systems with bounded rationality. Attention is a finite resource; salience is computable; optimization is possible. Any civilization that develops sufficient computational power and behavioral data will discover attention-capture mechanisms. The gap between stated preferences and revealed preferences is not a design failure but a structural feature of systems where fast (System 1) and slow (System 2) processing operate on different timescales and with different optimization targets. The constraint is universal: it would appear in any technologically advanced civilization with similar cognitive architecture.
constraint_indexing:constraint_classification(attention_as_capturable_resource, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: HIGH-AGENCY USER (MOUNTAIN) — Users with significant resources (executive assistants, custom device configurations, professional coaches) can construct environments that reduce exposure to attention-capture mechanisms. They experience mobile exit options — they can afford to opt out of mainstream platforms or pay for attention-preserving alternatives. Yet even from this position of relative power, the constraint appears as a mountain: the psychological vulnerabilities (novelty response, social validation seeking, variable reinforcement sensitivity) are not eliminable through wealth or willpower. The high-agency user sees that attention is capturable in principle, and their advantage is merely positional (ability to avoid contexts where capture occurs), not fundamental.
constraint_indexing:constraint_classification(attention_as_capturable_resource, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_as_capturable_resource_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(attention_as_capturable_resource, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_as_capturable_resource, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(attention_as_capturable_resource, ExtMetricName, E),
    domain_priors:suppression_score(attention_as_capturable_resource, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(attention_as_capturable_resource),
    narrative_ontology:constraint_metric(attention_as_capturable_resource, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(attention_as_capturable_resource, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(attention_as_capturable_resource_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The constraint is the capturable nature of attention itself, not the platform designs that exploit it. Platforms extract value by exploiting the constraint, but the constraint pre-exists the extraction. The modest extractiveness reflects that some coordination benefit exists (platforms do provide genuine value — connection, information, entertainment) even as they exploit attentional vulnerabilities. The value is low enough to satisfy the mountain threshold (ε ≤ 0.25) while acknowledging that the exploitation is not zero-sum — users receive some benefit even as their attention is captured. Suppression (0.03): Near-zero. No external force prevents users from reducing usage. App timers, grayscale mode, and digital detox options are widely available. The binding mechanism is not suppression of alternatives but the internal psychological architecture that makes attention capturable. The minimal suppression reflects friction costs (social pressure, FOMO, network effects) but these are second-order compared to the primary mechanism (the psychological vulnerability itself). Theater ratio (0.08): Very low. The attention-capture mechanisms are functional, not performative. Variable ratio reinforcement schedules genuinely produce compulsive checking behavior; novelty detection genuinely triggers dopamine release; social validation loops genuinely activate reward circuitry. The modest theater reflects that some platform features are performative (wellness dashboards, usage reports) but the core capture mechanisms are not. Accessibility collapse (0.92): Very high. The constraint is accessible to all observers once the preference-action gap is pointed out. Users recognize the pattern ('I want to use less but I can't stop'); designers recognize the mechanism ('engagement optimization is a design invariant'); researchers recognize the architecture ('System 1 overrides System 2'). The constraint does not require specialized knowledge to observe. Resistance (0.08): Very low. Attempts to resist the constraint (individual willpower, app timers, regulatory guardrails) fail systematically because they address symptoms rather than the underlying psychological architecture. The constraint persists across all intervention attempts.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is minimal by design — all perspectives classify as mountain. The gap that exists is not in classification type but in the structural position from which the mountain is observed. The powerless user at immediate time sees the mountain as an irresistible force in the moment. The moderate user at biographical time sees the mountain as a pattern they recognize but cannot escape. The institutional designer sees the mountain as a design invariant enforced by competitive dynamics. The organized coalition sees the mountain as a limit on what regulation can achieve. The analytical observer sees the mountain as a universal property of bounded rationality. The high-agency user sees the mountain as a vulnerability that wealth can avoid but not eliminate. The uniformity of classification across perspectives is itself the diagnostic signal: when a constraint appears as a mountain from all structural positions, it is either a genuine natural law or a successfully naturalized extraction mechanism. The omega variables probe this distinction — if neuroplasticity is high, the constraint may be a snare (platforms create the vulnerability they exploit); if collective action is possible, the constraint may be a scaffold (temporary coordination failure). But the base case, given current evidence, is that attention is genuinely capturable as a structural property of human cognition.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint is a uniform-type mountain — all perspectives classify as mountain because the constraint is a natural law of cognitive architecture rather than a contingent institutional arrangement. There are no beneficiaries or victims in the structural sense because the constraint is not an extraction mechanism but a property of the system. Platform designers exploit the constraint, but they do not create it. Users experience the constraint, but they are not victims of it in the way that powerless agents are victims of a snare — they are subject to a natural law. The absence of beneficiary/victim declarations is appropriate for a mountain constraint. Directionality values are derived from the canonical fallback (power atom to canonical d) rather than from structural relationships, because there is no extraction flow to measure — only differential experience of an immutable constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that very low extraction (0.18) combined with very low suppression (0.03) and high accessibility collapse (0.92) produces a mountain classification from all perspectives. The mandatrophy question 'Is this coordination or extraction?' is answered: it is neither — it is a natural law. The modest extractiveness reflects that platforms do provide genuine value even as they exploit attentional vulnerabilities, but the exploitation is of a pre-existing constraint rather than a created one. The constraint is not mislabeled coordination (there is minimal coordination function) nor mislabeled extraction (there is no asymmetric power relationship creating the vulnerability). It is a structural property of cognitive architecture that any sufficiently optimized information system will exploit. The mountain classification is not a naturalization of contingent institutional arrangements but a recognition of genuine limits. The omega variables preserve the possibility that future evidence could reclassify the constraint (if neuroplasticity is high, or if collective action succeeds), but the current structural data supports the mountain hypothesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neuroplasticity_boundary,
    'Can sustained exposure to attention-capture mechanisms permanently alter the psychological architecture that makes attention capturable, or is the vulnerability fixed across the lifespan?',
    'Longitudinal neuroimaging studies tracking attentional control capacity, novelty response, and reward sensitivity across cohorts with varying platform exposure; cross-generational comparison of digital natives vs digital immigrants',
    'If neuroplasticity is high: the constraint may be a snare (platform design creates the vulnerability it exploits). If neuroplasticity is low: the constraint is a mountain (the vulnerability pre-exists and is merely exploited).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neuroplasticity_boundary, empirical, 'Whether attention-capture vulnerability is fixed or induced by exposure').

omega_variable(
    collective_action_threshold,
    'Is there a critical mass threshold at which collective awareness of attention capture enables effective coordination to constrain platform behavior, or does the prisoner''s dilemma structure make individual defection inevitable?',
    'Analysis of historical cases where collective action successfully constrained addictive product design (tobacco regulation, gambling restrictions); identification of structural conditions that enabled coordination',
    'If collective action is possible: the constraint may be a scaffold (temporary coordination failure with a sunset). If collective action fails structurally: the constraint remains a mountain (individual rationality prevents collective solution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Whether collective action can overcome individual-level capture').

omega_variable(
    preference_stability,
    'Are stated preferences for reduced usage genuine but unactionable, or are they socially desirable responses that do not reflect true preferences?',
    'Revealed preference experiments with real costs (pay to reduce exposure, accept income loss for usage reduction); comparison of stated vs revealed preferences under varying stakes',
    'If stated preferences are genuine: the preference-action gap is evidence of a binding constraint. If stated preferences are performative: there is no constraint, only social desirability bias in survey responses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preference_stability, empirical, 'Whether the preference-action gap reflects a real constraint or measurement artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_as_capturable_resource, 2022, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_as_capturable_resource, theater_ratio, 0, 0.05).
narrative_ontology:measurement(attn_tr_t2, attention_as_capturable_resource, theater_ratio, 2, 0.06).
narrative_ontology:measurement(attn_tr_t4, attention_as_capturable_resource, theater_ratio, 4, 0.08).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_as_capturable_resource, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(attn_be_t2, attention_as_capturable_resource, base_extractiveness, 2, 0.17).
narrative_ontology:measurement(attn_be_t4, attention_as_capturable_resource, base_extractiveness, 4, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_as_capturable_resource, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a candidate for decomposition if future analysis reveals that 'attention as capturable resource' conflates multiple structurally distinct claims: (1) attention responds to novelty (neurological claim with ε ≈ 0.05), (2) variable reinforcement produces compulsive behavior (psychological claim with ε ≈ 0.08), (3) social validation activates reward circuitry (social neuroscience claim with ε ≈ 0.10), (4) platform design can exploit these mechanisms (engineering claim with ε ≈ 0.40). Current formulation treats these as a unified constraint, but if observables yield different ε values, decomposition into a constraint family would be warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
