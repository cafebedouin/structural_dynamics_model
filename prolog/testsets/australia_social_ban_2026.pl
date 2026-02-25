% ============================================================================
% CONSTRAINT STORY: australia_social_ban_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-23
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_australia_social_ban_2026, []).

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
 *   constraint_id: australia_social_ban_2026
 *   human_readable: Australian Under-16 Social Media Ban
 *   domain: political/technological
 *
 * SUMMARY:
 *   A proposed Australian federal law, set to take effect in 2025/2026, would
 *   ban social media access for all individuals under the age of 16. The law
 *   places the onus of enforcement on technology platforms, requiring them to
 *   implement robust age verification systems under threat of significant
 *   financial penalties. The stated goal is to protect youth mental health by
 *   reducing exposure to algorithmic amplification, social comparison, and
 *   online harms. This creates a complex constraint with multiple,
 *   conflicting perspectives.
 *
 * KEY AGENTS:
 *   - Australian Teens (Under 16): Primary target/victim (powerless/trapped) — lose access to digital social spaces.
 *   - Australian Government: Primary beneficiary/enforcer (institutional/arbitrage) — gains political capital and enacts a policy agenda.
 *   - Social Media Platforms: Secondary victim and enforcement agent (powerful/constrained) — bear compliance costs but gain regulatory certainty.
 *   - Parents/Guardians: A divided group of beneficiaries (seeking control) and victims (seeking to teach digital literacy).
 *   - Age Verification Tech Vendors: Tertiary beneficiaries (organized/arbitrage) — a new market is created for their products.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(australia_social_ban_2026, 0.48).
domain_priors:suppression_score(australia_social_ban_2026, 0.75).
domain_priors:theater_ratio(australia_social_ban_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(australia_social_ban_2026, extractiveness, 0.48).
narrative_ontology:constraint_metric(australia_social_ban_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(australia_social_ban_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(australia_social_ban_2026, tangled_rope).
narrative_ontology:human_readable(australia_social_ban_2026, "Australian Under-16 Social Media Ban").
narrative_ontology:topic_domain(australia_social_ban_2026, "political/technological").

domain_priors:requires_active_enforcement(australia_social_ban_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, australian_government).
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, pro_regulation_advocacy_groups).
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, age_verification_tech_vendors).
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, parents_seeking_control).
narrative_ontology:constraint_victim(australia_social_ban_2026, australian_teens_under_16).
narrative_ontology:constraint_victim(australia_social_ban_2026, social_media_platforms).
narrative_ontology:constraint_victim(australia_social_ban_2026, parents_promoting_digital_literacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE BANNED TEENAGER (SNARE) — Legally and technically barred from core social platforms, this group experiences the law as pure extraction of digital autonomy and social connection. They are the direct target with no formal recourse or exit. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.68, which meets the Snare threshold (≥0.66).
constraint_indexing:constraint_classification(australia_social_ban_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE GOVERNMENT REGULATOR (ROPE) — The law's author and enforcer perceives it as a pure coordination mechanism to achieve a stated public good (protecting youth mental health). From this position, the costs are externalities and the function is coordination. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.06. Negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(australia_social_ban_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SOCIAL MEDIA PLATFORM (TANGLED ROPE) — Forced to implement costly age verification and de-platform a user segment, the platform is a victim. However, it also benefits from regulatory clarity and a potential moat against new competitors. It is both extracted from and a key part of the enforcement apparatus. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.41, placing it in the Tangled Rope category.
constraint_indexing:constraint_classification(australia_social_ban_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CYNICAL PARENT (PITON) — This parent believes the ban is easily circumvented by tech-savvy teens (VPNs, lying about age) and thus functionally weak. The law's primary effect is performative—political signaling. The high theater_ratio (0.75) and low perceived function lead to a Piton classification, an inertial rule maintained for appearances.
constraint_indexing:constraint_classification(australia_social_ban_2026, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE) — This perspective sees the dual nature of the constraint: a genuine (if contested) coordination attempt to address a social problem, implemented via a highly coercive and extractive mechanism. It acknowledges both the public-good claim and the asymmetric costs. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.66. This value is on the border of Snare, reflecting the highly coercive nature of the policy.
constraint_indexing:constraint_classification(australia_social_ban_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(australia_social_ban_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(australia_social_ban_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(australia_social_ban_2026, TR),
    TR >= 0.70.

:- end_tests(australia_social_ban_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.48): Represents the significant loss of digital autonomy and social access for teens, combined with the high compliance costs (development, moderation, fines) imposed on platforms. Suppression (0.75): High. The constraint is a legal mandate with severe penalties, aiming to eliminate the option of social media use for the target group. Alternatives like VPNs are implicitly or explicitly targeted by enforcement. Theater Ratio (0.75): High. While there is a functional component (age verification systems), the political signaling of 'being tough on Big Tech' is a primary driver. The high likelihood of technical workarounds means its long-term state is more performative than functional.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The government frames the law as a benevolent Rope, coordinating society towards a safer environment for children. For the teens who are de-platformed, it is an inescapable Snare, removing a key part of their social lives without their consent. For platforms, it is a Tangled Rope, a costly mandate they must enforce which also solidifies their market position. For observers who believe it won't work, it's a Piton—a hollow institutional gesture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like the government (institutional/arbitrage) have a low derived directionality (d), resulting in a negative effective extraction (χ) and a Rope classification. Victims like the teens (powerless/trapped) have the highest possible d, leading to a high χ and a Snare classification. Agents in the middle, like platforms (powerful/constrained), have a moderate d, leading to a χ value that correctly identifies the constraint as a Tangled Rope from their position.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates how a single policy instrument resolves into multiple, valid classifications. There is no single 'correct' type. The mandatrophy of mislabeling a coercive Snare as a benevolent Rope is resolved by acknowledging the government's perspective while simultaneously centering the Snare classification experienced by the powerless. The framework's role is not to pick a winner, but to map the full topology of these perspectival disagreements, which are themselves data about the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    age_verification_effectiveness,
    'How effective and privacy-invasive will the mandated age verification technologies be?',
    'Empirical audit of deployed systems for circumvention rates and data handling practices.',
    'If highly effective, the Snare perspective is strengthened. If easily circumvented, the Piton perspective becomes dominant as the law''s function collapses into theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(age_verification_effectiveness, empirical, 'Effectiveness and privacy impact of age verification tech').

omega_variable(
    mental_health_impact,
    'Does the ban produce a net positive, negative, or neutral effect on youth mental health?',
    'Longitudinal studies comparing mental health metrics in Australian youth cohorts pre- and post-ban against control groups in other countries.',
    'A clear positive impact would validate the coordination claim (Rope/Tangled Rope). A negative or neutral impact would suggest the constraint is a pure Snare or performative Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mental_health_impact, empirical, 'Net effect of the social media ban on youth mental health').

omega_variable(
    circumvention_normalization,
    'Will circumvention techniques (VPNs, etc.) become normalized among the target demographic?',
    'Sociological and network analysis of youth digital practices post-ban.',
    'Widespread normalization of circumvention would render the law a Piton, maintained for political theater despite being functionally inert for its target population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circumvention_normalization, conceptual, 'Normalization of circumvention techniques among teens').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(australia_social_ban_2026, 2024, 2029).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aust_tr_t2024, australia_social_ban_2026, theater_ratio, 2024, 0.8).
narrative_ontology:measurement(aust_tr_t2026, australia_social_ban_2026, theater_ratio, 2026, 0.65).
narrative_ontology:measurement(aust_tr_t2029, australia_social_ban_2026, theater_ratio, 2029, 0.75).

% Extraction over time
narrative_ontology:measurement(aust_be_t2024, australia_social_ban_2026, base_extractiveness, 2024, 0.2).
narrative_ontology:measurement(aust_be_t2026, australia_social_ban_2026, base_extractiveness, 2026, 0.4).
narrative_ontology:measurement(aust_be_t2029, australia_social_ban_2026, base_extractiveness, 2029, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(australia_social_ban_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(australia_social_ban_2026, digital_identity_standards_au).
narrative_ontology:affects_constraint(australia_social_ban_2026, global_internet_fragmentation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
