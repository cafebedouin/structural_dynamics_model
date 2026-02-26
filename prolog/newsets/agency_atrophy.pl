% ============================================================================
% CONSTRAINT STORY: agency_atrophy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agency_atrophy, []).

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
 *   constraint_id: agency_atrophy
 *   human_readable: The Outsourced Cognition Trap
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   This constraint models the dynamic where initially helpful
 *   decision-support technologies (e.g., GPS navigation, AI assistants) lead
 *   to the atrophy of the user's underlying cognitive skills. What begins as
 *   a pure coordination tool (a Rope) for convenience gradually becomes a
 *   dependency. As the user's ability to perform the task without the tool
 *   diminishes, their exit options become constrained, allowing platform
 *   providers to extract value (data, attention, fees) asymmetrically. The
 *   system's design often accelerates this process by maximizing ease-of-use
 *   and discouraging the practice of the underlying skill.
 *
 * KEY AGENTS:
 *   - Platform Providers: Primary beneficiary (institutional/arbitrage) - Gain a locked-in user base, data, and revenue streams.
 *   - Dependent Users: Primary victim (powerless/trapped) - Lose cognitive skills and agency, making exit costly or impossible.
 *   - Cognitive Commons: Secondary victim (powerless/trapped) - The collective pool of human skill and resilience is degraded.
 *   - Power Users: Moderate actors (moderate/mobile) - Utilize the tools while consciously preserving their skills, navigating the trade-offs.
 *   - Digital Wellness Advocates: Organized resistance (organized/mobile) - Promote alternatives and strategies to escape or avoid the trap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agency_atrophy, 0.55).
domain_priors:suppression_score(agency_atrophy, 0.65).
domain_priors:theater_ratio(agency_atrophy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agency_atrophy, extractiveness, 0.55).
narrative_ontology:constraint_metric(agency_atrophy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(agency_atrophy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agency_atrophy, tangled_rope).
narrative_ontology:human_readable(agency_atrophy, "The Outsourced Cognition Trap").
narrative_ontology:topic_domain(agency_atrophy, "technological/cognitive").

domain_priors:requires_active_enforcement(agency_atrophy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agency_atrophy, platform_providers).
narrative_ontology:constraint_victim(agency_atrophy, dependent_users).
narrative_ontology:constraint_victim(agency_atrophy, cognitive_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DEPENDENT USER (SNARE) — Having lost the underlying skill (e.g., navigation, scheduling), the user is trapped. Exiting the system imposes a significant cognitive and practical cost. The system extracts data, attention, and subscription fees in exchange for a now-essential function. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(agency_atrophy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM PROVIDER (ROPE) — From the provider's view, the system is a pure coordination tool that efficiently matches user needs with information and services, creating immense value. User dependency is seen as a measure of product-market fit, not extraction. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. Negative effective extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(agency_atrophy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE POWER USER (TANGLED ROPE) — This user leverages the tool for productivity gains but consciously maintains their underlying skills, retaining the ability to switch to alternatives. They experience both the coordination benefits and the extractive pressures (e.g., data collection, addictive design). d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(agency_atrophy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE DIGITAL WELLNESS ADVOCATE (SCAFFOLD) — Organized groups promoting digital minimalism and cognitive sovereignty see the trap as a temporary state. They are actively building and promoting alternatives (e.g., privacy-focused tools, skill-building apps) that serve as a scaffold to help users exit dependency. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.26. The sunset clause is the successful adoption of these alternative cognitive habits and tools.
constraint_indexing:constraint_classification(agency_atrophy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees the full lifecycle: an initial coordination function (Rope) that, through active design and user adaptation, creates dependency and enables asymmetric extraction (Snare). The constraint possesses both a genuine coordination function and a powerful extractive mechanism. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(agency_atrophy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agency_atrophy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agency_atrophy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agency_atrophy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agency_atrophy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(agency_atrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): Represents the significant value extracted from dependent users, measured in lost agency, atrophied skills (a long-term cost), personal data, and direct monetary fees. Suppression (0.65): High. Once a skill like navigation is atrophied, the cost of relearning it is a powerful barrier to exit. This is compounded by network effects and ecosystem lock-in, making non-participation difficult. Theater Ratio (0.30): Moderate. While the core function is real, many features evolve to maximize engagement and create a perception of indispensability rather than pure utility.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The platform provider experiences the system as a highly effective Rope, solving a coordination problem for users and creating value. The dependent user, however, experiences it as a Snare, where the initial convenience has given way to a coercive dependency. The analytical observer recognizes both functions are present simultaneously, classifying it as a Tangled Rope—a system that provides real coordination while also creating the conditions for its own extractive necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation correctly captures the perspectival differences. The platform provider, as a beneficiary with arbitrage exit, has a negative effective extraction (χ < 0), seeing a Rope. The dependent user, as a victim with trapped exit, experiences maximum positive effective extraction (χ > 0.66), seeing a Snare. The power user and advocate, with more agency and better exit options, fall in between, perceiving a Tangled Rope or Scaffold. The system's classification is thus a function of the agent's dependency and ability to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic example of lifecycle drift, resolving a potential mandatrophy. To label the system only as a 'Rope' (its initial state and the provider's claim) would be to ignore the emergent extractive structure. To label it only as a 'Snare' (the end-state for a dependent user) would be to ignore its genuine, ongoing coordination function. The Tangled Rope classification from the analytical perspective correctly identifies the hybrid nature of the constraint, preventing misclassification by acknowledging both its function and its cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_inevitability,
    'Is cognitive skill atrophy an unavoidable side-effect of efficient tools, or a designed outcome to create dependency?',
    'Comparative analysis of tool designs (skill-preserving vs. skill-replacing UIs); review of internal platform design documents and patents.',
    'If atrophy is an unavoidable byproduct of efficiency, the constraint is a tragic Rope. If it is a designed feature to increase switching costs, it is a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_inevitability, empirical, 'Whether skill atrophy is an inevitable side-effect or a designed outcome').

omega_variable(
    skill_reversibility,
    'Is the cognitive atrophy caused by reliance on these systems reversible, and at what cost?',
    'Longitudinal studies of users who cease using automated tools and attempt to regain native skills.',
    'If skills are easily regained, the ''trapped'' exit condition is weakened, and the constraint is a less severe Tangled Rope. If atrophy is permanent or costly to reverse, it is a hard Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_reversibility, empirical, 'The cost and possibility of reversing cognitive atrophy after dependency').

omega_variable(
    net_cognitive_impact,
    'Does outsourcing low-level cognitive tasks lead to a net increase in higher-order cognitive capacity, or does it erode the foundational skills required for complex thought?',
    'Neuro-cognitive studies comparing problem-solving abilities on novel tasks between long-term dependent users and non-users.',
    'A net positive impact would re-classify the constraint as a beneficial Scaffold for humanity. A net negative impact confirms its classification as a Snare from the perspective of the cognitive commons.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(net_cognitive_impact, conceptual, 'Net effect on human capability from outsourcing mundane cognition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agency_atrophy, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agen_tr_t2005, agency_atrophy, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(agen_tr_t2015, agency_atrophy, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(agen_tr_t2025, agency_atrophy, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(agen_be_t2005, agency_atrophy, base_extractiveness, 2005, 0.1).
narrative_ontology:measurement(agen_be_t2015, agency_atrophy, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(agen_be_t2025, agency_atrophy, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agency_atrophy, resource_allocation).
narrative_ontology:affects_constraint(agency_atrophy, individual_resilience).
narrative_ontology:affects_constraint(agency_atrophy, civic_discourse_quality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
