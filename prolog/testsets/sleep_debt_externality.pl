% ============================================================================
% CONSTRAINT STORY: sleep_debt_externality
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sleep_debt_externality, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: sleep_debt_externality
 * human_readable: The Circadian Siphon
 * domain: biological/economic/social
 * * SUMMARY:
 * This constraint represents the systemic extraction of biological rest to
 * fuel 24/7 economic and digital activity. Institutions externalize the cost
 * of this "sleep debt" onto the individual’s long-term health and cognitive
 * function. It functions as a Snare for the subject, whose health is
 * liquidated, while serving as a Rope for a globalized economy that
 * coordinates across all time zones simultaneously.
 * * KEY AGENTS:
 * - Shift Worker / Heavy User: Subject (Powerless)
 * - 24/7 Service Infrastructure: Beneficiary (Institutional)
 * - Circadian Biologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.82) because the system siphons irreducible biological
% recovery time to generate short-term transactional surplus.
domain_priors:base_extractiveness(sleep_debt_externality, 0.82).
domain_priors:suppression_score(sleep_debt_externality, 0.65).
domain_priors:theater_ratio(sleep_debt_externality, 0.38). % Low theater; the exhaustion is physiologically inescapable.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(sleep_debt_externality, extractiveness, 0.82).
narrative_ontology:constraint_metric(sleep_debt_externality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sleep_debt_externality, theater_ratio, 0.38).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(sleep_debt_externality, tangled_rope).
narrative_ontology:human_readable(sleep_debt_externality, "The Circadian Siphon").
narrative_ontology:topic_domain(sleep_debt_externality, "biological/economic/social").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(sleep_debt_externality).
narrative_ontology:constraint_beneficiary(sleep_debt_externality, '24_7_service_infrastructure').
narrative_ontology:constraint_victim(sleep_debt_externality, shift_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: the economic necessity of the "always-on"
% environment makes adequate rest a luxury they cannot afford.
constraint_indexing:constraint_classification(sleep_debt_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The infrastructure views the elimination of "downtime" as a Rope—
% the only way to coordinate global logistics and digital markets.
constraint_indexing:constraint_classification(sleep_debt_externality, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BIOLOGICAL OBSERVER (MOUNTAIN)
% From a biological standpoint, the need for glymphatic clearance during
% sleep is an irreducible Mountain of mammalian physics. This is the underlying
% reality the socio-economic constraint operates upon.
constraint_indexing:constraint_classification(sleep_debt_externality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical context reveals the hybrid nature. The system provides
% real coordination but at a highly extractive cost to a specific group.
constraint_indexing:constraint_classification(sleep_debt_externality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sleep_debt_externality_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(sleep_debt_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sleep_debt_externality, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    % Ensure extraction (0.82) is correctly registered as a high-extraction constraint.
    domain_priors:base_extractiveness(sleep_debt_externality, E),
    E >= 0.46.

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(sleep_debt_externality),
    narrative_ontology:constraint_beneficiary(sleep_debt_externality, _),
    narrative_ontology:constraint_victim(sleep_debt_externality, _).

:- end_tests(sleep_debt_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) is extremely high because the constraint liquidates
 * an irreducible biological asset (cognitive and physical health) for short-term
 * economic gain. The suppression score (0.65) reflects the lack of viable
 * alternatives for workers in sectors demanding 24/7 availability. The Perspectival
 * Gap is stark: for the institutional beneficiary, it's a pure coordination Rope
 * enabling the global economy; for the individual, it's a Snare that drains
 * their health. The Mountain classification from the biological perspective
 * captures the physical law being violated.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical for resolving Mandatrophy. A simpler
 * model might classify this as a pure Snare, ignoring the genuine coordination
 * function that the 24/7 economy provides. By classifying it as a Tangled Rope,
 * the system correctly identifies that a valid coordination mechanism (global
 * commerce) has become "tangled" with a predatory extraction mechanism (health
 * liquidation). This prevents mislabeling the entire system as malicious
 * extraction and instead points to the need to "untangle" the two functions—
 * for example, through automation, better labor laws, or technologies that
 * mitigate circadian disruption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_sleep_simulation,
    'Can pharmaceutical or digital "compressed sleep" simulate biological rest, turning the Mountain into a solvable problem?',
    'Longitudinal clinical trials measuring glymphatic clearance, cognitive performance, and all-cause mortality in users of sleep-substitute technologies vs. control groups.',
    'If successful: The constraint morphs from a biological Mountain to a Snare of access/cost. If impossible: The constraint remains a hard biological Mountain, and the Tangled Rope becomes more severe.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sleep_debt_externality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified with globalization and digital technology.
% Extraction began with industrial shift work and accelerated dramatically
% in the internet era. Theater has risen slightly as corporations adopt
% superficial "wellness" rhetoric without changing core demands.

% Theater ratio over time:
narrative_ontology:measurement(sde_tr_t0, sleep_debt_externality, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sde_tr_t5, sleep_debt_externality, theater_ratio, 5, 0.25).
narrative_ontology:measurement(sde_tr_t10, sleep_debt_externality, theater_ratio, 10, 0.38).

% Extraction over time:
narrative_ontology:measurement(sde_ex_t0, sleep_debt_externality, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(sde_ex_t5, sleep_debt_externality, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(sde_ex_t10, sleep_debt_externality, base_extractiveness, 10, 0.82).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: Enables the 24/7 global economy.
narrative_ontology:coordination_type(sleep_debt_externality, global_infrastructure).

% Network relationships: Sleep debt is a direct causal factor in other
% socio-economic constraints like gig economy precarity and public health crises.
narrative_ontology:affects_constraint(sleep_debt_externality, gig_economy_precarity).
narrative_ontology:affects_constraint(sleep_debt_externality, public_health_burden).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */