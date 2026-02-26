% ============================================================================
% CONSTRAINT STORY: ancient_antibiotic_resistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancient_antibiotic_resistance, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ancient_antibiotic_resistance
 *   human_readable: The Inherent Evolutionary Potential for Antibiotic Resistance
 *   domain: biological/evolutionary
 *
 * SUMMARY:
 *   The discovery of antibiotic resistance genes in ancient, isolated
 *   bacteria (e.g., from 5,000-year-old ice or pristine caves) demonstrates
 *   that resistance is a natural and ancient feature of the microbial world,
 *   not a modern phenomenon created by human antibiotic use. This constraint
 *   represents the fundamental evolutionary potential for resistance to
 *   emerge. It acts as a permanent boundary condition on medicine and
 *   biotechnology. Human activity did not create this potential; it merely
 *   created a massive selective pressure that made latent resistance
 *   mechanisms clinically relevant on a global scale.
 *
 * KEY AGENTS:
 *   - Evolutionary Biologists: Analytical observers who see the constraint as a natural law.
 *   - Pharmaceutical R&D Sector: Institutional actors who must engineer around this fixed obstacle.
 *   - Public Health Authorities: Organized actors who must manage the consequences of this permanent environmental feature.
 *   - Microbial Collective: The non-human agents for whom resistance genes are a beneficial survival tool (Rope).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_antibiotic_resistance, 0.05).
domain_priors:suppression_score(ancient_antibiotic_resistance, 0.02).
domain_priors:theater_ratio(ancient_antibiotic_resistance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, extractiveness, 0.05).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancient_antibiotic_resistance, mountain).
narrative_ontology:human_readable(ancient_antibiotic_resistance, "The Inherent Evolutionary Potential for Antibiotic Resistance").
narrative_ontology:topic_domain(ancient_antibiotic_resistance, "biological/evolutionary").

domain_priors:emerges_naturally(ancient_antibiotic_resistance).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL (MOUNTAIN) — The existence of ancient resistance genes is a fundamental, unchangeable feature of the biosphere. It's a natural law of evolution that cannot be altered, only understood and navigated. Base extraction is near zero; it is a background condition of reality.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PHARMACEUTICAL R&D (MOUNTAIN) — From this perspective, the evolutionary potential for resistance is a fixed obstacle and a permanent cost of doing business. Any new antibiotic will eventually face resistance. This is an irreducible physical limit on the long-term efficacy of any single compound.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLIC HEALTH (MOUNTAIN) — Public health bodies must treat the potential for resistance as a permanent environmental hazard, like volcanoes or earthquakes. It cannot be eliminated, only monitored and managed through stewardship programs. It is a fixed parameter of the system they operate in.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MICROBIAL COLLECTIVE (ROPE) — For bacteria, resistance genes are a pure coordination tool for survival. In an environment with naturally-produced antibiotics from competing microbes, these genes are a public good, shared via horizontal gene transfer, enabling survival. There is no extraction, only a collective benefit.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, rope,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_antibiotic_resistance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, ExtMetricName, E),
    domain_priors:suppression_score(ancient_antibiotic_resistance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ancient_antibiotic_resistance),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ancient_antibiotic_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it represents a fundamental, unchangeable aspect of biology. Extractiveness (ε=0.05) and Suppression (0.02) are near zero because the *potential* for resistance does not, in itself, extract or coerce. It is a background fact. The Natural Law profile is strong: it `emerges_naturally` from evolution, `accessibility_collapse` (0.95) is high as evidence is found wherever ancient microbes are sampled, and `resistance` (0.05) is low as the phenomenon cannot be altered or wished away.
 *
 * PERSPECTIVAL GAP:
 *   The primary classification is Mountain, which is stable across most human perspectives (analytical, institutional, organized). The key perspectival gap is with the microbial collective itself. For bacteria, a resistance gene is not a limit but a tool—a pure coordination mechanism (Rope) for surviving in competitive environments. This highlights how the same object (a gene) can be part of a Mountain-class constraint for one species (humans) and a Rope-class solution for another (bacteria).
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the constraint is largely neutral (d≈0.5) for human observers; it is a symmetric fact of nature. The derived directionality for the microbial collective would be that of a beneficiary (low d), as the genes provide a survival advantage. This correctly yields the Rope classification from their perspective, where the effective extraction (χ) is very low or negative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a critical case for preventing mandatrophy. The modern crisis of clinical antibiotic resistance is a high-extraction Snare or Tangled Rope. However, conflating that downstream effect with its source—the ancient potential for resistance—is a category error. This story correctly identifies the source as a Mountain. This distinction is vital for policy: you cannot 'solve' or 'eliminate' a Mountain. Policy must focus on managing the selective pressures that turn a latent Mountain into an active Snare, for example through antibiotic stewardship. Labeling the ancient potential as a Snare would imply it is an artificial, coercive system that could be dismantled, which is biologically false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancient_antibiotic_resistance, 0, 5000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ancient_antibiotic_resistance, clinical_antibiotic_resistance).

% DUAL FORMULATION NOTE:
% This constraint, 'ancient_antibiotic_resistance' (ε≈0.05, Mountain), is the upstream natural law that enables the downstream constraint 'clinical_antibiotic_resistance' (ε>0.6, Tangled Rope/Snare). The former is the immutable potential; the latter is the socio-technical crisis created by human selective pressure. They are distinct constraints and must be modeled separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
