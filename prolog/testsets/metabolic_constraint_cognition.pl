% ============================================================================
% CONSTRAINT STORY: metabolic_constraint_cognition
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_metabolic_constraint_cognition, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: metabolic_constraint_cognition
 * human_readable: The ATP Ceiling as an Exploitable Limit
 * domain: biological/technological/economic
 * * SUMMARY:
 * This constraint represents the hard biological limit of the human brain's
 * metabolic capacity. The brain consumes ~20% of body energy despite being
 * 2% of mass; high-order cognition is metabolically expensive and
 * subject to rapid fatigue. While the biological limit itself is a Mountain,
 * this constraint models the *systemic exploitation* of that limit by
 * attention-economy platforms that induce "decision fatigue" to bypass
 * critical thinking and drive engagement or consumption.
 * * KEY AGENTS:
 * - Cognitive Laborer / Platform User: Subject (Powerless)
 * - Attention Architect / Platform Designer: Beneficiary (Institutional)
 * - Neuro-metabolic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.79) as systems intentionally exhaust the subject's
% limited glucose/ATP reserves to reduce resistance to predatory defaults.
domain_priors:base_extractiveness(metabolic_constraint_cognition, 0.79).
domain_priors:suppression_score(metabolic_constraint_cognition, 0.60).
domain_priors:theater_ratio(metabolic_constraint_cognition, 0.30). % Low theater; the exhaustion is physiologically real.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(metabolic_constraint_cognition, extractiveness, 0.79).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, theater_ratio, 0.30).

% The system claims the limit is a natural law, masking the constructed
% nature of its exploitation.
narrative_ontology:constraint_claim(metabolic_constraint_cognition, tangled_rope).
narrative_ontology:human_readable(metabolic_constraint_cognition, "The ATP Ceiling as an Exploitable Limit").

% Required for Tangled Rope: The system requires active enforcement through
% interface design (e.g., infinite scroll, notification barrages) to
% deplete cognitive resources.
domain_priors:requires_active_enforcement(metabolic_constraint_cognition).

% Structural property derivation hooks for Tangled Rope.
narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, attention_architects).
narrative_ontology:constraint_victim(metabolic_constraint_cognition, cognitive_laborers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user experiences the system as a Snare that traps them in cycles of
% fatigue and compliance, exploiting a biological need (rest) for profit.
% The effective extraction is amplified by powerlessness (1.5x).
constraint_indexing:constraint_classification(metabolic_constraint_cognition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions view this limit as a Rope—a predictable feature of human
% biology that allows them to coordinate schedules, product releases, and
% user interface designs for maximum compliance and engagement.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (for the beneficiary) and
% the severe, asymmetric extraction (from the victim), classifying it as a
% Tangled Rope. This acknowledges the dual nature of exploiting a real
% biological limit for systemic gain.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metabolic_constraint_cognition_tests).

test(perspectival_gap) :-
    % Verify the Snare vs. Rope conflict.
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure high extraction (0.79) is correctly registered.
    domain_priors:base_extractiveness(metabolic_constraint_cognition, E),
    E > 0.70,
    % Ensure all three requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(metabolic_constraint_cognition),
    narrative_ontology:constraint_beneficiary(metabolic_constraint_cognition, _),
    narrative_ontology:constraint_victim(metabolic_constraint_cognition, _).

:- end_tests(metabolic_constraint_cognition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this model is distinguishing the biological limit (a true Mountain)
 * from the socio-technical system built to exploit it (a Snare/Tangled Rope).
 * The base extraction of 0.79 is high because the system is designed to
 * convert a user's finite cognitive energy directly into revenue with high
 * efficiency. Suppression (0.60) comes from the difficulty of avoiding these
 * systems in a digitally integrated society.
 *
 * * PERSPECTIVAL GAP:
 * The gap is stark. The User feels a Snare because their biological need for
 * rest is weaponized against them. The Architect sees a Rope, a predictable
 * human factor to be engineered around for efficient system design. The
 * Analytical observer's Tangled Rope classification resolves this by acknowledging
 * both the coordination function (for architects) and the predatory extraction
 * it enables.
 *
 * * [RESOLVED MANDATROPHY]:
 * The Tangled Rope classification is critical here. A simpler model might
 * label the system a pure Snare, missing the (perverse) coordination function
 * that beneficiaries rely on. Or it might mislabel it as a Mountain, conflating
 * the biological limit with the artificial system exploiting it. Tangled Rope
 * correctly identifies that the system's "coordination" is built on the
 * predatory drainage of the subject's metabolic optionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_metabolic_augmentation,
    'Can exogenous glucose, stimulants, or neural-interfacing meaningfully expand the ATP ceiling, and would that shift the constraint from a biological limit to a purely economic one?',
    'Longitudinal metabolic study of cognitive performance under varying caloric/stimulant loads, cross-referenced with economic access to such enhancements.',
    'If ceiling rises significantly with accessible tech: constraint becomes a pure Snare of economic policy. If ceiling remains fixed: constraint remains a Tangled Rope exploiting a hard Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(metabolic_constraint_cognition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the intensification of the attention economy over the last decade.
% Extraction has risen sharply as platforms perfected techniques for inducing
% cognitive fatigue. Theater remains low but has slightly increased as platforms
% use "digital wellness" features to mask the underlying extractive mechanism.

% Theater ratio over time:
narrative_ontology:measurement(mcc_tr_t0, metabolic_constraint_cognition, theater_ratio, 0, 0.20).
narrative_ontology:measurement(mcc_tr_t5, metabolic_constraint_cognition, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mcc_tr_t10, metabolic_constraint_cognition, theater_ratio, 10, 0.30).

% Extraction over time:
narrative_ontology:measurement(mcc_ex_t0, metabolic_constraint_cognition, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(mcc_ex_t5, metabolic_constraint_cognition, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(mcc_ex_t10, metabolic_constraint_cognition, base_extractiveness, 10, 0.79).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a mechanism for allocating a scarce resource:
% the user's attention and cognitive energy.
narrative_ontology:coordination_type(metabolic_constraint_cognition, resource_allocation).

% This constraint is a foundational element for other technological constraints.
narrative_ontology:affects_constraint(metabolic_constraint_cognition, platform_engagement_algorithms).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */