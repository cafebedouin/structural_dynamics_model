% ============================================================================
% CONSTRAINT STORY: nasa_faster_better_cheaper
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_nasa_faster_better_cheaper, []).

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
 * * constraint_id: nasa_faster_better_cheaper
 * human_readable: The "Faster, Better, Cheaper" (FBC) Management Paradigm
 * domain: political/economic/technological
 * * SUMMARY:
 * "Faster, Better, Cheaper" (FBC) was a NASA management philosophy from 1992-1999 designed to increase mission frequency and reduce costs by accepting higher technical risk. It constrained engineering behavior by imposing strict, unyielding budget caps and compressed schedules, effectively attempting to optimize all three corners of the traditional management triangle simultaneously. The policy led to notable failures, including the Mars Climate Orbiter and Mars Polar Lander.
 * * KEY AGENTS:
 * - Mission Engineer: Subject (Powerless); forced to cut redundant systems and testing to stay under budget caps.
 * - NASA Administrator (Goldin): Beneficiary (Institutional); the architect of the policy aiming for high-volume scientific output and political capital.
 * - Mishap Investigation Board: Auditor (Analytical); post-mortem body identifying FBC as the cultural root of mission failures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: High extraction (0.80); FBC "extracted" safety margins, testing budgets, and redundant systems (capital) from missions to produce a higher *number* of launches, even if those launches failed at a higher rate.
domain_priors:base_extractiveness(nasa_faster_better_cheaper, 0.80).
% Rationale: High suppression (0.70); the philosophy actively suppressed traditional "safety-first" engineering alternatives by labeling them as "obsolete," "bureaucratic," or "inefficient."
domain_priors:suppression_score(nasa_faster_better_cheaper, 0.70).
% Rationale: Moderate theater (0.30); while focused on real metrics (launch tempo), the "Better" aspect became performative, prioritizing the appearance of innovation over robust engineering.
domain_priors:theater_ratio(nasa_faster_better_cheaper, 0.30).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, extractiveness, 0.80).
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, theater_ratio, 0.30).

% Constraint self-claim (what does the constraint claim to be?)
% FBC was presented as a pure coordination mechanism for efficiency.
narrative_ontology:constraint_claim(nasa_faster_better_cheaper, snare).
narrative_ontology:human_readable(nasa_faster_better_cheaper, "The \"Faster, Better, Cheaper\" (FBC) Management Paradigm").

% Binary flags
% Requires active enforcement via strict budgetary audits and schedule-driven performance reviews.
domain_priors:requires_active_enforcement(nasa_faster_better_cheaper).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(nasa_faster_better_cheaper, nasa_administration_metrics).
narrative_ontology:constraint_victim(nasa_faster_better_cheaper, mission_success_probability).
narrative_ontology:constraint_victim(nasa_faster_better_cheaper, engineering_safety_margins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE MISSION ENGINEER (MOUNTAIN)
% For the engineer, the budget cap is a Mountain. It is an unchangeable law
% of the project. If a test is required but not budgeted, it simply does
% not happen. The constraint is treated as an immovable physical limit.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE NASA ADMINISTRATOR (ROPE)
% Leadership viewed FBC as a Rope—a functional coordination tool to maximize
% scientific return on investment in a post-Cold War budget environment. They
% believed they were "weaving" a more efficient agency.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MISHAP INVESTIGATION BOARD (SNARE)
% The investigator sees the Snare. The constraint of "Cheaper" extracted
% the safety margin until the mission "choked" on the first unanticipated
% error (like the metric/English unit mismatch). The system guaranteed
% failure by removing the friction required to catch errors.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nasa_faster_better_cheaper_tests).

test(perspectival_gap) :-
    % Verify the gap between the engineer (Mountain) and administrator (Rope).
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == mountain),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify that the base extractiveness qualifies it as a high-extraction constraint.
    narrative_ontology:constraint_metric(nasa_faster_better_cheaper, extractiveness, E),
    assertion(E >= 0.46).

test(analytical_classification_is_snare) :-
    % Verify the analytical observer correctly identifies the Snare.
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == snare).

:- end_tests(nasa_faster_better_cheaper_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the core tension of FBC. The base extractiveness (0.80) is high because the policy's primary effect was to extract non-monetary capital—safety margins, testing time, and engineering redundancy—to fuel a non-functional metric (launch tempo). The suppression score (0.70) is high because this philosophy was mandated from the top down, actively penalizing traditional, more cautious engineering approaches.
 * The perspectival gap is profound:
 * - For the engineer, the budget is an absolute, unchangeable limit, appearing as a Mountain.
 * - For the administrator, the policy is a coordination tool (Rope) to achieve political and institutional goals.
 * - For the post-mortem analyst, the policy is a Snare that predictably led to failure by removing the system's ability to absorb errors.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This case is a classic example of a Snare masquerading as a Rope. The analytical classification as a Snare prevents the system from accepting the institutional claim of "coordination" at face value. The high extraction and suppression scores, combined with the documented mission failures, confirm that the primary function was not coordination but the extraction of technical integrity for the sake of performance metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fbc_better_metric,
    "Was 'Better' ever a measurable, non-rhetorical metric in FBC, or was it purely a marketing term to justify the extraction of safety margins?",
    "A quantitative comparison of scientific data returned per dollar spent during the FBC era versus pre/post-FBC eras, adjusted for mission complexity.",
    "If data/dollar was higher, FBC was a flawed but functional Tangled Rope. If lower or equivalent, FBC was a pure Snare where 'Better' was theatrical.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nasa_faster_better_cheaper, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the FBC policy over its lifecycle.
% As early missions succeeded under the new paradigm, pressure increased,
% leading to greater extraction of margins and a higher reliance on the
% appearance of success (theater).

% Theater ratio over time (metric_substitution drift):
narrative_ontology:measurement(fbc_tr_t0, nasa_faster_better_cheaper, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fbc_tr_t5, nasa_faster_better_cheaper, theater_ratio, 5, 0.20).
narrative_ontology:measurement(fbc_tr_t10, nasa_faster_better_cheaper, theater_ratio, 10, 0.30).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(fbc_ex_t0, nasa_faster_better_cheaper, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(fbc_ex_t5, nasa_faster_better_cheaper, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(fbc_ex_t10, nasa_faster_better_cheaper, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% FBC was implemented through top-down budgetary and scheduling mandates.
narrative_ontology:coordination_type(nasa_faster_better_cheaper, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */