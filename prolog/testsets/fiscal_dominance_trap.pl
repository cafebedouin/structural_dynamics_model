% ============================================================================
% CONSTRAINT STORY: fiscal_dominance_trap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_fiscal_dominance_trap, []).

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
 * * constraint_id: fiscal_dominance_trap
 * human_readable: The Debt-Monetary Bind
 * domain: economic/political
 * * SUMMARY:
 * This constraint represents a state where a central bank is no longer able
 * to act independently to control inflation because doing so would cause
 * the government's debt servicing costs to explode, leading to insolvency.
 * Monetary policy becomes a "Rope" for government survival at the cost of
 * being a "Snare" for the public's purchasing power through forced inflation.
 * * KEY AGENTS:
 * - Fixed-Income Saver: Subject (Powerless)
 * - Treasury Department: Beneficiary (Institutional)
 * - Monetary Policy Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) because the system liquidates the real value of
% private savings to subsidize public debt, siphoning the subject's
% long-term security.
domain_priors:base_extractiveness(fiscal_dominance_trap, 0.87).
domain_priors:suppression_score(fiscal_dominance_trap, 0.72).
domain_priors:theater_ratio(fiscal_dominance_trap, 0.85). % High theater: maintaining the "illusion" of central bank independence.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(fiscal_dominance_trap, extractiveness, 0.87).
narrative_ontology:constraint_metric(fiscal_dominance_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fiscal_dominance_trap, theater_ratio, 0.85).

% Constraint self-claim: The system claims to be necessary coordination for stability.
narrative_ontology:constraint_claim(fiscal_dominance_trap, piton).
narrative_ontology:human_readable(fiscal_dominance_trap, "The Debt-Monetary Bind").
narrative_ontology:topic_domain(fiscal_dominance_trap, "economic/political").

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(fiscal_dominance_trap).
narrative_ontology:constraint_beneficiary(fiscal_dominance_trap, treasury_department).
narrative_ontology:constraint_victim(fiscal_dominance_trap, fixed_income_savers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their savings lose value as inflation is
% permitted to rise, yet the "safe" assets they hold are the very
% instruments being used to sustain the trap.
constraint_indexing:constraint_classification(fiscal_dominance_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Treasury views this as a Rope—the essential coordination of
% monetary and fiscal policy required to avoid a catastrophic
% sovereign default and systemic collapse.
constraint_indexing:constraint_classification(fiscal_dominance_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: "Independent Monetary
% Policy" is an inertial spike of institutional rhetoric masking
% the reality of fiscal subjugation.
constraint_indexing:constraint_classification(fiscal_dominance_trap, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The canonical analytical view recognizes the hybrid nature: a coordination
% function (avoiding default) exists alongside severe asymmetric extraction
% (inflation tax), requiring active enforcement. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(fiscal_dominance_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_dominance_trap_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless saver vs Rope for the institutional treasury.
    constraint_indexing:constraint_classification(fiscal_dominance_trap, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_dominance_trap, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_dominance_trap, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater (0.85) results in Piton detection by systems auditors.
    constraint_indexing:constraint_classification(fiscal_dominance_trap, piton,
        context(agent_power(analytical), time_horizon(historical), _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.87) is high enough to trigger mandatory resolution logic.
    domain_priors:base_extractiveness(fiscal_dominance_trap, E),
    E > 0.70.

:- end_tests(fiscal_dominance_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the
 * preservation of the institutional status quo (avoiding sovereign default)
 * is achieved by siphoning the life-savings of the subject population via
 * an inflation tax. The suppression score (0.72) represents the lack of
 * viable alternative stores of value for the average saver. The high theater
 * ratio (0.85) captures the performative nature of central bank "independence"
 * announcements while its actions are fiscally constrained.
 *
 * PERSPECTIVAL GAP:
 * The Fixed-Income Saver feels a Snare because they pay for government
 * solvency through a hidden and unlegislated tax of inflation. The Treasury
 * sees a Rope because without this coordination, the government would default,
 * triggering a potentially greater systemic crisis.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the Tangled Rope classification from the
 * analytical perspective. This classification correctly identifies that the
 * system is neither a pure Rope (as the beneficiary claims) nor a pure Snare
 * (as the victim experiences). It is a hybrid system with a genuine, if
 * self-serving, coordination function (preventing default) that is structurally
 * dependent on severe, asymmetric extraction. The additional Piton classification
 * from the auditor's view reinforces this by showing the original claimed
 * function (independent monetary policy) has decayed into pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_fiscal_dominance_trap,
    'At what level of inflation does the public lose confidence entirely, causing the trap to collapse (Snare vs Mountain of social physics)?',
    'Tracking historical correlations between real interest rates, currency velocity, and social unrest.',
    'If order persists: Mountain of Social Compliance. If order breaks: Snare of Miscalculation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fiscal_dominance_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This trap develops over time as debt accumulates, forcing monetary policy
% to become subservient. Initially, coordination is genuine with low extraction.
% Over the interval, extraction and theater ramp up as the trap tightens.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(fdt_tr_t0, fiscal_dominance_trap, theater_ratio, 0, 0.20).
narrative_ontology:measurement(fdt_tr_t5, fiscal_dominance_trap, theater_ratio, 5, 0.55).
narrative_ontology:measurement(fdt_tr_t10, fiscal_dominance_trap, theater_ratio, 10, 0.85).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fdt_ex_t0, fiscal_dominance_trap, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(fdt_ex_t5, fiscal_dominance_trap, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(fdt_ex_t10, fiscal_dominance_trap, base_extractiveness, 10, 0.87).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: This is fundamentally a mechanism for allocating the
% resource cost of sovereign debt across the population.
narrative_ontology:coordination_type(fiscal_dominance_trap, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */