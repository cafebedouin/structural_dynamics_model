% ============================================================================
% CONSTRAINT STORY: responsibility_without_power
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_responsibility_without_power, []).

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
 * * constraint_id: responsibility_without_power
 * human_readable: The Scapegoat Architecture
 * domain: organizational/legal/socio-economic
 * * SUMMARY:
 * A scenario where a "Rope" for maintaining system safety or ethical standards
 * (e.g., liability for automated errors, compliance for supply chains, or
 * middle-management performance) assigns legal or moral responsibility to a
 * subject who lacks the actual power or tools to control the outcome.
 * This coordination substrate becomes a "Snare" as the subject's personal
 * assets and career agency are liquidated to absorb systemic shocks,
 * trapping them in a territory of terminal liability where they are
 * punished for failures inherent to a design they cannot modify.
 *
 * * KEY AGENTS:
 * - Compliance Officer: Subject (Powerless)
 * - Strategic Architect: Beneficiary (Institutional)
 * - Liability Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.91) reflects the total liquidation of the subject's
% individual security to subsidize the institution's risk profile.
domain_priors:base_extractiveness(responsibility_without_power, 0.91).
domain_priors:suppression_score(responsibility_without_power, 0.82). % "Protected" roles or whistleblowing are suppressed by the structural hierarchy.
domain_priors:theater_ratio(responsibility_without_power, 0.93).    % Extreme theater: "Empowerment Seminars" that performatively signal agency while siphoning power.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(responsibility_without_power, extractiveness, 0.91).
narrative_ontology:constraint_metric(responsibility_without_power, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(responsibility_without_power, theater_ratio, 0.93).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be an enforcement mechanism for compliance, but is a constructed trap.
narrative_ontology:constraint_claim(responsibility_without_power, tangled_rope).
narrative_ontology:human_readable(responsibility_without_power, "The Scapegoat Architecture").
narrative_ontology:topic_domain(responsibility_without_power, "organizational/legal/socio-economic").

% Binary flags
domain_priors:requires_active_enforcement(responsibility_without_power). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(responsibility_without_power, strategic_architects).
narrative_ontology:constraint_victim(responsibility_without_power, compliance_officers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The officer is trapped: they are legally responsible for the system's
% failures, but the lack of power liquidates their primary defensive agency.
constraint_indexing:constraint_classification(responsibility_without_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the assignment as a Rope—the essential coordination
% substrate for ensuring that *someone* is always "on the hook" to satisfy regulators.
constraint_indexing:constraint_classification(responsibility_without_power, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the combination of a genuine coordination function (satisfying regulators)
% with severe asymmetric extraction (liquidating the subject's agency) and active
% enforcement. The high theater ratio (0.93) indicates the coordination function
% is decaying into pure performance.
constraint_indexing:constraint_classification(responsibility_without_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_power_tests).

test(perspectival_gap_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap: Snare for the powerless, Rope for the institutional.
    constraint_indexing:constraint_classification(responsibility_without_power, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_without_power, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope, [nondet]) :-
    % Verify the analytical observer correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(responsibility_without_power, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % A Tangled Rope requires beneficiaries, victims, and active enforcement.
    narrative_ontology:constraint_beneficiary(responsibility_without_power, _),
    narrative_ontology:constraint_victim(responsibility_without_power, _),
    domain_priors:requires_active_enforcement(responsibility_without_power).

:- end_tests(responsibility_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.91) reflects a "Mandatrophy" state where the
 * "coordination" benefit of legal legibility is achieved by liquidating the
 * subject's primary capacity for personal safety. The high suppression (0.82)
 * and theater (0.93) scores confirm that this is not a bug, but a feature of
 * the architecture, designed to transfer risk downwards.
 *
 * * PERSPECTIVAL GAP:
 * The Compliance Officer feels a Snare because they are an insurance policy
 * with no control over the risk. The Strategic Architect sees a Rope
 * because the transfer coordinates the external legibility needed to
 * maintain the institution's license to operate.
 *
 * * [RESOLVED MANDATROPHY]:
 * The high extraction (0.91) could be misread as a necessary cost of coordination (a flawed Rope).
 * The Tangled Rope classification resolves this by structurally requiring both a coordination
 * function (beneficiary exists) and asymmetric extraction (victim exists), preventing the system
 * from collapsing the analysis into a pure Snare (which would ignore the coordination claim) or
 * a pure Rope (which would ignore the extraction). The extreme theater ratio (0.93) further
 * explains the mechanism: performative compliance has replaced actual control, allowing the
 * extraction to persist under the guise of legitimate governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_liability_decoupling,
    'Can "No-Fault Insurance" restore the Rope, or is scapegoating a physical law of hierarchies (Snare vs Mountain)?',
    'Tracking the delta between "Assigned Responsibility" and "Control Access" in 2026-style gig platforms.',
    'If insurance restores agency: Snare of current design. If it fails: Mountain of Organizational Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(responsibility_without_power, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system began as a flawed but functional risk management tool and
% degraded over time as loopholes were exploited to transfer, rather than
% mitigate, risk.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rwp_tr_t0, responsibility_without_power, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rwp_tr_t5, responsibility_without_power, theater_ratio, 5, 0.60).
narrative_ontology:measurement(rwp_tr_t10, responsibility_without_power, theater_ratio, 10, 0.93).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rwp_ex_t0, responsibility_without_power, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(rwp_ex_t5, responsibility_without_power, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rwp_ex_t10, responsibility_without_power, base_extractiveness, 10, 0.91).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's coordination function is to enforce a liability framework.
narrative_ontology:coordination_type(responsibility_without_power, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(responsibility_without_power, 0.1).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(responsibility_without_power, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */