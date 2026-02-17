% ============================================================================
% CONSTRAINT STORY: israel_norwegian_law
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_israel_norwegian_law, []).

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
 * * constraint_id: israel_norwegian_law
 * human_readable: The Norwegian Law (Amendment to Article 42c)
 * domain: political
 * * SUMMARY:
 * A mechanism allowing ministers to resign from the Knesset to focus on
 * executive duties, replaced by the next list member. While it expands
 * committee capacity, it creates "conditional" MKs whose tenure depends
 * entirely on the minister's cabinet status, effectively extracting their
 * political independence and incurring significant public cost.
 * * KEY AGENTS:
 * - Replacement MK: Subject (Powerless) - Vulnerable to the "revolving door."
 * - Coalition Leadership: Beneficiary (Institutional) - Staffs committees, maintains majority.
 * - Public Auditor: Auditor (Analytical) - Tracks fiscal and representative costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.52) due to the NIS 1.95M annual cost per new MK
% and the extraction of voting independence from replacement MKs.
domain_priors:base_extractiveness(israel_norwegian_law, 0.52).

% Suppression of alternatives is high; the system mandates a 120-seat cap,
% making this the primary bypass for cabinet-heavy coalitions.
domain_priors:suppression_score(israel_norwegian_law, 0.70).

domain_priors:theater_ratio(israel_norwegian_law, 0.25). % Functional governance bypass.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(israel_norwegian_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(israel_norwegian_law, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(israel_norwegian_law, theater_ratio, 0.25).

% Constraint self-claim: It is presented as a necessary tool for governance.
narrative_ontology:constraint_claim(israel_norwegian_law, tangled_rope).
narrative_ontology:human_readable(israel_norwegian_law, "The Norwegian Law (Amendment to Article 42c)").

% Binary flags & Structural properties
domain_priors:requires_active_enforcement(israel_norwegian_law).

% Structural property derivation hooks (FIX for Linter):
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(israel_norwegian_law, coalition_leadership).
narrative_ontology:constraint_victim(israel_norwegian_law, replacement_mk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE REPLACEMENT MK (SNARE)
% Viewed as a precarious trap where independence is traded for a seat
% that can be revoked at any moment by a minister's whim.
constraint_indexing:constraint_classification(israel_norwegian_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE COALITION (ROPE)
% Viewed as essential infrastructure to prevent the "onerous" workload
% of staffing committees in a small 120-member parliament.
constraint_indexing:constraint_classification(israel_norwegian_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: a genuine coordination function (staffing committees)
% coupled with asymmetric extraction (fiscal cost and loss of MK independence).
constraint_indexing:constraint_classification(israel_norwegian_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_norwegian_law_tests).

test(perspectival_gap) :-
    % Verify the law is a Snare for the vulnerable MK but a Rope for the leadership.
    constraint_indexing:constraint_classification(israel_norwegian_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_norwegian_law, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(israel_norwegian_law, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify that base extraction meets the high-extraction threshold for Tangled Rope/Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(israel_norwegian_law, ExtMetricName, E),
    E >= 0.46.

:- end_tests(israel_norwegian_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.52 reflects both the direct fiscal cost (over
 * NIS 35M annually for 18 MKs in 2023) and the indirect extraction of political
 * independence from replacement MKs. Suppression (0.70) is high because the
 * 120-seat Knesset cap is a hard limit, making this law the only viable
 * mechanism for large cabinets to maintain legislative capacity. The perspectival
 * gap is stark: for coalition leadership, it's a necessary coordination tool
 * (Rope), but for the replacement MK, it's a precarious position entirely
 * dependent on another's political fate (Snare).
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. A simpler model might
 * classify this as a pure Snare based on the high extraction. However, that
 * would ignore its genuine, albeit costly, coordination function in solving
 * the problem of ministers being unable to attend to both executive and
 * legislative duties effectively. The Tangled Rope correctly identifies that
 * a valid coordination goal is being achieved through a mechanism with
 * significant, asymmetrically borne costs. This prevents mislabeling a
 * functional (if flawed) governance tool as pure predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_israel_norwegian_law,
    'Does the law increase committee efficiency enough to offset the loss of direct ministerial oversight in the plenary and the fiscal cost?',
    'Comparative audit of committee legislative output and attendance vs. plenary question time frequency and public expenditure data.',
    'Determines if the coordination benefit outweighs the extraction, shifting the balance between a justifiable Tangled Rope and a wasteful Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(israel_norwegian_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the law's expansion over time. Initially used
% sparingly, its application grew, increasing both its coordination function
% and its extractive cost. This is required as base_extractiveness > 0.46.

% Theater ratio over time (remains low as the function is real):
narrative_ontology:measurement(inl_tr_t0, israel_norwegian_law, theater_ratio, 0, 0.10).
narrative_ontology:measurement(inl_tr_t5, israel_norwegian_law, theater_ratio, 5, 0.18).
narrative_ontology:measurement(inl_tr_t10, israel_norwegian_law, theater_ratio, 10, 0.25).

% Extraction over time (grows with increased use and cost):
narrative_ontology:measurement(inl_ex_t0, israel_norwegian_law, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(inl_ex_t5, israel_norwegian_law, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(inl_ex_t10, israel_norwegian_law, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The law is a mechanism for allocating legislative roles.
narrative_ontology:coordination_type(israel_norwegian_law, resource_allocation).

% Network relationships: This law directly impacts coalition stability and
% the passage of other legislation, such as budget approvals.
% narrative_ontology:affects_constraint(israel_norwegian_law, coalition_stability_pact).
% narrative_ontology:affects_constraint(israel_norwegian_law, annual_budget_approval).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */