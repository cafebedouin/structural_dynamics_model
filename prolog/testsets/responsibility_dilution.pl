% ============================================================================
% CONSTRAINT STORY: responsibility_dilution
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_responsibility_dilution, []).

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
 * * constraint_id: responsibility_dilution
 * human_readable: The Accountability Fog
 * domain: organizational/legal/technological
 * * SUMMARY:
 * A scenario where a critical decision is fragmented across so many
 * autonomous agents, bureaucratic layers, and algorithmic filters that
 * the "locus of responsibility" effectively vanishes. This "Rope" for
 * achieving high-scale institutional coordination becomes a "Snare" for
 * the subject (victim), as their grievance is liquidated by a system where
 * "everyone followed protocol, but the harm occurred," trapping them in
 * a territory of unaddressable injury with no legal or moral recourse.
 * * KEY AGENTS:
 * - Harmed Individual: Subject (Powerless)
 * - Distributed Organization: Beneficiary (Institutional)
 * - Forensic Governance Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.90) reflects the near-total liquidation of the subject's
% right to redress to maintain the institution's "risk-minimized" status.
domain_priors:base_extractiveness(responsibility_dilution, 0.90).
domain_priors:suppression_score(responsibility_dilution, 0.83).   % Individual blame is suppressed by the complexity of the "System."
domain_priors:theater_ratio(responsibility_dilution, 0.91).       % Extreme theater: "Ethics Committees" and "Compliance Audits" that cannot assign liability.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(responsibility_dilution, extractiveness, 0.90).
narrative_ontology:constraint_metric(responsibility_dilution, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(responsibility_dilution, theater_ratio, 0.91).

% Constraint self-claim: The system claims to be a necessary coordination mechanism.
narrative_ontology:constraint_claim(responsibility_dilution, tangled_rope).
narrative_ontology:human_readable(responsibility_dilution, "The Accountability Fog").

% Binary flags and structural properties required for Tangled Rope.
% The "enforcement" is the active maintenance of the bureaucratic protocols
% that ensure accountability is diffused.
domain_priors:requires_active_enforcement(responsibility_dilution).

% Structural property derivation hooks for Tangled Rope.
narrative_ontology:constraint_beneficiary(responsibility_dilution, distributed_organization).
narrative_ontology:constraint_victim(responsibility_dilution, harmed_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped: they have suffered a clear harm, but the
% system's architecture liquidates their ability to point to a responsible actor.
constraint_indexing:constraint_classification(responsibility_dilution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the dilution as a Rope—the only way to coordinate
% massive, high-risk operations without the "friction" of individual liability.
constraint_indexing:constraint_classification(responsibility_dilution, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.90) and high suppression (0.83) masking as
% essential coordination. The system has both a coordination function and
% asymmetric extraction, and requires active enforcement of its protocols.
constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_dilution_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless victim vs Rope for the institutional hierarchy.
    constraint_indexing:constraint_classification(responsibility_dilution, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_dilution, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_conditions_met) :-
    % Verify that all structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(responsibility_dilution, _),
    narrative_ontology:constraint_victim(responsibility_dilution, _),
    domain_priors:requires_active_enforcement(responsibility_dilution).

test(high_extraction_and_suppression) :-
    % Verify the base metrics meet the thresholds for a severe Tangled Rope.
    domain_priors:base_extractiveness(responsibility_dilution, E), E >= 0.50,
    domain_priors:suppression_score(responsibility_dilution, S), S > 0.40.

:- end_tests(responsibility_dilution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.90) reflects a "Mandatrophy" state where the
 * "coordination" benefit of large-scale organization is achieved by liquidating
 * the primary accountability agency of the affected subject. The high theater
 * ratio (0.91) does not trigger a Piton classification because the constraint's
 * primary coordination function has not atrophied; rather, the theatrical
 * performance of compliance and ethics is an integral part of the extraction
 * mechanism, serving to legitimize the dilution of responsibility.
 *
 * * PERSPECTIVAL GAP:
 * The Harmed Individual feels a Snare because they are screaming into a vacuum
 * of "automated workflows." The Organization sees a Rope because the
 * fragmentation of liability coordinates the massive risk-taking necessary
 * for modern industry.
 *
 * * [RESOLVED MANDATROPHY]:
 * The system's claim to be a 'Rope' is contradicted by the extreme extraction
 * felt by the subject. Mandatrophy is resolved by the Tangled Rope classification,
 * which correctly identifies the constraint as a hybrid: it possesses a genuine
 * coordination function (beneficiary view) but simultaneously imposes severe,
 * asymmetric extraction (subject view). This avoids misclassifying it as a pure
 * Snare (which would ignore the coordination) or a pure Rope (which would ignore
 * the harm).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_traceable_agency,
    'Can blockchain-based logging restore the Rope, or is dilution a physical law of bureaucracy (Snare vs Mountain)?',
    'Tracking the success rate of "immutable audit trails" in assigning individual legal blame in 2030.',
    'If blame sticks: Snare of current opacity. If it fails: Mountain of Organizational Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(responsibility_dilution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the system's degradation from a functional coordination mechanism
% into a highly extractive and theatrical one.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rd_tr_t0, responsibility_dilution, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rd_tr_t5, responsibility_dilution, theater_ratio, 5, 0.50).
narrative_ontology:measurement(rd_tr_t10, responsibility_dilution, theater_ratio, 10, 0.91).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rd_ex_t0, responsibility_dilution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rd_ex_t5, responsibility_dilution, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rd_ex_t10, responsibility_dilution, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's coordination function is the enforcement of bureaucratic
% protocols that diffuse liability.
narrative_ontology:coordination_type(responsibility_dilution, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */