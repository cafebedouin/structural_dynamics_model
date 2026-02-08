% ============================================================================
% CONSTRAINT STORY: peter_principle
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-21
% ============================================================================

:- module(constraint_peter_principle, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: peter_principle
 * human_readable: The Peter Principle (Promotion to Incompetence)
 * domain: organizational/social
 * * SUMMARY:
 * The Peter Principle states that "in a hierarchy, every employee tends to rise
 * to their level of incompetence." People are promoted based on performance in
 * their current role, not aptitude for the next, eventually reaching a position
 * where they are ineffective and remain, extracting value (salary, status) while
 * reducing overall organizational utility.
 * * KEY AGENTS:
 * - The Incompetent Manager: Subject (Powerless), trapped in a role they cannot perform.
 * - The Organization's HR/Leadership: Beneficiary (Institutional), using the promotion ladder as a standard coordination tool.
 * - The Systems Analyst: Auditor (Analytical), observing the dysfunctional hybrid of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(peter_principle, 0.55). % Extracts organizational efficiency and subordinate well-being. Snare >= 0.46.
domain_priors:suppression_score(peter_principle, 0.60).   % Alternatives (demotion without stigma, dual-track careers) are culturally suppressed.
domain_priors:theater_ratio(peter_principle, 0.20).       % Low theater; the dysfunction is real, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(peter_principle, extractiveness, 0.55).
narrative_ontology:constraint_metric(peter_principle, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(peter_principle, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% It's presented as a constructed, meritocratic system of advancement.
narrative_ontology:constraint_claim(peter_principle, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(peter_principle). % Required for Tangled Rope. Enforced by HR policy and cultural norms.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(peter_principle, senior_management_who_benefit_from_stable_hierarchy).
narrative_ontology:constraint_victim(peter_principle, organizational_efficiency_and_subordinates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The manager promoted to their level of incompetence. They are trapped,
% unable to perform effectively but also unable to be demoted without severe
% social and financial penalty. The system extracts their well-being and their
% team's productivity.
constraint_indexing:constraint_classification(peter_principle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% From the perspective of the institutional hierarchy (e.g., HR, senior leadership),
% the promotion ladder is a standard, necessary coordination mechanism (a Rope)
% for incentivizing performance and filling roles. The negative externalities are
% accepted as a cost of doing business.
constraint_indexing:constraint_classification(peter_principle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (the Rope of career progression)
% and the severe asymmetric extraction (the Snare of promoting incompetence).
% Because it has both functions and requires active enforcement through cultural
% norms and HR policy, it is a Tangled Rope.
constraint_indexing:constraint_classification(peter_principle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peter_principle_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(peter_principle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(peter_principle, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(peter_principle, ExtMetricName, E),
    E >= 0.46. % Ensures it qualifies as a high-extraction constraint.

:- end_tests(peter_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Peter Principle is a classic example of a Tangled Rope. It has a genuine
 * coordination function: the promotion ladder is how organizations structure
 * ambition and allocate human resources. This is why institutional agents see it
 * as a Rope. However, its core mechanism—promoting based on past, not future,
 * performance—creates a severe asymmetric extraction. Value is extracted from
 * the organization's overall efficiency and from the well-being of both the
 * over-promoted manager and their subordinates. This is the Snare felt by the
 * powerless. The analytical view must account for both, hence Tangled Rope.
 *
 * The base extractiveness is set to 0.55, reflecting significant harm to
 * organizational utility. Suppression is 0.60 because alternatives like
 * dual-track career paths or graceful demotions are strongly resisted by
 * cultural norms that equate promotion with success.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a pure Snare would be a mistake, as it would ignore the
 * coordination function that gives the system its stability and perceived
 * legitimacy. The Tangled Rope classification correctly identifies that the
 * system is a hybrid, preventing Mandatrophy by acknowledging that the
 * participants (like HR) are not simply malicious but are operating a flawed
 * coordination system. The path to resolution is not to destroy the ladder,
 * but to untangle it—for instance, by changing promotion criteria.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is whether this is an emergent, unavoidable property of
% any hierarchy (a Mountain) or a culturally constructed one that can be fixed (a Tangled Rope).
omega_variable(
    omega_peter_principle,
    'Is the Peter Principle an inevitable emergent property of all large hierarchies, or a contingent outcome of specific cultural assumptions about promotion and success?',
    'Comparative analysis of organizations with radically different promotion structures (e.g., flat hierarchies, dual-track careers, project-based roles) vs. traditional ones.',
    'If inevitable, it is a Mountain of social physics. If contingent, it is a Tangled Rope that can be redesigned.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(peter_principle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a high-extraction constraint (E > 0.46).
% Models the principle becoming more entrenched in bureaucratic culture over time,
% with a slight increase in extraction as organizational slack is reduced.
%
% Theater ratio over time (remains low):
narrative_ontology:measurement(peter_principle_tr_t0, peter_principle, theater_ratio, 0, 0.15).
narrative_ontology:measurement(peter_principle_tr_t5, peter_principle, theater_ratio, 5, 0.18).
narrative_ontology:measurement(peter_principle_tr_t10, peter_principle, theater_ratio, 10, 0.20).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(peter_principle_ex_t0, peter_principle, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(peter_principle_ex_t5, peter_principle, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(peter_principle_ex_t10, peter_principle, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The promotion ladder is a primary mechanism for allocating status, salary,
% and responsibility within an organization.
narrative_ontology:coordination_type(peter_principle, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */