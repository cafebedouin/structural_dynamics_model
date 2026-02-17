% ============================================================================
% CONSTRAINT STORY: cognitive_diversity_arbitrage
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_cognitive_diversity_arbitrage, []).

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
 * * constraint_id: cognitive_diversity_arbitrage
 * human_readable: Cognitive Diversity Arbitrage in the Workplace
 * domain: economic/social
 * * SUMMARY:
 * This constraint describes the modern corporate practice of reframing neurodiversity
 * (e.g., autism, ADHD) from a medical "disorder" to a strategic asset. Firms
 * "arbitrage" the unique cognitive strengths of neurodivergent individuals to
 * gain a competitive advantage. This creates a tension between genuine inclusion
 * and the potential for high-value cognitive extraction without commensurate support.
 * * KEY AGENTS:
 * - Neurodivergent Employee: The subject, whose unique cognitive skills are valued but who may lack adequate support, forcing them to "mask".
 * - Arbitraging Corporation: The beneficiary, which gains innovation and productivity by leveraging a diverse workforce.
 * - Traditional Manager: An agent who still views neurotypicality as the default "normal," perceiving diversity as a problem to manage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cognitive_diversity_arbitrage, 0.72). % High extraction from specialized cognitive labor.
domain_priors:suppression_score(cognitive_diversity_arbitrage, 0.45).   % Moderate suppression from lingering social/corporate pressure to conform.
domain_priors:theater_ratio(cognitive_diversity_arbitrage, 0.15).       % Low theater; this is a functional, not performative, system.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, extractiveness, 0.72).
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(cognitive_diversity_arbitrage, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The corporate framing is that this is a beneficial coordination mechanism.
narrative_ontology:constraint_claim(cognitive_diversity_arbitrage, tangled_rope).
narrative_ontology:human_readable(cognitive_diversity_arbitrage, "Cognitive Diversity Arbitrage in the Workplace").

% Binary flags
domain_priors:requires_active_enforcement(cognitive_diversity_arbitrage). % Requires active HR policies, management training, and support systems.

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(cognitive_diversity_arbitrage, arbitraging_corporation).
narrative_ontology:constraint_victim(cognitive_diversity_arbitrage, unsupported_neurodivergent_employee).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The unsupported neurodivergent employee who must "mask" to fit in. The
% pressure to conform while their unique skills are extracted feels like a trap.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The corporation views this as a pure coordination benefit—a rope for harnessing
% diverse talent to achieve better outcomes for everyone. Extraction is framed as
% productivity and is not felt by the institution.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the genuine coordination function (beneficiaries exist) and
% the asymmetric extraction (victims exist), supported by active enforcement.
% This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE TRADITIONAL MANAGER (MOUNTAIN)
% A manager operating under the old paradigm sees the "normal brain" as a
% fixed, natural law. Deviations are not assets but defects, making the
% pressure to conform seem like an unchangeable feature of reality.
constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, mountain,
    context(agent_power(powerful),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_diversity_arbitrage_tests).

test(perspectival_gap_subject_beneficiary) :-
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_observer_detects_tangled_rope) :-
    constraint_indexing:constraint_classification(cognitive_diversity_arbitrage, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three conditions for Tangled Rope are present in the file.
    narrative_ontology:constraint_beneficiary(cognitive_diversity_arbitrage, _),
    narrative_ontology:constraint_victim(cognitive_diversity_arbitrage, _),
    domain_priors:requires_active_enforcement(cognitive_diversity_arbitrage).

:- end_tests(cognitive_diversity_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.72 reflects the high economic value of specialized
 * cognitive labor, which can be disproportionate to the salary or support provided.
 * The suppression score of 0.45 captures the persistent cultural pressure for
 * neurotypical behavior, even in workplaces that claim to be inclusive.
 *
 * The Perspectival Gap is stark:
 * - The Corporation (Institutional) sees a pure 'Rope'—a coordination tool for innovation.
 * - The Employee (Powerless) experiences a 'Snare'—their value is extracted while they
 *   are trapped by the need to perform neurotypicality ("masking").
 * - The Analyst sees the truth: a 'Tangled Rope' that combines a real coordination
 *   benefit with significant, asymmetric extraction.
 *
 * [RESOLVED MANDATROPHY]: The Tangled Rope classification correctly resolves the
 * mandatrophy by acknowledging both the coordination function claimed by the
 * beneficiary and the extractive harm experienced by the subject, preventing a
 * misclassification as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_cognitive_diversity_arbitrage_1,
    "Is the corporate harvesting of 'unique strengths' a form of predatory cognitive extraction or a genuinely collaborative asset realization?",
    "Audit of salary, promotion, and support ratios for neurodivergent vs. neurotypical roles with similar output value.",
    "If predatory -> Snare; If collaborative -> Rope. The current state is ambiguous, hence Tangled Rope.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_cognitive_diversity_arbitrage_2,
    "Does 'proper support' actually neutralize the challenges, or just mask them enough for corporate benefit while leading to long-term burnout?",
    "Longitudinal studies of burnout and retention rates among neurodivergent employees in 'inclusive' vs. traditional workplaces.",
    "If it neutralizes -> moves toward Rope. If it masks -> remains a Snare from the subject's view.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cognitive_diversity_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the evolution of the practice. Initially, the concept was less
% understood, so extraction was lower. As firms optimized the "arbitrage,"
% extraction increased. Theater remains low as it's a functional strategy.

% Theater ratio over time (remains low and stable):
narrative_ontology:measurement(cda_tr_t0, cognitive_diversity_arbitrage, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cda_tr_t5, cognitive_diversity_arbitrage, theater_ratio, 5, 0.12).
narrative_ontology:measurement(cda_tr_t10, cognitive_diversity_arbitrage, theater_ratio, 10, 0.15).

% Extraction over time (increases as the "arbitrage" becomes more efficient):
narrative_ontology:measurement(cda_ex_t0, cognitive_diversity_arbitrage, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cda_ex_t5, cognitive_diversity_arbitrage, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(cda_ex_t10, cognitive_diversity_arbitrage, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is a form of human capital management.
narrative_ontology:coordination_type(cognitive_diversity_arbitrage, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */