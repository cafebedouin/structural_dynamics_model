% ============================================================================
% CONSTRAINT STORY: parkinsons_law
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_parkinsons_law, []).

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
 * * constraint_id: parkinsons_law
 * human_readable: Parkinson's Law
 * domain: organizational/economic
 * * SUMMARY:
 * Parkinson's Law states that "work expands so as to fill the time available for its completion." It describes the inherent tendency of bureaucracies to generate work for themselves and expand their personnel, regardless of the actual amount of productive work to be done. This extracts time and capital from productive ends to maintain the scale and status of the administrative class.
 * * KEY AGENTS:
 * - Productive Contributor: Subject (Powerless), whose time is consumed by administrative overhead.
 * - Middle Management: Beneficiary (Institutional), who uses the law to justify budget and headcount increases.
 * - Systems Auditor: Observer (Analytical), who sees both the coordination claims and the extractive reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(parkinsons_law, 0.50). % Extracts time and capital from productive ends.
domain_priors:suppression_score(parkinsons_law, 0.40).   % Alternatives (e.g., ROWE) are suppressed by the bureaucracy's self-preservation logic.
domain_priors:theater_ratio(parkinsons_law, 0.10).       % The work is genuinely performed, just not productive. Low theater.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(parkinsons_law, extractiveness, 0.50).
narrative_ontology:constraint_metric(parkinsons_law, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(parkinsons_law, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The bureaucracy claims its processes are necessary for coordination.
narrative_ontology:constraint_claim(parkinsons_law, tangled_rope).
narrative_ontology:human_readable(parkinsons_law, "Parkinson's Law").
narrative_ontology:topic_domain(parkinsons_law, "organizational/economic").

% Binary flags
domain_priors:requires_active_enforcement(parkinsons_law). % Required for Tangled Rope. Enforced via HR policies, performance reviews, etc.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(parkinsons_law, middle_management).
narrative_ontology:constraint_victim(parkinsons_law, productive_contributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The productive worker sees their time and energy consumed by non-productive tasks.
constraint_indexing:constraint_classification(parkinsons_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The manager uses the law as a coordination tool to justify resource allocation.
constraint_indexing:constraint_classification(parkinsons_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (for managers) and the
% asymmetric extraction (from producers), enforced by internal rules.
constraint_indexing:constraint_classification(parkinsons_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parkinsons_law_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(parkinsons_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parkinsons_law, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(tangled_rope_analytical_view) :-
    % The analytical view must resolve the conflict into a Tangled Rope.
    constraint_indexing:constraint_classification(parkinsons_law, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the base extractiveness is in the high-extraction range for Snare/Tangled Rope.
    narrative_ontology:constraint_metric(parkinsons_law, extractiveness, E),
    E >= 0.46.

:- end_tests(parkinsons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Parkinson's Law is a classic example of a perspectivally variant constraint.
 * - To the 'Productive Contributor' (powerless, trapped), the endless expansion of administrative tasks is a Snare that consumes their time and prevents meaningful work.
 * - To 'Middle Management' (institutional, mobile), this same expansion is a Rope—a coordination mechanism used to justify larger budgets and more subordinates, which is the primary path to career advancement in a bureaucracy.
 * - The Analytical observer, seeing both sides, classifies it as a Tangled Rope. It has a genuine coordination function (resource acquisition for the managerial class) but also imposes a severe asymmetric extraction (wasted time and effort for the productive class). This duality is its defining feature. The claim that it is a 'Mountain' (an inevitable law of nature) is a common misinterpretation that masks the agency of the beneficiaries who perpetuate it.
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical for resolving Mandatrophy. A naive analysis might label Parkinson's Law a pure Snare, ignoring the real coordination benefits it provides to managers. Conversely, accepting the managerial view would misclassify it as a benign Rope. The Tangled Rope acknowledges both the coordination claim and the extractive reality, correctly identifying it as a constructed system with winners and losers, rather than an immutable law or a purely predatory structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_parkinsons_law,
    'Will AI and automation break the personnel expansion loop, or simply create new, more complex administrative "oversight" work?',
    'Longitudinal tracking of administrative-to-production headcount ratios in large organizations post-AI implementation.',
    'If the loop breaks, the law may degrade into a Piton or be replaced. If it adapts, it remains a durable Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(parkinsons_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Parkinson's Law emerges and intensifies as an organization matures and
% its bureaucracy calcifies. Extraction grows as more processes are added.
% Theater remains low because the administrative work is genuinely performed.

% Theater ratio over time (remains low):
narrative_ontology:measurement(parkinsons_law_tr_t0, parkinsons_law, theater_ratio, 0, 0.05).
narrative_ontology:measurement(parkinsons_law_tr_t5, parkinsons_law, theater_ratio, 5, 0.08).
narrative_ontology:measurement(parkinsons_law_tr_t10, parkinsons_law, theater_ratio, 10, 0.10).

% Extraction over time (increases as bureaucracy grows):
narrative_ontology:measurement(parkinsons_law_ex_t0, parkinsons_law, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(parkinsons_law_ex_t5, parkinsons_law, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(parkinsons_law_ex_t10, parkinsons_law, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The law's function is to justify and manage the allocation of time and budget.
narrative_ontology:coordination_type(parkinsons_law, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */