% ============================================================================
% CONSTRAINT STORY: challenger_o_ring_integrity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_challenger_o_ring_integrity, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: challenger_o_ring_integrity
 *   human_readable: Challenger SRB O-Ring Integrity Failure (Institutional)
 *   domain: technological/organizational_failure
 *
 * SUMMARY:
 *   The 1986 Challenger disaster resulted from an institutional failure to
 *   respect a physical one. This constraint story models the institutional
 *   system of management pressure, suppressed engineering dissent, and 'go
 *   fever' that led to the launch. It is structurally downstream of a
 *   separate, Mountain-type constraint: the physical properties of Viton
 *   rubber at low temperatures. The institutional system leveraged the
 *   unforgiving nature of the physical constraint to produce a catastrophe.
 *   The high extractiveness (0.85) and suppression (0.90) reflect management
 *   actively overruling engineers' explicit 'do not launch' warnings.
 *
 * KEY AGENTS:
 *   - NASA & Thiokol Management: Primary beneficiaries (institutional/arbitrage) — sought to maintain launch schedules and contractual relationships.
 *   - Challenger Crew: Primary victims (powerless/trapped) — bore the ultimate cost of the decision.
 *   - Morton Thiokol Engineers: Secondary victims (organized/constrained) — understood the risk but were institutionally overruled.
 *   - The Rogers Commission: Analytical observer (analytical/analytical) — post-hoc investigation that revealed the full institutional failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(challenger_o_ring_integrity, 0.85).
domain_priors:suppression_score(challenger_o_ring_integrity, 0.9).
domain_priors:theater_ratio(challenger_o_ring_integrity, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(challenger_o_ring_integrity, extractiveness, 0.85).
narrative_ontology:constraint_metric(challenger_o_ring_integrity, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(challenger_o_ring_integrity, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(challenger_o_ring_integrity, snare).
narrative_ontology:human_readable(challenger_o_ring_integrity, "Challenger SRB O-Ring Integrity Failure (Institutional)").
narrative_ontology:topic_domain(challenger_o_ring_integrity, "technological/organizational_failure").

domain_priors:requires_active_enforcement(challenger_o_ring_integrity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(challenger_o_ring_integrity, nasa_management).
narrative_ontology:constraint_beneficiary(challenger_o_ring_integrity, thiokol_management).
narrative_ontology:constraint_victim(challenger_o_ring_integrity, challenger_crew).
narrative_ontology:constraint_victim(challenger_o_ring_integrity, thiokol_engineers).
narrative_ontology:constraint_victim(challenger_o_ring_integrity, us_taxpayer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CHALLENGER CREW (SNARE) — The ultimate victims, with no knowledge of the specific last-minute risk and no ability to exit. The system extracted their lives to meet a launch schedule. d≈0.95, f(d)≈1.42, σ=0.8 -> χ≈0.97.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ENGINEERS (SNARE) — As an organized group, they understood and communicated the risk but were institutionally constrained from stopping the launch. Their professional judgment was suppressed and extracted. d≈0.7, f(d)≈1.07, σ=1.0 -> χ≈0.91.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANAGEMENT (ROPE) — Beneficiaries of maintaining the launch schedule. They experienced the constraint as a pure coordination problem: getting engineers to 'sign off' and align with program goals. From this perspective, the pressure was a tool for coordination, not extraction. d≈0.05, f(d)≈-0.12, σ=1.0 -> χ≈-0.10 (net beneficiary).
constraint_indexing:constraint_classification(challenger_o_ring_integrity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE REVIEW PROCESS (PITON) — The formal safety review process had become a degraded ritual. Its function was no longer genuine risk assessment but performative approval. The high theater_ratio (0.75) and institutional inertia satisfy the Piton classification. The process was maintained despite its failure to function.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SNARE) — With full access to information post-disaster, the analytical view sees the system's structure clearly: a high-extraction, high-suppression mechanism that coerced compliance and suppressed vital safety data. The classification is unambiguously Snare. d≈0.73, f(d)≈1.15, σ=1.2 -> χ≈1.17.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: 'INHERENT RISK' NARRATIVE (MOUNTAIN) — This perspective frames the disaster as an unavoidable consequence of the inherent dangers of spaceflight, a 'Mountain' of physics. The engine will identify this as a false summit: the constraint's properties (ε=0.85, suppression=0.90, requires_active_enforcement=true) are those of a man-made Snare, not a natural law.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(challenger_o_ring_integrity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(challenger_o_ring_integrity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(challenger_o_ring_integrity, TR),
    TR >= 0.70.

:- end_tests(challenger_o_ring_integrity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): Extremely high. The system extracted professional silence from engineers and, ultimately, the lives of the seven crew members in service of a schedule. Suppression (0.90): Near-total. The alternative—delaying the launch—was explicitly presented by engineers and actively rejected by multiple layers of management. Theater Ratio (0.75): High. The formal 'Flight Readiness Review' process proved to be performative. It served as a ritual to manufacture consent rather than a functional mechanism for assessing safety, as evidenced by the decision to launch over strenuous, data-backed objections.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For management, the situation was a Rope: a coordination challenge to get all parties to agree to the goal of launching on time. For the engineers and crew, it was a Snare: a coercive system where safety concerns were suppressed and from which there was no escape. The analytical perspective of the Rogers Commission confirms the Snare classification, revealing the 'coordination' narrative to be a catastrophic fiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (NASA/Thiokol Management) with arbitrage exit options see the system as a net benefit (negative extraction), classifying it as a Rope. Victims with no escape (Challenger Crew, trapped) or limited recourse (Engineers, constrained) experience maximum or near-maximum extraction, classifying it as a Snare. The analytical observer, using a canonical directionality, also arrives at the Snare classification, confirming it as the objective structural assessment.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of resolving mandatrophy. A system that presented itself as a coordination mechanism (Rope) for achieving a collective good (space exploration) was revealed to be a lethal Snare that extracted human life for the sake of institutional goals like schedule adherence and budget justification. The perspectival gap between the Rope (management's view) and the Snare (the victims' reality and the analytical conclusion) is the core of the tragedy and what this framework is designed to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_individual_failure,
    'Was the failure a systemic property of NASA''s 1980s culture, or was it contingent on the decisions of a few specific managers?',
    'Analysis of decision-making patterns in other shuttle missions from the same era to determine if the Challenger decision was an outlier or the norm.',
    'If systemic, the constraint is a stable Snare. If individual, it was a contingent event more akin to a catastrophic failure of a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_individual_failure, empirical, 'Distinguishing systemic cultural failure from individual error').

omega_variable(
    acceptable_risk_threshold,
    'What was the actual, versus stated, threshold for acceptable risk within NASA management at the time?',
    'This is a non-empirical question of values and policy. It can only be resolved by examining internal communications to infer the de facto policy, as the de jure policy failed.',
    'A low de facto threshold confirms the Snare classification (deliberate risk acceptance). A high but miscalibrated threshold would suggest a Tangled Rope (a coordination system that failed catastrophically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(acceptable_risk_threshold, preference, 'The de facto vs de jure acceptable risk policy').

omega_variable(
    go_fever_inevitability,
    'Is ''go fever'' an inevitable emergent property of high-stakes, schedule-driven projects, or a contingent cultural flaw?',
    'Comparative analysis of management culture in other successful and unsuccessful large-scale technological projects (e.g., Apollo program, Manhattan Project).',
    'If inevitable, it suggests a Mountain-like property of human organizations under pressure. If contingent, it confirms the Snare is a correctable institutional design flaw.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(go_fever_inevitability, conceptual, 'Whether ''go fever'' is an inevitable or contingent feature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(challenger_o_ring_integrity, 1980, 1986).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chal_tr_t1980, challenger_o_ring_integrity, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(chal_tr_t1983, challenger_o_ring_integrity, theater_ratio, 1983, 0.5).
narrative_ontology:measurement(chal_tr_t1986, challenger_o_ring_integrity, theater_ratio, 1986, 0.75).

% Extraction over time
narrative_ontology:measurement(chal_be_t1980, challenger_o_ring_integrity, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(chal_be_t1983, challenger_o_ring_integrity, base_extractiveness, 1983, 0.55).
narrative_ontology:measurement(chal_be_t1986, challenger_o_ring_integrity, base_extractiveness, 1986, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(challenger_o_ring_integrity, enforcement_mechanism).
narrative_ontology:affects_constraint(challenger_o_ring_integrity, space_shuttle_return_to_flight_protocols).

% DUAL FORMULATION NOTE:
% This constraint, 'challenger_o_ring_integrity', models the institutional decision-making failure. It is structurally downstream of a separate, unmodeled Mountain constraint, 'o_ring_thermal_limits', which represents the immutable physics of the O-rings' loss of resiliency at low temperatures. The Snare leveraged the Mountain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
