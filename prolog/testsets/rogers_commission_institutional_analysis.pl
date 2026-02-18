% ============================================================================
% CONSTRAINT STORY: rogers_commission_institutional_analysis
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_rogers_commission_institutional_analysis, []).

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
 * * constraint_id: rogers_commission_institutional_analysis
 * human_readable: The Silent Safety Silo (NASA Decision-Making Pre-Challenger)
 * domain: political/technological
 * * SUMMARY:
 * The Rogers Commission identified a "silent safety" constraint where critical
 * technical data regarding O-ring erosion was filtered out before reaching
 * top-level NASA management. This institutional silo created a
 * false perception of safety, treating known technical flaws as "acceptable
 * flight risks" rather than terminal threats. The pressure to maintain a
 * rigorous launch schedule functioned as a Snare, extracting engineering
 * integrity and safety margins.
 * * KEY AGENTS:
 * - Field Engineers (e.g., at Thiokol): Powerless subjects who possessed the critical data but lacked the institutional authority to halt the launch.
 * - NASA Marshall Center Management: Institutional beneficiaries of the launch tempo, who enforced operational pressure and suppressed dissenting technical reports.
 * - The Rogers Commission: The analytical observer, a retrospective body tasked with identifying the structural "failure of communication".
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale (Extraction): 0.85. The system extracted engineering integrity,
% safety margins, and ultimately human life to serve the institutional goal
% of maintaining a high-tempo launch schedule.
domain_priors:base_extractiveness(rogers_commission_institutional_analysis, 0.85).
% Rationale (Suppression): 0.75. The Commission found that critical information about
% O-ring erosion was not communicated to the highest levels, effectively
% suppressing alternative risk assessments and reversing the burden of proof.
domain_priors:suppression_score(rogers_commission_institutional_analysis, 0.75).
% Rationale (Theater): 0.10. The safety review process was still functional,
% not purely performative, but its conclusions were marginalized. The theater
% was low, but the suppression of its output was high.
domain_priors:theater_ratio(rogers_commission_institutional_analysis, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, extractiveness, 0.85).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The system claimed to be a coordination mechanism for achieving ambitious goals.
narrative_ontology:constraint_claim(rogers_commission_institutional_analysis, snare).
narrative_ontology:human_readable(rogers_commission_institutional_analysis, "The Silent Safety Silo (NASA Decision-Making Pre-Challenger)").
narrative_ontology:topic_domain(rogers_commission_institutional_analysis, "political/technological").

% Binary flags
% Requires active enforcement via "Launch Fever" and the marginalization
% of the safety reporting structure. This is a key indicator for Tangled Rope/Snare.
domain_priors:requires_active_enforcement(rogers_commission_institutional_analysis).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(rogers_commission_institutional_analysis, nasa_institutional_prestige).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, engineering_safety_protocol).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, crew_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE FIELD ENGINEER (MOUNTAIN)
% To the engineers, the problem was a Mountain of physics (O-ring failure at
% low temperatures). However, the institutional process made this physical
% reality invisible to decision-makers, creating an artificial Mountain: the
% immovable requirement to prove 100% that the launch was unsafe.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NASA MANAGEMENT (ROPE)
% Management viewed the history of O-ring erosion as a manageable technical
% issue that had been "normalized" through prior successful flights. They
% perceived their role as coordinating a complex system towards a necessary
% milestone, making the process a Rope in their view.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The Rogers Commission's analysis reveals a Snare. By isolating top-level
% management from technical dissent, the institutional structure "choked off"
% the very safety data it was designed to ingest, extracting safety margins
% to the point of catastrophic failure.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_institutional_analysis_tests).

test(perspectival_gap_mountain_vs_rope) :-
    % Verify the critical gap between the engineer's view (Mountain) and management's view (Rope).
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare, context(agent_power(analytical), _, _, _)).

test(high_extraction_snare_thresholds) :-
    % Verify the base metrics meet the criteria for a high-extraction Snare.
    domain_priors:base_extractiveness(rogers_commission_institutional_analysis, E),
    domain_priors:suppression_score(rogers_commission_institutional_analysis, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(rogers_commission_institutional_analysis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the findings of the Rogers Commission. The high extraction (0.85)
 * represents the ultimate price paid (human lives and program integrity) for
 * maintaining the launch schedule. The high suppression (0.75) captures the
 * core finding: a "flaw in the decision-making process" that actively filtered
 * out critical safety data. The perspectival gap is profound: engineers saw an
 * immutable physical limit (Mountain), while management saw a routine coordination
 * challenge (Rope). The analytical truth, revealed by the disaster, was a
 * predatory institutional structure (Snare).
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This case is a classic example of potential Mandatrophy. The powerless agent
 * (the engineer) perceives the constraint as a Mountain—an unchangeable law of
 * physics. However, the *operative constraint* was the institutional Snare that
 * reversed the burden of proof, demanding proof of *certain failure* to stop the
 * launch, rather than proof of *safety* to proceed. This analysis resolves the
 * Mandatrophy by correctly identifying the institutional pressure, not the
 * physical law, as the primary, constructed constraint that led to the disaster.
 * The system correctly classifies the analytical perspective as a Snare, preventing
 * the misattribution of a policy failure to a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_rogers_commission_institutional_analysis,
    'Can a Snare-type organizational structure ever truly be reformed into a Rope without a total purge of the leadership that normalized deviance?',
    'Longitudinal study of NASA cultural shifts post-Challenger versus post-Columbia (2003), tracking safety reporting metrics and leadership continuity.',
    'If Yes: Cultural reform is a viable Rope. If No: Institutional failure is a generational Mountain, recurring despite interventions.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rogers_commission_institutional_analysis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the "normalization of deviance" at NASA. As flawed
% launches succeeded, the perceived risk decreased, and the pressure to
% maintain schedule (extraction) increased.

% Theater ratio over time (safety reviews become slightly more procedural):
narrative_ontology:measurement(rcia_tr_t0, rogers_commission_institutional_analysis, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rcia_tr_t5, rogers_commission_institutional_analysis, theater_ratio, 5, 0.08).
narrative_ontology:measurement(rcia_tr_t10, rogers_commission_institutional_analysis, theater_ratio, 10, 0.10).

% Extraction over time (schedule pressure intensifies, safety margins erode):
narrative_ontology:measurement(rcia_ex_t0, rogers_commission_institutional_analysis, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rcia_ex_t5, rogers_commission_institutional_analysis, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rcia_ex_t10, rogers_commission_institutional_analysis, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The launch schedule is a form of resource allocation (allocating launch
% windows, personnel, and material against a timeline).
narrative_ontology:coordination_type(rogers_commission_institutional_analysis, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */