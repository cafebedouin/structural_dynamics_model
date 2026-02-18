% ============================================================================
% CONSTRAINT STORY: colorado_sbe_decentralization_friction
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_colorado_sbe_decentralization_friction, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: colorado_sbe_decentralization_friction
 * human_readable: Colorado SBE Institutional Preservation (Educational Decentralization Friction)
 * domain: political/regulatory
 * * SUMMARY:
 * The Colorado State Board of Education (SBE) acts as an institutional gatekeeper for educational legitimacy. Its regulatory structures around per-pupil funding, standardized testing, and teacher licensure create significant friction for decentralized learning models like microschools. The constraint extracts compliance labor and data from innovators in exchange for legally required funding and legitimacy, while suppressing models that challenge the incumbent public school system.
 * * KEY AGENTS:
 * - Taxpayers/Parents: Subject (Powerless) - Fund the system but have limited direct control over its regulatory structure.
 * - Microschool Founders: Subject (Moderate) - Agents of decentralization who face high compliance costs.
 * - Legacy School Districts: Beneficiary (Institutional) - Benefit from a regulatory framework that favors large, established entities.
 * - SBE/Auditors: Observer (Analytical) - Views the system as a complex coordination and enforcement mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, 0.70). % High extraction of compliance labor and data.
domain_priors:suppression_score(colorado_sbe_decentralization_friction, 0.60).   % Compliance requirements favor incumbents and suppress alternatives.
domain_priors:theater_ratio(colorado_sbe_decentralization_friction, 0.15).       % The SBE's function is primarily operational, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, extractiveness, 0.70).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The SBE claims its regulations are for coordination (ensuring quality standards).
narrative_ontology:constraint_claim(colorado_sbe_decentralization_friction, tangled_rope).
narrative_ontology:human_readable(colorado_sbe_decentralization_friction, "Colorado SBE Institutional Preservation (Educational Decentralization Friction)").
narrative_ontology:topic_domain(colorado_sbe_decentralization_friction, "political/regulatory").

% Binary flags
domain_priors:requires_active_enforcement(colorado_sbe_decentralization_friction). % The SBE must actively audit, license, and mandate testing.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(colorado_sbe_decentralization_friction, legacy_school_districts).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, microschool_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE TAXPAYER/PARENT (POWERLESS)
% Experiences the system as a Snare: high extraction (taxes, compliance) for
% limited choice, with few viable alternatives for state-funded education.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SBE (INSTITUTIONAL)
% Views its own regulations as a Rope, a necessary coordination mechanism to
% ensure uniform quality and accountability across 178 independent districts.
% From this perspective, extraction is minimal and serves the public good.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The system is a Tangled Rope. It has a genuine coordination function
% (benefiting legacy districts) but also imposes asymmetric extraction on
% innovators (victims) and requires active enforcement to maintain.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE MICROSCHOOL FOUNDER (MODERATE)
% For the innovator, the regulations are a Snare. The reporting requirements
% are extractive, and the refusal to recognize alternative credentials creates
% an artificial barrier to entry, stifling growth.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colorado_sbe_decentralization_friction_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(tangled_rope_detection) :-
    % The analytical observer must see the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the base extraction meets the high-extraction criteria.
    narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, extractiveness, E),
    E >= 0.46.

:- end_tests(colorado_sbe_decentralization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.70 reflects the significant compliance labor and data demanded from educational units to receive state funding and legitimacy. The suppression score of 0.60 captures how these requirements inherently favor large, incumbent districts over small, innovative models.
 * The Perspectival Gap is stark:
 * - The SBE (Institutional) sees its role as pure coordination (Rope), managing a complex system for public good.
 * - Parents and Innovators (Powerless/Moderate) experience this same system as a Snare, where compliance costs are high and alternatives are suppressed.
 * - The Analytical view resolves this by identifying a Tangled Rope: a system with a real coordination function that has been captured to serve incumbents via asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A simple Snare classification would miss the SBE's genuine (and legally mandated) coordination function. Classifying it as a Tangled Rope correctly identifies that the mechanism is not *purely* extractive; it is a coordination system where the benefits and costs are unequally distributed. This prevents mislabeling a captured regulatory body as a simple predator and points analysis toward the specific mechanisms of capture and extraction layered onto the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_colorado_sbe,
    'Is the SBE primarily driven by a mandate for quality control (a difficult coordination problem) or by institutional capture from legacy education lobbies (an extractive agenda)?',
    'Analyze SBE meeting minutes and lobbying disclosures to correlate policy changes with influence from teachers unions and district administrator groups versus parent/innovator advocacy.',
    'If coordination-driven, the constraint might evolve into a Rope with better technology. If capture-driven, it will remain a Tangled Rope or degrade into a pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(colorado_sbe_decentralization_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (0.70 > 0.46), requiring temporal data.
% The model assumes a slight increase in extraction and theater over time as
% regulatory frameworks become more entrenched and resistant to change.

% Theater ratio over time:
narrative_ontology:measurement(csdf_tr_t0, colorado_sbe_decentralization_friction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(csdf_tr_t5, colorado_sbe_decentralization_friction, theater_ratio, 5, 0.12).
narrative_ontology:measurement(csdf_tr_t10, colorado_sbe_decentralization_friction, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(csdf_ex_t0, colorado_sbe_decentralization_friction, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(csdf_ex_t5, colorado_sbe_decentralization_friction, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(csdf_ex_t10, colorado_sbe_decentralization_friction, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The SBE's function is to enforce standards and rules.
narrative_ontology:coordination_type(colorado_sbe_decentralization_friction, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */