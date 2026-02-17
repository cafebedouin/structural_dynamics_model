% ============================================================================
% CONSTRAINT STORY: evidence_half_life
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_evidence_half_life, []).

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
 * * constraint_id: evidence_half_life
 * human_readable: The Epistemic Decay Constant
 * domain: technological/scientific/legal
 * * SUMMARY:
 * A scenario where the speed of information turnover and the ease of digital
 * alteration cause the "half-life" of a piece of evidence to drop below
 * the time required for judicial or scientific verification. This "Rope" for
 * rapid information iteration becomes a "Snare" for the truth-seeker,
 * liquidating the permanence of facts and trapping the subject in a
 * "perpetual present" where proof is impossible to maintain.
 * * KEY AGENTS:
 * - Forensic Investigator: Subject (Powerless)
 * - Real-Time Information Platform: Beneficiary (Institutional)
 * - Epistemic Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) reflects the liquidation of the subject's ability
% to build stable, grounded arguments over time.
domain_priors:base_extractiveness(evidence_half_life, 0.85).
domain_priors:suppression_score(evidence_half_life, 0.72).   % Alternatives (archival stability) are suppressed by the velocity of the medium.
domain_priors:theater_ratio(evidence_half_life, 0.81).       % High theater: "Timestamp" rituals that provide the optics of stability while the content drifts.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(evidence_half_life, extractiveness, 0.85).
narrative_ontology:constraint_metric(evidence_half_life, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(evidence_half_life, theater_ratio, 0.81).

% Constraint self-claim (what does the constraint claim to be?)
% The system claims to be a necessary coordination mechanism for a high-velocity market.
narrative_ontology:constraint_claim(evidence_half_life, tangled_rope).
narrative_ontology:human_readable(evidence_half_life, "The Epistemic Decay Constant").

% Binary flags
% Enforcement is active through algorithmic promotion of novelty and deprecation of old data.
domain_priors:requires_active_enforcement(evidence_half_life).

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(evidence_half_life, real_time_information_platforms).
narrative_ontology:constraint_victim(evidence_half_life, forensic_investigators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The investigator is trapped: by the time a fact is verified, the
% context has shifted or the data has decayed, liquidating their agency.
constraint_indexing:constraint_classification(evidence_half_life, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the decay as a Rope—the essential coordination
% substrate for maintaining a high-velocity, low-friction information market.
constraint_indexing:constraint_classification(evidence_half_life, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) and active enforcement masking as a
% functional coordination mechanism.
constraint_indexing:constraint_classification(evidence_half_life, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.81) > 0.70 triggers Piton: the "Official Archive"
% is an inertial spike; it performatively signals stability while siphoning agency.
constraint_indexing:constraint_classification(evidence_half_life, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(evidence_half_life, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evidence_half_life_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless investigator vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(evidence_half_life, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evidence_half_life, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(evidence_half_life, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.81) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(evidence_half_life, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(evidence_half_life, _),
    narrative_ontology:constraint_victim(evidence_half_life, _),
    domain_priors:requires_active_enforcement(evidence_half_life).

:- end_tests(evidence_half_life_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the
 * "coordination" benefit of real-time info is achieved by liquidating the
 * long-term stability of the epistemic commons. The high theater ratio (0.81)
 * captures the performative nature of "archival" or "timestamp" features
 * that provide an illusion of permanence without functional stability.
 *
 * * PERSPECTIVAL GAP:
 * The Forensic Investigator feels a Snare because their work requires a
 * persistence that the medium actively destroys. The Platform sees a Rope
 * because the rapid turnover coordinates the attention of millions in a
 * dynamic, low-latency environment. The Analytical observer sees a Tangled Rope,
 * recognizing both the coordination function and the severe, asymmetric extraction it imposes.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical
 * observer, the "Data Integrity" protocol is no longer functional (Theater 0.81);
 * it is an inert spike siphoning 0.85 of the subject's investigative agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_verification_latency,
    'Can verification speed ever match decay speed, or is the "Snare" a physical law of networks (Snare vs Mountain)?',
    'Tracking the success rate of multi-year archival grounding against real-time drift.',
    'If archival grounding holds: Snare of current tech. If it fails: Mountain of Information Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(evidence_half_life, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time as the value of real-time engagement
% displaced the value of archival stability.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(ehl_tr_t0, evidence_half_life, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ehl_tr_t5, evidence_half_life, theater_ratio, 5, 0.65).
narrative_ontology:measurement(ehl_tr_t10, evidence_half_life, theater_ratio, 10, 0.81).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(ehl_ex_t0, evidence_half_life, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ehl_ex_t5, evidence_half_life, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(ehl_ex_t10, evidence_half_life, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a form of global infrastructure for information exchange.
narrative_ontology:coordination_type(evidence_half_life, global_infrastructure).

% The decay of evidence directly impacts the integrity of adjacent legal processes.
narrative_ontology:affects_constraint(evidence_half_life, legal_due_process).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */