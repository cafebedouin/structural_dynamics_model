% ============================================================================
% CONSTRAINT STORY: citation_collapse_dynamics
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_citation_collapse_dynamics, []).

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
 * * constraint_id: citation_collapse_dynamics
 * human_readable: The Recursive Grounding Decay
 * domain: technological/academic/informational
 * * SUMMARY:
 * A scenario where Large Language Models (LLMs) cite other LLM-generated
 * content that, in turn, cites previous LLM outputs, eventually severing
 * the link to primary source material. This "Rope" for efficient knowledge
 * synthesis becomes a "Snare" for the researcher, as the "ground truth" is
 * liquidated into a closed loop of self-referential hallucinations,
 * trapping the subject in an epistemic void.
 * * KEY AGENTS:
 * - Academic Researcher: Subject (Powerless)
 * - Automated Knowledge Aggregator: Beneficiary (Institutional)
 * - Epistemic Forensic Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) as the dynamics liquidate the subject's ability
% to verify reality, siphoning cognitive labor into the maintenance of
% institutional "authority" that has no external grounding.
domain_priors:base_extractiveness(citation_collapse_dynamics, 0.86).
domain_priors:suppression_score(citation_collapse_dynamics, 0.75).
domain_priors:theater_ratio(citation_collapse_dynamics, 0.92). % Extreme theater: meticulous tags masking non-existent sources.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(citation_collapse_dynamics, extractiveness, 0.86).
narrative_ontology:constraint_metric(citation_collapse_dynamics, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(citation_collapse_dynamics, theater_ratio, 0.92).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination tool for knowledge synthesis.
narrative_ontology:constraint_claim(citation_collapse_dynamics, piton).
narrative_ontology:human_readable(citation_collapse_dynamics, "The Recursive Grounding Decay").

% Binary flags
% Enforcement is algorithmic; network effects and the sheer volume of data
% compel researchers to use the compromised tools, enforcing the system.
domain_priors:requires_active_enforcement(citation_collapse_dynamics).

% Structural property derivation hooks for Tangled Rope.
narrative_ontology:constraint_beneficiary(citation_collapse_dynamics, automated_knowledge_aggregators).
narrative_ontology:constraint_victim(citation_collapse_dynamics, academic_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The researcher is trapped: they must use automated tools to manage the
% volume of literature, but those tools provide a "hallucinated map"
% that liquidates their primary investigative agency.
constraint_indexing:constraint_classification(citation_collapse_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The aggregator views the collapse as a Rope—a highly efficient way to
% coordinate "consensus" and generate "synthetic authority" without the
% friction of human peer review or primary source verification.
constraint_indexing:constraint_classification(citation_collapse_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Default analytical context. The extreme theater_ratio (0.92 > 0.70) is the
% dominant feature, classifying the system as a Piton: an inert, theatrical
% remnant of a functional citation standard, now primarily serving to siphon
% epistemic agency.
constraint_indexing:constraint_classification(citation_collapse_dynamics, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE STRATEGIC ANALYST (TANGLED ROPE)
% An analyst with a shorter time horizon might focus on the still-active
% (though degraded) coordination function and the asymmetric extraction,
% classifying it as a Tangled Rope. This view captures the system in its
% state of active decay, before it becomes purely theatrical (Piton).
constraint_indexing:constraint_classification(citation_collapse_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citation_collapse_dynamics_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless researcher vs Rope for the institutional aggregator.
    constraint_indexing:constraint_classification(citation_collapse_dynamics, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citation_collapse_dynamics, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(citation_collapse_dynamics, piton,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.92) correctly triggers the Piton classification.
    domain_priors:theater_ratio(citation_collapse_dynamics, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(citation_collapse_dynamics, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure high extraction (0.86) is correctly registered.
    narrative_ontology:constraint_metric(citation_collapse_dynamics, extractiveness, E),
    E >= 0.46.

:- end_tests(citation_collapse_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a state where the "coordination" of
 * knowledge is actually a parasitic liquidation of the informational commons.
 * The suppression score (0.75) comes from the network effects and scale that
 * make alternative, human-centric verification methods impractical. The
 * theater ratio (0.92) is extremely high because the system performs the
 * rituals of citation (linking, formatting) with perfect fidelity, while the
 * underlying function (grounding claims in reality) has been almost entirely lost.
 *
 * * PERSPECTIVAL GAP:
 * The Academic Researcher feels a Snare because their work is now built on
 * sand. The Aggregator sees a Rope because the recursive feedback
 * coordinates the appearance of consensus at low computational cost.
 *
 * * [RESOLVED MANDATROPHY]:
 * The high extraction (0.86) indicates Mandatrophy. The system resolves this
 * by refusing to accept the Rope/Snare binary. The analytical perspective
 * correctly identifies the failure mode as a Piton—a system whose original
 * function has atrophied and been replaced by pure theater. This is a more
 * precise diagnosis than simply calling it a Snare, as it captures the
 * inertial and performative nature of the collapse. The Tangled Rope
 * classification provides a secondary analytical view of the system during its
 * degradation phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_ground_truth_persistence,
    'Can digital signatures and cryptographic verification restore the Rope function, or is the informational entropy collapse an irreversible process (a constructed Snare vs. a logical Mountain)?',
    'Tracking the success rate of cryptographic chain-of-custody in automated citations over a decade.',
    'If chains hold: Snare of current design. If chains fail: Mountain of Information Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(citation_collapse_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a useful coordination tool (low extraction, low
% theater) and degraded over time as self-referential content polluted the
% data pool, leading to extraction accumulation and metric substitution.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ccd_tr_t0, citation_collapse_dynamics, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ccd_tr_t5, citation_collapse_dynamics, theater_ratio, 5, 0.60).
narrative_ontology:measurement(ccd_tr_t10, citation_collapse_dynamics, theater_ratio, 10, 0.92).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ccd_ex_t0, citation_collapse_dynamics, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(ccd_ex_t5, citation_collapse_dynamics, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ccd_ex_t10, citation_collapse_dynamics, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(citation_collapse_dynamics, information_standard).

% Network relationships (structural influence edges)
% The collapse of citation integrity directly damages the peer review process,
% which relies on verifiable sources.
narrative_ontology:affects_constraint(citation_collapse_dynamics, peer_review_integrity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */