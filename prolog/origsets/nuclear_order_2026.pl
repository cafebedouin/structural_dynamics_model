% ============================================================================
% CONSTRAINT STORY: nuclear_order_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_nuclear_order_2026, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: nuclear_order_2026
 * human_readable: Post-New START Strategic Ambiguity
 * domain: geopolitical/technological
 * * SUMMARY:
 * Following the expiration of the New START treaty on February 5, 2026, the global 
 * nuclear framework has shifted from a regulated "Rope" of coordination to a 
 * "Snare" of strategic ambiguity. The loss of mandatory ceilings (1,550 warheads) 
 * and verification regimes creates an asymmetric environment where nuclear powers 
 * extract security at the cost of global existential risk.
 * * KEY AGENTS:
 * - Non-Nuclear States/Global Population: Subject (Powerless)
 * - Nuclear Weapon States (US/Russia/China): Beneficiary (Institutional)
 * - Arms Control Auditors/IAEA: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.55) due to the resource diversion into arms racing.
domain_priors:base_extractiveness(nuclear_order_2026, 0.55). 
% Suppression is high (0.88) as there is no viable alternative to the nuclear state paradigm.
domain_priors:suppression_score(nuclear_order_2026, 0.88).   
% Theater ratio is high (0.72) post-expiration as shadow talks (Oman/Abu Dhabi) 
% maintain the "theater" of control without legal limits.
domain_priors:theater_ratio(nuclear_order_2026, 0.72).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(nuclear_order_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(nuclear_order_2026, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(nuclear_order_2026, theater_ratio, 0.72).

% Primary keys for the classification engine
% Beneficiaries and Victims for high-extraction (E > 0.46)
narrative_ontology:constraint_beneficiary(nuclear_order_2026, nuclear_weapon_states).
narrative_ontology:constraint_victim(nuclear_order_2026, non_combatant_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The powerless view the lack of limits as a predatory trap where their safety 
% is the extracted variable for superpower posture.
constraint_indexing:constraint_classification(nuclear_order_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (TANGLED ROPE)
% Institutional actors view this as coordination (Rope) for deterrence, 
% but the asymmetric extraction of security makes it "Tangled."
constraint_indexing:constraint_classification(nuclear_order_2026, tangled_rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The system auditor sees the current "Shadow Diplomacy" as a Piton: 
% inertial maintenance of a non-functional order (Theater Ratio > 0.70).
constraint_indexing:constraint_classification(nuclear_order_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% The Oman/Abu Dhabi talks are classified as Scaffold: temporary support 
% with an implied (though fragile) sunset clause.
constraint_indexing:constraint_classification(nuclear_order_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(nuclear_order_2026).

% Declare sunset clause for the "Shadow Diplomacy" component
narrative_ontology:has_sunset_clause(nuclear_order_2026).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_order_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_order_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_order_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_order_2026, TR),
    TR > 0.70.

test(high_extraction_omega) :-
    domain_priors:base_extractiveness(nuclear_order_2026, E),
    E > 0.46.

:- end_tests(nuclear_order_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The classification of 'piton' for the analytical observer stems from the 
 * high theater_ratio (0.72). While formal limits have vanished, the 
 * performative shell of diplomacy in Oman and Abu Dhabi continues, 
 * masking the accumulation of nuclear warheads. 
 * * PERSPECTIVAL GAP:
 * The 'Subject' sees a Snare because they have zero exit options from 
 * a nuclear escalation. The 'Institutional' beneficiary sees a Tangled 
 * Rope because they still utilize the "Rope" of hotline communications 
 * and shadow talks to prevent total system collapse, despite the 
 * extraction of global stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% [RESOLVED MANDATROPHY]
% The system interprets high suppression (0.88) as structural rather than 
% purely coercive, as there is no physical "exit" from the global nuclear shadow.

omega_variable(
    omega_nuclear_2026,
    'Is the shadow diplomacy in Oman a genuine Scaffold or a performative Piton?',
    'Verification of technical data exchange resumption vs. continued silence.',
    'Scaffold implies eventual de-escalation; Piton implies permanent managed risk.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_order_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the drift from the Treaty Era (T=0) to the post-expiration era.
% Theater ratio rises as formal verification is replaced by "shadow" signaling.
narrative_ontology:measurement(nuc_tr_t0, nuclear_order_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nuc_tr_t5, nuclear_order_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(nuc_tr_t10, nuclear_order_2026, theater_ratio, 10, 0.72).

% Extraction rises as the costs of unregulated arms racing and uncertainty accumulate.
narrative_ontology:measurement(nuc_ex_t0, nuclear_order_2026, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(nuc_ex_t5, nuclear_order_2026, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(nuc_ex_t10, nuclear_order_2026, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
