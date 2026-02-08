% ============================================================================
% CONSTRAINT STORY: taiwan_existential_sovereignty
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_taiwan_existential_sovereignty, []).

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
 * * constraint_id: taiwan_existential_sovereignty
 * human_readable: The Taiwan Existential Sovereignty Constraint
 * domain: political/economic/technological
 * * SUMMARY:
 * This constraint models the condition of Taiwan's existence under persistent
 * existential threat. It is characterized by the "Silicon Shield" (economic
 * indispensability) acting as a coordination mechanism, while military imbalance
 * and diplomatic isolation function as a coercive extraction mechanism. For the
 * population, the "Status Quo" is a high-stakes reality navigated between the
 * catastrophic extraction of a conflict and the loss of autonomy.
 * * KEY AGENTS:
 * - Taiwanese Citizen: Subject (Powerless), experiences the constraint as a coercive Snare limiting life choices and national potential.
 * - Taiwanese State & Tech Sector: Beneficiary (Institutional), manages the "Silicon Shield" as a functional coordination Rope for global security.
 * - Geopolitical Analyst: Auditor (Analytical), observes a Tangled Rope where a vital coordination function is coupled with severe asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% High extraction: constant military readiness, diplomatic isolation, and psychological stress.
domain_priors:base_extractiveness(taiwan_existential_sovereignty, 0.70).
% High suppression: alternatives (formal independence, unification) are suppressed by overwhelming force.
domain_priors:suppression_score(taiwan_existential_sovereignty, 0.80).
% Low theater: the constraint is highly functional and not performative.
domain_priors:theater_ratio(taiwan_existential_sovereignty, 0.15).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, extractiveness, 0.70).
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(taiwan_existential_sovereignty, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The geopolitical arrangement is maintained by active pressure and deterrence.
narrative_ontology:constraint_claim(taiwan_existential_sovereignty, tangled_rope).

% Binary flags
% Requires constant military patrols, diplomatic maneuvering, and economic statecraft.
domain_priors:requires_active_enforcement(taiwan_existential_sovereignty).

% Structural property derivation hooks for Tangled Rope:
% The "Silicon Shield" coordinates global economic and security interests.
narrative_ontology:constraint_beneficiary(taiwan_existential_sovereignty, global_semiconductor_dependent_economies).
% The situation extracts diplomatic freedom and imposes immense defense burdens.
narrative_ontology:constraint_victim(taiwan_existential_sovereignty, taiwanese_citizenry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen experiences high coercion and extraction of resources/potential.
% χ = 0.70 (ε) * 1.5 (π(powerless)) * 1.0 (σ(national)) = 1.05 (High Snare)
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The state and its partners see a vital coordination mechanism (Silicon Shield).
% χ = 0.70 (ε) * -0.2 (π(institutional)) * 1.2 (σ(global)) = -0.168 (Felt as a net benefit)
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function and the severe asymmetric extraction.
% This is the canonical classification, resolving the Rope/Snare paradox.
% χ = 0.70 (ε) * 1.15 (π(analytical)) * 1.2 (σ(global)) = 0.966 (High Tangled Rope)
constraint_indexing:constraint_classification(taiwan_existential_sovereignty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_existential_sovereignty_tests).

test(perspectival_gap) :-
    % Verify the gap between the citizen (Snare) and the state (Rope).
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(tangled_rope_analytical_view) :-
    % Verify the analytical observer correctly identifies a Tangled Rope.
    constraint_indexing:constraint_classification(taiwan_existential_sovereignty, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeAnalytical == tangled_rope.

test(tangled_rope_structural_conditions_met) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(taiwan_existential_sovereignty, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(taiwan_existential_sovereignty, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(taiwan_existential_sovereignty).

:- end_tests(taiwan_existential_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect a situation of extreme pressure. Extraction (0.70) is high due to
 * the immense defense spending, diplomatic exclusion, and psychological burden required to
 * maintain autonomy. Suppression (0.80) is also high, as the primary alternatives (unification,
 * formal independence) are met with overwhelming coercive threats.
 *
 * The Perspectival Gap is stark:
 * - For the 'powerless' citizen, the daily reality is a Snare. Their choices are constrained,
 *   and national resources are extracted for survival rather than flourishing.
 * - For the 'institutional' state, the same set of facts is managed as a Rope. The "Silicon Shield"
 *   is a masterful coordination tool that aligns global interests with Taiwan's security.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of potential Mandatrophy, where a system could mistakenly
 * classify it as a pure Snare by focusing only on the coercion. The 'tangled_rope'
 * classification from the analytical perspective resolves this. It correctly identifies that
 * a genuine, high-value coordination function (the Silicon Shield) coexists with and is
 * inseparable from a severe, asymmetrically applied extraction mechanism (the military threat).
 * The system avoids collapsing the paradox into a single, less accurate label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_taiwan_existential_sovereignty,
    'Will global semiconductor diversification (e.g., US/EU fabs) weaken the Silicon Shield coordination function, causing the Tangled Rope to degrade into a pure Snare?',
    'Monitor changes in diplomatic and military support for Taiwan from key nations as their domestic chip production capacity increases.',
    'If support weakens, the Rope component was purely transactional and is fraying. If support holds, the Rope is reinforced by shared values and strategic alignment.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(taiwan_existential_sovereignty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the constraint over the last decade,
% reflecting increased "gray zone" pressure and geopolitical tension.
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(tes_tr_t0, taiwan_existential_sovereignty, theater_ratio, 0, 0.10).
narrative_ontology:measurement(tes_tr_t5, taiwan_existential_sovereignty, theater_ratio, 5, 0.12).
narrative_ontology:measurement(tes_tr_t10, taiwan_existential_sovereignty, theater_ratio, 10, 0.15).

% Extraction over time (shows increasing pressure):
narrative_ontology:measurement(tes_ex_t0, taiwan_existential_sovereignty, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(tes_ex_t5, taiwan_existential_sovereignty, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(tes_ex_t10, taiwan_existential_sovereignty, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The "Silicon Shield" is a piece of global infrastructure coordinating the world economy.
narrative_ontology:coordination_type(taiwan_existential_sovereignty, global_infrastructure).

% This constraint is structurally coupled with the stability of the global semiconductor supply.
narrative_ontology:affects_constraint(taiwan_existential_sovereignty, semiconductor_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */