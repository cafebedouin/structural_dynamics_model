% ============================================================================
% CONSTRAINT STORY: statecraft_virtu
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_statecraft_virtu, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: statecraft_virtu
 * human_readable: Machiavellian Virtù and State Maintenance
 * domain: political
 * * SUMMARY:
 * The practice of statecraft, as described by Machiavelli, is a set of strategic
 * constraints imposed by a ruler (the Prince) to maintain power and impose order
 * amidst the chaos of Fortune (unpredictable events). It involves the calculated
 * use of law, force, and manipulation to coordinate subjects, neutralize rivals,
 * and ensure the state's survival.
 * * KEY AGENTS:
 * - The Prince: The institutional agent who imposes the constraint system.
 * - The People: The subjects who desire security and not to be oppressed.
 * - Rival Nobles: Dispossessed or competing power centers who are victims of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(statecraft_virtu, 0.70). % High extraction of resources, loyalty, and autonomy.
domain_priors:suppression_score(statecraft_virtu, 0.90).   % Extremely high; the Prince must eliminate all alternative power centers.
domain_priors:theater_ratio(statecraft_virtu, 0.10).       % Low; Machiavelli prioritizes effective action over appearances.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(statecraft_virtu, extractiveness, 0.70).
narrative_ontology:constraint_metric(statecraft_virtu, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(statecraft_virtu, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The system is presented as a necessary mechanism for order and security.
narrative_ontology:constraint_claim(statecraft_virtu, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(statecraft_virtu). % Requires "good arms and good laws."

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(statecraft_virtu, the_prince).
narrative_ontology:constraint_beneficiary(statecraft_virtu, state_stability).
narrative_ontology:constraint_victim(statecraft_virtu, rival_nobles).
narrative_ontology:constraint_victim(statecraft_virtu, dispossessed_lords).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the people and especially rival nobles, the state is a coercive trap.
% Its laws and armies extract resources and suppress dissent.
constraint_indexing:constraint_classification(statecraft_virtu, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% To the Prince, the state is a functional coordination mechanism—a Rope—that
% he uses to manage society, defend against external threats, and build power.
constraint_indexing:constraint_classification(statecraft_virtu, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the coordination function (providing stability) and the
% severe, asymmetric extraction required to maintain it. It requires active
% enforcement, benefits the ruler, and victimizes rivals. This is a classic Tangled Rope.
constraint_indexing:constraint_classification(statecraft_virtu, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statecraft_virtu_tests).

test(perspectival_gap) :-
    % Verify the gap between the Prince (institutional) and his subjects (powerless).
    constraint_indexing:constraint_classification(statecraft_virtu, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(statecraft_virtu, snare, context(agent_power(powerless), _, _, _)),
    writeln('Perspectival gap (Rope vs Snare) confirmed.').

test(tangled_rope_analytical_view) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(statecraft_virtu, tangled_rope, context(agent_power(analytical), _, _, _)),
    writeln('Analytical classification as Tangled Rope confirmed.').

test(tangled_rope_structural_requirements) :-
    % Verify all three structural properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(statecraft_virtu, _),
    narrative_ontology:constraint_victim(statecraft_virtu, _),
    domain_priors:requires_active_enforcement(statecraft_virtu).

:- end_tests(statecraft_virtu_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect Machiavelli's core thesis: state stability is a constructed
 * order that requires immense coercion.
 * - Base Extractiveness (0.70): The state extracts resources, loyalty, and autonomy
 *   from its subjects to function.
 * - Suppression Score (0.90): The Prince must be willing to "extinguish" rival
 *   bloodlines and suppress any alternative power centers. This is the definition
 *   of high suppression.
 * - Perspectival Gap: The gap is the essence of Machiavellian politics. The Prince
 *   views the state as a tool for coordination and glory (Rope). His subjects and
 *   rivals experience it as a coercive, extractive system (Snare).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A naive analysis might classify this system as a pure Snare due to the high
 * extraction and suppression. However, this would be a mandatrophy error, as it
 * ignores the genuine coordination function the state provides (e.g., defense
 * against foreign invasion, stable laws for commerce). The Tangled Rope
 * classification correctly captures this duality: it is a system that provides
 * real coordination benefits, but these benefits are funded by asymmetric
 * extraction and maintained by coercive force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_statecraft_virtu,
    'Is the Prince acting for personal glory or for the long-term stability of the state?',
    'Observing state policy after the Prince is secure. Does he transition from extraction to public works, or continue to plunder?',
    'If for personal glory, the system is a pure Snare. If for state stability, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(statecraft_virtu, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the consolidation of a new principality. Initially, extraction is
% high to establish control. It remains high to maintain that control.
% Theater is consistently low, as the system is brutally functional.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(statecraft_virtu_tr_t0, statecraft_virtu, theater_ratio, 0, 0.10).
narrative_ontology:measurement(statecraft_virtu_tr_t5, statecraft_virtu, theater_ratio, 5, 0.10).
narrative_ontology:measurement(statecraft_virtu_tr_t10, statecraft_virtu, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(statecraft_virtu_ex_t0, statecraft_virtu, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(statecraft_virtu_ex_t5, statecraft_virtu, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(statecraft_virtu_ex_t10, statecraft_virtu, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The state is, at its core, a mechanism for enforcing the Prince's will.
narrative_ontology:coordination_type(statecraft_virtu, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */