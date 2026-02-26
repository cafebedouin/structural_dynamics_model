% ============================================================================
% CONSTRAINT STORY: antikythera_knowledge_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antikythera_knowledge_loss, []).

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
 *   constraint_id: antikythera_knowledge_loss
 *   human_readable: Loss of Hellenistic Precision Gearing Knowledge
 *   domain: technological/historical
 *
 * SUMMARY:
 *   The Antikythera Mechanism reveals a level of Hellenistic engineering that
 *   was subsequently lost for over 1,500 years. The constraint is not the
 *   device itself, but the set of systemic factors—knowledge concentration,
 *   institutional fragility, and societal upheaval—that allowed this advanced
 *   capability to be completely erased from the technological landscape. This
 *   systemic fragility acted as a snare, trapping knowledge and imposing a
 *   massive opportunity cost on subsequent generations.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) - Bore the 1500-year cost of the technological reset.
 *   - Hellenistic Engineers' Lineage: Secondary victim (moderate/constrained) - Possessed valuable skills but were dependent on a fragile system that ultimately collapsed.
 *   - Elite Patrons of Antiquity: Primary beneficiary (institutional/arbitrage) - Used the rare technology as a tool for coordination and status.
 *   - Dogmatic Knowledge Systems: Secondary beneficiary (institutional/constrained) - The absence of a mechanical, predictive model of the cosmos reinforced worldviews based on divine will rather than natural law.
 *   - Modern Historians: Analytical observer - Perceive the full scope of the systemic failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antikythera_knowledge_loss, 0.65).
domain_priors:suppression_score(antikythera_knowledge_loss, 0.75).
domain_priors:theater_ratio(antikythera_knowledge_loss, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antikythera_knowledge_loss, extractiveness, 0.65).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(antikythera_knowledge_loss, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antikythera_knowledge_loss, snare).
narrative_ontology:human_readable(antikythera_knowledge_loss, "Loss of Hellenistic Precision Gearing Knowledge").
narrative_ontology:topic_domain(antikythera_knowledge_loss, "technological/historical").

domain_priors:requires_active_enforcement(antikythera_knowledge_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antikythera_knowledge_loss, elite_patrons_of_antiquity).
narrative_ontology:constraint_beneficiary(antikythera_knowledge_loss, dogmatic_knowledge_systems).
narrative_ontology:constraint_victim(antikythera_knowledge_loss, future_generations).
narrative_ontology:constraint_victim(antikythera_knowledge_loss, hellenistic_engineers_lineage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) - Trapped by the arrow of time, future civilizations bore the full 1500-year opportunity cost of this lost knowledge. They had no exit and could not influence the past. The loss represents a pure, unrecoverable extraction of technological potential. d≈0.95, f(d)≈1.42, σ=1.2 -> χ≈1.11.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ELITE PATRONS (ROPE) - For the wealthy individuals who commissioned such devices, the technology was a coordination tool for demonstrating status, predicting events, and organizing society (e.g., timing of games). Its rarity enhanced its value. They were pure beneficiaries with arbitrage power. d≈0.05, f(d)≈-0.12, σ=0.9 -> χ≈-0.07.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: HELLENISTIC ENGINEERS (TANGLED ROPE) - The artisans who built the mechanism benefited from their unique, high-value skills (coordination) but were also victims of a system where this knowledge was not widely disseminated and was dependent on fragile patronage networks (extraction). Their lineage was trapped by societal collapse. d≈0.75, f(d)≈1.10, σ=0.8 -> χ≈0.57.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SNARE) - The analytical view sees a systemic failure. The combination of knowledge concentration, institutional fragility, and societal upheaval created a structural trap that captured and extinguished a critical technological capability for over a millennium. This matches the claimed type.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: CYCLICAL HISTORY VIEW (MOUNTAIN) - This perspective naturalizes the loss, framing it as an inevitable consequence of civilizational rise and fall, a 'law of history'. It misclassifies a contingent institutional failure as an unchangeable mountain. The engine will flag this as a false summit, as the base properties (high ε and suppression) are inconsistent with a natural law.
constraint_indexing:constraint_classification(antikythera_knowledge_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antikythera_knowledge_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antikythera_knowledge_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antikythera_knowledge_loss, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antikythera_knowledge_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antikythera_knowledge_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65): Represents the immense opportunity cost imposed on subsequent civilizations. The 1500-year gap in precision mechanical engineering is a direct measure of the value extracted from the timeline. Suppression (0.75): High. The combination of the collapse of the Roman empire, the destruction of libraries (like Alexandria), and the rise of worldviews less conducive to mechanical philosophy created an environment where rediscovery was nearly impossible. The alternative (possessing this knowledge) was effectively suppressed. Theater Ratio (0.10): Low. The loss was a structural failure, not a performative act.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the elite patron, the device was a useful Rope for coordination. For the engineer, it was a Tangled Rope of specialized skill and precarious employment. For future generations, the system that allowed its loss was a catastrophic Snare. For a determinist historian, the loss appears as an inevitable Mountain of civilizational cycles. This demonstrates how a single historical event is classified differently based on structural position relative to the flow of knowledge and time.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the ancient elites who monopolized the knowledge and the later dogmatic systems whose dominance was unchallenged by a mechanistic worldview. The victims are the artisans whose craft died and, most significantly, all subsequent generations who had to re-derive the lost principles from scratch. The high extraction value is primarily borne by these powerless, temporally distant victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy where 'history' is seen as a series of random events. The framework classifies the loss not as an accident, but as a structural failure of a knowledge-preservation system. It was a Snare with identifiable properties (high concentration, low dissemination, institutional fragility) that predictably traps and extinguishes complex, non-obvious knowledge during periods of instability. The 'Mountain' perspective is revealed as a false summit that naturalizes this preventable failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_dissemination_scope,
    'How widespread was the knowledge to build these mechanisms in the Hellenistic world?',
    'Archaeological discovery of other similar mechanisms, fragments, or workshops; textual analysis of engineering treatises.',
    'If knowledge was widespread (e.g., dozens of workshops), the loss is more indicative of active suppression (Snare). If it was confined to a single school or workshop, the loss is more a result of fragility (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_dissemination_scope, empirical, 'The extent of precision gearing knowledge in antiquity.').

omega_variable(
    active_suppression_vs_neglect,
    'Was the loss of knowledge a result of passive societal decay and neglect, or was there active suppression by rising dogmatic/religious ideologies?',
    'Historical analysis of late Roman and early medieval texts for evidence of suppression of mechanical philosophy or specific scientific traditions.',
    'Clear evidence of active suppression would confirm the Snare classification and the ''requires_active_enforcement'' flag. A finding of pure neglect would weaken the Snare case in favor of a catastrophic Piton (a functional system that simply broke and was forgotten).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(active_suppression_vs_neglect, empirical, 'Distinguishing between active suppression and passive neglect.').

omega_variable(
    mechanism_practical_efficacy,
    'How accurate and reliable was the mechanism in practice?',
    'Advanced modeling of gear wear, friction, and potential inaccuracies based on known Hellenistic manufacturing tolerances.',
    'If highly effective, its loss is a true Snare. If it was impressive but ultimately unreliable, it may have been a technological dead-end that was abandoned for practical reasons, making the ''loss'' more of a Piton-like degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_practical_efficacy, empirical, 'The real-world reliability and accuracy of the device.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antikythera_knowledge_loss, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antikythera_knowledge_loss, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t400, antikythera_knowledge_loss, theater_ratio, 400, 0.1).
narrative_ontology:measurement(anti_tr_t1600, antikythera_knowledge_loss, theater_ratio, 1600, 0.1).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antikythera_knowledge_loss, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anti_be_t400, antikythera_knowledge_loss, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(anti_be_t1600, antikythera_knowledge_loss, base_extractiveness, 1600, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antikythera_knowledge_loss, information_standard).
narrative_ontology:affects_constraint(antikythera_knowledge_loss, medieval_clockmaking_delay).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
