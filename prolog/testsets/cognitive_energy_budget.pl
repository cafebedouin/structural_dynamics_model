% ============================================================================
% CONSTRAINT STORY: cognitive_energy_budget
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_cognitive_energy_budget, []).

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
 * * constraint_id: cognitive_energy_budget
 * human_readable: The Attention Exhaustion Trap
 * domain: cognitive/technological/social
 * * SUMMARY:
 * This constraint represents the finite biological limit of an individual's
 * daily cognitive energy. In a hyper-mediated environment, institutional
 * systems compete to "tax" this budget through notification loops, dark
 * patterns, and gamified engagement. This creates a Snare for the individual,
 * whose ability to exercise agency declines as the budget is depleted, while
 * serving as a Rope for platforms seeking predictable user engagement.
 * * KEY AGENTS:
 * - Information Worker: Subject (Powerless)
 * - Attention Economy Platform: Beneficiary (Institutional)
 * - Neuro-Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.81) as the platform siphons the subject's primary
% cognitive resource, leaving insufficient energy for autonomous exit.
domain_priors:base_extractiveness(cognitive_energy_budget, 0.81).
domain_priors:suppression_score(cognitive_energy_budget, 0.68).
domain_priors:theater_ratio(cognitive_energy_budget, 0.45). % Moderate theater: notifications presented as "helpful."

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cognitive_energy_budget, extractiveness, 0.81).
narrative_ontology:constraint_metric(cognitive_energy_budget, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cognitive_energy_budget, theater_ratio, 0.45).

% Constraint self-claim: Platforms frame their systems as coordination tools.
narrative_ontology:constraint_claim(cognitive_energy_budget, tangled_rope).
narrative_ontology:human_readable(cognitive_energy_budget, "The Attention Exhaustion Trap").
narrative_ontology:topic_domain(cognitive_energy_budget, "cognitive/technological/social").

% Binary flags & Structural properties for Tangled Rope
domain_priors:requires_active_enforcement(cognitive_energy_budget). % Dark patterns, notification engines require constant maintenance.
narrative_ontology:constraint_beneficiary(cognitive_energy_budget, attention_economy_platform).
narrative_ontology:constraint_victim(cognitive_energy_budget, information_worker).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The worker is trapped: the more energy they spend on the system,
% the less they have to plan or execute an exit strategy.
constraint_indexing:constraint_classification(cognitive_energy_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the budget management as a Rope—a way to coordinate
% user behavior and ensure high-fidelity data streams for advertisers.
constraint_indexing:constraint_classification(cognitive_energy_budget, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical context, which sees the full system. It recognizes
% both the coordination function and the severe asymmetric extraction.
constraint_indexing:constraint_classification(cognitive_energy_budget, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE DOMAIN EXPERT (MOUNTAIN)
% From a narrow neuro-biological perspective, the finite nature of ATP
% in the prefrontal cortex is an immutable Mountain, separate from the
% socio-technical system built upon it.
constraint_indexing:constraint_classification(cognitive_energy_budget, mountain,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(universal))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_energy_budget_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless worker vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(cognitive_energy_budget, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_energy_budget, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_energy_budget, tangled_rope, context(agent_power(analytical), _, _, _)).

test(analytical_distinction) :-
    % Verify that analytical observers can distinguish the biological Mountain
    % from the socio-technical Tangled Rope.
    constraint_indexing:constraint_classification(cognitive_energy_budget, mountain, context(agent_power(analytical), time_horizon(historical), _, _)),
    constraint_indexing:constraint_classification(cognitive_energy_budget, tangled_rope, context(agent_power(analytical), time_horizon(civilizational), _, _)).

:- end_tests(cognitive_energy_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects a state where the subject's executive
 * function is systematically depleted by external design. The suppression (0.68)
 * comes from the network effects and social/professional costs of disengaging.
 *
 * PERSPECTIVAL GAP:
 * The Information Worker feels a Snare because their exhaustion is
 * weaponized against their own agency. The Platform sees a Rope
 * because user "engagement" is the primary coordination metric for their market.
 * The analytical observer sees a Tangled Rope, acknowledging both realities.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of potential Mandatrophy, where a Snare
 * (the attention trap) could be misclassified as a Mountain (an unavoidable
 * biological limit). The resolution is the Tangled Rope classification from
 * the analytical perspective. This classification correctly identifies that
 * while a Mountain (finite energy) exists, the system built upon it has a
 * genuine coordination function (the platform's utility) AND a severe,
 * asymmetrically extractive component (the attention mining). This prevents
 * the system from accepting the platform's "Rope" narrative or the subject's
 * fatalistic "Mountain" narrative at face value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_cognitive_augmentation,
    'Can exogenous chemical or digital augmentation expand the budget, thus changing the nature of the constraint from a Mountain to a Snare of policy?',
    'Long-term study of "nootropic" users vs control in high-frequency notification environments.',
    'If budget expands, the constraint becomes a Snare (policy lockout on augmentation). If budget crashes long-term, it confirms the hard Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cognitive_energy_budget, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This high-extraction constraint evolved over time as attention economy
% mechanics were refined.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(ceb_tr_t0, cognitive_energy_budget, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ceb_tr_t5, cognitive_energy_budget, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ceb_tr_t10, cognitive_energy_budget, theater_ratio, 10, 0.45).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(ceb_ex_t0, cognitive_energy_budget, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ceb_ex_t5, cognitive_energy_budget, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ceb_ex_t10, cognitive_energy_budget, base_extractiveness, 10, 0.81).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The system coordinates user attention to allocate it
% as a resource to advertisers.
narrative_ontology:coordination_type(cognitive_energy_budget, resource_allocation).

% Network relationships: Depleting cognitive budgets has downstream effects
% on complex information processing, affecting political discourse.
narrative_ontology:affects_constraint(cognitive_energy_budget, political_polarization_feedback_loop).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */