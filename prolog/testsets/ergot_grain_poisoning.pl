% ============================================================================
% CONSTRAINT STORY: ergot_grain_poisoning
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_ergot_grain_poisoning, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ergot_grain_poisoning
 * human_readable: The Ergot/Famine Dilemma
 * domain: social/technological/biological
 * * SUMMARY:
 * Ergot (Claviceps purpurea) is a fungal parasite of grains—primarily rye—that causes hallucinations, convulsions, and gangrenous death (St. Anthony's Fire). Historically, population pressure forced the cultivation of marginal lands where rye flourished, leading to a systemic trade-off: widespread famine vs. widespread poisoning. The constraint is the social system that managed this trade-off by asymmetrically distributing the poisoned grain to the poor.
 * * KEY AGENTS:
 * - The Peasantry: Subject (Powerless), forced to eat "spurred rye" during famines.
 * - Millers & Landowners: Beneficiary (Institutional), controlled grain distribution, keeping clean flour for the affluent and selling contaminated flour to the poor to avoid financial loss.
 * - The Analytical Historian: Auditor (Analytical), observes the pattern of "dancing manias" and the transition to the potato as a resolution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ergot_grain_poisoning, 0.80). % Snare extraction >= 0.46. Extracts health, sanity, and life.
domain_priors:suppression_score(ergot_grain_poisoning, 0.70).   % Alternatives (potato, scientific agriculture) were suppressed by ignorance and tradition.
domain_priors:theater_ratio(ergot_grain_poisoning, 0.10).       % Brutally functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ergot_grain_poisoning, extractiveness, 0.80).
narrative_ontology:constraint_metric(ergot_grain_poisoning, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ergot_grain_poisoning, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It was framed as a natural, unavoidable tragedy (natural_law), but the
% distribution system was a human construction.
narrative_ontology:constraint_claim(ergot_grain_poisoning, tangled_rope).
narrative_ontology:human_readable(ergot_grain_poisoning, "The Ergot/Famine Dilemma").

% Binary flags
domain_priors:requires_active_enforcement(ergot_grain_poisoning). % Required for Tangled Rope. Millers actively sold bad grain.

% Structural property derivation hooks:
% Beneficiaries derive has_coordination_function/1. Victims derive has_asymmetric_extraction/1.
narrative_ontology:constraint_beneficiary(ergot_grain_poisoning, landowners).
narrative_ontology:constraint_beneficiary(ergot_grain_poisoning, millers).
narrative_ontology:constraint_victim(ergot_grain_poisoning, peasantry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The peasantry faces a choice between starvation and poisoning. This is a
% classic predatory trap where survival comes at the cost of health and sanity.
constraint_indexing:constraint_classification(ergot_grain_poisoning, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For millers and landowners, the system is a Rope. It's a coordination
% mechanism to manage a kingdom-level resource crisis (famine), preserving
% social order and their own wealth by externalizing the biological cost.
constraint_indexing:constraint_classification(ergot_grain_poisoning, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The historian sees both the coordination function (avoiding mass famine) and
% the brutal, asymmetric extraction. It requires active enforcement (selling bad
% grain) and has clear victims and beneficiaries. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(ergot_grain_poisoning, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergot_grain_poisoning_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless (Snare) and institutional (Rope).
    constraint_indexing:constraint_classification(ergot_grain_poisoning, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergot_grain_poisoning, rope,
        context(agent_power(institutional), _, _, _)).

test(tangled_rope_properties) :-
    % Verify that the conditions for the analytical Tangled Rope classification are met.
    domain_priors:requires_active_enforcement(ergot_grain_poisoning),
    narrative_ontology:constraint_beneficiary(ergot_grain_poisoning, _),
    narrative_ontology:constraint_victim(ergot_grain_poisoning, _).

:- end_tests(ergot_grain_poisoning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system where a biological hazard was weaponized by social
 * stratification. The base extractiveness (0.80) is high due to the severe outcomes
 * (death, gangrene, madness) forced upon the victim class. The suppression (0.70)
 * reflects centuries of scientific ignorance, where the problem was framed as
 * demonic possession rather than a preventable fungal infection.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For the peasantry, it is an inescapable Snare (eat and be poisoned,
 * or don't eat and starve). For the institutional powers (millers, landowners), it is
 * a Rope—a tragic but necessary tool for coordinating resource scarcity to prevent
 * total societal collapse, while preserving their own well-being and profit.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Mandatrophy risk is that the system misinterprets the constraint as a pure
 * Mountain (an unavoidable natural tragedy) or a pure Snare (pointless cruelty).
 * The Tangled Rope classification for the analytical observer resolves this. It
 * correctly identifies that the constraint has a genuine, albeit grim, coordination
 * function (preventing famine) while simultaneously acknowledging that this function
 * is achieved through severe, asymmetric extraction imposed on a powerless group.
 * The system wasn't just cruel; it was functionally cruel to maintain a stratified order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ergot_grain_poisoning,
    "Was the distribution of spurred rye to the poor a functional necessity for kingdom survival (a tragic Tangled Rope) or an intentional, predatory extraction for profit (a pure Snare)?",
    "Audit of medieval grain stores and distribution logs vs. famine mortality rates to determine if clean grain was truly scarce or just hoarded.",
    "If necessity: Tangled Rope. If predatory hoarding: Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ergot_grain_poisoning, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This was a stable, long-term constraint. The extraction was consistently high,
% and the system was brutally functional (low theater) for centuries until
% scientific advances and new crops provided an exit.
%
% Theater ratio over time (consistently low):
narrative_ontology:measurement(ergot_grain_poisoning_tr_t0, ergot_grain_poisoning, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ergot_grain_poisoning_tr_t5, ergot_grain_poisoning, theater_ratio, 5, 0.10).
narrative_ontology:measurement(ergot_grain_poisoning_tr_t10, ergot_grain_poisoning, theater_ratio, 10, 0.10).

% Extraction over time (consistently high):
narrative_ontology:measurement(ergot_grain_poisoning_ex_t0, ergot_grain_poisoning, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(ergot_grain_poisoning_ex_t5, ergot_grain_poisoning, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(ergot_grain_poisoning_ex_t10, ergot_grain_poisoning, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's coordination function was managing the food supply.
narrative_ontology:coordination_type(ergot_grain_poisoning, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */