% ============================================================================
% CONSTRAINT STORY: bushman_money_magic
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_bushman_money_magic, []).

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
 * * constraint_id: bushman_money_magic
 * human_readable: The Trickster's Asymmetric Scam
 * domain: economic/social
 * * SUMMARY:
 * This constraint represents a "magic scam" where a trickster figure (Jackal) utilizes
 * deception and temporary narrative control to extract wealth from victims (cattle ranchers).
 * It functions by hiding physical causality (fire under sand, money in a donkey) to present
 * a "magic" alternative that appears highly beneficial but is functionally empty. The
 * constraint is the temporary information asymmetry that enables the fraudulent exchange.
 * * KEY AGENTS:
 * - Jackal (The Trickster): Beneficiary; constructs the "magic" constraint to extract wealth.
 * - Cattle Ranchers (The Victims): Subjects who "agree" to the asymmetric exchange due to greed and suppressed context.
 * - Anthropologist: Analytical observer viewing the event as an archetype.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: High (0.9). The exchange is entirely one-sided; Jackal extracts wealth
% in exchange for zero functional value.
domain_priors:base_extractiveness(bushman_money_magic, 0.90).
% Rationale: High (0.8). The physical reality (the fire under the sand) is
% actively hidden to maintain the "magic" narrative. The truth is suppressed.
domain_priors:suppression_score(bushman_money_magic, 0.80).
% Rationale: Low (0.1). The performance is the function of the scam, not a
% substitute for a decayed function. There is no institutional theater here.
domain_priors:theater_ratio(bushman_money_magic, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(bushman_money_magic, extractiveness, 0.90).
narrative_ontology:constraint_metric(bushman_money_magic, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(bushman_money_magic, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The scam is a constructed reality, not a natural law or a coordination tool.
narrative_ontology:constraint_claim(bushman_money_magic, tangled_rope).

% Binary flags
% The scam requires Jackal's active performance to maintain the illusion.
domain_priors:requires_active_enforcement(bushman_money_magic).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(bushman_money_magic, jackal_the_trickster).
narrative_ontology:constraint_victim(bushman_money_magic, cattle_ranchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE VICTIMS (CATTLE RANCHERS) - SNARE
% The scam is a Snare. The ranchers "agree" to the rules, but those rules
% systematically extract their wealth while providing a "magic" illusion that
% tightens their loss. They are trapped by the trickster's narrative control.
constraint_indexing:constraint_classification(bushman_money_magic, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (JACKAL) - ROPE
% For Jackal, the "magic" items are a "Rope"—a functional coordination mechanism
% he uses to pull resources from the ranchers to himself. It is a tool of agency
% to achieve a "full belly" in the desert.
constraint_indexing:constraint_classification(bushman_money_magic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ANTHROPOLOGIST) - MOUNTAIN
% To the observer, the "scam" is a "Mountain"—an immutable feature of human
% social systems where information asymmetry and greed inevitably produce
% asymmetric extraction. It is a natural law of social interaction.
constraint_indexing:constraint_classification(bushman_money_magic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bushman_money_magic_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(bushman_money_magic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bushman_money_magic, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_perspective) :-
    % Verify the analytical observer sees a Mountain.
    constraint_indexing:constraint_classification(bushman_money_magic, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_snare) :-
    % Verify the base metrics align with a high-extraction constraint.
    domain_priors:base_extractiveness(bushman_money_magic, E),
    domain_priors:suppression_score(bushman_money_magic, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(bushman_money_magic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a classic predatory scam. Base extractiveness is 0.90 because the
 * exchange is entirely one-sided, with the ranchers losing all their investment for
 * zero functional value. Suppression is 0.80 because the scam's existence depends
 * entirely on actively hiding the physical truth (the fire, the hidden money). Theater
 * ratio is low (0.10) because the performance *is* the function; it is not a decayed
 * ritual replacing a lost function.
 *
 * The Perspectival Gap is stark:
 * - For the Ranchers (powerless, trapped), it's a Snare. They are caught by a promise
 *   that extracts their resources.
 * - For Jackal (institutional, mobile), it's a Rope. He is the rule-maker of this
 *   micro-system, and the scam is his tool for survival/extraction.
 * - For the Analyst (analytical, civilizational), the specific event is an instance of
 *   a timeless pattern of deception, an immutable feature of social dynamics, hence a Mountain.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This is a clear case of a Snare, not a Tangled Rope. While Jackal
 * benefits, there is no genuine coordination function for a wider group. The system
 * correctly identifies the perspectival gap where the beneficiary sees a tool (Rope)
 * and the victim sees a trap (Snare), while the analytical view classifies it based on
 * its high-extraction, high-suppression metrics. The Mountain classification from the
 * analytical perspective is an interesting edge case, reflecting the view that "there will
 * always be tricksters," treating the archetype itself as a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    omega_bushman_money_magic_1,
    "Is Jackal's scam a functional necessity for survival in a hyper-arid 'Empty Quarter' or an intentional predatory choice?",
    "Audit of Jackal's resource alternatives vs. the frequency of the 'trickster' behavior in the folklore.",
    "If necessity: Survival Mountain. If predatory choice: Mandatrophy Snare.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_bushman_money_magic_2,
    "Do the ranchers learn to bypass the trickster in the next encounter, or is their greed a fixed trait that can always be exploited?",
    "Follow-up on 'successive occasions' of ranchers meeting Jackal in the narrative corpus.",
    "If they learn: Trickery is a temporary Snare. If not: It is a systemic Mountain of human behavior.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bushman_money_magic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraints (E > 0.46).
% The scam is a singular, stable event. The metrics do not drift over its short lifecycle.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(bmm_tr_t0, bushman_money_magic, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bmm_tr_t5, bushman_money_magic, theater_ratio, 5, 0.10).
narrative_ontology:measurement(bmm_tr_t10, bushman_money_magic, theater_ratio, 10, 0.10).

% Extraction over time (stable and high):
narrative_ontology:measurement(bmm_ex_t0, bushman_money_magic, base_extractiveness, 0, 0.90).
narrative_ontology:measurement(bmm_ex_t5, bushman_money_magic, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(bmm_ex_t10, bushman_money_magic, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is a pure extraction mechanism with no genuine coordination
% function, so Boltzmann data is not applicable. It is a standalone narrative
% with no specified network relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */