% ============================================================================
% CONSTRAINT STORY: ulysses_lotus_1904
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_ulysses_lotus, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ulysses_lotus_1904
 * human_readable: The Narcotic Social Rope (Lotus Eaters)
 * domain: social/religious/technological
 * * SUMMARY:
 * Leopold Bloom navigates the "lethargy" and "flowers of idleness" of 1904 
 * Dublin. Shared rituals—the post office secrecy, the aromatic tea 
 * advertisements, and the Catholic Mass—function as a Rope of coordination 
 * that anchors his social existence. However, for Bloom as "Henry Flower," 
 * these same rituals become a Snare of "Agenbite of inwit" and spiritual 
 * "sleeping sickness."
 * * KEY AGENTS:
 * - Leopold Bloom: Subject (Powerless) - The "naughty boy" seeking punishment.
 * - The Catholic Church: Beneficiary (Institutional) - The "wonderful organisation" of clockwork salvation.
 * - Martha Clifford: Auditor (Analytical) - Probing through the "language of flowers."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ulysses_lotus_1904, 0.48). 
domain_priors:suppression_score(ulysses_lotus_1904, 0.70).   
domain_priors:theater_ratio(ulysses_lotus_1904, 0.85). % High mummery: mass, flowers, and pseudonyms.

% Primary keys for classification engine
narrative_ontology:constraint_metric(ulysses_lotus_1904, extractiveness, 0.48).
narrative_ontology:constraint_metric(ulysses_lotus_1904, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ulysses_lotus_1904, theater_ratio, 0.85).

% Constraint classification claim
narrative_ontology:constraint_claim(ulysses_lotus_1904, piton).
narrative_ontology:human_readable(ulysses_lotus_1904, "The Narcotic Social Rope (Lotus Eaters)").
narrative_ontology:topic_domain(ulysses_lotus_1904, "social/religious/technological").

% Structural Property Declarations
narrative_ontology:constraint_beneficiary(ulysses_lotus_1904, holy_roman_church).
narrative_ontology:constraint_victim(ulysses_lotus_1904, leopold_bloom).
domain_priors:requires_active_enforcement(ulysses_lotus_1904). % Social etiquette and the secret correspondence.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: BLOOM AS "HENRY FLOWER" (SNARE)
% Effective Extraction: 0.48 * 1.5 (powerless) * 0.8 (local) = 0.576.
% Perceived as a "poison bouquet" of secret guilt and "punishment."
constraint_indexing:constraint_classification(ulysses_lotus_1904, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE CHURCH MILITANT (ROPE)
% Effective Extraction: 0.48 * -0.2 (institutional) * 0.8 = -0.0768.
% Viewed as the "clockwork" coordination of "bread of angels" and family parties.
constraint_indexing:constraint_classification(ulysses_lotus_1904, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Theater Ratio (0.85) indicates a Piton of "mummery" and "vanished crowds."
constraint_indexing:constraint_classification(ulysses_lotus_1904, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_lotus_1904_tests).

test(perspectival_gap) :-
    % Verify the shift from Snare (Bloom) to Rope (Church).
    constraint_indexing:constraint_classification(ulysses_lotus_1904, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_lotus_1904, rope, context(agent_power(institutional), _, _, _)).

test(piton_theater_check) :-
    % Piton classification requires theater_ratio >= 0.70.
    domain_priors:theater_ratio(ulysses_lotus_1904, TR),
    TR >= 0.70.

:- end_tests(ulysses_lotus_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Leopold Bloom experiences Dublin as a site of narcotic coordination. 
 * Shared spaces like the post office and church function as a Rope of 
 * daily movement, yet his internal "Henry Flower" identity is a Snare 
 * where extraction is felt as the "punishment" demanded by Martha Clifford. 
 * The high Theater Ratio (0.85) stems from the "mummery" of the Mass 
 * (described as "eating bits of a corpse") and the performative "language 
 * of flowers" that Bloom uses to mask his material and emotional isolation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_narcotic_solipsism,
    "Is Bloom's 'Henry Flower' persona a Rope for growth or a Snare of isolation?",
    "Review of the 'word known to all men' vs the 'language of flowers'.",
    "Communication confirms a Rope; secret letters harden the Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_lotus_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater Ratio (Peaking during the Communion ritual and the reading of Martha's letter)
narrative_ontology:measurement(ul_tr_t0, ulysses_lotus_1904, theater_ratio, 0, 0.70).
narrative_ontology:measurement(ul_tr_t5, ulysses_lotus_1904, theater_ratio, 5, 0.88).
narrative_ontology:measurement(ul_tr_t10, ulysses_lotus_1904, theater_ratio, 10, 0.85).

% Extraction (The increasing weight of Bloom's "naughty boy" guilt and loneliness)
narrative_ontology:measurement(ul_ex_t0, ulysses_lotus_1904, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ul_ex_t5, ulysses_lotus_1904, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(ul_ex_t10, ulysses_lotus_1904, base_extractiveness, 10, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
