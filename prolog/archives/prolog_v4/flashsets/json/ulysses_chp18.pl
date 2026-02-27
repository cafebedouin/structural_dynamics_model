% ============================================================================
% CONSTRAINT STORY: ulysses_chp18
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp18, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp18
 *   human_readable: The Penelopean Affirmation (7 Eccles Street)
 *   domain: social/psychological/biological
 *
 * SUMMARY:
 *   Chapter 18 of Ulysses presents Molly Bloom's interior monologue,
 *   culminating in her affirmation. This constraint story focuses on the
 *   affirmation's role in providing closure and meaning to the novel, and how
 *   different actors derive benefit. The overall extractiveness score is low,
 *   as this affirmation mostly serves to coordinate a satisfying narrative
 *   experience. However, it also extracts from Molly Bloom's autonomy as a
 *   character.
 *
 * KEY AGENTS:
 *   - Readers seeking emotional connection: Primary beneficiary (moderate/mobile) — gains insight and empathy from Molly's perspective.
 *   - Narrative coherence of Ulysses: Secondary beneficiary (institutional/arbitrage) — achieves resolution and thematic unity through the affirmation.
 *   - Literary scholars: Analytical observer (analytical/analytical) — interprets the chapter's significance and lasting impact.
 *   - Molly Bloom: Powerful agent (powerful/constrained) — benefits from narrative structure, but extracted for use as a literary device
 *   - Molly Bloom's Autonomy: Primary Victim (powerless/trapped) - Molly's autonomy as a character is trapped within the narrative. She has no agency outside of Joyce's writing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp18, 0.3).
domain_priors:suppression_score(ulysses_chp18, 0.15).
domain_priors:theater_ratio(ulysses_chp18, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp18, extractiveness, 0.3).
narrative_ontology:constraint_metric(ulysses_chp18, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ulysses_chp18, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp18, tangled_rope).
narrative_ontology:human_readable(ulysses_chp18, "The Penelopean Affirmation (7 Eccles Street)").
narrative_ontology:topic_domain(ulysses_chp18, "social/psychological/biological").

domain_priors:requires_active_enforcement(ulysses_chp18).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp18, readers_seeking_emotional_connection).
narrative_ontology:constraint_beneficiary(ulysses_chp18, narrative_coherence_of_ulysses).
narrative_ontology:constraint_victim(ulysses_chp18, molly_blooms_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The reader engages with the text and derives meaning and emotional connection from Molly's stream of consciousness.
constraint_indexing:constraint_classification(ulysses_chp18, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Scholars analyze the chapter's structure and significance within the broader context of Ulysses.
constraint_indexing:constraint_classification(ulysses_chp18, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% The chapter contributes to the novel's enduring status and influence within the literary canon. This is coordination on a large scale.
constraint_indexing:constraint_classification(ulysses_chp18, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Molly Bloom is a character within the confines of the Ulysses text. Her exit_options are constrained. She benefits from the narrative structure and affirmation provided by Joyce, but she is also being extracted as a literary device.
constraint_indexing:constraint_classification(ulysses_chp18, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Molly Bloom's autonomy as a character is trapped within the narrative. She has no agency outside of Joyce's writing.
constraint_indexing:constraint_classification(ulysses_chp18, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp18_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp18, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp18, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ulysses_chp18_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.30): Low-moderate. Molly's thoughts are being used, and some argue exploited, to generate a connection within the novel. The benefit flows primarily towards the reader. The suppression here is low: readers are able to interpret the novel as they see fit. The theater is also low: this constraint is an honest coordination of literary intention. The addition of Molly Bloom's autonomy as a victim and the requires_active_enforcement flag changes the classification to Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The individual reader connects with Molly's experience, while scholars examine the chapter's structure and implications. This is a rope for the reader and scholars since the constraint serves primarily to coordinate a satisfying reading experience. However, for Molly Bloom's autonomy, it is a snare, as she is trapped within the narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The reader benefits from accessing a deeply human view of the world. Literary scholars can extract meaning from the novel, but they are also a beneficiary of the overall work. The institutional context solidifies it as a cultural touchstone. Molly Bloom's autonomy, however, is a victim of the narrative structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp18, 1904, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp18, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
