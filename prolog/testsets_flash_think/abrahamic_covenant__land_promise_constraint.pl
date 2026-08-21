% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant: Land Promise Constraint (Land-Grant Reading)
 *   domain: religious_studies/geopolitical/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Abrahamic covenant,
 *   focusing on the literal and unconditional territorial grant of the Land
 *   of Canaan. This reading is actively leveraged by state actors and
 *   religious nationalists to legitimize territorial claims and policies,
 *   directly mapping onto the modern Israeli-Palestinian conflict. The
 *   constraint operates as a snare, extracting land and sovereignty from
 *   indigenous populations through active enforcement and ideological
 *   suppression. The high extractiveness and suppression reflect the ongoing
 *   conflict and displacement.
 *
 * KEY AGENTS:
 *   - state_actors_leveraging_claim: Primary agenda_setter (institutional/arbitrage) — benefits from constraint
 *   - religious_nationalists: Primary beneficiary (organized/identity_locked) — benefits from constraint
 *   - displaced_populations: Primary payer (powerless/trapped) — bears extraction
 *   - indigenous_inhabitants: Primary payer (powerless/identity_locked) — bears extraction
 *   - international_observers: Analytical observer (institutional/analytical) — monitors conflict
 *   - theologians_and_scholars: Analytical observer (analytical/analytical) — analyzes interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.85).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.9).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.85).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant: Land Promise Constraint (Land-Grant Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/geopolitical/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, 'c6513abb-d7e7-41ea-b5eb-0b13d3facdfb').
narrative_ontology:cs_kernel_codification('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', fixed_text).
narrative_ontology:cs_authority_grounding('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', lineage).
narrative_ontology:cs_interpretation_layer_present('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb').
narrative_ontology:cs_reading_relation('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', foundational, land_promise_is_unconditional_and_eternal).
narrative_ontology:cs_axiom_status(land_promise_is_unconditional_and_eternal, holdable).
narrative_ontology:cs_axiom_grounding('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', land_promise_is_unconditional_and_eternal, theological).
narrative_ontology:cs_axiom('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', foundational, divine_grant_supersedes_prior_occupancy).
narrative_ontology:cs_axiom_status(divine_grant_supersedes_prior_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', divine_grant_supersedes_prior_occupancy, theological).
narrative_ontology:cs_reference_frame('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', unconditional_eternal_territorial_grant).
narrative_ontology:cs_drift_state('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', contemporary_geopolitical_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6513abb-d7e7-41ea-b5eb-0b13d3facdfb', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, state_actors_leveraging_claim).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_nationalists).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, indigenous_inhabitants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforce and benefit from the territorial claim derived from this reading of the covenant. They use the religious narrative to legitimize political and military control over disputed land, collecting resources and sovereignty.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, state_actors_leveraging_claim, agenda_setter,
    institutional, generational, arbitrage, national).

% Ideologically committed to the literal and unconditional interpretation of the land promise. They benefit from the political and social validation of their claims and the expansion of control over the 'promised land'.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_nationalists, beneficiary,
    organized, generational, identity_locked, national).

% Bear the direct costs of displacement, loss of ancestral lands, and denial of self-determination. They are often refugees or stateless, with severely constrained options for return or compensation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_populations, payer,
    powerless, generational, trapped, local).

% Their ancestral claims and historical presence on the land are overridden by the covenantal narrative. They face ongoing dispossession, restrictions on movement, and denial of rights, often feeling identity-locked to their land despite coercive pressures.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, indigenous_inhabitants, payer,
    powerless, generational, identity_locked, local).

% Monitor the conflict, document human rights abuses, and engage in diplomatic efforts. They analyze the situation from a secular, international law perspective, often finding the covenantal claims problematic but lack direct enforcement power over the religious interpretation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_observers, observer,
    institutional, immediate, analytical, global).

% Analyze the covenant texts, their historical contexts, and diverse interpretations. Many disagree with the literal, unconditional, and ongoing application of the land-grant reading to modern geopolitics, emphasizing conditionalities, spiritual interpretations, or universal justice.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, theologians_and_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of this reading, it coordinates the claim to a specific territory (Land of Canaan) for a designated people, providing a foundational narrative for national identity and territorial rights.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from indigenous inhabitants and other claimants to those who assert a divine right based on this covenantal reading, often through military and political means.
% ABSENT_VOICES: Indigenous voices, alternative theological interpretations that emphasize justice, universalism, or the conditional nature of the promise, and secular perspectives that reject the literal application of ancient religious texts to modern statehood. These voices are often suppressed or marginalized in the dominant discourse.
% DISAPPEARANCE_RATIONALE: If this specific reading of the Abrahamic covenant (as an unconditional, ongoing, and literal territorial grant) vanished overnight, the primary ideological and religious justification for certain territorial claims would disappear. This would necessitate a fundamental re-evaluation of land ownership, sovereignty, and national identity, potentially de-escalating conflict and reorganizing geopolitical arrangements in the region.
% FOUNDING_PROBLEM: To establish a divine promise of land and lineage for Abraham and his descendants, ensuring their future prosperity and identity as a chosen people.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the religious texts themselves (Genesis narratives) and centuries of theological tradition within specific faith communities. However, its status as 'live' in a literal, geopolitical sense is contested by secular historians, international legal scholars, and alternative theological readings that view the promise as fulfilled, conditional, or spiritual rather than territorial. Corroboration for its ongoing literal application is primarily internal to the benefiting parties.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the direct transfer of land and resources from one group to another, justified by a religious claim. Suppression (0.90) is also very high, reflecting the active military, legal, and political mechanisms used to enforce the territorial claims and prevent alternative outcomes or the return of displaced populations. The theater ratio (0.10) is low because the constraint's function is very real and actively maintained, with little performative overhead. Accessibility collapse (0.80) is high as alternatives for victims (e.g., self-determination, return to land) are severely curtailed. Resistance (0.90) is high, reflecting the ongoing, active opposition from those who bear the costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state actors and religious nationalists, this reading of the covenant provides a legitimate, even divine, basis for their actions and claims. For displaced populations and indigenous inhabitants, the same constraint is experienced as an existential threat, a source of dispossession, and a justification for their ongoing suffering. The engine's classification will highlight this divergence, showing a snare for the victims and a beneficiary position for the agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors and religious nationalists are clear beneficiaries, leveraging the covenant for territorial control and ideological fulfillment (low d). Displaced populations and indigenous inhabitants are direct targets, losing land and rights (high d). International observers and scholars are analytical seats, not directly benefiting or paying, but observing the extractive dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved; the constraint is actively maintained and enforced because it serves a live, albeit contested, political and ideological function for its beneficiaries. The 'founding problem' of establishing a divine land promise is still considered 'live' by the benefiting parties, even if its contemporary application is highly disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine divine promise of land, or a human interpretation used to legitimize political and territorial control?',
    'Analysis of theological hermeneutics, historical context, and the material consequences of its application. If the primary function is found to be political legitimation rather than divine imperative, it supports the ''constructed constraint'' view.',
    'If primarily a human construct, the constraint''s naturalness claim is false, reinforcing its classification as a snare and highlighting the ideological cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between divine mandate and human political interpretation.').

omega_variable(
    conditionality_of_promise,
    'Is the land promise in the Abrahamic covenant unconditional and eternal, or is it conditional on obedience and subject to fulfillment or revocation?',
    'Comparative theological analysis of covenant texts, prophetic literature, and rabbinic/Islamic interpretive traditions regarding conditions for land tenure. If strong conditionalities are found, it challenges the unconditional nature of this reading.',
    'If conditional, the justification for ongoing, unconditional territorial claims weakens, potentially reducing the perceived legitimacy of extraction and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditionality_of_promise, empirical, 'Whether the land promise is conditional or unconditional.').

omega_variable(
    fulfillment_status_of_promise,
    'Has the land promise already been fulfilled in ancient times (e.g., during the Israelite monarchy), or is it an ongoing, unfulfilled promise applicable to modern statehood?',
    'Historical and archaeological research combined with theological interpretation of fulfillment narratives within the texts. If historical fulfillment is established, the ''ongoing promise'' claim is challenged.',
    'If fulfilled historically, the justification for contemporary territorial claims based on an ''unfulfilled'' promise is undermined, potentially reclassifying the constraint''s persistence as purely extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fulfillment_status_of_promise, empirical, 'Whether the land promise is fulfilled or ongoing.').

omega_variable(
    scope_of_descendants,
    'Who precisely constitutes ''Abraham''s descendants'' in the modern context, and does this include all ethnic, religious, or national groups claiming lineage?',
    'Genetic, historical, and theological studies of lineage, as well as analysis of how different traditions define ''descendant.'' If the definition is found to be narrowly constructed to exclude certain groups, it highlights the selective application of the covenant.',
    'A broader or more inclusive definition of ''descendants'' would challenge the exclusivity of the land claim, potentially expanding the beneficiary set or re-framing the constraint as a shared heritage rather than an exclusive grant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_descendants, conceptual, 'Definition of ''Abraham''s descendants'' in modern context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(abra_tr_t1987, abrahamic_covenant__land_promise_constraint, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(abra_tr_t2023, abrahamic_covenant__land_promise_constraint, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.65).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.75).
narrative_ontology:measurement(abra_be_t1987, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1987, 0.8).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(abra_be_t2023, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(abra_su_t1987, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1987, 0.85).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(abra_su_t2023, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2023, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, israeli_palestinian_conflict_dynamics).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, right_of_return_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Abrahamic covenant kernel, which also includes the 'isaac_covenant_reading' and 'ishmael_covenant_reading' as sibling interpretations. This specific reading focuses on the literal territorial grant and its geopolitical application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
