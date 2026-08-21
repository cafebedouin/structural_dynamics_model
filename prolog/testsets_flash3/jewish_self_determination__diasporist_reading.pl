% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Reading of Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the diasporist reading of Jewish
 *   self-determination, which posits that Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism and minority
 *   rights, rather than territorial sovereignty. It views Zionism as a
 *   dangerous deviation that ties Jewish fate to a militarized state,
 *   endangering Jews globally through association. The constraint is
 *   classified as a Piton because the diasporist alternative, while
 *   historically significant, has been largely atrophied and marginalized by
 *   the hegemonic Zionist narrative, persisting more through intellectual and
 *   cultural inertia than active institutional support, yet still extracting
 *   costs from those who adhere to it or are perceived to be associated with
 *   it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.55).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.65).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__diasporist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '1f81b445-aa3a-4c63-b569-1ace22ecb992').
narrative_ontology:cs_kernel_codification('1f81b445-aa3a-4c63-b569-1ace22ecb992', distributed).
narrative_ontology:cs_authority_grounding('1f81b445-aa3a-4c63-b569-1ace22ecb992', distributed).
narrative_ontology:cs_reading_relation('1f81b445-aa3a-4c63-b569-1ace22ecb992', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f81b445-aa3a-4c63-b569-1ace22ecb992', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f81b445-aa3a-4c63-b569-1ace22ecb992', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f81b445-aa3a-4c63-b569-1ace22ecb992', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('1f81b445-aa3a-4c63-b569-1ace22ecb992', foundational, diaspora_pluralism_is_optimal_survival_strategy).
narrative_ontology:cs_axiom_status(diaspora_pluralism_is_optimal_survival_strategy, holdable).
narrative_ontology:cs_axiom_grounding('1f81b445-aa3a-4c63-b569-1ace22ecb992', diaspora_pluralism_is_optimal_survival_strategy, empirically_contingent).
narrative_ontology:cs_axiom('1f81b445-aa3a-4c63-b569-1ace22ecb992', foundational, nationalist_militarized_state_endangers_jews).
narrative_ontology:cs_axiom_status(nationalist_militarized_state_endangers_jews, holdable).
narrative_ontology:cs_axiom_grounding('1f81b445-aa3a-4c63-b569-1ace22ecb992', nationalist_militarized_state_endangers_jews, empirically_contingent).
narrative_ontology:cs_reference_frame('1f81b445-aa3a-4c63-b569-1ace22ecb992', historical_diaspora_flourishing).
narrative_ontology:cs_drift_state('1f81b445-aa3a-4c63-b569-1ace22ecb992', post_1948_zionist_hegemony, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1f81b445-aa3a-4c63-b569-1ace22ecb992', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_actions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities historically thrived through pluralism and minority rights in host nations. They benefit from maintaining distinct identities and avoiding entanglement with a militarized state, but face pressure to align with Zionist narratives.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Individuals who feel compelled to adopt a Zionist identity or support the Israeli state, even if it conflicts with their diasporist values, due to social pressure, institutional funding, or fear of ostracization. Their identity is often fused with the collective, making exit difficult.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism, payer,
    powerless, biographical, identity_locked, global).

% Jews in various diasporas who face increased antisemitism or physical danger due to their perceived association with the actions of the Israeli state, despite not supporting Zionism themselves. They are trapped by external perceptions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_actions, payer,
    powerless, immediate, trapped, global).

% Organizations and states that actively promote Zionism as the sole legitimate expression of Jewish self-determination. They administer funding, educational programs, and political advocacy that marginalize diasporist alternatives, effectively setting the agenda for 'Jewish interests'.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Governments and societies where diaspora Jewish communities reside. Their policies on minority rights and anti-discrimination are crucial for diasporist flourishing, but they often face diplomatic pressure to align with Israeli foreign policy or conflate Jewish identity with Zionism.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_nations, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective identity and political action around a framework that prioritizes diaspora flourishing through pluralism and minority rights, rather than territorial nationalism.
% TRANSFER_FUNCTION: Transfers political and cultural capital from a singular, militarized state project (Zionism) back to diverse, locally integrated diaspora communities, emphasizing their distinct identities and political agency.
% ABSENT_VOICES: The voices of historical diasporist movements and contemporary anti-Zionist Jewish groups are often marginalized or actively suppressed within mainstream Jewish communal organizations, which are frequently dominated by Zionist narratives and funding. They would advocate for a complete decoupling of Jewish identity from Israeli state policy.
% DISAPPEARANCE_RATIONALE: If the diasporist reading of Jewish self-determination vanished, the dominant Zionist narrative would face even less internal challenge, potentially leading to further consolidation of Jewish identity around the Israeli state. Diaspora communities would lose a critical framework for understanding their own history and future outside of a nationalist paradigm, and the political landscape of Jewish identity would become less pluralistic.
% FOUNDING_PROBLEM: The historical problem of Jewish vulnerability to antisemitism and persecution, and the question of how to secure Jewish collective survival and flourishing in a diverse world.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish thought and various anti-Zionist Jewish intellectual traditions corroborate that the problem of Jewish survival and flourishing is ongoing, and that diasporism offers a distinct, historically grounded approach to it, often in tension with nationalist solutions. This perspective is attested by scholars and activists outside of Zionist institutions.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the cost borne by Jews who are either coerced into a Zionist framework or endangered by its actions, as their preferred mode of self-determination is suppressed. Suppression (0.65) is high due to the active marginalization of diasporist voices within mainstream Jewish institutions and the pressure to conform to Zionist narratives. The theater ratio (0.4) indicates that while some diasporist cultural and intellectual activity persists, it often operates in the shadow of, or in performative opposition to, the dominant Zionist discourse, rather than as a fully autonomous and flourishing alternative. The rising extractiveness and suppression over time reflect the increasing consolidation of Zionist hegemony and the associated costs for diasporist-aligned Jews, particularly after key historical events like the 1967 war.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions, the diasporist reading is a dangerous anachronism or even an act of disloyalty, while from the perspective of diasporist advocates, Zionism is the deviation. The engine's classification as a Piton reflects the current structural reality where the diasporist alternative is largely inert but still imposes costs, rather than a vibrant, actively coordinated Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities are the beneficiaries, as this reading champions their historical mode of flourishing. However, their 'benefit' is often theoretical or aspirational, as the actual institutional landscape is dominated by Zionist organizations. Jews coerced into Zionism and those endangered by Israeli actions are the primary payers, bearing the direct costs of this constraint's suppression. Zionist institutions act as the agenda-setter, actively marginalizing diasporist alternatives. Host nations are observers, whose policies can either support or undermine the conditions for diasporist flourishing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (securing Jewish survival and flourishing) is still live, but the diasporist approach to fulfilling it has atrophied in influence relative to the Zionist one. The Piton classification prevents mislabeling it as a Snare, as there isn't a concentrated beneficiary actively maintaining the suppression of diasporism for direct profit, but rather a diffuse institutional inertia and ideological hegemony that marginalizes alternatives. It also prevents mislabeling it as a Rope, as the coordination function for diasporist flourishing is severely degraded and not widely adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diasporist_institutional_capacity,
    'What is the actual institutional capacity of diasporist-aligned Jewish organizations to offer a robust alternative to Zionist frameworks for Jewish collective life?',
    'Empirical study of funding, membership, and political influence of non-Zionist Jewish organizations compared to Zionist ones.',
    'If capacity is found to be higher than currently perceived, the constraint might shift towards a more active ''Tangled Rope'' or ''Snare'' (if extraction is concentrated), as the suppression of a viable alternative would be more deliberate. If capacity is negligible, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diasporist_institutional_capacity, empirical, 'Assesses the real-world strength of the diasporist alternative.').

omega_variable(
    identity_coercion_mechanism,
    'To what extent is the ''coercion into Zionism'' structural (e.g., funding conditionalities, social exclusion) versus internalized (e.g., self-censorship, ideological conviction)?',
    'Qualitative sociological studies of Jewish communal life, examining narratives of individuals who have shifted their stance on Zionism, and analysis of institutional funding mechanisms.',
    'If coercion is primarily structural, the suppression metric is accurate. If largely internalized, the effective suppression is higher, as individuals carry the constraint within their self-concept, making exit even harder and potentially shifting the classification towards a ''Snare'' for those identity-locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coercion_mechanism, empirical, 'Distinguishes between external and internal mechanisms of ideological conformity.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the ''Jewish self-determination'' kernel, or a distinct, unrelated claim?',
    'Analysis of historical and contemporary Jewish political thought, identifying whether diasporist arguments directly engage with and offer an alternative to the core questions of Jewish collective agency and future, or if they operate on a different conceptual plane.',
    'If it is not a genuine reading, then the ''cs_structure'' block is misapplied, and the constraint should be re-evaluated as an independent claim. If it is, the current structural analysis holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the validity of framing this as a kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__diasporist_reading, theater_ratio, 1987, 0.35).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__diasporist_reading, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.48).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__diasporist_reading, base_extractiveness, 1987, 0.52).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__diasporist_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__diasporist_reading, suppression_requirement, 1987, 0.62).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__diasporist_reading, suppression_requirement, 2014, 0.64).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__diasporist_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the 'Jewish self-determination' kernel. Each reading offers a distinct structural claim about how Jewish collective survival and flourishing are best secured, leading to different classifications and stakeholder dynamics. This diasporist reading emphasizes pluralism and minority rights over territorial sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
