% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination & Statehood (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the liberal nationalist reading of
 *   Jewish sovereignty in Palestine. It posits that the Jewish people possess
 *   a collective right to self-determination, legitimately exercised through
 *   statehood in their ancestral homeland. Crucially, this reading
 *   acknowledges Palestinians as co-equal self-determination claimants,
 *   necessitating a partition or binational framework. The constraint is
 *   classified as a Tangled Rope due to its genuine coordination function
 *   (Jewish statehood) combined with asymmetric extraction (Palestinians bear
 *   costs of limited sovereignty and territorial compromise) and the
 *   requirement for active enforcement to maintain borders and security. The
 *   'moderate' extractiveness reflects the theoretical commitment to
 *   territorial compromise, even if practical implementation has been more
 *   extractive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.65).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.6).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination & Statehood (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, 'a4a0a48d-85b6-425b-928e-77fa3038a6ce').
narrative_ontology:cs_kernel_codification('a4a0a48d-85b6-425b-928e-77fa3038a6ce', formalized).
narrative_ontology:cs_authority_grounding('a4a0a48d-85b6-425b-928e-77fa3038a6ce', lineage).
narrative_ontology:cs_interpretation_layer_present('a4a0a48d-85b6-425b-928e-77fa3038a6ce').
narrative_ontology:cs_reading_relation('a4a0a48d-85b6-425b-928e-77fa3038a6ce', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4a0a48d-85b6-425b-928e-77fa3038a6ce', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4a0a48d-85b6-425b-928e-77fa3038a6ce', jewish_sovereignty_palestine__religious_zionist_reading, influences).
narrative_ontology:cs_reading_relation('a4a0a48d-85b6-425b-928e-77fa3038a6ce', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('a4a0a48d-85b6-425b-928e-77fa3038a6ce', foundational, jewish_people_are_a_nation).
narrative_ontology:cs_axiom_status(jewish_people_are_a_nation, holdable).
narrative_ontology:cs_axiom_grounding('a4a0a48d-85b6-425b-928e-77fa3038a6ce', jewish_people_are_a_nation, deontological).
narrative_ontology:cs_axiom('a4a0a48d-85b6-425b-928e-77fa3038a6ce', foundational, self_determination_right_is_universal).
narrative_ontology:cs_axiom_status(self_determination_right_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('a4a0a48d-85b6-425b-928e-77fa3038a6ce', self_determination_right_is_universal, deontological).
narrative_ontology:cs_reference_frame('a4a0a48d-85b6-425b-928e-77fa3038a6ce', partition_principle).
narrative_ontology:cs_drift_state('a4a0a48d-85b6-425b-928e-77fa3038a6ce', contemporary_settlement_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a4a0a48d-85b6-425b-928e-77fa3038a6ce', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_people_as_nation).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_zionist_advocates).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises collective self-determination through statehood in the ancestral homeland, seeking security and cultural flourishing. Bears the costs of ongoing conflict but benefits from national sovereignty.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_people_as_nation, agenda_setter,
    institutional, generational, constrained, national).

% Experiences the constraint as a limitation on their own self-determination and territorial claims, bearing the costs of occupation, displacement, and limited sovereignty. Seeks co-equal statehood or a binational framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation, payer,
    organized, generational, trapped, national).

% Administers the state, enforces its borders, and manages security, acting as the primary institutional agent for Jewish self-determination. Navigates international pressure for a two-state solution while managing internal political divisions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_government, agenda_setter,
    institutional, biographical, constrained, national).

% Exercises limited self-governance in parts of the West Bank, operating under the overarching constraint of Israeli sovereignty. Seeks full statehood and recognition, but is dependent on international aid and Israeli cooperation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority, payer,
    moderate, biographical, constrained, regional).

% Observes the conflict, provides humanitarian aid, and attempts to mediate a resolution, often advocating for a two-state solution based on international law. Can impose diplomatic or economic pressure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_community, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, international_community, agenda_setter).

% Supports the existence of Israel as a Jewish and democratic state, advocating for a two-state solution that respects Palestinian rights. Their ideological framework is vindicated by the constraint's stated aims, even if its implementation is contested.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_zionist_advocates, beneficiary,
    organized, biographical, mobile, global).

% Advocates for maximalist territorial claims based on religious and historical grounds, rejecting the concept of Palestinian co-equal self-determination or territorial compromise. Their views are outside the liberal nationalist framework, though they exert significant political influence.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_movement, excluded,
    powerful, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the Jewish people to exercise collective self-determination and establish a secure national homeland, aiming to resolve historical statelessness and persecution.
% TRANSFER_FUNCTION: Transfers territorial control and sovereign authority to the Jewish nation, while intending to allocate a portion of the land and sovereignty to the Palestinian nation through a negotiated settlement.
% ABSENT_VOICES: Maximalist religious Zionists (who reject territorial compromise on theological grounds) and proponents of a single, secular democratic state (who reject ethnic-national partition) are structurally excluded from the core liberal nationalist discourse, though their actions significantly impact the constraint's operation.
% DISAPPEARANCE_RATIONALE: If the right to Jewish self-determination and statehood in the ancestral homeland vanished overnight, the entire political and legal framework of the Israeli-Palestinian conflict would be fundamentally altered, leading to a complete reorganization of regional power dynamics, national identities, and international relations.
% FOUNDING_PROBLEM: The historical persecution and statelessness of the Jewish people, culminating in the Holocaust, necessitated a secure national homeland where they could exercise self-determination and ensure their survival.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by international resolutions (e.g., UN Partition Plan, post-Holocaust consensus), historical scholarship on antisemitism, and the ongoing need for a secure homeland in a volatile region. While the specific *solution* is contested, the underlying problem of Jewish statelessness and vulnerability is widely acknowledged outside of benefiting parties.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because while the constraint aims for a just resolution, the reality of its implementation has involved significant costs for Palestinians, including displacement and limited sovereignty. Suppression is moderate (0.60) as active enforcement is required to maintain the state's security and borders, often at the expense of Palestinian movement and rights. Theater ratio is low (0.15) because the core function of statehood and self-determination is real and actively pursued, not merely performative. Resistance is high (0.70) due to the ongoing, deeply entrenched conflict with the Palestinian people.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people and liberal Zionist advocates, the constraint is a necessary and just exercise of self-determination, a coordination mechanism to secure a homeland. From the perspective of the Palestinian people, it is an extractive structure that limits their own self-determination and imposes significant costs. The engine's computation of per-seat classifications will reflect this divergence, with beneficiaries experiencing a lower effective extraction and victims a higher one.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people, as a nation, and liberal Zionist advocates are beneficiaries, as the constraint directly enables their self-determination and vindicates their ideological framework. The Palestinian people, as a nation, and the Palestinian Authority are payers/victims, as they bear the costs of territorial division, limited sovereignty, and the ongoing conflict. The Israeli government acts as the agenda-setter, enforcing the state's existence and policies. The international community acts as an observer and, at times, an agenda-setter through mediation and policy recommendations. The settler movement is excluded from this reading's framework, as their maximalist claims contradict its liberal tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a valid instantiation of the ''liberal_nationalist_reading'' of the ''jewish_sovereignty_palestine'' kernel, or does its practical implementation drift towards a more extractive sibling reading?',
    'Analysis of policy outcomes, territorial control, and human rights records against the stated principles of liberal nationalism (e.g., equality, self-determination for all).',
    'If practical implementation consistently deviates from liberal nationalist principles, the constraint might be reclassified as a different, more extractive reading (e.g., closer to a ''settler_colonial_reading'' in practice, despite stated intent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Verifies the fidelity of this reading to its stated principles in practice.').

omega_variable(
    two_state_solution_viability,
    'Is the ''partition_principle'' (two-state solution) still a viable framework for resolving the conflict, given ongoing settlement expansion and political fragmentation?',
    'Empirical assessment of territorial contiguity, demographic trends, and political will for a two-state solution, as evaluated by independent international bodies.',
    'If the two-state solution is deemed no longer viable, the ''liberal_nationalist_reading'' would face a fundamental conceptual challenge, potentially requiring a re-evaluation of its core tenets or a shift towards a binational framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_solution_viability, empirical, 'Assesses the practical feasibility of the core resolution mechanism of this reading.').

omega_variable(
    co_equal_self_determination_interpretation,
    'How is ''co-equal self-determination'' interpreted and applied in practice, particularly regarding territorial allocation, resource access, and freedom of movement for both peoples?',
    'Detailed legal and political analysis of specific policies and their impact on both Jewish and Palestinian populations, comparing outcomes against international human rights standards.',
    'If ''co-equal'' is found to be consistently applied asymmetrically, the ''liberal_nationalist_reading''s claim to fairness would be undermined, increasing its effective extraction and potentially shifting its classification towards a more overtly extractive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_equal_self_determination_interpretation, conceptual, 'Examines the practical meaning of ''co-equal self-determination'' within the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.13).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(jewi_tr_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(jewi_be_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.59).
narrative_ontology:measurement(jewi_su_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
