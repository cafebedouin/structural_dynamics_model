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
 *   human_readable: Jewish Self-Determination in Palestine (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the liberal nationalist reading of Jewish
 *   self-determination in Palestine, which asserts the right of the Jewish
 *   people to statehood in their ancestral homeland while simultaneously
 *   recognizing the co-equal self-determination rights of the Palestinian
 *   people. This reading typically advocates for a two-state solution or a
 *   binational framework, emphasizing territorial compromise and shared
 *   sovereignty. It is one reading of the broader 'Jewish Sovereignty in
 *   Palestine' kernel, distinct from more exclusivist or purely cultural
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.45).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.3).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Self-Determination in Palestine (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '1d093f94-c604-4d3c-a7b1-5698becc0d45').
narrative_ontology:cs_kernel_codification('1d093f94-c604-4d3c-a7b1-5698becc0d45', formalized).
narrative_ontology:cs_authority_grounding('1d093f94-c604-4d3c-a7b1-5698becc0d45', lineage).
narrative_ontology:cs_interpretation_layer_present('1d093f94-c604-4d3c-a7b1-5698becc0d45').
narrative_ontology:cs_reading_relation('1d093f94-c604-4d3c-a7b1-5698becc0d45', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d093f94-c604-4d3c-a7b1-5698becc0d45', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d093f94-c604-4d3c-a7b1-5698becc0d45', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d093f94-c604-4d3c-a7b1-5698becc0d45', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('1d093f94-c604-4d3c-a7b1-5698becc0d45', foundational, jewish_people_possess_collective_self_determination_right).
narrative_ontology:cs_axiom_status(jewish_people_possess_collective_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('1d093f94-c604-4d3c-a7b1-5698becc0d45', jewish_people_possess_collective_self_determination_right, deontological).
narrative_ontology:cs_axiom('1d093f94-c604-4d3c-a7b1-5698becc0d45', foundational, palestinian_people_possess_collective_self_determination_right).
narrative_ontology:cs_axiom_status(palestinian_people_possess_collective_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('1d093f94-c604-4d3c-a7b1-5698becc0d45', palestinian_people_possess_collective_self_determination_right, deontological).
narrative_ontology:cs_reference_frame('1d093f94-c604-4d3c-a7b1-5698becc0d45', two_state_solution_framework).
narrative_ontology:cs_drift_state('1d093f94-c604-4d3c-a7b1-5698becc0d45', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d093f94-c604-4d3c-a7b1-5698becc0d45', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_people_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the recognition of collective self-determination and the establishment of a state in the ancestral homeland, providing security and cultural flourishing. However, constrained by the need for territorial compromise and recognition of Palestinian rights.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_people_as_nation, beneficiary,
    institutional, generational, constrained, national).

% Bears the cost of territorial partition or a binational framework, which may not fully align with their own claims to the entire land. Their self-determination is recognized, but its exercise is constrained by the co-equal claim of the Jewish people.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation, payer,
    organized, generational, constrained, national).

% Actively promote and defend the idea of Jewish self-determination within a framework that acknowledges Palestinian rights. They shape policy proposals for partition or shared sovereignty, navigating internal and external pressures.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_zionist_advocates, agenda_setter,
    powerful, biographical, mobile, global).

% Interpret and apply principles of self-determination, national rights, and human rights to the Israeli-Palestinian conflict. Their pronouncements influence the legitimacy and enforceability of various solutions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for the Jewish people to exercise their right to collective self-determination and statehood, while simultaneously acknowledging and accommodating the co-equal self-determination rights of the Palestinian people, aiming for a just and peaceful resolution.
% TRANSFER_FUNCTION: Transfers legitimacy and territorial rights to the Jewish people for statehood, while requiring a reciprocal transfer of rights and territory to the Palestinian people, often through partition or a binational state. It also transfers the burden of compromise and negotiation to both parties.
% ABSENT_VOICES: Extremist factions on both sides who reject any compromise or recognition of the other's national rights are excluded from the liberal nationalist discourse. They would argue for exclusive sovereignty over the entire land.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist reading of Jewish self-determination vanished, the framework for a two-state solution or binational state would collapse. The conflict would likely intensify, with both sides pursuing maximalist claims, leading to greater instability and violence. International efforts for a negotiated settlement would lose their primary conceptual grounding.
% FOUNDING_PROBLEM: The historical statelessness and persecution of the Jewish people, coupled with their deep historical and religious connection to the land of Israel, necessitated a framework for national self-determination, while also addressing the existing Palestinian population's rights.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars corroborate the historical context of Jewish statelessness and the emergence of nationalist movements. Palestinian national narratives and international human rights organizations corroborate the ongoing need to address Palestinian self-determination and rights within any proposed solution.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because this reading inherently calls for compromise and partition, meaning neither side achieves maximalist claims. Suppression is moderate (0.30) as it requires active political and diplomatic efforts to manage competing claims, but does not rely on outright coercion to maintain its core tenets. Theater ratio is low (0.10) as the core tenets of this reading are genuinely pursued by its proponents, though implementation often falls short. The temporal measurements reflect periods of increased tension (e.g., post-1967, Second Intifada) where extractiveness and suppression might rise, and periods of peace efforts (e.g., Oslo Accords) where they might dip.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people (as framed by this reading), the constraint is a legitimate exercise of national rights, a 'rope' for survival and flourishing. From the perspective of the Palestinian people, it represents a 'tangled rope' or 'snare' that imposes significant costs and limits their own self-determination, even if their rights are nominally acknowledged. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people as a nation are the primary beneficiaries, gaining statehood and security. The Palestinian people as a nation are payers, as they must compromise on their maximalist territorial claims. Liberal Zionist advocates act as agenda-setters, pushing for this specific framework. International law bodies serve as observers, evaluating the claims against universal principles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_equal_self_determination_feasibility,
    'Is the concept of co-equal self-determination for two national groups in the same contested territory genuinely implementable without one implicitly subordinating the other?',
    'Empirical observation of successful or failed binational/partition models in other deeply contested territories, and the long-term stability of any implemented solution in Palestine.',
    'If found infeasible, the liberal nationalist reading''s core premise would be challenged, potentially reclassifying it towards a more extractive type (e.g., tangled_rope or snare) from the perspective of the subordinated group. If feasible, it reinforces the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_equal_self_determination_feasibility, empirical, 'The practical viability of truly co-equal national self-determination in a zero-sum territorial dispute.').

omega_variable(
    territorial_compromise_threshold,
    'What is the minimum territorial compromise required from the Jewish national project to genuinely satisfy Palestinian self-determination rights, and vice-versa, within this liberal nationalist framework?',
    'Detailed mapping of proposed partition plans against demographic, historical, and economic needs of both populations, assessed by independent international bodies.',
    'If the required compromise is consistently rejected by one side, the ''rope'' classification for that side becomes tenuous, indicating a higher degree of extraction or suppression is being maintained. If a mutually acceptable threshold is found, it strengthens the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_compromise_threshold, preference, 'The specific, mutually acceptable territorial division that embodies co-equal self-determination.').

omega_variable(
    liberal_nationalist_vs_settler_colonial_framing,
    'Is the liberal nationalist reading fundamentally distinct from, or merely a more palatable articulation of, a settler-colonial dynamic?',
    'Analysis of historical and ongoing practices: if the ''liberal'' framework consistently leads to displacement, dispossession, or denial of rights, it would align more with the settler-colonial reading. If it genuinely enables equitable coexistence, the distinction holds.',
    'If the distinction collapses, the constraint would be reclassified as a snare from the Palestinian perspective, and the overall extractiveness would be significantly higher. If the distinction holds, the current classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liberal_nationalist_vs_settler_colonial_framing, conceptual, 'The conceptual boundary between liberal nationalism and settler colonialism in this context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_sovereignty_palestine' kernel. It is linked to other readings (settler_colonial_reading, religious_zionist_reading, cultural_zionist_reading, post_zionist_reading) through the cs_structure.reading_relations field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
