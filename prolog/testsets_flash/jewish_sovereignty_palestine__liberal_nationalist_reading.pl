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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the liberal nationalist reading of Jewish
 *   self-determination in Palestine, which asserts the right of the Jewish
 *   people to statehood in their ancestral homeland while simultaneously
 *   recognizing the co-equal right of the Palestinian people to
 *   self-determination, typically advocating for a two-state solution or a
 *   binational framework. The constraint is framed as a 'tangled rope'
 *   because it attempts to coordinate two competing national claims but
 *   inherently involves asymmetric extraction from the Palestinian side due
 *   to the historical power imbalance and territorial outcomes. The metrics
 *   reflect a moderate level of extraction and suppression, as this reading
 *   necessitates active enforcement to maintain a delicate balance and often
 *   faces resistance from both maximalist Jewish and Palestinian factions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.45).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.6).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, 'da660c32-8e2c-4a1b-8d85-bcf25f7c5110').
narrative_ontology:cs_kernel_codification('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', formalized).
narrative_ontology:cs_authority_grounding('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', lineage).
narrative_ontology:cs_interpretation_layer_present('da660c32-8e2c-4a1b-8d85-bcf25f7c5110').
narrative_ontology:cs_reading_relation('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', foundational, co_equal_national_self_determination).
narrative_ontology:cs_axiom_status(co_equal_national_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', co_equal_national_self_determination, deontological).
narrative_ontology:cs_axiom('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', secondary, territorial_partition_as_just_solution).
narrative_ontology:cs_axiom_status(territorial_partition_as_just_solution, holdable).
narrative_ontology:cs_axiom_grounding('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', territorial_partition_as_just_solution, instrumental).
narrative_ontology:cs_reference_frame('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', mutual_recognition_and_partition).
narrative_ontology:cs_drift_state('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', contemporary_political_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('da660c32-8e2c-4a1b-8d85-bcf25f7c5110', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the recognition of collective self-determination and the establishment of a state in the ancestral homeland. This reading emphasizes the right to national self-expression and security, often requiring territorial compromise with Palestinian claims.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation, beneficiary,
    institutional, generational, identity_locked, national).

% Bears the costs of territorial partition or shared sovereignty, as their own self-determination claims are constrained by the establishment of a Jewish state. This reading acknowledges their co-equal right to self-determination.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation, payer,
    organized, generational, trapped, national).

% Advocates for a two-state solution or a binational framework based on principles of national self-determination and human rights. They seek to mediate between competing claims and enforce international law.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_international_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Monitor the implementation of self-determination rights for both Jewish and Palestinian peoples, ensuring that any statehood arrangement respects individual and collective rights, and that no population is dispossessed or denied fundamental freedoms.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, human_rights_advocates, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for the Jewish people to exercise their right to collective self-determination and establish a national home, while simultaneously acknowledging and accommodating the self-determination rights of the Palestinian people, typically through a two-state solution or binational arrangement.
% TRANSFER_FUNCTION: Transfers territorial control and sovereign authority to the Jewish collective, while simultaneously requiring a transfer of land or shared governance to the Palestinian collective, often mediated by international frameworks.
% ABSENT_VOICES: Radical anti-Zionist groups who reject any form of Jewish national self-determination in Palestine, and maximalist Zionist groups who reject any Palestinian national rights in the same territory. Both are excluded from the liberal nationalist discourse of mutual recognition and compromise.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist framework for Jewish self-determination vanished, the conflict would likely revert to more zero-sum, maximalist claims from both sides, potentially leading to increased violence and a breakdown of any existing peace processes or shared governance structures. The international diplomatic efforts built around this framework would collapse.
% FOUNDING_PROBLEM: The historical statelessness and persecution of the Jewish people, coupled with their deep historical and cultural ties to the land of Palestine, alongside the indigenous presence and developing national consciousness of the Palestinian people in the same territory.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars corroborate the historical context of Jewish persecution and connection to the land, as well as the emergence of Palestinian national identity. UN resolutions and numerous international diplomatic efforts attest to the ongoing nature of the problem and the need for a resolution that respects both national claims.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because this reading, by its nature, seeks a compromise that involves territorial division and shared sovereignty, which is less extractive than a zero-sum claim but still imposes significant costs on the Palestinian collective. Suppression (0.6) is necessary to enforce any agreed-upon partition or binational framework against resistance from those who reject compromise. The theater ratio (0.2) is relatively low, as the coordination function (managing two national claims) is genuine, though often strained. The temporal measurements show fluctuations in extractiveness and suppression, reflecting periods of intensified conflict and diplomatic efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish collective (beneficiary), this framework is a legitimate exercise of national rights and a necessary path to security. From the perspective of the Palestinian collective (payer), it represents a compromise that still entails significant loss and ongoing suppression of their full national aspirations. The engine's per-seat classification would highlight this divergence, showing a more 'rope-like' experience for beneficiaries and a more 'snare-like' experience for payers.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish collective, as a nation, is the primary beneficiary, gaining statehood and self-determination. The Palestinian collective, as a nation, is the primary payer, bearing the costs of territorial compromise and constrained sovereignty. Liberal international institutions act as agenda-setters, attempting to mediate and enforce a balanced solution. Human rights advocates observe and critique the process, ensuring adherence to international norms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_compromise_feasibility,
    'Is a genuinely equitable territorial compromise, as envisioned by this liberal nationalist reading, still feasible given existing settlement expansion and demographic realities?',
    'Empirical assessment of land use, demographic trends, and political will for land swaps or shared sovereignty arrangements.',
    'If not feasible, the ''tangled rope'' classification would drift towards ''snare'' as the coordination function (equitable partition) becomes performative, and the extraction from Palestinians becomes more absolute. If feasible, the ''tangled rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_compromise_feasibility, empirical, 'Feasibility of equitable territorial compromise.').

omega_variable(
    co_equal_self_determination_sincerity,
    'To what extent is the ''co-equal self-determination'' axiom genuinely held and acted upon by the primary beneficiaries and agenda-setters, versus being a rhetorical cover for continued asymmetric power dynamics?',
    'Analysis of policy decisions, resource allocation, and diplomatic actions over time, particularly regarding Palestinian state-building and sovereignty.',
    'If found to be largely rhetorical, the constraint''s extractiveness and suppression would be re-evaluated upwards, pushing it closer to a ''snare'' as the coordination narrative loses its grounding. If genuine, the ''tangled rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_equal_self_determination_sincerity, conceptual, 'Sincerity of the co-equal self-determination principle.').

omega_variable(
    liberal_nationalist_vs_settler_colonial_framing,
    'Is this liberal nationalist reading fundamentally distinct from, or merely a more palatable articulation of, a settler-colonial dynamic?',
    'Comparative historical analysis of other settler-colonial contexts, focusing on land acquisition, indigenous displacement, and the role of ''liberal'' justifications. Examination of the ''settler_colonial_reading'' sibling constraint.',
    'If the distinction is found to be superficial, the classification would shift dramatically towards ''snare'' from the perspective of the Palestinian collective, and the ''tangled rope'' framing would be seen as a form of legitimizing cover. If the distinction holds, the current classification is maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liberal_nationalist_vs_settler_colonial_framing, conceptual, 'Distinction between liberal nationalism and settler colonialism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(jewi_tr_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(jewi_be_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(jewi_su_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_sovereignty_palestine' kernel. Its extractiveness and suppression metrics reflect the specific structural outcomes of the liberal nationalist approach, which seeks to balance competing national claims through territorial compromise and mutual recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
