% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Jewish Sovereignty in Palestine (Post-Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the 'post-Zionist reading' of Jewish
 *   sovereignty in Palestine, which argues that while the Zionist project
 *   achieved statehood, its founding narrative and ethnic-national framework
 *   now actively obstruct civic equality for all citizens and hinder regional
 *   integration. This reading identifies Israeli Palestinians and occupied
 *   populations as victims of an ongoing ethnic privilege structure, with
 *   Jewish citizens benefiting from legal and land access asymmetries. The
 *   constraint is classified as a Tangled Rope because it provides a
 *   coordination function for Jewish national self-determination and
 *   security, but simultaneously enforces asymmetric extraction and
 *   suppression against non-Jewish populations.
 *
 * KEY AGENTS:
 *   - State of Israel: Agenda setter, enforces the ethnic-national framework.
 *   - Jewish Citizens of Israel: Primary beneficiaries, experience national self-determination and privilege.
 *   - Israeli Palestinians: Payers, bear costs of institutional discrimination and unequal citizenship.
 *   - Occupied Palestinian Populations: Payers, experience severe extraction and denial of self-determination.
 *   - Regional Integration Advocates: Excluded, propose alternative civic-national structures.
 *   - International Human Rights Organizations: Observers, critique the framework from an external perspective.
 *   - Postcolonial Scholars: Observers, provide analytical critique of the constraint's historical and ongoing effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.75).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.8).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Jewish Sovereignty in Palestine (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, 'db953d54-4cce-45c8-855b-8e7875bdc204').
narrative_ontology:cs_kernel_codification('db953d54-4cce-45c8-855b-8e7875bdc204', formalized).
narrative_ontology:cs_authority_grounding('db953d54-4cce-45c8-855b-8e7875bdc204', lineage).
narrative_ontology:cs_interpretation_layer_present('db953d54-4cce-45c8-855b-8e7875bdc204').
narrative_ontology:cs_reading_relation('db953d54-4cce-45c8-855b-8e7875bdc204', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('db953d54-4cce-45c8-855b-8e7875bdc204', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('db953d54-4cce-45c8-855b-8e7875bdc204', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('db953d54-4cce-45c8-855b-8e7875bdc204', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('db953d54-4cce-45c8-855b-8e7875bdc204', foundational, ethnic_privilege_undermines_civic_democracy).
narrative_ontology:cs_axiom_status(ethnic_privilege_undermines_civic_democracy, holdable).
narrative_ontology:cs_axiom_grounding('db953d54-4cce-45c8-855b-8e7875bdc204', ethnic_privilege_undermines_civic_democracy, deontological).
narrative_ontology:cs_axiom('db953d54-4cce-45c8-855b-8e7875bdc204', secondary, ethnic_national_framework_obstructs_regional_integration).
narrative_ontology:cs_axiom_status(ethnic_national_framework_obstructs_regional_integration, holdable).
narrative_ontology:cs_axiom_grounding('db953d54-4cce-45c8-855b-8e7875bdc204', ethnic_national_framework_obstructs_regional_integration, empirically_contingent).
narrative_ontology:cs_reference_frame('db953d54-4cce-45c8-855b-8e7875bdc204', civic_equality_ideal).
narrative_ontology:cs_drift_state('db953d54-4cce-45c8-855b-8e7875bdc204', contemporary_nation_state_law_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('db953d54-4cce-45c8-855b-8e7875bdc204', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the ethnic-national framework, including laws like the Law of Return and the Nation-State Law, which prioritize Jewish identity and rights. It justifies these actions as necessary for national security and self-determination.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the ethnic-national framework through preferential immigration rights (Law of Return), land allocation policies, and the state's self-definition as the nation-state of the Jewish people. They experience the state as fulfilling a historical right to self-determination.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel, beneficiary,
    organized, generational, mobile, national).

% Bear the costs of the ethnic-national framework through institutionalized discrimination in land, housing, and public services, and by being treated as a demographic threat rather than equal citizens. Their identity is locked into a state that defines itself ethnically against them.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinians, payer,
    powerless, generational, identity_locked, national).

% Experience the most severe extraction, including military occupation, displacement, and denial of self-determination, directly linked to the expansionist implications of the ethnic-national framework. They have virtually no exit options from the imposed structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_populations, payer,
    powerless, generational, trapped, regional).

% Propose alternative regional structures that prioritize civic equality and integration over ethnic nationalism. Their voices are largely excluded from the dominant political discourse within Israel, which frames such alternatives as existential threats.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, regional_integration_advocates, excluded,
    moderate, generational, constrained, regional).

% Monitor and report on human rights violations and discriminatory practices stemming from the ethnic-national framework. They provide external critique and advocate for changes in policy and law, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_organizations, observer,
    institutional, biographical, analytical, global).

% Analyze the Zionist project and the state of Israel through the lens of postcolonial theory, highlighting the continuities of settler-colonial patterns and the impact of ethnic nationalism on indigenous populations. They contribute to the intellectual critique of the constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, postcolonial_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for Jewish national self-determination, cultural continuity, and security in the ancestral homeland, coordinating the collective identity and aspirations of Jewish people globally and locally.
% TRANSFER_FUNCTION: Transfers land, resources, and civic rights/privileges from non-Jewish populations (particularly Palestinians) to Jewish citizens, maintaining an ethnic-national majority and character within the state's institutions and legal framework.
% ABSENT_VOICES: Advocates for a secular, democratic state for all its citizens, a bi-national state, or a regional confederation are structurally marginalized. Their perspectives, which challenge the ethnic-national foundation, are often dismissed as anti-Zionist or existential threats.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight, the state's legal and social structures would undergo fundamental transformation. Laws like the Law of Return would be abolished, land allocation policies would be re-evaluated, and the basis of citizenship would shift from ethnic-national to civic, leading to a profound reordering of power, rights, and regional relations.
% FOUNDING_PROBLEM: To establish a secure homeland and self-determination for the Jewish people after centuries of persecution, antisemitism, and statelessness, culminating in the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: While the historical need for a Jewish homeland is widely acknowledged, the ongoing necessity of an ethnic-national framework that obstructs civic equality and regional integration is contested. Post-Zionist scholars, international legal bodies, human rights organizations, and a segment of Israeli and Palestinian civil society argue that the founding problem of Jewish statelessness is largely solved, but the framework now creates new problems of inequality and conflict. The state and its supporters maintain that the founding problem of security and self-determination remains live.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) due to the systemic nature of ethnic privilege and resource allocation. Suppression is also high (0.80) as the state actively enforces laws and policies that maintain its ethnic character and marginalize non-Jewish populations, often through military and legal means. Theater ratio is low (0.20) because the obstruction to civic equality and regional integration is a direct, functional outcome of the state's structure, not merely performative. The increasing extractiveness and suppression over the interval (1967-2017) reflect the hardening of the occupation and the formalization of ethnic priority through legislation like the Nation-State Law (2018, just outside the interval but reflecting trends).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish citizens and the State of Israel, the ethnic-national framework is a legitimate and necessary expression of self-determination and security, a 'Rope' or even 'Mountain' of national existence. From the perspective of Israeli Palestinians and occupied populations, the same framework operates as a 'Snare' or 'Tangled Rope' that extracts rights, land, and self-determination. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel, as the agenda-setter, and Jewish citizens, as beneficiaries, experience low directionality (d near 0.0) as they benefit from the constraint's operation. Israeli Palestinians and occupied Palestinian populations, as payers, experience high directionality (d near 1.0) due to the significant costs and lack of exit options. Regional integration advocates are excluded, their d is high as their alternative is suppressed. International human rights organizations and postcolonial scholars are analytical observers, with d near 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests a form of mandatrophy where the original mandate of establishing a secure Jewish homeland has been achieved, but the framework designed for it has persisted and evolved into an extractive structure that obstructs new mandates (civic equality, regional integration). The 'contested' status of the founding problem reflects this: for some, the mandate is fulfilled and the constraint is now obsolete in its current form; for others, it remains live and necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''jewish_sovereignty_palestine'' kernel, or is it fundamentally a different constraint altogether?',
    'Analysis of whether the core commitment (Jewish sovereignty in Palestine) is still the referent, even if critically reinterpreted, or if the critique constitutes an entirely new commitment.',
    'If a distinct reading, it contributes to the kernel''s contested nature. If a new constraint, it should be re-authored as independent, with network links to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as one reading within a contested kernel.').

omega_variable(
    security_vs_extraction_ambiguity,
    'To what extent is the ethnic-national framework a genuine security imperative, versus a mechanism for maintaining ethnic privilege and extraction?',
    'Empirical analysis of security threats and their direct linkage to the ethnic-national character of the state, versus alternative security arrangements that prioritize civic equality. Comparative studies with other states facing similar security challenges.',
    'If primarily security-driven, the extractiveness might be re-evaluated as a necessary cost of coordination. If primarily extractive, it strengthens the Snare/Tangled Rope classification and calls for de-Zionization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_extraction_ambiguity, empirical, 'Distinguishing security needs from extractive practices within the ethnic-national framework.').

omega_variable(
    zionism_conceptual_ambiguity,
    'Does ''Zionism'' inherently imply an ethnic-national state that obstructs civic equality, or can a ''civic Zionism'' reconcile Jewish self-determination with full equality for all citizens?',
    'Conceptual analysis of Zionist thought and practice, and the viability of alternative Zionist interpretations that prioritize civic over ethnic nationalism. Examination of historical and contemporary movements advocating for such a reconciliation.',
    'If ''civic Zionism'' is viable, the constraint might be re-framed as a ''Tangled Rope'' with a clearer path to ''Rope'' through internal reform. If not, the ''Snare'' aspect is reinforced, suggesting fundamental structural change is required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zionism_conceptual_ambiguity, conceptual, 'Ambiguity in the definition and implications of Zionism itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1967, 2017).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(jewi_tr_t1977, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1977, 0.23).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1987, 0.22).
narrative_ontology:measurement(jewi_tr_t1997, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1997, 0.21).
narrative_ontology:measurement(jewi_tr_t2007, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2007, 0.2).
narrative_ontology:measurement(jewi_tr_t2017, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2017, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(jewi_be_t1977, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1977, 0.65).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1987, 0.68).
narrative_ontology:measurement(jewi_be_t1997, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1997, 0.7).
narrative_ontology:measurement(jewi_be_t2007, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2007, 0.73).
narrative_ontology:measurement(jewi_be_t2017, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2017, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(jewi_su_t1977, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1977, 0.7).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1987, 0.75).
narrative_ontology:measurement(jewi_su_t1997, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1997, 0.78).
narrative_ontology:measurement(jewi_su_t2007, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2007, 0.79).
narrative_ontology:measurement(jewi_su_t2017, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2017, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, palestinian_right_of_return).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'jewish_sovereignty_palestine' kernel, each representing a distinct structural claim about the nature and effects of Jewish sovereignty in the region. This 'post_zionist_reading' focuses on the obstructive aspects of the ethnic-national framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
