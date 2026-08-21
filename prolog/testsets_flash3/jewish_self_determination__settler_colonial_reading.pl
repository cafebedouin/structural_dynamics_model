% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as Settler-Colonial Dispossession
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint models Zionism as a settler-colonial project, focusing on
 *   the systematic dispossession of indigenous Palestinians through violence
 *   and legal exclusion. It is one reading of the 'jewish_self_determination'
 *   kernel. The constraint's structure is designed to extract from and
 *   eliminate the indigenous population, benefiting European Jewish settlers
 *   and the Israeli state, while victimizing Palestinian Arabs through
 *   displacement, occupation, and legal asymmetry (e.g., the Law of Return).
 *   The high extractiveness and suppression reflect the ongoing nature of
 *   this process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.92).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as Settler-Colonial Dispossession").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '84e9469e-275e-472d-95fe-acbeaded7498').
narrative_ontology:cs_kernel_codification('84e9469e-275e-472d-95fe-acbeaded7498', formalized).
narrative_ontology:cs_authority_grounding('84e9469e-275e-472d-95fe-acbeaded7498', extraction).
narrative_ontology:cs_interpretation_layer_present('84e9469e-275e-472d-95fe-acbeaded7498').
narrative_ontology:cs_reading_relation('84e9469e-275e-472d-95fe-acbeaded7498', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('84e9469e-275e-472d-95fe-acbeaded7498', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('84e9469e-275e-472d-95fe-acbeaded7498', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('84e9469e-275e-472d-95fe-acbeaded7498', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('84e9469e-275e-472d-95fe-acbeaded7498', foundational, zionism_is_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('84e9469e-275e-472d-95fe-acbeaded7498', zionism_is_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('84e9469e-275e-472d-95fe-acbeaded7498', foundational, palestinian_dispossession_is_systematic).
narrative_ontology:cs_axiom_status(palestinian_dispossession_is_systematic, holdable).
narrative_ontology:cs_axiom_grounding('84e9469e-275e-472d-95fe-acbeaded7498', palestinian_dispossession_is_systematic, empirically_contingent).
narrative_ontology:cs_reference_frame('84e9469e-275e-472d-95fe-acbeaded7498', european_colonial_expansion).
narrative_ontology:cs_drift_state('84e9469e-275e-472d-95fe-acbeaded7498', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('84e9469e-275e-472d-95fe-acbeaded7498', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, indigenous_palestinians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional actor that establishes and enforces laws, policies, and military actions to maintain control over land and resources, benefiting its Jewish citizens and dispossessing Palestinians. Its existence is predicated on the settler-colonial project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Individuals and communities who benefit directly from land acquisition, resource control, and preferential legal status within the settler-colonial framework. They are often supported by state policies and military protection.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    powerful, biographical, mobile, local).

% The indigenous population subjected to displacement, land confiscation, military occupation, and legal discrimination (e.g., via the Law of Return). They bear the direct costs of the settler-colonial project, including loss of life, property, and self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Monitor and document human rights abuses, legal discrimination, and violations of international law stemming from the settler-colonial project. They advocate for Palestinian rights and challenge the legitimacy of the constraint through reports and legal actions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Analyze Zionism through the lens of settler-colonial theory, identifying patterns of dispossession, racialization, and state formation that align with other historical settler-colonial contexts. Their work provides the theoretical framework for this reading.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, postcolonial_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the acquisition and control of land, resources, and political power for European Jewish settlers, establishing a new society at the expense of the indigenous population.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from indigenous Palestinians to the Israeli state and European Jewish settlers, facilitated by legal and military structures.
% ABSENT_VOICES: The voices of dispossessed Palestinians, particularly those in diaspora or under occupation, are systematically marginalized or silenced within dominant narratives that legitimize the Israeli state. Their perspectives would highlight the ongoing violence and injustice of the settler-colonial project.
% DISAPPEARANCE_RATIONALE: If the settler-colonial structures and legal frameworks underpinning Zionism vanished overnight, the political and demographic landscape would fundamentally rearrange. Palestinians would assert their right to return and self-determination, and the current Israeli state's territorial claims and legal basis would collapse, leading to a radical reordering of power and land ownership.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and persecution in Europe, particularly in the wake of rising antisemitism and the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: While the historical problem of Jewish persecution is widely acknowledged, this reading argues that Zionism's solution to this problem was achieved through the creation of a new problem for indigenous Palestinians. Palestinian historians, postcolonial theorists, and international legal scholars corroborate that the founding problem for Jewish people was addressed by creating a settler-colonial structure for Palestinians.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.92) due to continuous land confiscation, resource control, and the imposition of a legal system that privileges one group over another. Suppression is also very high (0.88) because the project relies on military occupation, checkpoints, blockades, and legal mechanisms to prevent Palestinian self-determination and return. Theater ratio is low (0.15) as the primary function is direct control and dispossession, with minimal performative cover for its core operations. Accessibility collapse is high (0.75) as alternatives for Palestinians (e.g., independent statehood, return) are systematically dismantled. Resistance is high (0.85) reflecting ongoing Palestinian struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and settlers, the constraint is framed as legitimate self-determination and defense. From the Palestinian perspective, and that of postcolonial analysis, it is a structure of ongoing extraction and oppression. The engine's classification will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and European Jewish settlers are the primary beneficiaries, gaining land, resources, and political power (low d). Palestinian Arabs are the primary victims, experiencing dispossession, violence, and legal exclusion (high d). International human rights organizations and postcolonial scholars act as observers, documenting and analyzing the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_vs_indigenous_return,
    'Is the Jewish presence in Palestine a settler-colonial project or an indigenous return movement?',
    'Historical and anthropological analysis of pre-Zionist Jewish communities in Palestine, patterns of immigration, and the relationship between Zionist settlers and existing indigenous populations, compared to other settler-colonial contexts.',
    'If primarily settler-colonial, the constraint is a snare of dispossession. If primarily indigenous return, the constraint''s extractiveness and suppression metrics would be re-evaluated, potentially shifting towards a tangled rope or even a rope, depending on the recognition of Palestinian rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_colonial_vs_indigenous_return, conceptual, 'Ambiguity regarding the historical and political nature of Zionism.').

omega_variable(
    legal_exclusion_vs_security_necessity,
    'Are Israeli legal frameworks (e.g., Law of Return, land laws) primarily tools of ethnic exclusion and dispossession, or are they necessary for the security and self-determination of the Jewish state?',
    'Comparative legal analysis with other nation-states'' immigration and land laws, assessment of security threats independent of demographic control, and examination of the differential impact on Jewish vs. Palestinian populations.',
    'If primarily exclusionary, the suppression and extractiveness metrics are accurate for a snare. If primarily security-driven, the suppression might be re-read as a coordination cost, potentially shifting the classification towards a tangled rope, though extractiveness would likely remain high due to asymmetric impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_exclusion_vs_security_necessity, empirical, 'The underlying justification for discriminatory legal structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__settler_colonial_reading, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__settler_colonial_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__settler_colonial_reading, theater_ratio, 2014, 0.16).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1987, 0.88).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2000, 0.91).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2014, 0.93).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1987, 0.82).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2000, 0.86).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2014, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
