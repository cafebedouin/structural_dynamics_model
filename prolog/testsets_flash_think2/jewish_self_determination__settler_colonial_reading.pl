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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as a Settler-Colonial Project
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'settler_colonial_reading' of the
 *   'jewish_self_determination' kernel. It frames Zionism as a European
 *   settler-colonial project that systematically dispossessed indigenous
 *   Palestinians through violence, legal exclusion, and ongoing occupation.
 *   The constraint's persistence relies on active enforcement and suppression
 *   of Palestinian resistance, while narratives of security and
 *   self-determination serve as a partial cover for extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.92).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as a Settler-Colonial Project").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'bf9cdd35-8c4f-4aef-a31a-114d96140281').
narrative_ontology:cs_kernel_codification('bf9cdd35-8c4f-4aef-a31a-114d96140281', formalized).
narrative_ontology:cs_authority_grounding('bf9cdd35-8c4f-4aef-a31a-114d96140281', extraction).
narrative_ontology:cs_interpretation_layer_present('bf9cdd35-8c4f-4aef-a31a-114d96140281').
narrative_ontology:cs_reading_relation('bf9cdd35-8c4f-4aef-a31a-114d96140281', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf9cdd35-8c4f-4aef-a31a-114d96140281', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('bf9cdd35-8c4f-4aef-a31a-114d96140281', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf9cdd35-8c4f-4aef-a31a-114d96140281', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('bf9cdd35-8c4f-4aef-a31a-114d96140281', foundational, zionism_as_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_as_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('bf9cdd35-8c4f-4aef-a31a-114d96140281', zionism_as_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('bf9cdd35-8c4f-4aef-a31a-114d96140281', foundational, palestinian_dispossession_as_structural_violence).
narrative_ontology:cs_axiom_status(palestinian_dispossession_as_structural_violence, holdable).
narrative_ontology:cs_axiom_grounding('bf9cdd35-8c4f-4aef-a31a-114d96140281', palestinian_dispossession_as_structural_violence, empirically_contingent).
narrative_ontology:cs_reference_frame('bf9cdd35-8c4f-4aef-a31a-114d96140281', foundational_act_of_dispossession).
narrative_ontology:cs_drift_state('bf9cdd35-8c4f-4aef-a31a-114d96140281', contemporary_occupation_and_settlement_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bf9cdd35-8c4f-4aef-a31a-114d96140281', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional actor that establishes and enforces laws, policies, and military actions to maintain and expand the settler-colonial project, benefiting from the dispossession of Palestinians and control over land and resources. Its existence is predicated on this structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals and communities who migrated to Palestine/Israel, often from Europe, and benefit directly from land acquisition, preferential legal status, and state protection, which are secured through the displacement and subjugation of indigenous Palestinians. Their identity and prosperity are tied to the project's success.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    powerful, generational, constrained, regional).

% The indigenous population dispossessed of their land, homes, and self-determination through systematic violence, legal exclusion (e.g., denial of right of return), and ongoing occupation. They bear the full cost of the settler-colonial project, facing displacement, discrimination, and violence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Monitor and document human rights abuses, land confiscations, and discriminatory practices perpetrated by the Israeli state against Palestinians. They provide critical analysis and advocacy but lack direct enforcement power over the constraint.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Academics who analyze Zionism through the lens of settler-colonial theory, providing the conceptual framework for this reading. They contribute to the intellectual discourse and challenge dominant narratives but are external to the direct operation of the constraint.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, postcolonial_scholars, observer,
    moderate, biographical, analytical, global).

% Jewish individuals and groups in the diaspora who reject the settler-colonial nature of Zionism and advocate for Palestinian rights. While they share a heritage with beneficiaries, their voices are often marginalized or actively suppressed within mainstream Jewish institutions and Israeli political discourse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, diaspora_jews_critical_of_zionism, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the acquisition and control of land, resources, and political power for European Jewish settlers, establishing a new society on dispossessed indigenous territory. It provides a framework for collective identity and security for the settler population.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from indigenous Palestinian Arabs to European Jewish settlers and the Israeli state, along with the associated political, economic, and social benefits.
% ABSENT_VOICES: The voices of dispossessed Palestinians, particularly those denied the right of return, are systematically excluded from the political and legal frameworks that define the Israeli state. Their narratives of dispossession and resistance are actively suppressed within the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the settler-colonial structure vanished overnight, the entire political, legal, and demographic landscape of Israel/Palestine would fundamentally rearrange. Land would revert to its original owners, the 'Law of Return' would be nullified, and the state's foundational ethno-nationalist character would collapse, leading to a complete reordering of power and rights.
% FOUNDING_PROBLEM: The problem Zionism was built to solve, from this reading, was the 'Jewish Question' in Europe – antisemitism and the lack of a secure national home – by establishing a sovereign Jewish state through territorial acquisition and demographic engineering in Palestine.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, Palestinian oral histories, and analyses by postcolonial scholars (e.g., Patrick Wolfe, Ilan Pappé, Edward Said) corroborate the framing of Zionism as a settler-colonial project addressing a European 'Jewish problem' through the displacement of an indigenous population. This perspective is widely attested outside the benefiting parties.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is very high (0.92) due to the systematic and ongoing transfer of land, resources, and rights from Palestinians to the Israeli state and Jewish settlers. Suppression is also very high (0.88) reflecting the extensive military occupation, legal discrimination, and control mechanisms required to maintain the dispossession and prevent Palestinian self-determination. Theater ratio is moderate (0.45) as some state functions (e.g., security, infrastructure development for settlers) are genuine, but a significant portion of the justification (e.g., 'making the desert bloom' or 'only democracy in the Middle East') serves to mask the underlying extractive and suppressive mechanisms. Accessibility collapse is high (0.85) as Palestinians have few viable alternatives to the existing structure, facing severe restrictions on movement, residency, and political participation. Resistance is high (0.75) reflecting continuous Palestinian struggle against the occupation and dispossession.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and Jewish settlers, the constraint is often framed as a legitimate act of national self-determination and defense (closer to a Rope or even Mountain). However, from the perspective of Palestinians and postcolonial analysis, it is a clear Snare, designed for extraction and elimination. The engine's classification will highlight this divergence from the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and European Jewish settlers are the clear beneficiaries, gaining land, resources, and security at the expense of Palestinians. Palestinian Arabs are the primary victims, experiencing dispossession, violence, and legal exclusion. International human rights organizations and postcolonial scholars act as observers, documenting and analyzing the constraint's operation. Diaspora Jews critical of Zionism are excluded, as their dissenting voices challenge the foundational narratives of the project.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_settler_colonial_vs_national_liberation,
    'Is Zionism fundamentally a European settler-colonial project, or a legitimate national liberation movement for the Jewish people?',
    'Historical and sociological analysis comparing the foundational acts, demographic patterns, and legal structures of Zionism to established settler-colonial case studies (e.g., Australia, US, South Africa) versus other national liberation movements.',
    'If confirmed as settler-colonial, the constraint is a Snare by design, with high extraction and suppression. If re-framed as national liberation, its extractiveness and suppression might be re-evaluated as costs of state-building and defense (potentially a Tangled Rope or Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_settler_colonial_vs_national_liberation, conceptual, 'The core conceptual framing of Zionism.').

omega_variable(
    indigeneity_claim_ambiguity,
    'Can Jewish claims of indigeneity to the land coexist with the historical evidence of European migration and Palestinian dispossession, or do they fundamentally contradict?',
    'Interdisciplinary historical, archaeological, and genetic research, alongside critical analysis of the political function of indigeneity claims in colonial contexts.',
    'If Jewish indigeneity is affirmed in a way that does not negate Palestinian indigeneity, the ''indigenous_return_reading'' gains ground, potentially shifting the constraint''s classification towards a contested Rope. If the claims are found to be mutually exclusive in practice, the settler-colonial framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_claim_ambiguity, empirical, 'Contested claims of indigeneity and their implications for the settler-colonial framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Palestinians primarily structural (external barriers, military occupation) or internalized (cognitive patterns, identity fusion with the oppressor)?',
    'Post-occupation trajectory: if suppression persists after the military occupation is removed, reclassify as partially internalized. Analysis of Palestinian cultural resilience and resistance narratives.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as Palestinians carry the suppression with them. If purely structural, removal of external barriers would lead to a more rapid collapse of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Palestinians.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__settler_colonial_reading, theater_ratio, 1987, 0.4).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__settler_colonial_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__settler_colonial_reading, theater_ratio, 2014, 0.44).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.82).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1987, 0.86).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2014, 0.91).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.78).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1987, 0.83).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2014, 0.87).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, israeli_nation_state_law).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, gaza_blockade).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_self_determination' kernel, focusing on its settler-colonial aspects. Other readings exist with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
