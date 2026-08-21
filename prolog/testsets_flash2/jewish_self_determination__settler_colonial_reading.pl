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
 *   This constraint models Zionism as a settler-colonial project, focusing on
 *   the systematic dispossession of indigenous Palestinians. It is one
 *   reading of the 'Jewish self-determination' kernel. The constraint's
 *   structure is designed to extract resources and eliminate the indigenous
 *   population, classifying it as a snare. The high extractiveness and
 *   suppression reflect the ongoing violence, legal exclusion, and
 *   displacement experienced by Palestinians since 1948. The claimed type
 *   'snare' directly reflects this reading's interpretation of the structural
 *   function.
 *
 * KEY AGENTS:
 *   - israeli_state: Agenda setter (institutional/constrained) — enforces dispossession
 *   - european_jewish_settlers: Beneficiary (powerful/mobile) — benefits from land and state protection
 *   - palestinian_arabs: Payer (powerless/trapped) — bears the costs of dispossession and violence
 *   - international_human_rights_organizations: Observer (organized/analytical) — documents abuses, lacks enforcement
 *   - anti_colonial_scholars_and_activists: Observer (moderate/analytical) — critiques the project through a settler-colonial lens
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
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as a Settler-Colonial Project").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'b0443ff1-037d-4265-a863-a293f2f63ed9').
narrative_ontology:cs_kernel_codification('b0443ff1-037d-4265-a863-a293f2f63ed9', formalized).
narrative_ontology:cs_authority_grounding('b0443ff1-037d-4265-a863-a293f2f63ed9', extraction).
narrative_ontology:cs_interpretation_layer_present('b0443ff1-037d-4265-a863-a293f2f63ed9').
narrative_ontology:cs_reading_relation('b0443ff1-037d-4265-a863-a293f2f63ed9', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0443ff1-037d-4265-a863-a293f2f63ed9', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('b0443ff1-037d-4265-a863-a293f2f63ed9', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0443ff1-037d-4265-a863-a293f2f63ed9', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('b0443ff1-037d-4265-a863-a293f2f63ed9', foundational, zionism_is_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('b0443ff1-037d-4265-a863-a293f2f63ed9', zionism_is_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('b0443ff1-037d-4265-a863-a293f2f63ed9', foundational, palestinian_indigenous_rights_are_primary).
narrative_ontology:cs_axiom_status(palestinian_indigenous_rights_are_primary, holdable).
narrative_ontology:cs_axiom_grounding('b0443ff1-037d-4265-a863-a293f2f63ed9', palestinian_indigenous_rights_are_primary, deontological).
narrative_ontology:cs_reference_frame('b0443ff1-037d-4265-a863-a293f2f63ed9', post_nakba_dispossession).
narrative_ontology:cs_drift_state('b0443ff1-037d-4265-a863-a293f2f63ed9', contemporary_international_law_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b0443ff1-037d-4265-a863-a293f2f63ed9', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional actor that establishes and enforces laws, policies, and military actions to maintain control over land and resources, benefiting its Jewish citizens and dispossessing Palestinians. Its legitimacy is tied to the ongoing project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Individuals and communities who have migrated to or settled in historic Palestine, benefiting from land allocation, state protection, and preferential legal status, often at the expense of Palestinian displacement. Their material well-being is directly linked to the settler-colonial project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    powerful, biographical, mobile, local).

% The indigenous population dispossessed of their land, subjected to military occupation, legal discrimination, and forced displacement. They bear the direct costs of the settler-colonial project, with severely constrained options for exit or resistance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Monitor and document human rights abuses, displacement, and legal discrimination against Palestinians. They provide critical analysis and advocacy, but lack direct enforcement power over the Israeli state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Analyze and critique Zionism through a settler-colonial framework, highlighting historical patterns of dispossession and ongoing structural violence. They contribute to the intellectual and political discourse challenging the constraint.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, anti_colonial_scholars_and_activists, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the acquisition and control of land, resources, and political power for a specific ethno-religious group (Jewish settlers) by systematically displacing and disempowering the indigenous population (Palestinians).
% TRANSFER_FUNCTION: Transfers land, water, and sovereignty from indigenous Palestinian Arabs to European Jewish settlers and the Israeli state, along with the associated political and economic benefits.
% ABSENT_VOICES: Palestinian refugees and exiles, whose right of return is denied, are systematically excluded from any political process that would challenge the settler-colonial structure. Their voices would demand decolonization and restitution.
% DISAPPEARANCE_RATIONALE: If the settler-colonial framework vanished, the entire political, legal, and demographic structure of the region would be fundamentally altered. Land ownership, citizenship rights, and resource allocation would be renegotiated, leading to a radical rearrangement of power and territory.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and persecution in Europe, addressed by establishing a sovereign Jewish state in historic Palestine.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and its supporters attest that the founding problem of Jewish security remains live. Palestinian and postcolonial scholars, along with international legal bodies, attest that the founding problem has been superseded by the problem of Palestinian dispossession, and the current arrangement functions as an ongoing colonial project.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.92) due to the ongoing seizure of land, control of resources, and denial of rights to Palestinians. Suppression is also very high (0.88) because the project relies on military occupation, legal frameworks (e.g., the Law of Return vs. denial of Palestinian right of return), and systematic violence to prevent Palestinian resistance and return. Theater ratio is low (0.15) as the primary function is direct extraction and control, with minimal performative cover. Accessibility collapse is high (0.75) because alternatives for Palestinians (e.g., self-determination, return) are systematically foreclosed. Resistance is high (0.85) reflecting the continuous struggle of Palestinians against the project.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state and European Jewish settlers experience this as a project of national liberation and security, justifying the measures as necessary for survival. Palestinian Arabs, however, experience it as a violent, extractive, and suppressive force leading to their ongoing dispossession. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a 'rope' or 'scaffold' (from their perspective) and victims experiencing a 'snare'.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and European Jewish settlers are clear beneficiaries, with the state acting as the primary agenda-setter and enforcer, thus having low directionality (subsidized). Palestinian Arabs are the primary targets, bearing the full cost of the constraint, leading to high directionality (extracted from). International observers and activists have an analytical directionality, seeking to understand and challenge the constraint without being directly subject to its extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames the constraint as a snare, arguing that its original mandate (Jewish self-determination) has been co-opted to justify ongoing settler-colonial extraction. The classification prevents mislabeling the project as a legitimate 'rope' or 'scaffold' by highlighting the systematic dispossession and violence inherent in its operation, which are central to its persistence, not merely side effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_vs_national_liberation,
    'Is Zionism fundamentally a settler-colonial project, or a national liberation movement for the Jewish people?',
    'Historical analysis of land acquisition, demographic changes, legal frameworks, and the treatment of indigenous populations, compared against established definitions of settler-colonialism and national liberation movements.',
    'If confirmed as settler-colonial, the constraint is a snare, with high extraction and suppression. If reclassified as national liberation, it would be a rope or scaffold, with lower extraction and a focus on coordination for self-determination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_colonial_vs_national_liberation, conceptual, 'The core conceptual framing of Zionism.').

omega_variable(
    indigenous_status_of_jewish_people,
    'Are Jewish people indigenous to the land of Israel/Palestine, and does this status alter the settler-colonial analysis?',
    'Anthropological, historical, and genetic studies of Jewish connection to the land, alongside a comparative analysis of indigenous rights frameworks and the historical context of European Jewish migration.',
    'If Jewish indigeneity is affirmed and recognized as primary, it could complicate or challenge the settler-colonial framing, potentially shifting the constraint towards a contested ''tangled_rope'' or even ''rope'' for some seats. If not, the settler-colonial framing remains robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_of_jewish_people, empirical, 'The role of Jewish indigeneity in the settler-colonial framework.').

omega_variable(
    law_of_return_asymmetry,
    'Is the Israeli Law of Return, coupled with the denial of Palestinian right of return, an inherently extractive and discriminatory legal mechanism?',
    'Comparative legal analysis of citizenship laws in other states, international human rights law, and the demographic impact of these policies on both Jewish and Palestinian populations.',
    'If confirmed as discriminatory and extractive, it reinforces the snare classification and high extractiveness. If argued as a legitimate national self-determination tool, it would reduce the perceived extractiveness from the perspective of Jewish beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(law_of_return_asymmetry, empirical, 'Legal mechanism for demographic engineering and dispossession.').


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
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__settler_colonial_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__settler_colonial_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__settler_colonial_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.88).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1993, 0.9).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2005, 0.91).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2014, 0.92).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1993, 0.86).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2005, 0.87).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2014, 0.88).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'Jewish self-determination' kernel. Each reading presents a distinct structural analysis of the same historical and political phenomena, leading to different classifications and stakeholder dynamics. This reading emphasizes the settler-colonial aspects, while others focus on national liberation, indigenous return, religious covenant, or diasporic identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
