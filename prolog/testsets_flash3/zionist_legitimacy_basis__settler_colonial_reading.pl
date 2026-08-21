% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis: Settler-Colonial Reading
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint models Zionism as a European settler-colonial movement,
 *   focusing on its structural function in establishing an ethno-state
 *   through the systematic displacement and suppression of indigenous
 *   Palestinians. The 'claimed_type' is 'snare' because, from this reading's
 *   perspective, the coordination narrative (Jewish self-determination)
 *   serves as cover for a fundamentally extractive and coercive project. The
 *   metrics reflect high extraction and suppression, consistent with a
 *   colonial framework. This is one reading of the 'zionist_legitimacy_basis'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.95).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.98).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis: Settler-Colonial Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '49234f8f-2f00-4585-bc54-553f8ae2fcff').
narrative_ontology:cs_kernel_codification('49234f8f-2f00-4585-bc54-553f8ae2fcff', formalized).
narrative_ontology:cs_authority_grounding('49234f8f-2f00-4585-bc54-553f8ae2fcff', extraction).
narrative_ontology:cs_interpretation_layer_present('49234f8f-2f00-4585-bc54-553f8ae2fcff').
narrative_ontology:cs_reading_relation('49234f8f-2f00-4585-bc54-553f8ae2fcff', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('49234f8f-2f00-4585-bc54-553f8ae2fcff', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('49234f8f-2f00-4585-bc54-553f8ae2fcff', foundational, colonial_settlement_is_illegitimate).
narrative_ontology:cs_axiom_status(colonial_settlement_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('49234f8f-2f00-4585-bc54-553f8ae2fcff', colonial_settlement_is_illegitimate, deontological).
narrative_ontology:cs_axiom('49234f8f-2f00-4585-bc54-553f8ae2fcff', foundational, indigenous_displacement_is_constitutive).
narrative_ontology:cs_axiom_status(indigenous_displacement_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('49234f8f-2f00-4585-bc54-553f8ae2fcff', indigenous_displacement_is_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('49234f8f-2f00-4585-bc54-553f8ae2fcff', european_colonial_expansion).
narrative_ontology:cs_drift_state('49234f8f-2f00-4585-bc54-553f8ae2fcff', contemporary_post_colonial_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('49234f8f-2f00-4585-bc54-553f8ae2fcff', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_settlers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, international_law_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer of the ethno-state, actively implementing policies that dispossess indigenous populations and consolidate control over land and resources. Its legitimacy is derived from and maintained by the settler-colonial project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from the displacement of indigenous populations, gaining access to land, housing, and resources. They are active participants in the colonial project, often supported by state policies and infrastructure.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_settlers, beneficiary,
    organized, biographical, mobile, local).

% The primary victims of the settler-colonial project, experiencing dispossession, displacement, violence, and the systematic denial of their rights and self-determination. Their existence is actively suppressed by the state apparatus.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians, payer,
    powerless, generational, trapped, local).

% A body of principles and conventions (e.g., laws against ethnic cleansing, right of return) that would fundamentally challenge the legitimacy of the settler-colonial project. Its authority is systematically undermined and ignored by the Israeli state and its allies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_law_framework, excluded,
    institutional, civilizational, constrained, global).

% Academics and researchers who analyze Zionism through a settler-colonial lens, documenting its historical and ongoing impacts on indigenous populations. They provide an analytical counter-narrative to official state justifications.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, critical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment and maintenance of an exclusive ethno-state for Jewish people by systematically dispossessing and controlling the indigenous Palestinian population, ensuring demographic and territorial dominance.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from indigenous Palestinians to Jewish settlers and the Israeli state, along with the transfer of political and economic power.
% ABSENT_VOICES: The voices of indigenous Palestinians, particularly those displaced or living under occupation, are systematically suppressed and excluded from official narratives and international forums that legitimize the Israeli state. International legal bodies and human rights organizations are often sidelined or their findings dismissed.
% DISAPPEARANCE_RATIONALE: If the settler-colonial basis of Zionism vanished overnight, the entire structure of the Israeli state would be fundamentally challenged. Land ownership, citizenship laws, and demographic policies would need to be dismantled and rebuilt on principles of equality and decolonization, leading to a radical rearrangement of political and social life in the region.
% FOUNDING_PROBLEM: The perceived need for a secure homeland for Jewish people in response to historical persecution in Europe, framed as a return to an ancestral land.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and its supporters attest that the founding problem (Jewish self-determination and security) remains live. Indigenous Palestinians, critical scholars, and international human rights organizations attest that the 'problem' was solved through colonial means, creating new problems of dispossession and oppression, and that the original justification is now a cover for ongoing extraction.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) as the entire project is predicated on the transfer of land and resources from indigenous to settler populations. Suppression is near total (0.98) due to the active military, legal, and administrative mechanisms used to control and dispossess Palestinians. Theater ratio is high (0.75) because the justifications of security and national liberation increasingly serve to mask the underlying colonial project, with a significant portion of state activity dedicated to maintaining this narrative and suppressing counter-narratives. Resistance is high (0.85) reflecting ongoing Palestinian struggle against the colonial project.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and Zionist settlers, the project is one of national liberation and self-determination, justifying the actions taken. From the perspective of indigenous Palestinians and critical scholars, it is a clear case of settler-colonialism, with the state acting as an agenda-setter for an extractive system. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and Zionist settlers are clear beneficiaries, gaining land, resources, and political power. Indigenous Palestinians are the primary victims, bearing the full cost of dispossession and violence. The international law framework is 'excluded' as its principles are systematically violated and its authority denied by the state, even as it theoretically offers a path to justice for victims. Critical scholars act as 'observers', analyzing the structural dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (Jewish self-determination) is presented as live, but from this reading, its implementation has become a mechanism for ongoing extraction and suppression, indicating a potential mandatrophy where the original goal is achieved through means that fundamentally contradict universal principles of justice and self-determination for all. The high theater ratio suggests that the 'coordination' narrative is largely performative, masking the underlying snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_ambiguity,
    'Is Zionism primarily a national liberation movement for an indigenous people, or a settler-colonial project?',
    'Comprehensive historical and sociological analysis, including indigenous perspectives, land tenure records, and patterns of migration and displacement, evaluated against established definitions of indigeneity and settler-colonialism.',
    'Resolution would fundamentally alter the perceived legitimacy and ethical status of the Israeli state, reclassifying its foundational claims from a ''rope'' (national liberation) to a ''snare'' (settler-colonialism) or vice-versa, with profound implications for international law and political solutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_narrative_ambiguity, conceptual, 'Ambiguity in the historical classification of Zionism.').

omega_variable(
    indigenous_status_of_jews,
    'Are Jewish people indigenous to the land of Israel/Palestine in a manner that justifies the displacement of existing populations?',
    'Anthropological and historical research on continuous presence, cultural ties, and political self-organization, alongside legal definitions of indigeneity that account for historical and contemporary claims without negating the rights of other long-standing populations.',
    'If Jewish indigeneity is affirmed without justifying displacement, the constraint might shift towards a ''tangled_rope'' (coordination with extraction) rather than a ''snare''. If denied, the settler-colonial reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_of_jews, conceptual, 'The contested indigenous status of Jewish people in the context of land claims.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Palestinians structural (external barriers) or internalized (cognitive patterns)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., if Palestinians gain full sovereignty but still face internal barriers to self-determination), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making decolonization more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Palestinian context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1900, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(zion_tr_t1920, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.6).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1993, 0.7).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(zion_be_t1900, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(zion_be_t1920, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1920, 0.7).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1993, 0.92).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1900, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(zion_su_t1920, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1993, 0.95).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, gaza_blockade).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. This 'settler_colonial_reading' focuses on the extractive and suppressive aspects of the state's formation and maintenance, contrasting with 'national_liberation_reading' and 'religious_restoration_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
