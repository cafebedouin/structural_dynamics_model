% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: Hong Kong National Security Law: Democratic Enclosure Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the Hong Kong National Security Law (NSL)
 *   from the 'democratic enclosure' reading. In this reading, the NSL
 *   functions as a snare, systematically dismantling democratic institutions,
 *   criminalizing dissent, and closing the space for civil society and
 *   independent media in Hong Kong. It is actively enforced by the Beijing
 *   central government and the Hong Kong establishment, with identifiable
 *   victims among the city's pro-democracy movement and general populace. The
 *   high extractiveness and suppression reflect the law's comprehensive
 *   impact on fundamental freedoms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.95).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law: Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, 'f503dda0-05cd-4fa5-b6b9-0f72e895d8be').
narrative_ontology:cs_kernel_codification('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', formalized).
narrative_ontology:cs_authority_grounding('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', extraction).
narrative_ontology:cs_interpretation_layer_present('f503dda0-05cd-4fa5-b6b9-0f72e895d8be').
narrative_ontology:cs_reading_relation('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', foundational, democratic_participation_is_fundamental_right).
narrative_ontology:cs_axiom_status(democratic_participation_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', democratic_participation_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', foundational, national_security_claims_are_pretextual).
narrative_ontology:cs_axiom_status(national_security_claims_are_pretextual, holdable).
narrative_ontology:cs_axiom_grounding('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', national_security_claims_are_pretextual, empirically_contingent).
narrative_ontology:cs_reference_frame('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', one_country_two_systems_autonomy).
narrative_ontology:cs_drift_state('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', post_nsl_enactment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f503dda0-05cd-4fa5-b6b9-0f72e895d8be', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_media).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and imposed the NSL, directly benefiting from the suppression of dissent and consolidation of control over Hong Kong. Views the NSL as a necessary tool for national security and stability, allowing it to bypass local legislative processes.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Comprises pro-Beijing political figures, business elites, and civil servants who benefit from the NSL by gaining increased political stability, reduced opposition, and alignment with Beijing's agenda. They administer the law locally, often with direct guidance from Beijing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment, beneficiary,
    institutional, biographical, constrained, national).

% Includes NGOs, student groups, and advocacy organizations that previously operated in Hong Kong's vibrant democratic space. They face severe restrictions, arrests, and forced dissolution under the NSL, effectively criminalizing their activities and closing avenues for peaceful dissent.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society, payer,
    powerless, immediate, trapped, local).

% Journalists and media outlets that provided critical reporting on Hong Kong and mainland affairs. They are targeted by the NSL for 'collusion with foreign forces' or 'inciting secession,' leading to arrests, asset freezes, and self-censorship, effectively dismantling press freedom.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_media, payer,
    moderate, immediate, identity_locked, local).

% Elected representatives and activists who advocated for greater democracy in Hong Kong. They have been disqualified, arrested, and imprisoned under the NSL, effectively eliminating political opposition and ensuring a rubber-stamp legislature.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_politicians, payer,
    powerless, biographical, trapped, local).

% Experience a chilling effect on free speech, assembly, and protest. Many have self-censored, deleted social media posts, or emigrated due to fear of prosecution under the broad terms of the NSL. Their democratic aspirations are systematically suppressed.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_citizens, payer,
    powerless, biographical, constrained, local).

% Monitor and report on human rights abuses in Hong Kong under the NSL. They condemn the law's impact on fundamental freedoms but have limited direct power to alter its enforcement within Hong Kong.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of the Beijing and Hong Kong establishment, the NSL coordinates the suppression of perceived threats to national security and political stability, ensuring alignment with central government directives.
% TRANSFER_FUNCTION: Transfers democratic freedoms, civil liberties, and political autonomy from Hong Kong citizens, civil society, and independent media to the control and authority of the Beijing central government and the Hong Kong establishment.
% ABSENT_VOICES: The voices of exiled pro-democracy activists, international legal scholars advocating for common law principles, and human rights advocates are absent from the internal discourse within Hong Kong, as their views are criminalized or dismissed as 'foreign interference.'
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, Hong Kong's democratic space would immediately begin to reopen. Civil society organizations would re-emerge, independent media would resume critical reporting, and political opposition would re-mobilize. The power dynamics between Beijing and Hong Kong would fundamentally shift, leading to a rapid reorganization of political and social life.
% FOUNDING_PROBLEM: The NSL was ostensibly enacted to address 'national security risks' in Hong Kong, particularly after the 2019 anti-government protests, which Beijing framed as secessionist, subversive, and colluding with foreign forces.
% FOUNDING_PROBLEM_CORROBORATION: The Beijing and Hong Kong governments assert the founding problem is live, citing ongoing threats to national security. However, international legal bodies, human rights organizations, and exiled Hong Kong activists widely contest this, arguing that the 'threats' were legitimate expressions of dissent and that the NSL's true purpose is to eliminate political opposition and democratic freedoms. Independent legal analysis from outside the benefiting parties supports the latter reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.92) is extremely high because the NSL effectively seizes fundamental rights and political agency from a large population, transferring control to an authoritarian center. Suppression (0.95) is near-total, as the law's broad definitions and extraterritorial reach create a pervasive chilling effect, backed by arrests and severe penalties. Theater ratio (0.15) is low because the law's primary function is direct, coercive control, with minimal performative elements masking a different purpose. The law is a snare because its coordination story (national security) is a cover for pure extraction of democratic space and political power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Beijing and Hong Kong establishment, the NSL is a legitimate and necessary measure for national security, restoring order and stability. From the perspective of Hong Kong citizens and civil society, it is an instrument of political oppression and democratic enclosure. The engine's classification as a snare reflects the structural reality of asymmetric extraction and suppression, regardless of the official narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Beijing central government and the Hong Kong establishment are clear beneficiaries and agenda-setters, gaining consolidated power and control (d near 0.0). Hong Kong civil society, independent media, pro-democracy politicians, and citizens are the primary targets, bearing the full cost of lost freedoms and facing criminalization (d near 1.0). International human rights organizations act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_purpose_of_nsl,
    'Is the NSL primarily a genuine national security instrument, or is its primary function the suppression of political opposition and democratic space?',
    'Long-term analysis of prosecution patterns (e.g., ratio of cases against violent actors vs. peaceful dissidents), and independent assessment of actual security threats versus political dissent.',
    'If primarily for security, the extractiveness might be re-evaluated as a necessary cost of a coordination function (e.g., a Tangled Rope). If primarily for suppression of dissent, its Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(true_purpose_of_nsl, empirical, 'Distinguishing national security from political suppression as the NSL''s core function.').

omega_variable(
    international_legal_challenge_efficacy,
    'To what extent can international legal challenges or sanctions effectively alter the enforcement or interpretation of the NSL?',
    'Observation of the impact of international pressure on specific NSL cases, or changes in Hong Kong''s legal framework in response to international condemnation.',
    'If international pressure proves effective, the suppression metric might be slightly lower due to external constraints on enforcement, and the exit options for some stakeholders might become less ''trapped.'' If ineffective, the current high suppression and trapped status are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_legal_challenge_efficacy, empirical, 'The efficacy of external legal and political pressure on the NSL.').

omega_variable(
    internalized_suppression_dynamics,
    'Is the measured suppression primarily structural (external barriers) or internalized (cognitive patterns, self-censorship) among Hong Kong citizens?',
    'Post-NSL repeal/amendment studies: if self-censorship and fear persist after structural barriers are removed, it indicates a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as citizens carry the suppression with them even in the absence of direct enforcement. This would deepen the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_dynamics, empirical, 'Structural vs. internalized suppression mechanism among Hong Kong citizens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2023, 0.16).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2021, 0.88).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2022, 0.9).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2023, 0.91).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2021, 0.91).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2022, 0.93).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2023, 0.94).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).

% DUAL FORMULATION NOTE:
% The Hong Kong National Security Law (NSL) is a contested kernel with multiple readings. This file represents the 'democratic enclosure' reading, focusing on the law's impact on civil liberties and political space. Other readings (jurisdictional capture, sovereignty restoration) are modeled as separate constraints, linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
