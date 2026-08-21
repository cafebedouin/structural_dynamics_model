% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law as Jurisdictional Capture
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the National Security Law (NSL) in Hong
 *   Kong as a vehicle for the transplantation of mainland China's legal
 *   system, leading to the erosion of Hong Kong's common law autonomy. This
 *   reading focuses on the structural changes to the legal framework and
 *   judicial independence, rather than solely on the criminalization of
 *   dissent. The constraint is claimed as a Tangled Rope because it presents
 *   a coordination function (legal alignment for national security) but
 *   operates with substantial extraction of institutional independence and
 *   requires active enforcement.
 *
 * KEY AGENTS:
 *   - mainland_security_apparatus: Primary agenda_setter (institutional/arbitrage) — expands jurisdiction and control.
 *   - central_government_officials: Primary beneficiary (institutional/arbitrage) — benefits from consolidated political and legal control.
 *   - hong_kong_judiciary: Primary payer (institutional/identity_locked) — loses autonomy, constrained by new legal framework.
 *   - hong_kong_legal_profession: Payer (organized/constrained) — faces new interpretations and professional risks.
 *   - hong_kong_citizens: Payer (powerless/trapped) — loses common law protections.
 *   - international_legal_observers: Analytical observer (analytical/analytical) — monitors erosion of autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.7).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.8).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law as Jurisdictional Capture").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '772ce108-0030-4d8e-a288-9a02955090e0').
narrative_ontology:cs_kernel_codification('772ce108-0030-4d8e-a288-9a02955090e0', formalized).
narrative_ontology:cs_authority_grounding('772ce108-0030-4d8e-a288-9a02955090e0', extraction).
narrative_ontology:cs_interpretation_layer_present('772ce108-0030-4d8e-a288-9a02955090e0').
narrative_ontology:cs_reading_relation('772ce108-0030-4d8e-a288-9a02955090e0', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('772ce108-0030-4d8e-a288-9a02955090e0', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('772ce108-0030-4d8e-a288-9a02955090e0', foundational, mainland_legal_system_transplantation_is_primary_goal).
narrative_ontology:cs_axiom_status(mainland_legal_system_transplantation_is_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('772ce108-0030-4d8e-a288-9a02955090e0', mainland_legal_system_transplantation_is_primary_goal, conventional).
narrative_ontology:cs_axiom('772ce108-0030-4d8e-a288-9a02955090e0', secondary, hong_kong_common_law_autonomy_is_subordinate).
narrative_ontology:cs_axiom_status(hong_kong_common_law_autonomy_is_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('772ce108-0030-4d8e-a288-9a02955090e0', hong_kong_common_law_autonomy_is_subordinate, conventional).
narrative_ontology:cs_reference_frame('772ce108-0030-4d8e-a288-9a02955090e0', hong_kong_common_law_autonomy).
narrative_ontology:cs_drift_state('772ce108-0030-4d8e-a288-9a02955090e0', post_nsl_enactment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('772ce108-0030-4d8e-a288-9a02955090e0', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, central_government_officials).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively implements and enforces the National Security Law, expanding its jurisdiction and operational reach into Hong Kong's legal system. Benefits from increased control and the ability to bypass Hong Kong's traditional legal processes in national security cases.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the NSL's role in consolidating political control over Hong Kong and ensuring legal alignment with mainland China's objectives. Sees the NSL as a necessary tool for national unity and stability.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, central_government_officials, beneficiary,
    institutional, generational, arbitrage, global).

% Experiences a significant erosion of its common law autonomy and judicial independence. Judges are constrained by new interpretations and the NSL's supremacy, facing pressure to align with mainland legal principles. Exit means abandoning their professional identity and the rule of law they swore to uphold.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, biographical, identity_locked, local).

% Faces new legal interpretations, professional risks, and a shrinking space for advocating common law principles. Lawyers must navigate a legal landscape increasingly influenced by mainland law, with limited avenues for challenging NSL decisions. Exit means leaving a specialized legal market.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    organized, biographical, constrained, local).

% Loses common law protections, including due process and freedom of expression, particularly in cases deemed related to national security. Faces increased surveillance and the risk of being tried under a legal framework that differs significantly from Hong Kong's traditional system. Exit often means emigration.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens, payer,
    powerless, immediate, trapped, local).

% Monitors and reports on the erosion of Hong Kong's common law autonomy and judicial independence. Provides critical analysis but has no direct power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates Hong Kong's legal system more closely with mainland China's, ensuring alignment on national security matters and reducing perceived legal divergence.
% TRANSFER_FUNCTION: Transfers legal and judicial autonomy, along with associated powers and control, from Hong Kong's common law system to the mainland's legal framework and its enforcing apparatus.
% ABSENT_VOICES: Pro-democracy activists, independent legal scholars, and international human rights organizations are largely excluded from the formal legal and political discourse in Hong Kong, many having been imprisoned, exiled, or silenced. They would advocate for the restoration of common law autonomy and judicial independence.
% DISAPPEARANCE_RATIONALE: If the NSL's jurisdictional capture vanished overnight, Hong Kong's common law system would immediately reassert its full autonomy, previous legal interpretations would be restored, and the mainland's influence over judicial appointments and case handling would recede. This would fundamentally alter the legal and political landscape, restoring the 'one country, two systems' legal framework as it was understood prior to the NSL.
% FOUNDING_PROBLEM: The central government perceived a lack of effective legal mechanisms to address perceived threats to national security and sovereignty in Hong Kong, particularly after large-scale protests and perceived foreign interference, leading to a desire for greater legal alignment.
% FOUNDING_PROBLEM_CORROBORATION: The central government and its aligned media attest the problem is still live and the NSL is a necessary solution. However, Hong Kong's legal community, international observers, and pro-democracy groups attest that the original problem was exaggerated or manufactured, and the NSL's primary function is now to consolidate political control and legal integration, not merely security. Legislative-hearing testimony and independent legal analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70) because the NSL fundamentally alters the balance of legal power, transferring significant judicial and legal autonomy from Hong Kong to mainland-aligned structures. Suppression is also high (0.80) due to the active enforcement mechanisms, including new security agencies and broad powers of interpretation, which actively curtail legal challenges and dissent. The theater ratio is moderate (0.40) as some common law procedures and institutions persist, but their substantive independence is compromised, with a growing portion of their function becoming performative adherence to the new legal order. Accessibility collapse is high (0.75) as alternatives to the mainland-influenced legal framework are severely curtailed, and resistance is moderate (0.50) reflecting ongoing, albeit suppressed, efforts by legal professionals and civil society to uphold common law principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the mainland security apparatus and central government officials, the NSL is a legitimate and necessary instrument for national security and constitutional order. From the perspective of the Hong Kong judiciary and legal profession, it represents an illegitimate capture of their institutional independence and a fundamental alteration of the common law system. The engine's classification will highlight this divergence by computing different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainland security apparatus and central government officials are clear beneficiaries, gaining expanded powers and control (low directionality). The Hong Kong judiciary and legal profession are primary targets, bearing the costs of lost autonomy and professional constraints (high directionality). Hong Kong citizens are also targets, losing legal protections and facing increased risks (high directionality). International legal observers maintain an analytical stance, neither directly benefiting nor paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The NSL's original mandate to address national security threats is contested; this reading argues that its persistence is increasingly driven by the goal of legal system transplantation and jurisdictional capture, rather than solely by the initial security concerns. The constraint's high extractiveness and suppression, coupled with the contested founding problem status, suggest a drift towards a more extractive function beyond its initial coordination claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_true_purpose_ambiguity,
    'Is the primary purpose of the NSL to genuinely address national security threats, or is it a vehicle for the systematic transplantation of mainland legal principles and erosion of common law autonomy?',
    'Long-term observation of legal reforms, judicial appointments, and the types of cases prosecuted under the NSL. If the scope of ''national security'' continuously expands to cover broader political and legal integration, it supports the transplantation reading.',
    'If primarily transplantation, the constraint''s extractiveness and suppression are more accurately attributed to a deliberate strategy of legal capture, rather than a necessary security measure, reinforcing its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nsl_true_purpose_ambiguity, conceptual, 'Ambiguity regarding the NSL''s core intent: security vs. legal transplantation.').

omega_variable(
    common_law_resilience,
    'To what extent can elements of Hong Kong''s common law system and judicial independence genuinely persist and adapt under the NSL, or is its erosion inevitable and complete?',
    'Empirical analysis of court judgments, legal challenges, and the professional practices of lawyers and judges over time. Evidence of successful common law challenges or persistent judicial independence would indicate resilience.',
    'Higher resilience would slightly lower the effective extractiveness and suppression, suggesting a more complex hybrid system rather than outright capture. Lower resilience would confirm the severe erosion of autonomy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_resilience, empirical, 'The degree of resilience of Hong Kong''s common law system under the NSL.').

omega_variable(
    international_response_impact,
    'How significantly does international legal and political pressure influence the pace and extent of legal transplantation and the erosion of common law autonomy?',
    'Comparative analysis of legal developments in Hong Kong against periods of heightened or diminished international scrutiny and diplomatic action. Correlation between pressure and changes in legal practice or policy would be indicative.',
    'Strong international influence would suggest that the constraint''s persistence and severity are partly modulated by external factors, potentially affecting the perceived ''naturalness'' or inevitability of the legal changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_response_impact, empirical, 'The role of international pressure in shaping the NSL''s impact on Hong Kong''s legal system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2022, 0.34).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2024, 0.37).
narrative_ontology:measurement(nsl__tr_t2026, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2026, 0.39).
narrative_ontology:measurement(nsl__tr_t2028, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2028, 0.4).
narrative_ontology:measurement(nsl__tr_t2030, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2024, 0.67).
narrative_ontology:measurement(nsl__be_t2026, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2026, 0.69).
narrative_ontology:measurement(nsl__be_t2028, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2028, 0.7).
narrative_ontology:measurement(nsl__be_t2030, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2030, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2022, 0.74).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2024, 0.77).
narrative_ontology:measurement(nsl__su_t2026, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2026, 0.79).
narrative_ontology:measurement(nsl__su_t2028, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2028, 0.8).
narrative_ontology:measurement(nsl__su_t2030, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2030, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nsl_legal_text' kernel. This 'jurisdictional_capture_reading' focuses on the NSL as a vehicle for mainland legal system transplantation, eroding common law autonomy. It coexists with the 'sovereignty_restoration_reading' (NSL as legitimate security instrument) and influences the 'democratic_enclosure_reading' (NSL as mechanism for permanent closure of democratic space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
