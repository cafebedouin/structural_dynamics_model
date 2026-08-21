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
 *   human_readable: NSL as Vehicle for Mainland Legal System Transplantation (Jurisdictional Capture Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the National Security Law (NSL) in Hong
 *   Kong from the 'jurisdictional capture' reading. In this framing, the NSL
 *   is understood not merely as a security measure, but as a deliberate
 *   vehicle for transplanting elements of the mainland Chinese legal system
 *   into Hong Kong, thereby eroding the autonomy and distinctiveness of its
 *   common law tradition. The constraint's persistence depends on active
 *   enforcement and the suppression of legal and political alternatives, with
 *   identifiable victims in the Hong Kong judiciary, legal profession, and
 *   citizenry.
 *
 * KEY AGENTS:
 *   - mainland_security_apparatus: Primary beneficiary/agenda_setter (institutional/arbitrage) — gains expanded powers and jurisdiction.
 *   - central_government_officials: Primary beneficiary/agenda_setter (institutional/arbitrage) — gains greater control over Hong Kong's governance.
 *   - hong_kong_judiciary: Primary target/payer (institutional/trapped) — loses autonomy and is forced to apply mainland legal concepts.
 *   - hong_kong_legal_profession: Primary target/payer (organized/identity_locked) — faces erosion of common law principles and professional identity.
 *   - hong_kong_citizens: Primary target/payer (powerless/trapped) — bears the direct costs of reduced civil liberties and legal protections.
 *   - international_legal_bodies: Analytical observer (institutional/analytical) — monitors and critiques the legal changes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.75).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.85).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL as Vehicle for Mainland Legal System Transplantation (Jurisdictional Capture Reading)").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, 'c95c058f-5dc1-4168-a752-5286991dc617').
narrative_ontology:cs_kernel_codification('c95c058f-5dc1-4168-a752-5286991dc617', formalized).
narrative_ontology:cs_authority_grounding('c95c058f-5dc1-4168-a752-5286991dc617', extraction).
narrative_ontology:cs_interpretation_layer_present('c95c058f-5dc1-4168-a752-5286991dc617').
narrative_ontology:cs_reading_relation('c95c058f-5dc1-4168-a752-5286991dc617', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('c95c058f-5dc1-4168-a752-5286991dc617', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('c95c058f-5dc1-4168-a752-5286991dc617', foundational, common_law_autonomy_is_subordinate).
narrative_ontology:cs_axiom_status(common_law_autonomy_is_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('c95c058f-5dc1-4168-a752-5286991dc617', common_law_autonomy_is_subordinate, conventional).
narrative_ontology:cs_axiom('c95c058f-5dc1-4168-a752-5286991dc617', foundational, mainland_legal_system_supremacy_in_security_matters).
narrative_ontology:cs_axiom_status(mainland_legal_system_supremacy_in_security_matters, holdable).
narrative_ontology:cs_axiom_grounding('c95c058f-5dc1-4168-a752-5286991dc617', mainland_legal_system_supremacy_in_security_matters, conventional).
narrative_ontology:cs_reference_frame('c95c058f-5dc1-4168-a752-5286991dc617', one_country_two_systems_eroded).
narrative_ontology:cs_drift_state('c95c058f-5dc1-4168-a752-5286991dc617', post_nsl_enactment, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c95c058f-5dc1-4168-a752-5286991dc617', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, central_government_officials).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hong_kong_pro_beijing_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefits from the NSL's expanded jurisdiction and enforcement powers, allowing it to operate within Hong Kong with reduced accountability to local legal norms. It actively enforces the NSL, ensuring its provisions are applied to suppress perceived threats to national security.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Oversee the implementation of the NSL, viewing it as a necessary tool to assert sovereign control and integrate Hong Kong more fully into the national governance framework. They benefit from the erosion of Hong Kong's common law autonomy, which simplifies governance from Beijing's perspective.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, central_government_officials, agenda_setter,
    institutional, generational, arbitrage, global).

% Forced to interpret and apply the NSL, which introduces mainland legal concepts and principles that conflict with common law traditions. Their autonomy and independence are severely constrained, and their decisions are subject to review by mainland authorities in certain NSL cases.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, biographical, trapped, local).

% Faces a rapidly changing legal landscape where common law principles are undermined, and the scope for independent legal practice is shrinking. Many feel their professional identity, rooted in common law, is under attack, making exit difficult due to career path dependence and a sense of duty.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    organized, biographical, identity_locked, local).

% Bear the direct consequences of the NSL, including reduced civil liberties, increased surveillance, and the chilling effect on free expression. They are subject to a legal system increasingly influenced by mainland norms, with limited avenues for redress or resistance.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens, payer,
    powerless, immediate, trapped, local).

% Benefits from aligning with the central government's agenda, gaining political influence and economic opportunities. They support the NSL as a means to restore order and stability, often echoing official narratives about its necessity.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_pro_beijing_establishment, beneficiary,
    powerful, biographical, constrained, local).

% Monitor the erosion of Hong Kong's legal autonomy and issue reports condemning the NSL's impact on human rights and the rule of law. They have no direct enforcement power but exert diplomatic and reputational pressure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NSL claims to coordinate national security efforts across Hong Kong and the mainland, ensuring a unified approach to threats against state sovereignty and social stability.
% TRANSFER_FUNCTION: Transfers legal and political autonomy from Hong Kong's common law institutions to mainland authorities, effectively transplanting elements of the mainland legal system into Hong Kong. It also transfers the burden of maintaining 'national security' from the central government to Hong Kong's legal and judicial system, and ultimately to its citizens.
% ABSENT_VOICES: Exiled pro-democracy activists, independent international legal scholars, and human rights organizations are excluded from the official discourse. They would argue that the NSL is a tool of political repression and legal subjugation, not a legitimate security instrument.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement vanished overnight, Hong Kong's common law system would immediately begin to reassert its autonomy. Mainland legal concepts would recede, political prisoners would be released, and the legal profession would regain its independence. The political landscape would shift dramatically, with a resurgence of civil society and pro-democracy movements.
% FOUNDING_PROBLEM: The central government stated the NSL was built to address perceived threats to national security and social stability in Hong Kong, particularly after the 2019 pro-democracy protests, which it characterized as foreign interference and secessionist activity.
% FOUNDING_PROBLEM_CORROBORATION: Mainland authorities and the pro-Beijing establishment in Hong Kong assert that the founding problem of national security threats remains live. However, independent international legal bodies, human rights organizations, and many within the Hong Kong legal community argue that the 'problem' was largely a pretext for asserting greater control, and that the NSL has created more instability than it solved. Legislative hearing testimony and independent legal analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the significant transfer of legal and political autonomy from Hong Kong to mainland authorities. Suppression (0.85) is very high due to the NSL's broad scope, severe penalties, and the active enforcement by security forces, which systematically eliminate legal and political alternatives. The moderate theater ratio (0.45) indicates that while there is a veneer of legal process, a substantial portion of the activity serves to legitimize the transplantation of mainland legal norms and suppress dissent, rather than genuinely uphold common law principles. Accessibility collapse is high (0.75) as avenues for legal challenge or political expression are systematically closed. Resistance (0.55) is moderate, reflecting ongoing, albeit suppressed, efforts by legal professionals and civil society to push back against the erosion of autonomy.
 *
 * PERSPECTIVAL GAP:
 *   The mainland agenda-setter seats (mainland_security_apparatus, central_government_officials) experience the NSL as a legitimate and necessary instrument for national security and sovereign restoration. In contrast, the payer seats (hong_kong_judiciary, hong_kong_legal_profession, hong_kong_citizens) experience it as a coercive mechanism that systematically dismantles their legal and political freedoms. The engine's per-seat classification will highlight this divergence, showing the NSL as a Snare for the victims and potentially a Rope or even a Mountain (from a 'sovereignty' perspective) for the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainland_security_apparatus and central_government_officials are clear beneficiaries, gaining expanded powers and control, leading to a low directionality (d near 0.0). The hong_kong_judiciary, hong_kong_legal_profession, and hong_kong_citizens are direct targets, bearing the costs of eroded autonomy and suppressed rights, resulting in high directionality (d near 1.0). The hong_kong_pro_beijing_establishment benefits from its alignment with the central government, placing it as a beneficiary with moderate exit options. International_legal_bodies are analytical observers, not directly subject to the constraint's extraction or benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_kernel_reading_identity,
    'Is this constraint a genuine security instrument, a tool for political control, or a vehicle for legal transplantation?',
    'Comparative legal analysis of NSL application against international human rights standards and common law precedents; empirical study of legal outcomes and judicial independence over time.',
    'If primarily a security instrument (sovereignty_restoration_reading), extractiveness and suppression might be re-evaluated as legitimate costs of statecraft. If primarily for political control (democratic_enclosure_reading), the focus shifts to the criminalization of dissent. This ''jurisdictional capture'' reading emphasizes the structural erosion of legal autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nsl_kernel_reading_identity, conceptual, 'This constraint is one reading of the ''nsl_legal_text'' kernel, emphasizing jurisdictional capture.').

omega_variable(
    common_law_erosion_irreversibility,
    'To what extent is the erosion of Hong Kong''s common law autonomy under the NSL irreversible, or could it be restored if political conditions changed?',
    'Analysis of legal precedents set by NSL cases, the extent of mainland legal interpretation integration, and the capacity of the Hong Kong legal system to revert to prior norms.',
    'If irreversible, the constraint''s long-term extractiveness and suppression are higher, as the structural changes are permanent. If reversible, the constraint might be re-evaluated as a temporary (albeit severe) Snare, rather than a fundamental re-ordering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_erosion_irreversibility, empirical, 'Assessing the permanence of common law erosion in Hong Kong.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2, 0.4).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 3, 0.43).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 4, 0.45).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 1, 0.65).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2, 0.7).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 3, 0.73).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 4, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 1, 0.75).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2, 0.8).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 3, 0.83).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 4, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, hong_kong_basic_law_interpretation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'nsl_legal_text' kernel. This 'jurisdictional capture' reading focuses on the erosion of common law autonomy and legal transplantation, distinct from the 'sovereignty restoration' (legitimate security instrument) and 'democratic enclosure' (criminalization of dissent) readings. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
