% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Sovereign Restoration
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty restoration' reading
 *   of the National Security Law (NSL) enacted in 2020. From this
 *   perspective, the NSL is a legitimate and necessary instrument for the
 *   central government to restore constitutional order and national security
 *   after the widespread unrest of 2019. It is framed as a response to
 *   genuine threats to state integrity and stability, rather than as a tool
 *   for political repression or jurisdictional capture. The claimed type is
 *   'rope' because, from this reading's viewpoint, it solves a critical
 *   collective action problem (national security) for the benefit of the
 *   state and loyal citizens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.45).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.7).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Sovereign Restoration").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'f83847d4-3ca1-4ca0-8103-1d49c10517de').
narrative_ontology:cs_kernel_codification('f83847d4-3ca1-4ca0-8103-1d49c10517de', fixed_text).
narrative_ontology:cs_authority_grounding('f83847d4-3ca1-4ca0-8103-1d49c10517de', lineage).
narrative_ontology:cs_interpretation_layer_present('f83847d4-3ca1-4ca0-8103-1d49c10517de').
narrative_ontology:cs_reading_relation('f83847d4-3ca1-4ca0-8103-1d49c10517de', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('f83847d4-3ca1-4ca0-8103-1d49c10517de', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('f83847d4-3ca1-4ca0-8103-1d49c10517de', foundational, national_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f83847d4-3ca1-4ca0-8103-1d49c10517de', national_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('f83847d4-3ca1-4ca0-8103-1d49c10517de', foundational, constitutional_order_requires_security).
narrative_ontology:cs_axiom_status(constitutional_order_requires_security, holdable).
narrative_ontology:cs_axiom_grounding('f83847d4-3ca1-4ca0-8103-1d49c10517de', constitutional_order_requires_security, conventional).
narrative_ontology:cs_reference_frame('f83847d4-3ca1-4ca0-8103-1d49c10517de', pre_2019_constitutional_order).
narrative_ontology:cs_drift_state('f83847d4-3ca1-4ca0-8103-1d49c10517de', post_nsl_enactment, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f83847d4-3ca1-4ca0-8103-1d49c10517de', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, loyalist_population).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_opposition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, mainland_legal_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the National Security Law, viewing it as a necessary instrument to restore order and assert sovereign authority after periods of unrest. Benefits from increased control and stability.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Perceives the NSL as restoring stability, public order, and national dignity. Benefits from a perceived return to normalcy and security, and the suppression of perceived threats to the state.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, loyalist_population, beneficiary,
    moderate, biographical, constrained, national).

% Are directly targeted by the NSL's provisions, facing arrest, prosecution, and severe penalties for activities previously considered legitimate protest or dissent. Their ability to organize and express opposition is severely curtailed.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Faces criminalization of various forms of political activity, leading to self-censorship, dissolution of organizations, and reduced participation in elections. The scope for legitimate political challenge is narrowed.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, political_opposition, payer,
    organized, biographical, constrained, national).

% Monitor the implementation and impact of the NSL, assessing its compliance with international human rights standards and its effects on civil liberties and political autonomy. Their analysis is external to the constraint's direct operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_observers, observer,
    analytical, civilizational, analytical, global).

% Benefits from the NSL's alignment with its principles of national security and state authority, extending its influence and legal philosophy into the local jurisdiction. It provides a framework for direct intervention in national security cases.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, mainland_legal_system, beneficiary,
    institutional, civilizational, arbitrage, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To restore public order and national security by criminalizing acts of secession, subversion, terrorism, and collusion with foreign forces, thereby stabilizing governance and asserting sovereign control.
% TRANSFER_FUNCTION: Transfers authority and control over national security matters from local institutions to central government bodies, and transfers the risk of political instability from the state to individuals engaging in proscribed activities.
% ABSENT_VOICES: International human rights organizations, independent legal scholars, and exiled pro-democracy figures would object, arguing for the protection of civil liberties, due process, and the right to peaceful assembly and expression. They are excluded from the official discourse on the NSL's legitimacy.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the central government would lose a key instrument for asserting sovereignty and maintaining order, potentially leading to renewed unrest, a resurgence of political opposition, or the implementation of alternative, less formalized, security measures. The political landscape would significantly reorganize.
% FOUNDING_PROBLEM: Widespread and prolonged anti-government protests in 2019, perceived by the central government as threatening national sovereignty, constitutional order, and public safety.
% FOUNDING_PROBLEM_CORROBORATION: The central government and state media consistently attest that the founding problem of national security threats remains live. This is corroborated by statements from loyalist political figures and some segments of the local population. However, international bodies and opposition groups dispute this assessment, arguing the problem has been exaggerated or resolved through other means.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).
:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.45) because, from this reading, the law primarily targets specific political opposition and activities deemed a threat to national security, not the general population. Suppression is high (0.70) due to the broad powers granted to security forces and the severe penalties for proscribed acts, which are seen as necessary to deter threats. Theater ratio is low (0.10) as this reading views the NSL's enforcement as genuinely functional in achieving its stated security objectives, with minimal performative aspects. Accessibility collapse is moderate (0.60) as political alternatives are significantly curtailed, and resistance is moderate (0.50) reflecting ongoing, albeit suppressed, opposition.
 *
 * PERSPECTIVAL GAP:
 *   This story explicitly adopts the 'sovereignty restoration' perspective. Other readings, such as 'democratic enclosure' or 'jurisdictional capture,' would assign significantly different metric values and classifications, particularly for extractiveness and suppression, and identify different beneficiaries and victims. This divergence is precisely what the kernel framework is designed to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   The central government authorities and loyalist population are beneficiaries, as they gain perceived stability and security. Pro-democracy activists and the political opposition are victims, bearing the direct costs of criminalization and curtailed freedoms. The mainland legal system is an indirect beneficiary, as its principles are reinforced. International observers are analytical seats, assessing the constraint's impact without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_security_vs_political_repression,
    'Is the NSL primarily a genuine security instrument to restore order, or is its primary function the repression of political dissent?',
    'Empirical analysis of enforcement patterns: if enforcement disproportionately targets non-violent political expression rather than acts of violence or foreign collusion, it supports the repression hypothesis. Conversely, if it primarily targets violent acts or clear foreign interference, it supports the security instrument hypothesis.',
    'If primarily political repression, the constraint''s effective extractiveness and suppression are higher, and its coordination function is largely cover, shifting its classification towards a Snare or Tangled Rope. If genuine security, the Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_security_vs_political_repression, empirical, 'Ambiguity regarding the NSL''s true operational purpose.').

omega_variable(
    sovereign_assertion_vs_legal_transplantation,
    'Does the NSL genuinely restore sovereign constitutional order, or does it serve as a vehicle for the transplantation of the mainland legal system, eroding local common law autonomy?',
    'Legal analysis of judicial precedents and interpretive practices: if local courts increasingly adopt mainland legal principles and procedures in NSL cases, it supports the transplantation hypothesis. If local common law principles are robustly maintained, it supports the sovereign assertion hypothesis.',
    'If legal transplantation is dominant, the constraint''s impact on local legal institutions is more severe, and the ''sovereignty restoration'' claim is conceptually undermined, potentially shifting the classification towards a Snare for local legal professionals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_assertion_vs_legal_transplantation, conceptual, 'Ambiguity regarding the NSL''s impact on local legal autonomy and its alignment with the ''sovereignty restoration'' claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2024, 0.1).
narrative_ontology:measurement(nsl__tr_t2025, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2022, 0.44).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement(nsl__be_t2025, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement(nsl__su_t2025, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, local_common_law_autonomy).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, freedom_of_expression_local_jurisdiction).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'nsl_legal_text' kernel, alongside 'democratic_enclosure_reading' and 'jurisdictional_capture_reading'. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
