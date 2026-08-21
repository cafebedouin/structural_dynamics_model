% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Digital Money Origin: Regulatory Recognition Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'regulatory recognition' reading
 *   of the 'digital_money_origin' kernel. It posits that digital money truly
 *   'emerged' when monetary authorities formally incorporated it into
 *   statistical aggregates and regulatory frameworks. This framing emphasizes
 *   the role of state power and institutional definitions in shaping monetary
 *   reality, leading to a later origin date and a constraint set dominated by
 *   legal and regulatory barriers. The constraint is claimed as a Tangled
 *   Rope, reflecting a genuine coordination function (financial stability)
 *   intertwined with significant asymmetric extraction benefiting incumbent
 *   financial institutions and monetary authorities, at the expense of
 *   unregulated innovators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.7).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.8).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money Origin: Regulatory Recognition Reading").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '96fb5537-e2f0-4ef4-be55-52859a1d42b3').
narrative_ontology:cs_kernel_codification('96fb5537-e2f0-4ef4-be55-52859a1d42b3', formalized).
narrative_ontology:cs_authority_grounding('96fb5537-e2f0-4ef4-be55-52859a1d42b3', extraction).
narrative_ontology:cs_interpretation_layer_present('96fb5537-e2f0-4ef4-be55-52859a1d42b3').
narrative_ontology:cs_reading_relation('96fb5537-e2f0-4ef4-be55-52859a1d42b3', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('96fb5537-e2f0-4ef4-be55-52859a1d42b3', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('96fb5537-e2f0-4ef4-be55-52859a1d42b3', foundational, state_monopoly_on_money).
narrative_ontology:cs_axiom_status(state_monopoly_on_money, holdable).
narrative_ontology:cs_axiom_grounding('96fb5537-e2f0-4ef4-be55-52859a1d42b3', state_monopoly_on_money, conventional).
narrative_ontology:cs_axiom('96fb5537-e2f0-4ef4-be55-52859a1d42b3', foundational, financial_system_stability_mandate).
narrative_ontology:cs_axiom_status(financial_system_stability_mandate, holdable).
narrative_ontology:cs_axiom_grounding('96fb5537-e2f0-4ef4-be55-52859a1d42b3', financial_system_stability_mandate, instrumental).
narrative_ontology:cs_reference_frame('96fb5537-e2f0-4ef4-be55-52859a1d42b3', central_bank_monetary_control).
narrative_ontology:cs_drift_state('96fb5537-e2f0-4ef4-be55-52859a1d42b3', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96fb5537-e2f0-4ef4-be55-52859a1d42b3', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, digital_asset_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (central banks, financial regulators) define what counts as official money, incorporate it into statistical aggregates, and establish regulatory frameworks. They benefit from maintaining control over the monetary system and ensuring financial stability.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Traditional banks and financial service providers benefit from the clarity and legitimacy provided by regulatory recognition. They are often the first to integrate new forms of digital money once formally recognized, gaining a competitive advantage over unregulated entities.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, constrained, global).

% Developers and companies creating digital assets or payment systems outside established regulatory frameworks face significant barriers, legal uncertainty, and exclusion from mainstream financial infrastructure. They bear the costs of non-recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, global).

% Individuals using digital assets not formally recognized by monetary authorities may face higher risks, limited interoperability with traditional finance, and potential legal repercussions. They pay the cost of operating outside the recognized system.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, digital_asset_users, payer,
    powerless, immediate, constrained, global).

% Academics, policy researchers, and independent analysts who study the evolution of money and financial systems. They observe the effects of regulatory recognition on innovation, stability, and economic inclusion without directly participating in the constraint's operation.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, trusted, and legally recognized framework for what constitutes official digital money, ensuring financial stability, preventing illicit use, and integrating new forms of value into existing economic and legal systems.
% TRANSFER_FUNCTION: Transfers legitimacy, market access, and state protection to recognized forms of digital money and their issuers, while imposing costs, barriers, and exclusion on unrecognized forms and their innovators.
% ABSENT_VOICES: Advocates for decentralized digital currencies, privacy advocates, and those who believe money should emerge from market practice rather than state decree are structurally excluded from the formal recognition process. They would argue for more open and permissionless monetary innovation.
% DISAPPEARANCE_RATIONALE: If regulatory recognition as the origin point for digital money vanished, the distinction between official digital money and other digital assets would collapse. This would lead to a chaotic reordering of financial markets, legal frameworks, and public trust in monetary instruments, as the state's role in defining money would be fundamentally undermined.
% FOUNDING_PROBLEM: To maintain state sovereignty over money, ensure financial stability, prevent illicit finance, and integrate new digital forms of value into existing economic and legal systems, particularly as new technologies challenged traditional definitions of money.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and incumbent financial institutions attest that the problem of maintaining financial stability and controlling illicit finance in a rapidly evolving digital landscape is an ongoing and live challenge. However, critics (e.g., some economists, tech advocates) argue that the problem is often reframed to justify control and extraction, rather than purely for stability. Independent economic analysis often highlights the rent-seeking aspects of such regulatory frameworks.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the significant costs imposed on innovators and users operating outside the recognized system, and the rents captured by incumbents. Suppression (0.8) is high due to active enforcement of regulatory barriers, licensing requirements, and exclusion from traditional financial rails. The moderate theater ratio (0.4) acknowledges that while genuine regulatory functions (e.g., anti-money laundering, financial stability) exist, a substantial portion of the activity serves to maintain the incumbents' privileged position and the authorities' control. Accessibility collapse (0.7) is high because while alternatives exist, they are heavily constrained by legal and practical barriers. Resistance (0.6) is also high, as unregulated innovators and digital asset users actively seek to circumvent or challenge these frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of monetary authorities and incumbent institutions, this constraint is a necessary mechanism for financial stability and legitimate monetary evolution. From the perspective of unregulated innovators and digital asset users, it is a barrier to entry and a mechanism for rent extraction, stifling innovation and limiting access to new forms of value.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities are the primary agenda-setters and beneficiaries, as they define and control the monetary system. Incumbent financial institutions are also beneficiaries, gaining legitimacy and market access from regulatory recognition. Unregulated innovators and digital asset users are the primary payers/victims, bearing the costs of exclusion, compliance, or operating in a less legitimate space. Analytical observers provide an external perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_date_ambiguity,
    'Is the ''true'' origin of digital money primarily a matter of regulatory definition, or does it precede formal recognition in conceptual or practical terms?',
    'Historical analysis of technological development and social adoption patterns, independent of regulatory pronouncements. Comparative legal analysis of different jurisdictions'' approaches to digital assets.',
    'If earlier origins (conceptual or practical) are deemed more fundamental, this reading''s claim of a ''latest origin date'' would be challenged, potentially reclassifying the constraint as a Snare that suppresses pre-existing forms rather than defining emergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_date_ambiguity, conceptual, 'Ambiguity regarding the primary determinant of digital money''s origin.').

omega_variable(
    regulatory_stability_vs_rent_seeking,
    'What proportion of the observed extraction is genuinely necessary for financial stability and consumer protection, versus rent-seeking by incumbent institutions and authorities?',
    'Independent economic modeling of the costs of regulatory compliance and the benefits of stability, compared against the profits generated by recognized entities and the barriers faced by unrecognized ones. Analysis of regulatory arbitrage opportunities.',
    'A higher proportion of rent-seeking would strengthen the Snare-like aspects of the constraint, indicating that the coordination story is largely cover for extraction. A higher proportion of genuine stability costs would reinforce the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_stability_vs_rent_seeking, empirical, 'Balance between genuine regulatory function and extractive practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(digi_tr_t2005, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(digi_tr_t2015, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(digi_tr_t2025, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(digi_be_t2005, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(digi_be_t2015, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(digi_be_t2025, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(digi_su_t2005, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(digi_su_t2015, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(digi_su_t2025, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel, each representing a different structural claim about when digital money 'emerged'. This reading focuses on formal regulatory recognition, while siblings focus on conceptualization and practical adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
