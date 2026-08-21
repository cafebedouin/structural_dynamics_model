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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Digital Money Origin: Regulatory Recognition Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'regulatory recognition' reading
 *   of the 'digital_money_origin' kernel. From this perspective, digital
 *   money 'emerged' when monetary authorities formally acknowledged and
 *   integrated it into their statistical and regulatory frameworks. This
 *   reading emphasizes the institutional and legal aspects of money's
 *   definition, rather than its technical possibility or initial practical
 *   use. The constraint operates as a Snare, as the coordination story of
 *   stability and oversight often serves as a cover for extracting control
 *   and suppressing competition from unregulated innovators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.85).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.9).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, snare).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money Origin: Regulatory Recognition Reading").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, 'f622bb3d-ceed-4ee1-8140-725071c69d4f').
narrative_ontology:cs_kernel_codification('f622bb3d-ceed-4ee1-8140-725071c69d4f', formalized).
narrative_ontology:cs_authority_grounding('f622bb3d-ceed-4ee1-8140-725071c69d4f', extraction).
narrative_ontology:cs_interpretation_layer_present('f622bb3d-ceed-4ee1-8140-725071c69d4f').
narrative_ontology:cs_reading_relation('f622bb3d-ceed-4ee1-8140-725071c69d4f', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('f622bb3d-ceed-4ee1-8140-725071c69d4f', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('f622bb3d-ceed-4ee1-8140-725071c69d4f', foundational, monetary_authority_defines_money).
narrative_ontology:cs_axiom_status(monetary_authority_defines_money, holdable).
narrative_ontology:cs_axiom_grounding('f622bb3d-ceed-4ee1-8140-725071c69d4f', monetary_authority_defines_money, conventional).
narrative_ontology:cs_axiom('f622bb3d-ceed-4ee1-8140-725071c69d4f', foundational, formal_incorporation_confers_legitimacy).
narrative_ontology:cs_axiom_status(formal_incorporation_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f622bb3d-ceed-4ee1-8140-725071c69d4f', formal_incorporation_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('f622bb3d-ceed-4ee1-8140-725071c69d4f', central_bank_monetary_control).
narrative_ontology:cs_drift_state('f622bb3d-ceed-4ee1-8140-725071c69d4f', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f622bb3d-ceed-4ee1-8140-725071c69d4f', '').
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

% Define what constitutes 'money' within their jurisdiction, incorporate digital forms into statistical aggregates, and establish regulatory frameworks. They benefit from maintaining control over monetary policy and financial stability, and from the legitimacy conferred by formal recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, civilizational, analytical, global).

% Operate within the established regulatory frameworks, gaining legitimacy and market access for their digital offerings. They benefit from the barriers to entry created for unregulated competitors and from the stability provided by central oversight.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, constrained, global).

% Develop novel digital monetary instruments outside or at the fringes of existing regulatory frameworks. They face significant legal, operational, and market barriers to adoption, and their innovations are often suppressed or co-opted by the established system.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    powerless, immediate, constrained, global).

% Seek to use digital assets for various purposes, but their options are limited by regulatory recognition. They bear the costs of restricted access, higher compliance burdens for recognized assets, or the risks associated with unregulated alternatives.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, digital_asset_users, payer,
    moderate, biographical, constrained, global).

% Analyze the historical evolution of money and technology, observing how definitions and regulatory structures shape the emergence and adoption of new monetary forms. They provide an external, analytical perspective on the contest over 'digital money's origin'.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, regulated, and officially recognized definition of 'digital money', integrating it into existing financial systems and ensuring oversight for financial stability and anti-illicit finance measures.
% TRANSFER_FUNCTION: Transfers legitimacy, market access, and operational stability to digital monetary forms that comply with regulatory frameworks, while simultaneously transferring costs and suppression to unregulated or non-compliant innovations.
% ABSENT_VOICES: Proponents of fully decentralized, permissionless digital currencies and alternative monetary systems are largely excluded from the formal recognition process; they would argue for a more inclusive definition of 'money' based on technical properties or social adoption, rather than state sanction.
% DISAPPEARANCE_RATIONALE: If formal regulatory recognition as the definition of digital money's emergence vanished, the concept of 'digital money' would fragment into numerous competing technical and social definitions. The stability of existing digital financial instruments would be undermined, and the role of central banks in monetary policy would be severely challenged, leading to a significant reorganization of global finance.
% FOUNDING_PROBLEM: To maintain monetary sovereignty, financial stability, and control over illicit finance in the face of rapidly evolving digital technologies that could create new forms of money outside traditional state control.
% FOUNDING_PROBLEM_CORROBORATION: Central bank reports, international financial bodies (e.g., IMF, BIS), and academic economists consistently attest that the challenges of digital money to monetary sovereignty and financial stability remain live and are actively being addressed through ongoing regulatory development. This corroborates the authorities' framing of the problem from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant rents and control gained by incumbent financial institutions and monetary authorities through their gatekeeping role. Suppression (0.90) is very high due to the active legal and regulatory barriers that effectively collapse alternatives for unregulated digital money. The theater ratio (0.40) indicates that while genuine regulatory functions exist (e.g., anti-money laundering), a substantial portion of the activity is performative, aimed at maintaining the existing power structure rather than solely ensuring public good. Accessibility collapse (0.80) is high because formal recognition is a prerequisite for widespread adoption and legitimacy, making unregulated paths largely unviable. Resistance (0.60) is moderate, as innovators and users continuously challenge these frameworks, but face powerful institutional opposition. The increasing trends in extractiveness and suppression over time reflect the hardening of regulatory control as digital money became more prevalent.
 *
 * PERSPECTIVAL GAP:
 *   Monetary authorities and incumbent financial institutions perceive this constraint as a necessary Rope or Scaffold, providing stability and preventing chaos. Unregulated innovators and many digital asset users, however, experience it as a Snare, where the 'coordination' function is a pretext for rent-seeking and suppression of innovation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and incumbent financial institutions are clear beneficiaries, gaining control, legitimacy, and market share. Unregulated innovators are primary victims, facing exclusion and suppression. Digital asset users are also victims, albeit often indirectly, through limited choices and higher costs for compliant services. Economic historians serve as analytical observers, documenting the structural dynamics without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_emergence_ambiguity,
    'Does ''digital money'' truly emerge with formal regulatory recognition, or with its technical conceivability or first practical use?',
    'A consensus shift among economic historians and monetary theorists on the primary criterion for ''monetary emergence'' in a digital context, or a re-evaluation of historical precedents.',
    'If an alternative reading (e.g., ''became_thinkable_reading'' or ''first_held_reading'') were adopted as the primary definition, the origin date of digital money would shift significantly earlier, altering the historical narrative of its development and potentially re-framing the legitimacy of current regulatory structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_emergence_ambiguity, conceptual, 'Ambiguity regarding the definitional criteria for the ''emergence'' of digital money.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the legitimacy of digital money primarily derived from formal state/regulatory recognition, or from its technical properties and social adoption/network effects?',
    'Empirical observation of the long-term viability and widespread adoption of digital monetary systems that operate entirely outside state recognition, or a philosophical re-evaluation of the nature of money''s authority.',
    'If legitimacy is found to derive primarily from technical properties and social adoption, the extractiveness and suppression of this ''regulatory recognition'' constraint would be re-evaluated as less about genuine coordination and more about maintaining an artificial monopoly on monetary definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Uncertainty about the fundamental source of legitimacy for digital monetary instruments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(digi_tr_t1998, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement(digi_tr_t2006, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2006, 0.35).
narrative_ontology:measurement(digi_tr_t2014, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(digi_tr_t2024, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(digi_be_t1998, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(digi_be_t2006, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2006, 0.78).
narrative_ontology:measurement(digi_be_t2014, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2014, 0.82).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(digi_be_t2024, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(digi_su_t1998, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1998, 0.75).
narrative_ontology:measurement(digi_su_t2006, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2006, 0.83).
narrative_ontology:measurement(digi_su_t2014, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2014, 0.87).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(digi_su_t2024, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
