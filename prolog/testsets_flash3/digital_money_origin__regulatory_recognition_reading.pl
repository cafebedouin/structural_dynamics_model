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
 *   'emerged' when monetary authorities formally acknowledged and integrated
 *   it into their statistical and regulatory frameworks. This reading
 *   emphasizes the institutional and legal aspects of money's definition,
 *   rather than its technical possibility or initial adoption by individuals.
 *   The constraint operates as a Tangled Rope, coordinating the financial
 *   system around recognized forms of digital money while extracting from and
 *   suppressing non-recognized innovations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.65).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.78).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money Origin: Regulatory Recognition Reading").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '848984e3-98fe-455d-bd73-aee0b59fdc22').
narrative_ontology:cs_kernel_codification('848984e3-98fe-455d-bd73-aee0b59fdc22', formalized).
narrative_ontology:cs_authority_grounding('848984e3-98fe-455d-bd73-aee0b59fdc22', lineage).
narrative_ontology:cs_interpretation_layer_present('848984e3-98fe-455d-bd73-aee0b59fdc22').
narrative_ontology:cs_reading_relation('848984e3-98fe-455d-bd73-aee0b59fdc22', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('848984e3-98fe-455d-bd73-aee0b59fdc22', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('848984e3-98fe-455d-bd73-aee0b59fdc22', foundational, money_is_what_the_state_recognizes).
narrative_ontology:cs_axiom_status(money_is_what_the_state_recognizes, holdable).
narrative_ontology:cs_axiom_grounding('848984e3-98fe-455d-bd73-aee0b59fdc22', money_is_what_the_state_recognizes, conventional).
narrative_ontology:cs_axiom('848984e3-98fe-455d-bd73-aee0b59fdc22', secondary, financial_stability_requires_centralized_definition).
narrative_ontology:cs_axiom_status(financial_stability_requires_centralized_definition, holdable).
narrative_ontology:cs_axiom_grounding('848984e3-98fe-455d-bd73-aee0b59fdc22', financial_stability_requires_centralized_definition, instrumental).
narrative_ontology:cs_reference_frame('848984e3-98fe-455d-bd73-aee0b59fdc22', state_centric_monetary_definition).
narrative_ontology:cs_drift_state('848984e3-98fe-455d-bd73-aee0b59fdc22', contemporary_crypto_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('848984e3-98fe-455d-bd73-aee0b59fdc22', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, monetary_authorities).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, alternative_currency_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define what counts as 'money' for statistical and regulatory purposes. Their formal recognition confers legitimacy and imposes a framework that benefits established players while constraining new entrants. They actively enforce these definitions through regulation and policy.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the regulatory clarity and legitimacy conferred by formal recognition. This framework often aligns with their existing operational models, creating barriers to entry for disruptive innovations that fall outside the recognized definitions.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, mobile, global).

% Bear the costs of non-recognition, facing legal uncertainty, exclusion from mainstream financial systems, and difficulty scaling. Their innovations are not considered 'money' until formally incorporated, limiting their reach and legitimacy.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, global).

% Use non-recognized digital monetary instruments, often for ideological or practical reasons. They face friction, limited acceptance, and potential legal risks due to the lack of formal regulatory status for their chosen medium of exchange.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, alternative_currency_users, payer,
    powerless, immediate, identity_locked, local).

% Analyze the historical evolution of monetary systems and the role of institutional definitions. They observe the impact of regulatory recognition on the development and adoption of new forms of money.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, legally recognized definition of 'digital money' for statistical aggregation, taxation, and financial stability, allowing for consistent policy and oversight.
% TRANSFER_FUNCTION: Transfers legitimacy, market access, and regulatory certainty to formally recognized digital monetary instruments and their issuers, while imposing costs and barriers on non-recognized forms.
% ABSENT_VOICES: Early digital currency pioneers and advocates for decentralized monetary systems are largely excluded from the formal recognition process; they would argue for a broader, more inclusive definition of money that does not depend on state sanction.
% DISAPPEARANCE_RATIONALE: If regulatory recognition as the origin point vanished, the narrative of digital money's emergence would shift to earlier technical or social innovations, fundamentally altering how its history and legitimacy are understood, and potentially empowering currently unregulated forms.
% FOUNDING_PROBLEM: The problem of defining and integrating novel digital instruments into existing monetary and regulatory frameworks to maintain financial stability and control.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and incumbent financial institutions attest that the problem of defining and regulating new digital forms of money is ongoing and critical for economic stability. Economic historians corroborate the historical challenge of integrating new monetary forms into existing systems.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the regulatory framework, while providing coordination, also creates significant barriers to entry and imposes compliance costs on innovators, effectively channeling value to incumbent, recognized entities. Suppression is also high, as the non-recognition of alternative digital monies is actively enforced through legal and financial exclusion. The theater ratio is low because the regulatory function is genuinely active and consequential, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of monetary authorities, this constraint is a necessary coordination mechanism for financial stability. From the perspective of unregulated innovators, it is an extractive and suppressive barrier to innovation and market access. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and incumbent financial institutions are clear beneficiaries, as the constraint solidifies their control and market position. Unregulated innovators and alternative currency users are victims, facing significant hurdles due to the lack of formal recognition. Economic historians act as observers, analyzing the structural impact of this definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_definition_ambiguity,
    'Is the ''origin'' of digital money best defined by technical possibility, individual adoption, or formal institutional recognition?',
    'Conceptual analysis and historical consensus among economic historians and technologists, acknowledging the multi-faceted nature of ''emergence''.',
    'If technical possibility or individual adoption are prioritized, the ''origin'' date shifts earlier, and the constraint''s classification might change to reflect a more ''natural'' or ''social'' emergence, rather than a regulatory one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_definition_ambiguity, conceptual, 'Ambiguity in defining the ''origin'' of digital money.').

omega_variable(
    regulatory_capture_extent,
    'To what extent does the regulatory recognition framework primarily serve the interests of incumbent financial institutions rather than genuine public good?',
    'Empirical studies on lobbying efforts, regulatory revolving doors, and comparative analysis of regulatory outcomes in jurisdictions with different institutional structures.',
    'Higher evidence of regulatory capture would increase the perceived extractiveness and suppression, potentially reclassifying the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'The degree to which regulatory recognition is influenced by incumbent interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(digi_tr_t2024, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(digi_be_t2024, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(digi_su_t2024, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel. This 'regulatory recognition' reading emphasizes institutional definition, while 'became_thinkable_reading' focuses on conceptual/technical possibility and 'first_held_reading' on individual adoption. Each reading yields a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
