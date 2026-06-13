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
 *   This constraint story examines the 'regulatory recognition' reading of
 *   digital money's origin: digital money is defined as having emerged when
 *   monetary authorities formally incorporated it into statistical aggregates
 *   and regulatory frameworks. This perspective emphasizes the institutional
 *   and legal construction of money, rather than its technical possibility or
 *   individual adoption. The constraint operates as a Tangled Rope, providing
 *   coordination for recognized entities while extracting from and
 *   suppressing unregulated innovators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.65).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.75).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money Origin: Regulatory Recognition Reading").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, 'a4c620f3-3536-4165-b8e3-22e214ec534b').
narrative_ontology:cs_kernel_codification('a4c620f3-3536-4165-b8e3-22e214ec534b', formalized).
narrative_ontology:cs_authority_grounding('a4c620f3-3536-4165-b8e3-22e214ec534b', lineage).
narrative_ontology:cs_interpretation_layer_present('a4c620f3-3536-4165-b8e3-22e214ec534b').
narrative_ontology:cs_reading_relation('a4c620f3-3536-4165-b8e3-22e214ec534b', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4c620f3-3536-4165-b8e3-22e214ec534b', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('a4c620f3-3536-4165-b8e3-22e214ec534b', foundational, money_is_a_creature_of_the_state).
narrative_ontology:cs_axiom_status(money_is_a_creature_of_the_state, holdable).
narrative_ontology:cs_axiom_grounding('a4c620f3-3536-4165-b8e3-22e214ec534b', money_is_a_creature_of_the_state, conventional).
narrative_ontology:cs_reference_frame('a4c620f3-3536-4165-b8e3-22e214ec534b', state_monopoly_on_money).
narrative_ontology:cs_drift_state('a4c620f3-3536-4165-b8e3-22e214ec534b', contemporary_crypto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a4c620f3-3536-4165-b8e3-22e214ec534b', '').
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

% Define what counts as 'money' for statistical and regulatory purposes, thereby legitimizing or delegitimizing new forms of value. They benefit from maintaining control over the monetary system and the stability it provides.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate within the established regulatory framework, benefiting from the exclusion of unregulated competitors and the stability provided by central bank oversight. They adapt to new digital forms as long as they remain within the recognized system.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, mobile, global).

% Develop new digital forms of value that may or may not fit existing regulatory definitions. They bear the cost of non-recognition, including lack of legal tender status, exclusion from mainstream financial systems, and potential regulatory crackdowns.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, global).

% Utilize digital forms of value that are not recognized by monetary authorities. They face friction, limited acceptance, and potential legal risks, but may be committed to these forms for ideological or privacy reasons.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, alternative_currency_users, payer,
    powerless, immediate, identity_locked, local).

% Observe and measure monetary aggregates, providing the data that informs regulatory decisions. Their definitions of 'money' are influenced by, and in turn influence, the regulatory recognition process.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, economic_statisticians, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, legally recognized definition of 'money' that allows for consistent economic measurement, taxation, and financial regulation, ensuring stability and trust in the national currency.
% TRANSFER_FUNCTION: Transfers legitimacy and operational scope from unregulated digital value forms to those formally recognized by authorities, thereby channeling economic activity and associated rents towards incumbent financial institutions and the state.
% ABSENT_VOICES: Advocates for 'free banking' or fully decentralized, unregulated digital currencies are excluded from the formal recognition process; they would argue that money's origin is a spontaneous market phenomenon, not a regulatory decree.
% DISAPPEARANCE_RATIONALE: If regulatory recognition as the origin of digital money vanished, the concept of 'money' would become highly fragmented, leading to widespread uncertainty in financial markets, challenges to central bank authority, and a proliferation of competing, unrecognized digital assets. The entire financial system would need to redefine its operating principles.
% FOUNDING_PROBLEM: The proliferation of novel digital value forms created ambiguity regarding their monetary status, posing challenges for economic measurement, financial stability, and regulatory oversight.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and incumbent financial institutions attest that the problem of defining and controlling money in a digital age is ongoing. Independent economists and financial historians corroborate that regulatory clarity is essential for modern monetary systems, even if they dispute the specific mechanisms of control.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).

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
 *   The extractiveness (0.65) stems from the rents captured by incumbent financial institutions and the state through their control over the definition and regulation of money. Suppression (0.75) is high due to active enforcement against non-compliant digital assets and the legal barriers to entry for unregulated innovators. The theater ratio (0.20) is relatively low, as the regulatory function is genuinely active, though it increasingly serves to maintain existing power structures. Accessibility collapse (0.60) reflects that while alternatives exist, their utility and reach are significantly curtailed by non-recognition. Resistance (0.45) comes from innovators and alternative currency communities pushing back against regulatory control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of monetary authorities, this constraint is a necessary coordination mechanism for financial stability. From the perspective of unregulated innovators, it is an extractive barrier to entry that stifles innovation and entrenches incumbents. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and incumbent financial institutions are clear beneficiaries and agenda-setters, shaping the rules to their advantage (low directionality). Unregulated innovators and alternative currency users are targets, bearing the costs of non-recognition and exclusion (high directionality). Economic statisticians act as observers, their work both reflecting and influencing the regulatory framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_definition_ambiguity,
    'Is the ''origin'' of digital money a technical, social, or legal event?',
    'Historical analysis of the impact of different ''origin'' definitions on policy and innovation trajectories. If different definitions lead to different policy outcomes, the choice of definition is consequential.',
    'If the origin is primarily technical or social, this ''regulatory recognition'' reading is a Snare, as its coordination function is a cover for extraction. If it is primarily legal, this reading is a more legitimate Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_definition_ambiguity, conceptual, 'Ambiguity in the fundamental definition of ''origin'' for digital money.').

omega_variable(
    regulatory_capture_extent,
    'To what extent does the regulatory recognition process primarily serve the interests of incumbent financial institutions rather than broader public good?',
    'Analysis of lobbying expenditures, revolving door appointments, and the differential impact of regulations on incumbents versus new entrants. If regulations disproportionately benefit incumbents without clear public good justification, regulatory capture is indicated.',
    'Higher regulatory capture would shift the constraint closer to a Snare, as the coordination function would be further revealed as a cover for private benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Degree of regulatory capture influencing the definition of digital money.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(digi_tr_t2024, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(digi_be_t2024, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(digi_su_t2024, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, central_bank_digital_currency_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel. This 'regulatory recognition' reading emphasizes the institutional construction of money, influencing and being influenced by other perspectives on its emergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
