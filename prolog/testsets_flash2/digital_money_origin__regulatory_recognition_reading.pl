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
 *   'emerged' when it was formally incorporated into official statistical
 *   aggregates and regulatory frameworks by monetary authorities. This
 *   reading emphasizes the institutional and legal aspects of money's
 *   definition, rather than its technical possibility or individual adoption.
 *   The constraint operates as a Tangled Rope, providing coordination for
 *   incumbent institutions while extracting from and suppressing unregulated
 *   innovators.
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
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9').
narrative_ontology:cs_kernel_codification('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', formalized).
narrative_ontology:cs_authority_grounding('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', lineage).
narrative_ontology:cs_interpretation_layer_present('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9').
narrative_ontology:cs_reading_relation('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', foundational, money_is_a_legal_construct).
narrative_ontology:cs_axiom_status(money_is_a_legal_construct, holdable).
narrative_ontology:cs_axiom_grounding('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', money_is_a_legal_construct, conventional).
narrative_ontology:cs_axiom('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', foundational, monetary_authority_defines_legitimacy).
narrative_ontology:cs_axiom_status(monetary_authority_defines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', monetary_authority_defines_legitimacy, conventional).
narrative_ontology:cs_reference_frame('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', post_bretton_woods_monetary_order).
narrative_ontology:cs_drift_state('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', contemporary_crypto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2d7b82bd-b3e6-40cd-a8f5-cbd02b1ab0a9', '').
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

% Benefit from the regulatory clarity and legitimacy conferred by formal recognition. This framework often aligns with their existing operational models and provides a barrier to entry for disruptive, unregulated competitors. They participate in shaping the regulatory landscape.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, generational, mobile, global).

% Bear the costs of non-recognition or the burden of compliance if they seek to integrate into the formal system. Their innovations are often deemed 'not money' or 'risky' until they conform to established regulatory definitions, limiting their market access and growth.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, global).

% Individuals who use non-recognized digital instruments for transactions. They face legal uncertainty, lack of consumer protection, and difficulty integrating with the mainstream financial system, making their 'money' less liquid and more risky.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, alternative_currency_users, payer,
    powerless, immediate, identity_locked, global).

% Analyze the historical evolution of monetary forms and the role of institutional definitions. They observe the contest between different 'origin' narratives and their implications for economic theory and policy.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, legally recognized definition of 'money' that allows for consistent statistical measurement, regulatory oversight, and integration into the existing financial system, reducing ambiguity for large-scale economic activity.
% TRANSFER_FUNCTION: Transfers legitimacy, market access, and regulatory protection to formally recognized digital monetary instruments and their issuers, while imposing costs and barriers on non-recognized or non-compliant forms.
% ABSENT_VOICES: Early digital currency pioneers and advocates for decentralized, permissionless monetary systems are largely excluded from the formal recognition process; they would argue for a more inclusive definition of money based on functional use rather than institutional decree.
% DISAPPEARANCE_RATIONALE: If formal regulatory recognition of digital money vanished, the existing digital financial system would face immense uncertainty. Incumbent institutions would lose their legal basis for operation, statistical aggregates would become meaningless, and the distinction between 'money' and other digital assets would collapse, leading to a chaotic reorganization of financial markets.
% FOUNDING_PROBLEM: The proliferation of digital assets and payment systems created ambiguity about what constitutes 'money' in the digital age, posing challenges for monetary policy, financial stability, and consumer protection.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and incumbent financial institutions attest that the problem of defining and regulating digital money remains live, citing ongoing innovation and the need for stability. Academic economists and legal scholars, from outside the directly benefiting parties, corroborate the ongoing challenge of integrating novel digital forms into established monetary frameworks.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the regulatory framework imposes significant costs on non-compliant digital money forms, effectively channeling value towards recognized entities. Suppression is also high, as active enforcement (regulation, legal barriers) is required to maintain the distinction between 'official' and 'unofficial' digital money. The theater ratio is low, as the regulatory function is genuinely active and not merely performative. The increasing extractiveness and suppression over time reflect the hardening of regulatory boundaries as digital innovation accelerated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of monetary authorities and incumbent institutions, this constraint is a necessary coordination mechanism for financial stability and integrity. From the perspective of unregulated innovators, it is an extractive and suppressive barrier designed to protect incumbents. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and incumbent financial institutions are clear beneficiaries, as the constraint legitimizes their operations and creates barriers to entry for competitors. Unregulated innovators and alternative currency users are victims, facing exclusion or high compliance costs. The constraint subsidizes the established order by defining the terms of legitimate participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (defining and regulating money for stability) is still live, but its application has shifted to actively managing the threat of new digital forms. The classification as Tangled Rope prevents mislabeling it as pure coordination (Rope) by highlighting the asymmetric extraction and active suppression of alternatives, or as a pure Snare by acknowledging the genuine coordination function for the formal system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_money_ambiguity,
    'Is ''money'' fundamentally a social convention, a technical artifact, or a legal construct?',
    'A shift in global consensus among economists, legal scholars, and policymakers towards one dominant definition, or a clear empirical demonstration that one definition consistently predicts monetary phenomena better than others.',
    'If money is primarily a technical artifact or social convention, this ''regulatory recognition'' reading would be reclassified as a Snare, as its coordination function would be revealed as a cover for institutional control. If it is primarily a legal construct, this reading''s classification as Tangled Rope would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_money_ambiguity, conceptual, 'Ambiguity in the foundational definition of ''money'' itself.').

omega_variable(
    regulatory_capture_extent,
    'To what extent does the regulatory framework for digital money primarily serve the public interest (stability, consumer protection) versus the private interests of incumbent financial institutions (barrier to entry, rent-seeking)?',
    'Independent audits of regulatory lobbying, analysis of regulatory outcomes on competition and innovation, and comparison with jurisdictions with different regulatory approaches.',
    'Higher evidence of regulatory capture would increase the measured extractiveness and suppression, potentially shifting the classification closer to a Snare. Lower evidence would reinforce the coordination aspect, moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'The degree to which regulation is influenced by incumbent interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(digi_tr_t2015, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(digi_tr_t2024, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(digi_be_t2015, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(digi_be_t2024, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(digi_su_t2015, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(digi_su_t2024, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, financial_innovation_regulation).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, central_bank_digital_currency_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel, each representing a different perspective on when digital money truly 'emerged'. This reading emphasizes formal institutional recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
