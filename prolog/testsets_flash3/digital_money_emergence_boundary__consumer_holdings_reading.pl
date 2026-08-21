% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Digital Money Emergence Boundary (Consumer Holdings Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money through the lens
 *   of consumer direct holdings and transacting with digital instruments,
 *   exemplified by 1990s e-purses and the 2000 Electronic Money Directive
 *   (EMD). It posits that 'digital money' truly exists when it can be held
 *   outside traditional bank accounts, necessitating distinctions like M4/M5
 *   for monetary aggregates. This reading benefits regulatory bodies by
 *   providing clear categories and fintech firms by validating their e-money
 *   products. The claimed type is 'rope' because it facilitates coordination
 *   in a new financial landscape, with relatively low extraction and
 *   suppression, as it primarily clarifies a boundary rather than imposing
 *   heavy costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.15).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.2).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Emergence Boundary (Consumer Holdings Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '1363588b-8e45-4428-acba-aa8e9aab3ded').
narrative_ontology:cs_kernel_codification('1363588b-8e45-4428-acba-aa8e9aab3ded', formalized).
narrative_ontology:cs_authority_grounding('1363588b-8e45-4428-acba-aa8e9aab3ded', lineage).
narrative_ontology:cs_interpretation_layer_present('1363588b-8e45-4428-acba-aa8e9aab3ded').
narrative_ontology:cs_reading_relation('1363588b-8e45-4428-acba-aa8e9aab3ded', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('1363588b-8e45-4428-acba-aa8e9aab3ded', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_axiom('1363588b-8e45-4428-acba-aa8e9aab3ded', foundational, money_requires_direct_consumer_agency).
narrative_ontology:cs_axiom_status(money_requires_direct_consumer_agency, holdable).
narrative_ontology:cs_axiom_grounding('1363588b-8e45-4428-acba-aa8e9aab3ded', money_requires_direct_consumer_agency, conventional).
narrative_ontology:cs_axiom('1363588b-8e45-4428-acba-aa8e9aab3ded', secondary, distinction_from_bank_deposits_is_key).
narrative_ontology:cs_axiom_status(distinction_from_bank_deposits_is_key, holdable).
narrative_ontology:cs_axiom_grounding('1363588b-8e45-4428-acba-aa8e9aab3ded', distinction_from_bank_deposits_is_key, conventional).
narrative_ontology:cs_reference_frame('1363588b-8e45-4428-acba-aa8e9aab3ded', post_emd_regulatory_clarity).
narrative_ontology:cs_drift_state('1363588b-8e45-4428-acba-aa8e9aab3ded', contemporary_cbdc_discourse, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('1363588b-8e45-4428-acba-aa8e9aab3ded', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define legal categories for digital money (e.g., Electronic Money Institutions in Europe) and oversee its issuance. This reading provides a clear boundary for their regulatory scope, distinguishing e-money from traditional bank deposits.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Issue e-money products that allow consumers to hold and transact digital value outside traditional bank accounts. This reading validates their products as 'digital money' and provides a framework for their operation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms, beneficiary,
    organized, biographical, mobile, global).

% Benefit from new ways to hold and transact digital value, offering convenience and sometimes lower fees compared to traditional banking. Their ability to directly hold digital instruments is central to this reading's definition of digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, beneficiary,
    moderate, immediate, mobile, local).

% Face competition from fintech firms offering e-money products. This reading, by defining digital money outside their traditional deposit-taking function, necessitates adjustments to their business models and regulatory compliance.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, payer,
    institutional, generational, constrained, national).

% Analyze the evolution of money and its definitions. This reading provides a specific historical and functional boundary for the emergence of digital money, influencing their conceptual frameworks and classifications (e.g., M4/M5 distinctions).
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, functional definition for 'digital money' that distinguishes it from other forms of electronic value, enabling consistent regulation, product development, and economic analysis around consumer-held digital instruments.
% TRANSFER_FUNCTION: Facilitates the transfer of digital value directly between consumers or between consumers and merchants, bypassing traditional bank accounts, and enables regulatory oversight of these new financial flows.
% ABSENT_VOICES: Advocates for broader definitions of digital money (e.g., including all electronic transfers or purely conceptual forms) might argue this reading is too narrow, but they are not structurally excluded from the discourse, merely offering alternative framings.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the regulatory and product landscape for e-money would become ambiguous. Fintech firms would lose a clear legal basis for their offerings, and central banks would struggle to categorize and oversee new forms of digital value, leading to significant re-evaluation and re-structuring in financial markets.
% FOUNDING_PROBLEM: The problem of defining and regulating new forms of electronic value that allowed direct consumer holdings, distinct from traditional bank deposits, which emerged with technologies like e-purses and electronic money directives.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and fintech firms attest that the problem of clearly defining and managing consumer-held digital instruments remains live, especially with the continuous innovation in digital payments. Monetary theorists corroborate the ongoing need for clear distinctions in monetary aggregates (e.g., M4/M5) to accurately reflect the money supply.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).
:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading primarily provides a definitional framework, which, while benefiting certain actors, does not inherently extract value in an asymmetric way. Suppression is also low (0.2) as it's a conceptual boundary that, once understood, largely self-enforces through legal and market clarity rather than active coercion. Theater ratio is minimal (0.05) as the definition serves a genuine functional purpose in distinguishing new forms of money. Accessibility collapse is high (0.8) because once this definition is adopted, alternative understandings of 'digital money' in this specific context become less viable for practical application. Resistance is low (0.1) because the clarity it provides is generally beneficial for market participants and regulators.
 *
 * PERSPECTIVAL GAP:
 *   Monetary theorists might hold different conceptualizations of digital money's emergence (e.g., focusing on infrastructure or pure theory), leading to different classifications. However, this reading focuses on the practical, consumer-facing aspect, which is distinct but not necessarily contradictory to other perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech firms are beneficiaries, as this reading provides a clear framework for their operations and products. Consumers also benefit from the clarity and new financial instruments. Traditional banks might be considered payers as they face new competition and regulatory adjustments, but the overall structure is coordinative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_functional_boundary,
    'Is the emergence of digital money primarily a conceptual breakthrough, an infrastructural development, or a functional shift in consumer holdings?',
    'Historical analysis of policy adoption and market behavior: if regulatory frameworks and product innovations consistently align with consumer-held instruments, this reading is strengthened.',
    'If the conceptual or infrastructural readings gain dominance, the classification of this constraint might shift to reflect a different primary coordination function or beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_functional_boundary, conceptual, 'Ambiguity in the primary driver of digital money''s emergence.').

omega_variable(
    regulatory_capture_potential,
    'Does the definition of digital money based on consumer holdings disproportionately benefit incumbent fintech firms by creating barriers to entry for alternative models?',
    'Market analysis of new entrants and competitive dynamics in the e-money sector following regulatory definitions. If new models struggle to gain traction despite innovation, it suggests potential capture.',
    'If evidence of regulatory capture emerges, the extractiveness and suppression metrics for this constraint would need to be re-evaluated upwards, potentially reclassifying it as a Tangled Rope or Snare for excluded innovators.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_potential, empirical, 'Potential for the definition to become an extractive mechanism for incumbents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.02).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(digi_tr_t2010, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(digi_tr_t2024, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(digi_be_t2010, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(digi_be_t2024, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(digi_su_t2010, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(digi_su_t2024, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'digital_money_emergence_boundary' kernel, focusing on consumer holdings. It is linked to the 'conceptualization_reading' and 'infrastructure_reading' as part of a constraint family that explores different historical and functional boundaries of digital money's emergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
