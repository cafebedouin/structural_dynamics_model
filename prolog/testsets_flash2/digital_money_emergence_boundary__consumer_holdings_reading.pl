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
    narrative_ontology:constraint_vindicates/2,
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
 *   of direct consumer holdings, specifically with the advent of e-purses in
 *   the 1990s and the E-Money Directive (EMD) around 2000. It posits that
 *   'money' in a digital context truly emerges when individuals can hold and
 *   transact with digital instruments outside the traditional bank account
 *   system. This reading is a 'rope' because it provides a clear, beneficial
 *   coordination function for regulators and fintech firms, with relatively
 *   low extraction and suppression, primarily focused on defining a new
 *   category rather than coercing behavior. The metrics reflect a gradual
 *   increase in extractiveness and suppression as the regulatory framework
 *   solidifies and new market participants emerge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.25).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.15).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Emergence Boundary (Consumer Holdings Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '09a020ef-40fd-4421-beeb-38283d82ccad').
narrative_ontology:cs_kernel_codification('09a020ef-40fd-4421-beeb-38283d82ccad', formalized).
narrative_ontology:cs_authority_grounding('09a020ef-40fd-4421-beeb-38283d82ccad', lineage).
narrative_ontology:cs_interpretation_layer_present('09a020ef-40fd-4421-beeb-38283d82ccad').
narrative_ontology:cs_reading_relation('09a020ef-40fd-4421-beeb-38283d82ccad', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('09a020ef-40fd-4421-beeb-38283d82ccad', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('09a020ef-40fd-4421-beeb-38283d82ccad', foundational, money_requires_direct_consumer_agency).
narrative_ontology:cs_axiom_status(money_requires_direct_consumer_agency, holdable).
narrative_ontology:cs_axiom_grounding('09a020ef-40fd-4421-beeb-38283d82ccad', money_requires_direct_consumer_agency, conventional).
narrative_ontology:cs_reference_frame('09a020ef-40fd-4421-beeb-38283d82ccad', e_money_directive_framework).
narrative_ontology:cs_drift_state('09a020ef-40fd-4421-beeb-38283d82ccad', contemporary_cbdc_discussions, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('09a020ef-40fd-4421-beeb-38283d82ccad', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, e_money_directive_framework).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, consumer_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the legal categories for digital money, distinguishing e-money from traditional bank deposits. They benefit from a clear regulatory perimeter that allows for oversight and consumer protection in a rapidly evolving financial landscape.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, regional).

% Issue e-money products and benefit from the regulatory clarity that defines their offerings as distinct from traditional banking. This allows them to innovate and operate within a defined legal framework, attracting consumers who value direct digital holdings.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the ability to hold and transact with digital instruments directly, offering convenience and new payment options outside traditional banking. Their choices are shaped by the availability and acceptance of these digital instruments.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, beneficiary,
    moderate, immediate, constrained, local).

% Face competition from fintech firms offering e-money products. They must adapt their services and regulatory compliance to account for the new categories of digital money, which can be seen as a cost or a challenge to their traditional dominance.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, payer,
    institutional, generational, constrained, national).

% Analyze the implications of direct consumer holdings of digital instruments for the definition and measurement of money, leading to distinctions like M4/M5. They seek to refine theoretical models to reflect new financial realities.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear boundary for what constitutes 'digital money' based on direct consumer holdings, enabling regulatory oversight and fostering innovation in non-bank digital payment systems.
% TRANSFER_FUNCTION: Facilitates the transfer of value directly between consumers and merchants using digital instruments, bypassing traditional bank accounts for certain transactions. It also transfers regulatory authority over these new instruments to specific bodies.
% ABSENT_VOICES: Early conceptualizers of digital money might argue that the 'emergence' was much earlier, at the point of theoretical possibility, rather than practical consumer adoption. Their perspective is often subsumed by the practical and regulatory focus of this reading.
% DISAPPEARANCE_RATIONALE: If the concept of digital money as directly held consumer instruments vanished, the regulatory frameworks for e-money would collapse, fintech firms would lose their distinct product category, and consumers would revert to traditional bank-intermediated digital payments, fundamentally altering the financial landscape.
% FOUNDING_PROBLEM: The increasing use of digital payment methods blurred the lines between traditional bank deposits and new forms of digital value, creating a need for clear definitions to ensure consumer protection and financial stability.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and fintech firms attest that the problem of defining and regulating new forms of digital money is ongoing and critical. Independent financial analysts and consumer advocacy groups corroborate the need for clear boundaries to protect consumers and maintain market integrity.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low because the primary function is definitional and coordinative, creating new market space rather than extracting from existing ones. Suppression is also low, as the constraint primarily involves legal definitions and market acceptance, not active coercion against alternatives (which are still developing). Theater ratio is minimal, indicating that the regulatory and market activities are genuinely functional in establishing this new category. The gradual increase in extractiveness and suppression over the interval reflects the maturation of the e-money market and the increasing importance of its regulatory boundaries.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of digital money emergence (conceptualization, infrastructure) would place the boundary earlier or define it differently. This reading, focused on consumer holdings, emphasizes the practical and regulatory aspects, which are crucial for market participants but might seem arbitrary to those focused on theoretical or technological precursors. The engine's classification as 'rope' reflects the beneficial coordination function from the perspective of those operating within this specific definition.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech firms are beneficiaries, as this reading provides them with a clear framework for operation and innovation. Consumers also benefit from new, convenient payment options. Traditional banks are payers, as they must adapt to new competition and regulatory distinctions. Monetary theorists are observers, refining their understanding of money in light of these developments.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_criteria_ambiguity,
    'Is direct consumer holding the most appropriate criterion for the ''emergence'' of digital money, or are theoretical conceptualization or infrastructural capabilities more fundamental?',
    'A consensus among monetary historians and economists on a unified definition of ''digital money'' emergence, or a shift in regulatory focus to prioritize other criteria.',
    'If theoretical or infrastructural criteria were prioritized, the ''emergence'' date would shift earlier, and the beneficiaries/victims might change to reflect those earlier stages of development. This would likely reclassify the constraint as a different type, possibly a ''mountain'' if based on fundamental technological shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_criteria_ambiguity, conceptual, 'Ambiguity in the foundational criteria for digital money''s emergence.').

omega_variable(
    regulatory_capture_potential,
    'To what extent do fintech firms, as beneficiaries of this definition, influence the regulatory bodies to maintain or refine this specific boundary for their own benefit?',
    'Analysis of lobbying efforts, regulatory capture studies, and the independence of regulatory decision-making processes from industry influence.',
    'If significant capture is detected, the constraint''s extractiveness might be higher than currently assessed, and its classification could drift towards a ''tangled_rope'' or ''snare'' if the coordination story becomes a cover for industry-specific rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_potential, empirical, 'Potential for regulatory capture by fintech firms benefiting from the definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.01).
narrative_ontology:measurement(digi_tr_t1993, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1993, 0.02).
narrative_ontology:measurement(digi_tr_t1996, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1996, 0.03).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(digi_be_t1993, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1993, 0.18).
narrative_ontology:measurement(digi_be_t1996, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1996, 0.21).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(digi_su_t1993, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1993, 0.08).
narrative_ontology:measurement(digi_su_t1996, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1996, 0.12).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel. This 'consumer_holdings_reading' focuses on the practical availability of digital instruments to individuals, influencing and coexisting with other conceptualizations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
