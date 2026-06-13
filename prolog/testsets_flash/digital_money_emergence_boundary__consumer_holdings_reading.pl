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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Digital Money Emergence Boundary (Consumer Holdings Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money through the lens
 *   of consumer direct holdings, distinguishing it from earlier
 *   conceptualizations or mere infrastructure. It posits that 'digital money'
 *   truly exists when individuals can hold and transact with digital
 *   instruments (like e-purses or e-money accounts) outside the traditional
 *   banking system. This reading is crucial for regulatory bodies to define
 *   legal categories (e.g., under the E-Money Directive) and for fintech
 *   firms to operate, while also creating new competitive pressures for
 *   traditional banks. The constraint is claimed as a Mountain because it
 *   reflects a natural evolution in the functional definition of money, but
 *   with identifiable beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.2).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.1).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Emergence Boundary (Consumer Holdings Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '7ccb08f7-30ac-45ab-8782-ca6a2d59eff3').
narrative_ontology:cs_kernel_codification('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', formalized).
narrative_ontology:cs_authority_grounding('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', lineage).
narrative_ontology:cs_interpretation_layer_present('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3').
narrative_ontology:cs_reading_relation('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_axiom('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', foundational, money_requires_direct_holdability).
narrative_ontology:cs_axiom_status(money_requires_direct_holdability, holdable).
narrative_ontology:cs_axiom_grounding('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', money_requires_direct_holdability, conventional).
narrative_ontology:cs_axiom('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', secondary, e_money_distinct_from_bank_deposits).
narrative_ontology:cs_axiom_status(e_money_distinct_from_bank_deposits, holdable).
narrative_ontology:cs_axiom_grounding('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', e_money_distinct_from_bank_deposits, conventional).
narrative_ontology:cs_reference_frame('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', post_e_money_directive_clarity).
narrative_ontology:cs_drift_state('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', contemporary_crypto_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('7ccb08f7-30ac-45ab-8782-ca6a2d59eff3', '').
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

% Benefit from a clear, legally defined boundary for digital money that allows for specific regulatory oversight (e.g., EMI/ECB defining e-money categories). This reading provides a stable basis for their mandate.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, beneficiary,
    institutional, generational, analytical, regional).

% Benefit from the legal clarity that allows them to issue e-money products directly to consumers, distinct from traditional banking. This enables their business model and market entry.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the ability to hold and transact with digital instruments outside traditional bank accounts, offering new payment methods and potentially lower fees. Their benefit is direct utility and choice.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, beneficiary,
    moderate, immediate, constrained, global).

% Face new competition from e-money issuers and must adapt their services. This reading necessitates new distinctions (like M4/M5) that segment their traditional market dominance in money creation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, payer,
    institutional, generational, constrained, global).

% Analyze the implications of this emergence boundary for the definition and function of money, integrating new forms of digital value into existing frameworks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, legally recognized definition for 'digital money' based on consumer direct holdings, enabling regulatory frameworks and market development for e-money products.
% TRANSFER_FUNCTION: Transfers regulatory authority and market opportunity from traditional banking to new e-money institutions, while providing consumers with new digital payment options.
% ABSENT_VOICES: Early digital currency pioneers who conceptualized digital money without direct consumer holding mechanisms might argue this definition is too narrow, but their conceptual frameworks are not directly impacted by this legal/market boundary.
% DISAPPEARANCE_RATIONALE: If this boundary vanished, the legal and market distinctions between bank deposits and e-money would collapse, creating regulatory confusion and undermining the business models of fintech firms built on e-money issuance. The financial landscape would need to redefine digital value.
% FOUNDING_PROBLEM: The problem of defining and regulating new forms of digital value that function as money but are not traditional bank deposits, ensuring consumer protection and financial stability.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and fintech firms attest to the ongoing relevance of this problem, citing the need for clear legal definitions to manage innovation and risk. Independent financial historians and legal scholars corroborate the historical necessity of such distinctions as digital instruments evolved.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__consumer_holdings_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because this boundary primarily provides clarity and enables new markets rather than extracting rents directly. Suppression is also low (0.1) as it's a definitional boundary, not actively enforced coercion against alternatives, but rather a legal distinction. Theater ratio is minimal (0.05) as the distinction serves a genuine regulatory and market-enabling function. Accessibility collapse is high (0.85) because once this definition is adopted, alternatives for legally defining 'digital money' in this specific functional sense largely collapse. Resistance is low (0.05) as the boundary is generally accepted by those operating within the e-money framework, though traditional banks may experience competitive pressure.
 *
 * PERSPECTIVAL GAP:
 *   Regulatory bodies and fintech firms view this as a necessary and beneficial clarification, enabling innovation and oversight. Traditional banks, while adapting, may see it as a redefinition that fragments their historical monopoly on money creation. Consumers largely experience it as increased choice and convenience. The analytical observer sees a functional boundary that emerged from technological and legal evolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech firms are beneficiaries as this reading provides the legal and market framework for their operations. Consumers are also beneficiaries through increased choice. Traditional banks are payers as they face new competition and must adapt to new definitions of money. Monetary theorists are observers, analyzing the implications.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint, to clearly define and regulate new forms of digital money, remains live. The emergence of new digital assets (e.g., cryptocurrencies) continues to challenge and refine this boundary, preventing mandatrophy. The constraint's persistence is tied to the ongoing evolution of digital finance, not to inertial maintenance of an obsolete function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_boundary,
    'Is this emergence boundary a natural consequence of technological and functional evolution, or a constructed legal/regulatory definition that benefits specific actors?',
    'Comparative analysis of jurisdictions with different legal definitions of e-money; if the functional emergence patterns are consistent across varied legal frameworks, it suggests a more natural boundary.',
    'If more constructed, the ''mountain'' claim would be reclassified, likely to a ''tangled_rope'' or ''snare'' if the benefits to regulatory bodies and fintech firms are found to be disproportionate to the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_boundary, conceptual, 'Ambiguity between natural emergence and regulatory construction of the digital money boundary.').

omega_variable(
    scope_of_consumer_holdings,
    'How broadly should ''consumer direct holdings'' be interpreted? Does it include non-custodial crypto wallets, or only regulated e-money accounts?',
    'Legal precedent and evolving regulatory guidance on new digital asset classes; technological developments in self-custody solutions.',
    'A broader interpretation would expand the scope of ''digital money'' and potentially shift regulatory burdens; a narrower one would maintain the current distinction, potentially excluding new forms of digital value from this definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_consumer_holdings, empirical, 'Interpretation of ''consumer direct holdings'' in the context of digital money emergence.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''digital_money_emergence_boundary'' kernel. How would the classification change if the ''conceptualization_reading'' or ''infrastructure_reading'' were adopted as primary?',
    'Applying the full DR framework to the sibling readings and comparing their computed classifications and stakeholder impacts.',
    'The ''conceptualization_reading'' might compute as a ''mountain'' with fewer direct beneficiaries, while the ''infrastructure_reading'' might compute as a ''rope'' or ''tangled_rope'' with different sets of beneficiaries (e.g., payment processors, network operators). This reading emphasizes legal and market definitions over theoretical or purely technical ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.01).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.02).
narrative_ontology:measurement(digi_tr_t2010, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2010, 0.03).
narrative_ontology:measurement(digi_tr_t2020, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(digi_be_t2010, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(digi_be_t2020, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2020, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(digi_su_t2010, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2010, 0.09).
narrative_ontology:measurement(digi_su_t2020, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, e_money_directive_regulation).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, fintech_innovation_incentives).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banking_competition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel. This 'consumer_holdings_reading' focuses on the point where individuals could directly hold digital instruments, distinct from the 'conceptualization_reading' (theoretical possibility) and 'infrastructure_reading' (electronic transfer mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
