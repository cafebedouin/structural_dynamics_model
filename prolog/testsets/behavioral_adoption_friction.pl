% ============================================================================
% CONSTRAINT STORY: behavioral_adoption_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_adoption_friction, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: behavioral_adoption_friction
 *   human_readable: CBDC Behavioral Adoption Friction
 *   domain: monetary_policy/digital_currency/behavioral_economics
 *
 * SUMMARY:
 *   A central bank deploys a retail CBDC designed according to rational-actor
 *   monetary models: frictionless digital payments, legal tender status, and
 *   policy programmability. Behavioral data reveals systematic adoption
 *   resistance correlated with econographic factors the design did not
 *   account for—privacy preferences, status quo bias, technology trust
 *   deficits, and social network effects. The constraint is the mismatch
 *   between design assumptions and actual decision-making patterns, which
 *   creates friction that benefits some actors (commercial banks, private
 *   payment platforms) while imposing costs on others (privacy-preferring
 *   users, cash-dependent populations). The claim is rope (coordination with
 *   minimal extraction) while the metrics describe moderate and rising
 *   extraction as the system's enforcement requirements increase to overcome
 *   behavioral resistance.
 *
 * KEY AGENTS:
 *   - central_bank_monetary_authority: Agenda-setter (institutional/analytical) — designs the CBDC based on policy models, interprets low adoption as a communication problem
 *   - financial_inclusion_advocates: Beneficiary (organized/mobile) — support CBDC as a tool for unbanked populations
 *   - privacy_preferring_users: Payer (moderate/constrained) — bear the cost of transactional privacy erosion
 *   - cash_dependent_populations: Payer (powerless/trapped) — forced digital transition without resources the system assumes
 *   - commercial_banks: Payer/Excluded (institutional/constrained) — lose deposits but benefit indirectly from slow adoption
 *   - behavioral_economists: Observer (analytical/analytical) — document the design-reality gap
 *   - private_payment_platforms: Excluded (powerful/arbitrage) — protected by the friction from CBDC competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_adoption_friction, 0.38).
domain_priors:suppression_score(behavioral_adoption_friction, 0.42).
domain_priors:theater_ratio(behavioral_adoption_friction, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_adoption_friction, extractiveness, 0.38).
narrative_ontology:constraint_metric(behavioral_adoption_friction, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(behavioral_adoption_friction, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(behavioral_adoption_friction, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(behavioral_adoption_friction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_adoption_friction, rope).
narrative_ontology:human_readable(behavioral_adoption_friction, "CBDC Behavioral Adoption Friction").
narrative_ontology:topic_domain(behavioral_adoption_friction, "monetary_policy/digital_currency/behavioral_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behavioral_adoption_friction, '40b8f324-aa9f-4768-8861-d515b94da8b4').
narrative_ontology:cs_kernel_codification('40b8f324-aa9f-4768-8861-d515b94da8b4', distributed).
narrative_ontology:cs_authority_grounding('40b8f324-aa9f-4768-8861-d515b94da8b4', distributed).
narrative_ontology:cs_reading_relation('40b8f324-aa9f-4768-8861-d515b94da8b4', behavioral_adoption_friction__digital_money_legitimacy_regulated_stablecoin, coexists_with).
narrative_ontology:cs_reading_relation('40b8f324-aa9f-4768-8861-d515b94da8b4', behavioral_adoption_friction__digital_money_legitimacy_crypto_permissionless, coexists_with).
narrative_ontology:cs_axiom('40b8f324-aa9f-4768-8861-d515b94da8b4', foundational, state_monopoly_on_legitimate_money).
narrative_ontology:cs_axiom_status(state_monopoly_on_legitimate_money, holdable).
narrative_ontology:cs_axiom_grounding('40b8f324-aa9f-4768-8861-d515b94da8b4', state_monopoly_on_legitimate_money, conventional).
narrative_ontology:cs_axiom('40b8f324-aa9f-4768-8861-d515b94da8b4', secondary, central_bank_policy_transmission_primacy).
narrative_ontology:cs_axiom_status(central_bank_policy_transmission_primacy, holdable).
narrative_ontology:cs_axiom_grounding('40b8f324-aa9f-4768-8861-d515b94da8b4', central_bank_policy_transmission_primacy, instrumental).
narrative_ontology:cs_reference_frame('40b8f324-aa9f-4768-8861-d515b94da8b4', bretton_woods_monetary_sovereignty).
narrative_ontology:cs_drift_state('40b8f324-aa9f-4768-8861-d515b94da8b4', post_crypto_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40b8f324-aa9f-4768-8861-d515b94da8b4', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_adoption_friction, central_bank_monetary_authority).
narrative_ontology:constraint_beneficiary(behavioral_adoption_friction, financial_inclusion_advocates).
narrative_ontology:constraint_victim(behavioral_adoption_friction, privacy_preferring_users).
narrative_ontology:constraint_victim(behavioral_adoption_friction, cash_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(behavioral_adoption_friction, commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and deploys the CBDC infrastructure based on macroeconomic models and policy transmission assumptions. Sets wallet features, KYC requirements, transaction limits, and programmability rules. Measures success by activation rates and transaction volumes, interpreting low adoption as a technical or communication problem rather than a structural mismatch between design assumptions and actual user decision-making.
narrative_ontology:constraint_stakeholder(behavioral_adoption_friction, central_bank_monetary_authority, agenda_setter,
    institutional, generational, analytical, national).

% Support CBDC rollout as a tool to reach unbanked populations and reduce cash-handling costs for low-income users. They frame the technology as democratizing access to digital payments. Their advocacy depends on the CBDC actually being adopted by target populations, which the behavioral friction undermines.
narrative_ontology:constraint_stakeholder(behavioral_adoption_friction, financial_inclusion_advocates, beneficiary,
    organized, biographical, mobile, national).

% Face a choice between cash (anonymous but increasingly inconvenient as merchant acceptance declines) and CBDC (convenient but fully traceable). The CBDC design assumes privacy concerns are secondary to convenience, but behavioral data shows these users systematically avoid wallet activation or use it only for mandatory transactions. Their cost is the erosion of transactional privacy as cash infrastructure atrophies.
narrative_ontology:constraint_stakeholder(behavioral_adoption_friction, privacy_preferring_users, payer,
    moderate, biographical, constrained, national).

% Elderly, rural, or digitally illiterate populations whose economic decision-making is shaped by habit, trust in physical money, and lack of smartphone access. CBDC design assumes rational adoption once infrastructure is available, but these populations exhibit strong status quo bias and technology aversion. As cash acceptance declines, they bear the cost of forced digital transition without the cognitive or material resources the system assumes.
narrative_ontology:constraint_stakeholder(behavioral_adoption_friction, cash_dependent_populations, payer,
    powerless, immediate, trapped, local).

% Lose deposit base and payment fee revenue as CBDC competes directly with bank accounts. They are consulted during design but their structural interest (preserving deposit intermediation) is subordinated to central bank policy goals. The friction benefits them indirectly by slowing disintermediation, but they cannot openly advocate for adoption failure.
narrative_ontology:constraint_stakeholder(behavioral_adoption_friction, commercial_banks, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(behavioral_adoption_friction, commercial_banks, excluded).

% Study the gap between CBDC design assumptions (rational utility maximization, frictionless technology adoption) and observed behavior (loss aversion, status quo bias, privacy salience, trust heuristics). They document that econographic principal components—age, digital literacy, income volatility, social network density—predict adoption better than the rational-actor variables the design optimizes for.
narrative_ontology:constraint_stakeholder(behavioral_adoption_friction, behavioral_economists, observer,
    analytical, biographical, analytical, global).

% Operate mobile payment systems that already solve the coordination problem CBDC targets, with user interfaces optimized for actual behavioral patterns rather than policy models. They are structurally excluded from the CBDC design process because their business model (data monetization, cross-border arbitrage) conflicts with central bank control objectives. The behavioral friction protects their market position.
narrative_ontology:constraint_stakeholder(behavioral_adoption_friction, private_payment_platforms, excluded,
    powerful, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified digital payment rail with legal tender status, solving the collective action problem of merchant acceptance and interoperability that fragments private payment systems.
% TRANSFER_FUNCTION: Moves transactional privacy and payment autonomy from users to the central bank, which gains visibility into all CBDC transactions and the capacity to program money with policy-embedded rules (expiration dates, spending restrictions, negative interest rates).
% ABSENT_VOICES: Privacy advocates and cryptography communities who would argue for zero-knowledge proof architectures or privacy-preserving CBDC designs are excluded from the design process because their technical proposals conflict with the central bank's policy transmission and financial surveillance objectives.
% DISAPPEARANCE_RATIONALE: If the CBDC and its adoption incentives vanished, users would revert to the existing payment equilibrium: cash for privacy-sensitive transactions, commercial bank accounts and private payment apps for digital convenience. The central bank would lose its direct retail payment channel and the programmable money capacity it was designed to enable.
% FOUNDING_PROBLEM: Fragmented digital payment systems with no legal tender guarantee, combined with declining cash infrastructure and the need for direct monetary policy transmission channels in a digital economy.
% FOUNDING_PROBLEM_CORROBORATION: The central bank attests the problem is live, citing cash decline and payment system fragmentation. Behavioral economists and user surveys attest that the founding problem is partly solved by existing private payment systems, and that the CBDC's low adoption reflects users' revealed preference for the current equilibrium over the central bank's designed alternative. Independent analysis from payment system researchers supports the view that coordination is already substantially achieved outside the CBDC framework.
narrative_ontology:disappearance_verdict(behavioral_adoption_friction, world_rearranges).
narrative_ontology:founding_problem_status(behavioral_adoption_friction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(behavioral_adoption_friction, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-15',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(behavioral_adoption_friction, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_adoption_friction_tests).
:- end_tests(behavioral_adoption_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38 at interval end) because the CBDC design transfers transactional privacy and payment autonomy to the central bank, but the transfer is incomplete due to low adoption. Suppression is moderate (0.42) because enforcement is indirect—merchant incentives, cash infrastructure decline, regulatory pressure on banks—rather than direct coercion. Theater ratio is moderate-low (0.28) because the system performs real coordination functions (interoperability, legal tender guarantee) but a growing share of activity is adoption campaigns and behavioral nudges that address symptoms rather than the structural mismatch. Resistance is substantial (0.58) because users with strong privacy preferences or technology aversion actively avoid the system. Accessibility collapse is low (0.35) because alternatives (cash, private payment apps) remain available, though cash infrastructure is eroding. The measurement series shows rising extraction and suppression as the central bank intensifies adoption incentives and cash alternatives decline.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank's seat, the constraint is a coordination mechanism with adoption friction as a temporary implementation challenge. From the privacy-preferring and cash-dependent seats, the same structure operates as a forced transition that extracts privacy and autonomy. The engine computes this divergence from the power/exit/beneficiary structure—the agenda-setter with analytical exit options sees coordination, while trapped payers see extraction. The behavioral friction is not a bug in the coordination design; it is the system's response to a structural mismatch between the central bank's policy objectives and users' actual decision-making patterns.
 *
 * DIRECTIONALITY LOGIC:
 *   The central bank is the primary beneficiary (gains transaction visibility and programmable money capacity, d near 0.1). Financial inclusion advocates are secondary beneficiaries (benefit from the coordination narrative, d near 0.2). Privacy-preferring users and cash-dependent populations are the primary targets (bear privacy costs and forced transition costs, d near 0.75 and 0.85 respectively). Commercial banks are ambiguous (lose deposits but benefit from slow adoption, d near 0.4). Private payment platforms are excluded rather than targeted. The structural asymmetry is that the central bank experiences the constraint as a coordination tool it controls, while powerless users experience it as an imposed transition that erodes their existing payment autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is claimed as rope (genuine coordination solving payment fragmentation) but the metrics describe moderate extraction rising over time. The mandatrophy question is whether the CBDC's coordination function justifies the privacy transfer and forced digital transition, or whether the coordination story is cover for expanding central bank control. The behavioral friction itself is diagnostic: if the CBDC were pure coordination, adoption would be voluntary and rapid; the need for intensifying incentives and cash infrastructure decline suggests the coordination function is bundled with extraction that users resist. The classification prevents mislabeling this as pure coordination by requiring the engine to compute effective extraction from the structural data—beneficiaries, victims, power asymmetries, and exit constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    design_assumption_validity,
    'Are the CBDC''s design assumptions (rational utility maximization, frictionless technology adoption, privacy as secondary preference) empirically valid for the target population, or do they reflect the central bank''s institutional worldview?',
    'Longitudinal behavioral studies comparing predicted adoption curves from rational-actor models against observed adoption patterns, controlling for econographic principal components. If observed patterns systematically deviate from predictions, the assumptions are invalid.',
    'If the assumptions are invalid, the behavioral friction is not a temporary implementation problem but a structural feature of the mismatch between policy models and actual decision-making. This would reclassify the constraint from coordination-with-friction to extraction-with-coordination-cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_assumption_validity, empirical, 'Whether CBDC design assumptions match actual user decision-making patterns').

omega_variable(
    privacy_coordination_separability,
    'Is the CBDC''s coordination function (legal tender guarantee, interoperability) structurally separable from its surveillance function (full transaction visibility), or does the coordination require the surveillance?',
    'Technical analysis of privacy-preserving CBDC architectures (zero-knowledge proofs, blind signatures, tiered anonymity). If coordination can be achieved with cryptographic privacy guarantees, the functions are separable.',
    'If separable, the surveillance component is pure extraction riding on genuine coordination. If inseparable, the privacy cost is inherent to the coordination function and the extraction measurement must be adjusted downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(privacy_coordination_separability, conceptual, 'Whether coordination and surveillance functions are structurally separable').

omega_variable(
    cash_decline_causality,
    'Is cash infrastructure decline a natural market response to digital payment efficiency, or is it policy-driven to increase CBDC adoption by eliminating alternatives?',
    'Comparative analysis across jurisdictions with different cash policies. If cash decline is faster in jurisdictions with active CBDC rollouts and merchant incentive programs, the decline is policy-driven.',
    'If policy-driven, the suppression metric should be higher because the constraint''s persistence depends on actively eliminating the exit option. If market-driven, the current suppression measurement is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cash_decline_causality, empirical, 'Whether cash decline is market-driven or policy-driven').

omega_variable(
    behavioral_model_under_determination,
    'Do the econographic principal components that predict adoption (age, digital literacy, income volatility, social network density) reflect stable preferences, or are they proxies for trust deficits and privacy concerns that could shift with different CBDC design choices?',
    'Experimental studies offering users CBDC variants with different privacy guarantees and observing whether adoption patterns change. If patterns shift substantially, the components are proxies rather than stable preferences.',
    'If the components are proxies, the behavioral friction is responsive to design changes and the constraint could be reengineered toward genuine coordination. If they reflect stable preferences, the friction is structural and the extraction is inherent to the central bank''s control objectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_model_under_determination, empirical, 'Whether econographic predictors reflect stable preferences or design-responsive proxies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_adoption_friction, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beha_tr_t0, behavioral_adoption_friction, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(beha_tr_t0, observed).
narrative_ontology:measurement(beha_tr_t6, behavioral_adoption_friction, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(beha_tr_t6, observed).
narrative_ontology:measurement(beha_tr_t12, behavioral_adoption_friction, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(beha_tr_t12, observed).
narrative_ontology:measurement(beha_tr_t18, behavioral_adoption_friction, theater_ratio, 18, 0.25).
narrative_ontology:measurement_basis(beha_tr_t18, observed).
narrative_ontology:measurement(beha_tr_t24, behavioral_adoption_friction, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(beha_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(beha_be_t0, behavioral_adoption_friction, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(beha_be_t0, observed).
narrative_ontology:measurement(beha_be_t6, behavioral_adoption_friction, base_extractiveness, 6, 0.26).
narrative_ontology:measurement_basis(beha_be_t6, observed).
narrative_ontology:measurement(beha_be_t12, behavioral_adoption_friction, base_extractiveness, 12, 0.31).
narrative_ontology:measurement_basis(beha_be_t12, observed).
narrative_ontology:measurement(beha_be_t18, behavioral_adoption_friction, base_extractiveness, 18, 0.35).
narrative_ontology:measurement_basis(beha_be_t18, observed).
narrative_ontology:measurement(beha_be_t24, behavioral_adoption_friction, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(beha_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(beha_su_t0, behavioral_adoption_friction, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(beha_su_t0, observed).
narrative_ontology:measurement(beha_su_t6, behavioral_adoption_friction, suppression_requirement, 6, 0.3).
narrative_ontology:measurement_basis(beha_su_t6, observed).
narrative_ontology:measurement(beha_su_t12, behavioral_adoption_friction, suppression_requirement, 12, 0.35).
narrative_ontology:measurement_basis(beha_su_t12, observed).
narrative_ontology:measurement(beha_su_t18, behavioral_adoption_friction, suppression_requirement, 18, 0.39).
narrative_ontology:measurement_basis(beha_su_t18, observed).
narrative_ontology:measurement(beha_su_t24, behavioral_adoption_friction, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(beha_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_adoption_friction, resource_allocation).
narrative_ontology:affects_constraint(behavioral_adoption_friction, digital_money_legitimacy_sovereign_cbdc).
narrative_ontology:affects_constraint(behavioral_adoption_friction, financial_surveillance_infrastructure).
narrative_ontology:affects_constraint(behavioral_adoption_friction, cash_infrastructure_decline).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
