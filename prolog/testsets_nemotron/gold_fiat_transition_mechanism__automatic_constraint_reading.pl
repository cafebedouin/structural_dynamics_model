% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Automatic Gold Constraint to Discretionary Fiat Authority
 *   domain: economic/political/historical
 *
 * SUMMARY:
 *   The gold standard functioned as an automatic physical constraint: money
 *   creation was limited by gold reserves, and balance-of-payments deficits
 *   triggered gold outflows that automatically contracted the money supply.
 *   The transition to fiat money (1914–1971) replaced this with discretionary
 *   central bank authority — a human institutional constraint requiring
 *   active enforcement (legal tender laws, capital controls, regulatory
 *   frameworks). This reading emphasizes the structural shift: from a
 *   material constraint that operated without human discretion to an
 *   institutional constraint that extracts seigniorage from money holders
 *   while providing coordination benefits (elastic money, lender of last
 *   resort). The claimed type is tangled_rope because the system genuinely
 *   coordinates (payment system, credit elasticity, crisis management) AND
 *   extracts asymmetrically (inflation tax on creditors and wage earners).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.45).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Automatic Gold Constraint to Discretionary Fiat Authority").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "economic/political/historical").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '7ffe6932-9cf6-4212-84f1-cc4f20beb2e4').
narrative_ontology:cs_kernel_codification('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', implicit).
narrative_ontology:cs_authority_grounding('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', practice).
narrative_ontology:cs_interpretation_layer_present('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4').
narrative_ontology:cs_reading_relation('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', foundational, automatic_constraint_replacement).
narrative_ontology:cs_axiom_status(automatic_constraint_replacement, holdable).
narrative_ontology:cs_axiom_grounding('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', automatic_constraint_replacement, empirically_contingent).
narrative_ontology:cs_axiom('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', foundational, discretionary_authority_expands_extraction).
narrative_ontology:cs_axiom_status(discretionary_authority_expands_extraction, holdable).
narrative_ontology:cs_axiom_grounding('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', discretionary_authority_expands_extraction, instrumental).
narrative_ontology:cs_reference_frame('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', gold_standard_automatic_adjustment).
narrative_ontology:cs_drift_state('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', post_1971_fiat_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ffe6932-9cf6-4212-84f1-cc4f20beb2e4', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banks).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_and_wage_earners).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, countercyclical_policy_capability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and treasury departments gained discretionary control over money creation and interest rates. They now set policy rates, conduct open market operations, and act as lenders of last resort without gold reserve constraints. This discretion enables countercyclical policy but also permits monetary financing of deficits.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter).

% Bondholders, foreign reserve holders, and institutional lenders lost the automatic gold redemption threat that disciplined debtor nations. Their claims are now denominated in fiat currencies whose purchasing power depends on central bank credibility. Exit options include currency diversification, inflation-linked bonds, or shifting to real assets — all imperfect hedges against systemic debasement.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Sovereign borrowers gained fiscal flexibility without gold convertibility constraints. They can issue debt in their own currency and rely on central bank accommodation. However, they bear the cost of higher inflation risk premiums and potential loss of market access if credibility erodes.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations, payer).

% Banks operate in a system where reserves are created by central bank policy rather than gold deposits. This enables credit expansion but subjects them to regulatory capital requirements and monetary policy transmission. They benefit from lender-of-last-resort backstop but face supervisory constraints.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banks, beneficiary,
    organized, biographical, constrained, national).

% Workers hold nominal wage claims vulnerable to unexpected inflation. They lack the financial sophistication and instruments to hedge currency debasement effectively. Their exit is limited to collective bargaining for cost-of-living adjustments or political pressure — both slow and uncertain.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_and_wage_earners, payer,
    moderate, biographical, constrained, national).

% The intellectual framework that evaluates monetary regimes — from classical gold standard theory through Keynesian activation to modern inflation targeting. Does not collect rents or bear costs directly but shapes the legitimacy conditions under which the constraint operates.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_orthodoxy, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_orthodoxy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common monetary denominator and lender-of-last-resort facility that enables complex credit intermediation, international trade invoicing, and countercyclical stabilization — solving the coordination problem of elastic money supply in a fractional-reserve banking system.
% TRANSFER_FUNCTION: Transfers seigniorage and inflation tax from money holders (creditors, wage earners, fixed-income recipients) to monetary authorities and fiscal authorities who issue and spend the currency. The automatic gold constraint limited this transfer; its removal expanded the transfer capacity.
% ABSENT_VOICES: Future generations who inherit the long-term credibility consequences of current monetary policy; small savers in developing economies who hold dollars as a store of value without political representation in the issuing authority; historical creditor classes whose contractual expectations were unilaterally altered by the 1971 Nixon Shock.
% DISAPPEARANCE_RATIONALE: If discretionary fiat authority vanished overnight and gold convertibility were restored, global trade and finance would contract sharply — credit chains would break, sovereign debt markets would reprice, and the institutional architecture of central banking would collapse. The world financial system is organized around the fiat constraint.
% FOUNDING_PROBLEM: The gold standard's automatic adjustment mechanism forced deflationary contractions on deficit countries during downturns, transmitting shocks internationally and preventing countercyclical policy. The Great Depression demonstrated this catastrophically. The transition aimed to give monetary authorities the discretion to stabilize output and employment.
% FOUNDING_PROBLEM_CORROBORATION: Keynesian and post-Keynesian economists (outside central banking institutions) corroborate that the founding problem — deflationary bias of gold — was real and the transition solved it. Monetarists and Austrian-school economists (also outside beneficiary institutions) argue the founding problem was misdiagnosed: the issue was not gold but central bank mismanagement of gold-standard rules. Central bank official histories self-assert the founding problem remains live; independent economic historians note the transition also solved a fiscal problem for war-financing states.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the substantial seigniorage and inflation tax capacity gained by monetary authorities after gold convertibility ended. Suppression (0.45) is moderate: legal tender laws and capital controls enforce the fiat system, but alternatives (crypto, gold, foreign currency) exist at the margins. Theater ratio (0.25) captures the growing gap between central banks' stated mandates (price stability, full employment) and their operational role in fiscal accommodation and asset price support. Accessibility collapse (0.35) is low because alternatives to fiat money persist and the constraint does not foreclose them entirely. Resistance (0.55) is significant: creditor resistance (bond vigilantes, currency substitution), political resistance (anti-inflation movements), and intellectual resistance (monetarist critique) all persist.
 *
 * PERSPECTIVAL GAP:
 *   From the monetary authority seat, the constraint appears as a coordination achievement — elastic money enabling modern financial intermediation. From the creditor seat, it appears as extraction — the inflation tax eroding real returns. From the labor seat, it appears as an opaque force degrading purchasing power. The engine computes these divergent seat classifications from the declared power, exit, and beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities are primary beneficiaries (d ≈ 0.15): they gained discretionary policy tools and seigniorage revenue. Creditor class are primary victims (d ≈ 0.85): they lost the automatic gold redemption discipline and now bear inflation risk. Debtor nations are secondary beneficiaries (d ≈ 0.3): they gained fiscal flexibility but pay higher risk premiums. Labor/wage earners are secondary victims (d ≈ 0.7): they hold unhedged nominal claims. Commercial banks are beneficiaries (d ≈ 0.25): they gained credit expansion capacity but face regulatory constraints. The engine computes these directionalities from the structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deflationary bias of gold) was genuinely solved — the fiat system enables countercyclical policy. However, the solution has expanded beyond its original mandate: discretion now routinely accommodates fiscal deficits, suppresses interest rates below market levels, and supports asset prices. This mission creep is mandatrophy — the constraint's justification (stabilization) has been exceeded by its operation (financial repression). The theater ratio rise from 0.05 to 0.25 tracks this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_vs_institutional_naturalness,
    'Is the automatic gold constraint genuinely a natural law (mountain) or a constructed institutional arrangement that presented itself as natural?',
    'Historical analysis of gold standard operation: did it operate without human discretion (no central bank sterilization, no capital controls, no suspension clauses)? The historical record shows frequent suspensions and managed adjustments.',
    'If the gold constraint was always partially managed, the transition is not mountain→institution but managed_institution→discretionary_institution — lower epsilon delta, different mandatrophy profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_vs_institutional_naturalness, conceptual, 'Whether the pre-transition constraint was genuinely automatic or a performed automaticity.').

omega_variable(
    extraction_beneficiary_boundary,
    'Does the inflation tax accrue primarily to monetary authorities (seigniorage) or to fiscal authorities (deficit financing), and does this distinction matter for classification?',
    'Central bank balance sheet analysis: seigniorage remitted to treasury vs. retained; fiscal dominance episodes where monetary policy serves debt sustainability.',
    'If fiscal authorities are the true beneficiary, monetary_authorities as beneficiary is mis-specified — the extraction flows through them to the fisc. Would reclassify the beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_boundary, empirical, 'Ultimate vs. proximate beneficiary of fiat discretion.').

omega_variable(
    creditor_coalition_power,
    'Can the creditor class (bondholders, reserve holders) exercise coalition power to discipline monetary authorities, effectively recreating a gold-like constraint through market mechanisms?',
    'Empirical study of bond vigilante episodes, sovereign debt crises, and currency substitution dynamics — do creditors impose effective discipline on high-inflation issuers?',
    'If creditor coalition power is effective, the victim role is overstated — creditors have exit/discipline options that reduce effective extraction. If ineffective, victim role is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_coalition_power, empirical, 'Whether market discipline substitutes for the lost automatic constraint.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''gold_fiat_transition_mechanism'' admit a single correct structural decomposition, or do the three readings capture irreducible complementary facets?',
    'Comparative analysis: does each reading predict different observables? If all three are needed to explain the full transition dynamics, the kernel is genuinely overdetermined and decomposition into three constraints is methodologically necessary.',
    'If readings are complementary, the epsilon-invariance principle is satisfied by the family structure. If one reading subsumes the others, the family contains redundant constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s contest reflects structural overdetermination or authorial framing choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1914, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1914, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1914, 0.05).
narrative_ontology:measurement(gold_tr_t1933, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1944, 0.12).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(gold_tr_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(gold_tr_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(gold_be_t1914, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1914, 0.15).
narrative_ontology:measurement(gold_be_t1933, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1933, 0.35).
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.55).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(gold_be_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(gold_be_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1914, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1914, 0.2).
narrative_ontology:measurement(gold_su_t1933, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1933, 0.3).
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.4).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement(gold_su_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2008, 0.44).
narrative_ontology:measurement(gold_su_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.15).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, inflation_targeting_regime).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, fiscal_dominance_constraint).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, global_reserve_currency_structure).

% DUAL FORMULATION NOTE:
% This constraint (automatic_constraint_reading) and its siblings form the gold_fiat_transition_mechanism family. The automatic_constraint_reading emphasizes the constraint-type shift (material→institutional) with epsilon ~0.68. The creditor_discipline_reading emphasizes the geopolitical power shift (creditor→debtor) with different beneficiary/victim structure. The composite_overdetermination_reading denies a single causal node, distributing epsilon across multiple convergent transitions. All three are needed to capture the full structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, institutional, 0.15).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, powerful, 0.85).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
