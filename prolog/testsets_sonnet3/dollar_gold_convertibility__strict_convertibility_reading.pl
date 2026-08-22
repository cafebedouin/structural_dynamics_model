% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Convertibility as Binding Legal Obligation on U.S. Monetary Policy
 *   domain: international_political_economy/monetary_law
 *
 * SUMMARY:
 *   This story instantiates the STRICT CONVERTIBILITY READING of the
 *   dollar-gold kernel: that Article IV of the Bretton Woods Articles of
 *   Agreement creates a binding legal obligation on the United States to
 *   redeem dollars held by foreign monetary authorities in gold at $35/oz,
 *   and that this obligation genuinely constrains U.S. monetary policy as
 *   enforceable international law rather than as a conditional or
 *   structurally doomed arrangement. Under this reading the U.S. is a
 *   constrained obligor whose domestic policy space is legally subordinated
 *   to a treaty-enforced external claim; European and gold-pool creditor
 *   nations hold enforceable rights they can and do exercise. This is a
 *   distinct constraint from the policy_flexible_reading (which treats
 *   convertibility as conditional on domestic stability and therefore reads
 *   U.S. discretion as substantially intact) and from the
 *   triffin_structural_reading (which treats the entire arrangement as a
 *   doomed design rather than a live enforceable obligation with identifiable
 *   payers and beneficiaries). Each reading has a different ε and a different
 *   victim/beneficiary structure by construction — this file authors only the
 *   strict-obligation claim.
 *
 * KEY AGENTS:
 *   - us_treasury: primary payer (institutional/trapped) — bears the legal redemption duty
 *   - us_federal_reserve: primary payer (institutional/constrained) — externally disciplined monetary authority
 *   - european_creditor_central_banks: primary beneficiary (institutional/arbitrage) — holds enforceable convertibility claim
 *   - gold_pool_surplus_nations: beneficiary/agenda-setter (organized/arbitrage) — administers collective enforcement
 *   - imf_article_iv_secretariat: agenda-setter (institutional/analytical) — treats deviation as treaty violation
 *   - us_domestic_policy_constituencies: diffuse payer (moderate/trapped) — bears cost of gold-defense monetary tightening
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.71).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.62).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Convertibility as Binding Legal Obligation on U.S. Monetary Policy").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc').
narrative_ontology:cs_kernel_codification('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', formalized).
narrative_ontology:cs_authority_grounding('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', lineage).
narrative_ontology:cs_interpretation_layer_present('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc').
narrative_ontology:cs_reading_relation('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', foundational, article_iv_creates_unconditional_redemption_duty).
narrative_ontology:cs_axiom_status(article_iv_creates_unconditional_redemption_duty, holdable).
narrative_ontology:cs_axiom_grounding('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', article_iv_creates_unconditional_redemption_duty, conventional).
narrative_ontology:cs_axiom('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', secondary, external_legal_obligation_subordinates_domestic_monetary_discretion).
narrative_ontology:cs_axiom_status(external_legal_obligation_subordinates_domestic_monetary_discretion, overridden).
narrative_ontology:cs_axiom_grounding('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', external_legal_obligation_subordinates_domestic_monetary_discretion, conventional).
narrative_ontology:cs_reference_frame('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', bretton_woods_founding_par_value_commitment).
narrative_ontology:cs_drift_state('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', pre_nixon_shock_1971, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('1cccc7e8-a6fc-4972-83d7-4bfcdf0d21bc', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, european_creditor_central_banks).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_surplus_nations).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_policy_constituencies).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_par_value_system).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, rules_based_international_monetary_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the legal duty under Article IV to redeem dollars presented by foreign monetary authorities at $35/oz on demand. Cannot suspend redemption without breaching the Bretton Woods Articles of Agreement and triggering a formal treaty crisis. Gold stock depletes as foreign dollar holdings accumulate, and the obligation binds regardless of the domestic fiscal or monetary stance the U.S. would otherwise prefer.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_treasury, payer,
    institutional, biographical, trapped, global).

% Must set interest rates and credit conditions with an eye to the external gold drain rather than purely domestic employment or inflation objectives. The convertibility obligation externally disciplines what would otherwise be sovereign monetary discretion, forcing rate defenses (e.g., 'gold policy' tightening cycles) that conflict with domestic stabilization goals.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve, payer,
    institutional, biographical, constrained, national).

% Workers, borrowers, and fiscal beneficiaries who bear the cost of externally-driven monetary tightening imposed to defend the gold parity, even when domestic conditions (unemployment, growth) call for the opposite policy stance. They have no vote in the treaty obligation that produces this constraint and no direct recourse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_policy_constituencies, payer,
    moderate, biographical, trapped, national).

% Accumulate dollar reserves through trade surpluses and hold an enforceable legal claim to convert them into gold at the fixed official price. Can exercise or threaten to exercise this claim to extract concessions from U.S. policy, effectively holding a put option against the U.S. gold stock that the U.S. cannot legally refuse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, european_creditor_central_banks, beneficiary,
    institutional, biographical, arbitrage, global).

% Coordinate through the London Gold Pool and IMF Article IV consultations to enforce par value discipline on the U.S., pressing for adjustment when U.S. balance-of-payments deficits threaten the system. Collect the enforceable convertibility right and simultaneously administer collective pressure mechanisms that hold the U.S. to the letter of the obligation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_surplus_nations, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_surplus_nations, agenda_setter).

% Administers the par value and convertibility rules as binding treaty law, conducts consultations, and treats deviation from the $35/oz commitment as a violation requiring correction rather than a policy choice available to the issuing sovereign.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_article_iv_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Hold dollar reserves for trade purposes but lack the institutional standing or gold-pool coordination capacity that the major creditor central banks have; their interests in system stability are not represented in the bilateral gold-pool enforcement dynamics between the U.S. and Europe.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, developing_country_reserve_holders, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a credible nominal anchor for international trade and reserve management by fixing the dollar's gold value, allowing other central banks to hold dollars as a gold-equivalent reserve asset without individually verifying U.S. monetary conduct.
% TRANSFER_FUNCTION: Moves monetary policy discretion from the United States to its creditor counterparts: as foreign dollar holdings accumulate, the legal redemption right transfers effective veto power over U.S. domestic monetary and fiscal choices to the central banks holding those claims, and transfers real resources (gold) from the U.S. Treasury to redeeming foreign holders.
% ABSENT_VOICES: Developing-country reserve holders and the broader U.S. public bearing the domestic costs of gold-defense policy are not party to the Article IV consultations or gold-pool arrangements that adjudicate how strictly the obligation is enforced.
% DISAPPEARANCE_RATIONALE: Removal of the binding convertibility obligation (as eventually occurred in August 1971) immediately freed U.S. monetary policy from external gold-defense discipline, ended the gold pool's function, and forced a systemic renegotiation of exchange arrangements — demonstrating the obligation was load-bearing on the actual conduct of U.S. and creditor-nation policy, not a formality.
% FOUNDING_PROBLEM: Post-WWII architects needed a credible nominal anchor to prevent competitive devaluations and restore confidence in international trade settlement after the interwar currency chaos; gold convertibility of the reserve currency was designed to discipline the issuing country against inflationary finance.
% FOUNDING_PROBLEM_CORROBORATION: Foreign creditor central banks and the IMF secretariat (parties who benefit from the enforceable claim) attest the discipline problem remains live and requires strict enforcement. Independent monetary historians and, ultimately, the U.S. Treasury itself (by suspending convertibility unilaterally in 1971) attest that by the late 1960s the obligation had become structurally impossible to honor at $35/oz given accumulated dollar liabilities relative to the gold stock — corroboration from outside the beneficiary set exists in the Triffin dilemma literature published well before the closing of the gold window.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (1958, early strains) to 0.71 (1971, terminal crisis) as accumulated foreign dollar claims against a shrinking U.S. gold stock made the legal obligation increasingly binding and increasingly costly to honor. Suppression (0.62) reflects the genuine coercive force of treaty law: the U.S. could not unilaterally revalue or suspend redemption without violating Article IV, and did in fact conduct 'gold defense' interest rate policy explicitly to protect the parity, subordinating domestic stabilization goals. Theater ratio stays comparatively low (0.28) because the enforcement was substantively real, not performative — actual gold left Treasury vaults and actual policy rates moved to defend the peg. Accessibility collapse is moderate (0.4) reflecting that alternative arrangements (devaluation, suspension, capital controls) were legally foreclosed under the strict reading even though they were technically available, which is precisely what the reading claims: the binding-obligation frame closes off unilateral policy space that would otherwise exist.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury/Federal Reserve seat, this constraint should compute as extractive and coercive: their policy discretion is externally captured by a legal obligation they cannot exit without treaty breach. From the creditor central bank seat, the same structure computes as legitimate coordination: a rules-based system in which they hold an enforceable right they are owed under the system's own terms. This divergence is the point of the strict reading — it is exactly ε-invariant across seats (extraction is real and high) but experienced asymmetrically, which is the tangled-rope signature: genuine coordination function (a credible reserve anchor) coexisting with asymmetric extraction (U.S. domestic policy space transferred to creditor claimants).
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. Treasury and Federal Reserve are coded as targets (d near the full-target end): they bear the compliance cost, cannot exit without violating binding law, and the obligation actively redirects policy away from their preferred domestic settings. European and gold-pool creditor central banks are coded as beneficiaries (d near the full-beneficiary end): they hold an enforceable claim, face no comparable obligation, and can exercise arbitrage-grade exit (converting dollars to gold at will). Domestic U.S. constituencies are downstream payers with no direct legal standing but real cost exposure, appropriately coded as trapped/moderate.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict reading treats the founding problem (post-war confidence anchor against competitive devaluation) as having a contested status by 1971: the obligation persisted as binding law even as its original justification (disciplining inflationary finance to preserve trade confidence) had been overtaken by the accumulation of dollar liabilities that made continued gold-price honoring arithmetically implausible. Classifying this as tangled_rope rather than snare or mountain matters: it preserves that the arrangement DID solve a genuine coordination problem (reserve currency credibility) even as it simultaneously extracted policy sovereignty from the obligor — collapsing it to pure extraction (snare) would erase the real coordination function that made other nations willingly accumulate dollars in the first place; treating it as a mountain (natural, unavoidable) would erase the treaty-legal, human-constructed, and eventually abandoned character of the obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_bindingness_vs_political_convention,
    'Was Article IV convertibility genuinely binding international law that constrained U.S. policy as a matter of legal obligation, or was it a political convention that the U.S. treated as binding only so long as domestically convenient (i.e., was the ''obligation'' always conditional, making this reading indistinguishable from policy_flexible_reading in practice)?',
    'Examine whether U.S. policymakers (Treasury, Fed) internally treated gold-defense measures as legally compelled versus strategically chosen; review contemporaneous legal opinions on the enforceability of Article IV against a sovereign issuer with no external adjudicator empowered to compel specific performance.',
    'If the obligation was always understood internally as conditional/political rather than strictly binding, this story''s high extractiveness and victim-coding of the U.S. Treasury/Fed overstate the constraint''s coercive force, and the policy_flexible_reading would be the more accurate structural account for the same historical period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_bindingness_vs_political_convention, conceptual, 'Whether Article IV functioned as genuinely binding law or as a conditional political convention — the central fork between this reading and policy_flexible_reading.').

omega_variable(
    unilateral_suspension_precedent,
    'Does the U.S.''s unilateral suspension of convertibility in August 1971 (the Nixon Shock) retroactively demonstrate that the obligation was never truly binding in the strict legal sense this reading claims, or does it demonstrate that a binding obligation was breached under crisis conditions (which is consistent with, not contradictory to, the obligation having been binding beforehand)?',
    'Legal and diplomatic historical analysis of how IMF member states and the Article IV secretariat treated the 1971 suspension — as a lawful exercise of reserved sovereign discretion, or as a treaty breach requiring subsequent renegotiation (which occurred via the Smithsonian Agreement).',
    'If treated internationally as a breach requiring formal renegotiation, this corroborates the strict_convertibility_reading''s premise that the obligation was binding until unilaterally broken. If treated as a lawful exercise of an implicit escape clause, this favors the policy_flexible_reading or triffin_structural_reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_suspension_precedent, empirical, 'Whether the 1971 suspension confirms or undermines the binding-obligation premise of this reading.').

omega_variable(
    coordination_extraction_inseparability,
    'Is the reserve-currency coordination function (stable nominal anchor for trade) separable from the extraction of U.S. policy sovereignty by creditor claim-holders, or are they the same mechanism viewed from two sides — i.e., is there any way to have had a credible gold-backed dollar without the associated policy constraint?',
    'Comparative analysis against alternative reserve-currency designs (e.g., a gold-exchange standard with adjustable parities, or an SDR-based system) that attempted to preserve the coordination benefit while reducing the binding constraint on the issuer.',
    'If inseparable, this supports classifying the arrangement as tangled_rope (coordination and extraction are the same structure) rather than snare (extraction with a fake coordination cover story). If separable, the coordination story may be closer to a rationalization for a fundamentally extractive arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, conceptual, 'Whether the coordination function and the extraction of U.S. policy sovereignty are structurally inseparable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1961, 0.14).
narrative_ontology:measurement(doll_tr_t1964, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1964, 0.19).
narrative_ontology:measurement(doll_tr_t1967, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1967, 0.23).
narrative_ontology:measurement(doll_tr_t1969, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1969, 0.26).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.28).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.42).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1961, 0.5).
narrative_ontology:measurement(doll_be_t1964, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1964, 0.58).
narrative_ontology:measurement(doll_be_t1967, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(doll_be_t1969, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1969, 0.7).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1961, 0.44).
narrative_ontology:measurement(doll_su_t1964, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1964, 0.52).
narrative_ontology:measurement(doll_su_t1967, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(doll_su_t1969, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1969, 0.6).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__strict_convertibility_reading, 0.1).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dollar_gold_convertibility kernel. strict_convertibility_reading (this file) treats Article IV as binding law constraining U.S. policy, coding the U.S. as a high-extraction victim and creditor nations as enforceable-claim beneficiaries. policy_flexible_reading treats the same text as conditional on domestic stability, preserving substantial U.S. discretion and yielding much lower ε. triffin_structural_reading locates the problem in the system's design (an inherently unsustainable fixed-parity reserve-currency architecture) rather than in a genuinely binding bilateral obligation, producing a different victim topology (the system itself, not the U.S. specifically). The three stories share the historical kernel but are not the same constraint — each carries its own ε, its own beneficiary/victim structure, and its own classification, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
