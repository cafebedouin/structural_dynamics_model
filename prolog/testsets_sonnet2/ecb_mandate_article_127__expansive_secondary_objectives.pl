% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Article 127 — Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This story instantiates one contested reading of the ECB's Article 127
 *   TFEU mandate: that the 'without prejudice to the objective of price
 *   stability' clause authorizes the Governing Council to operationally
 *   weight employment and growth objectives whenever price stability is not
 *   itself threatened. This is a distinct constraint from the orthodox
 *   reading (exclusive focus on the 2% target, secondary objectives
 *   non-operational) and from the climate-incorporation reading (mandatory
 *   climate risk integration under Article 11 TFEU) — the three readings are
 *   not the same constraint measured differently; they are structurally
 *   distinct claims about what the treaty text permits, each with its own
 *   beneficiary structure and its own ε. This story authors only the
 *   expansive-secondary-objectives reading.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda_setter (institutional/analytical) — administers the interpretive discretion
 *   - wage_earners and indebted_households: primary beneficiaries (moderate-powerless/trapped) — benefit from accommodative policy tilt
 *   - fixed_income_savers and hard_money_member_states: primary payers (moderate-institutional/constrained) — bear the cost of tolerated inflation and diluted price-stability discipline
 *   - eu_court_of_justice: analytical observer — adjudicates the boundary of ECB discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.42).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.38).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Article 127 — Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'fc89c599-fdd7-459e-b870-19af59fee7d3').
narrative_ontology:cs_kernel_codification('fc89c599-fdd7-459e-b870-19af59fee7d3', fixed_text).
narrative_ontology:cs_authority_grounding('fc89c599-fdd7-459e-b870-19af59fee7d3', practice).
narrative_ontology:cs_interpretation_layer_present('fc89c599-fdd7-459e-b870-19af59fee7d3').
narrative_ontology:cs_reading_relation('fc89c599-fdd7-459e-b870-19af59fee7d3', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('fc89c599-fdd7-459e-b870-19af59fee7d3', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('fc89c599-fdd7-459e-b870-19af59fee7d3', foundational, without_prejudice_clause_grants_operational_discretion).
narrative_ontology:cs_axiom_status(without_prejudice_clause_grants_operational_discretion, holdable).
narrative_ontology:cs_axiom_grounding('fc89c599-fdd7-459e-b870-19af59fee7d3', without_prejudice_clause_grants_operational_discretion, conventional).
narrative_ontology:cs_axiom('fc89c599-fdd7-459e-b870-19af59fee7d3', secondary, secondary_objectives_become_operative_absent_price_threat).
narrative_ontology:cs_axiom_status(secondary_objectives_become_operative_absent_price_threat, holdable).
narrative_ontology:cs_axiom_grounding('fc89c599-fdd7-459e-b870-19af59fee7d3', secondary_objectives_become_operative_absent_price_threat, instrumental).
narrative_ontology:cs_reference_frame('fc89c599-fdd7-459e-b870-19af59fee7d3', maastricht_treaty_price_stability_primacy).
narrative_ontology:cs_drift_state('fc89c599-fdd7-459e-b870-19af59fee7d3', post_2010_sovereign_debt_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc89c599-fdd7-459e-b870-19af59fee7d3', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, wage_earners).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, peripheral_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, export_dependent_industries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, hard_money_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_price_stability_constituency).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, central_bank_discretionary_balancing_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127(1) TFEU's 'without prejudice' clause as authorizing operational weight on employment and growth objectives whenever price stability is judged not threatened. Sets the threshold for when secondary objectives become operative, and can shift policy stance (rate paths, asset purchase composition) toward employment support under this reading without treaty amendment.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit when the ECB tolerates modestly higher inflation or maintains looser policy to support employment, since wage growth and job availability track the labor-market slack the mandate reading permits the ECB to weigh. Cannot exit the currency union or directly influence Governing Council interpretation; benefit passively from the reading being operative.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, wage_earners, beneficiary,
    moderate, biographical, trapped, continental).

% Benefit from policy accommodation that keeps borrowing costs lower and inflation moderately erodes real debt burdens. Have no seat in monetary policy deliberation and no exit from the currency they borrowed in.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households, beneficiary,
    powerless, biographical, trapped, national).

% Structurally benefit from a reading that permits growth-weighted policy, since their economies are more sensitive to tight monetary conditions and sovereign borrowing costs than core economies. Can lobby through Council representation but cannot unilaterally alter ECB mandate interpretation; leaving the eurozone is a possible but extreme exit.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, peripheral_member_states, beneficiary,
    organized, generational, constrained, national).

% Benefit from accommodative policy stances that support demand and can influence exchange rate dynamics favorably. Lobby national governments and EU institutions but have no direct standing before the ECB.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, export_dependent_industries, beneficiary,
    organized, biographical, constrained, continental).

% Bear the cost when the mandate's discretionary balancing tolerates inflation above target for longer to support employment objectives, eroding the real value of savings and fixed-income returns. Limited exit — can shift asset allocation toward inflation-protected instruments but cannot escape currency-wide monetary conditions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, fixed_income_savers, payer,
    moderate, biographical, constrained, continental).

% Historically favor a narrow price-stability mandate reflecting domestic monetary culture (notably Germany's Bundesbank tradition) and view discretionary balancing as diluting the ECB's founding commitment. Hold Governing Council votes and can contest interpretation politically and via constitutional courts, but are one voice among many and cannot unilaterally restore the orthodox reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, hard_money_member_states, payer,
    institutional, generational, constrained, national).

% Economists, central bank veterans, and financial institutions who hold that treaty text subordinates employment/growth objectives categorically and that operational weighting represents mandate drift. They publish critiques and file legal challenges but cannot compel a specific interpretation absent Court of Justice intervention.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_price_stability_constituency, payer,
    organized, generational, constrained, continental).

% Has previously ruled (PSPP judgment, 2020) that ECB proportionality assessments must not implicitly expand the mandate into economic policy. Its constitutional concerns about mandate creep are adjacent to but not determinative of ECB self-interpretation; it can create friction (ultra vires findings against national central bank participation) but has no direct authority over ECB Governing Council decisions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, german_constitutional_court, excluded,
    institutional, generational, constrained, national).

% Ultimate arbiter of whether ECB actions stay within Article 127's mandate, having previously upheld ECB discretion broadly (Gauweiler, Weiss rulings) under a proportionality standard. Adjudicates disputes between the ECB's self-interpretation and challengers but has consistently deferred to ECB technical judgment on where price stability is threatened.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the ECB Governing Council flexibility to weigh employment and growth conditions in setting monetary policy without requiring treaty amendment, allowing the institution to respond to divergent economic conditions across member states without triggering a constitutional crisis every time growth considerations enter deliberation.
% TRANSFER_FUNCTION: Moves real value from fixed-income savers, hard-money-culture member states, and those favoring monetary austerity toward wage earners, indebted households, and growth-sensitive peripheral economies and industries, via the inflation and interest-rate paths that discretionary balancing permits.
% ABSENT_VOICES: Non-eurozone EU member states affected by spillover monetary conditions are excluded from Governing Council deliberation. National parliaments whose constituents bear inflation costs have no direct mechanism to contest ECB interpretive choices. The German Constitutional Court has voiced structural objections but sits outside the ECB's own interpretive process.
% DISAPPEARANCE_RATIONALE: If the expansive reading were foreclosed and the orthodox reading became binding, the ECB would lose discretionary latitude to weight employment/growth in policy decisions during periods of price stability, reallocating real costs and benefits: peripheral states and indebted households would bear tighter conditions, while savers and hard-money constituencies would see mandate discipline restored. Policy paths during the 2010s sovereign debt crisis and post-2020 recovery would likely have differed substantially under the orthodox reading.
% FOUNDING_PROBLEM: Article 127 TFEU was drafted to establish central bank independence with price stability as the primary objective while avoiding a text so rigid it could not accommodate legitimate secondary economic policy support once price stability was secured — the 'without prejudice to price stability' clause was meant to permit some responsiveness to the EU's broader economic objectives (Article 3 TEU) without subordinating the primary mandate.
% FOUNDING_PROBLEM_CORROBORATION: ECB officials and academic economists sympathetic to flexible interpretation attest the founding problem (need for legitimate operational flexibility) remains live and the reading is a faithful, evolving application of the drafters' intent. The German Constitutional Court's PSPP ruling and orthodox monetary economists attest from outside the ECB's own institutional interest that the reading has drifted beyond the drafters' contemplated scope into de facto economic policy-making the treaty structure was designed to keep separate from monetary policy.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).
:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) because the reading redistributes real value (from savers/hard-money constituencies to debtors/workers) through legitimate discretionary policy channels rather than through coercive extraction — but it is not zero because the redistribution is asymmetric and largely insulated from direct democratic contestation. Suppression is moderate (0.38): alternatives (the orthodox reading) are not eliminated — they remain a live legal and political position, contestable via the CJEU and national constitutional courts — but the ECB's own interpretive authority substantially forecloses the practical alternative absent judicial intervention, which the CJEU has been reluctant to grant. Theater ratio is moderate (0.30): the ECB's proportionality assessments and public communications genuinely track real policy tradeoffs, but a portion of the interpretive framing functions to legitimate discretion the treaty text does not unambiguously grant.
 *
 * PERSPECTIVAL GAP:
 *   From the Governing Council's seat, discretionary balancing is a faithful, moderate exercise of textually-granted flexibility. From the orthodox price-stability constituency and hard-money member states' seats, the same interpretive practice is mandate creep that erodes the treaty's core independence guarantee by allowing economic-policy considerations to enter what was meant to be an insulated technical mandate. The engine should register this divergence structurally rather than adjudicate which reading is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Wage earners, indebted households, and peripheral member states are beneficiaries with low-to-moderate power and largely trapped or constrained exit — they cannot influence ECB interpretation directly but gain when the reading is operative, so their directionality sits toward the beneficiary end. Fixed-income savers, hard-money member states, and the orthodox price-stability constituency are payers whose real returns or preferred monetary discipline are diluted; despite institutional power in some cases (member state governments), their exit from currency-wide monetary conditions is constrained, pushing their directionality toward the target end even though they are not powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem — legitimate flexibility for a currency union facing divergent economic conditions — has NOT become dead; it remains actively invoked, particularly by peripheral economies and during crisis periods (2010s sovereign debt crisis, 2020 pandemic response). This forecloses classifying the reading as pure atrophied inertia (piton); the coordination function is live, which combined with real asymmetric costs to savers and hard-money states supports the tangled_rope reading rather than snare or mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansive_reading_committer_location,
    'Is the disagreement between this reading and the orthodox_price_stability reading located in the interpretation of ''without prejudice'' as a genuine grant of operational discretion, or in a disagreement about the empirical threshold at which price stability is ''not threatened''?',
    'Textual and drafting-history analysis of the Maastricht Treaty negotiations combined with a review of ECB Governing Council minutes to determine whether officials treat the clause as granting substantive discretion or as a residual safety valve triggered only in narrow circumstances.',
    'If the disagreement is purely about the empirical threshold (when is price stability ''not threatened''), the two readings could in principle converge given sufficient data; if it is about the scope of the grant itself, the readings are irreconcilable within a single interpretive framework and the kernel genuinely forks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_reading_committer_location, conceptual, 'Where exactly the expansive and orthodox readings diverge structurally within the shared treaty text.').

omega_variable(
    sibling_reading_structural_delta,
    'How would adopting the orthodox_price_stability reading instead change the beneficiary/victim structure and the classification computed for this constraint?',
    'Author the orthodox_price_stability sibling story with its own beneficiaries (savers, hard-money constituencies) and victims (debtors, growth-sensitive economies) and compare engine-computed classifications across the two stories at matched time points.',
    'If the orthodox reading computes as mountain or rope (minimal extraction, minimal suppression) while this reading computes as tangled_rope, that divergence would itself be evidence that treaty ambiguity permits genuinely different real-world extraction profiles depending on which reading institutional actors adopt — the kernel''s indeterminacy has material distributive consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, empirical, 'Comparative classification delta between this reading and its orthodox sibling.').

omega_variable(
    cjeu_deference_durability,
    'Does the CJEU''s consistent deference to ECB self-interpretation (Gauweiler, Weiss) reflect genuine institutional judgment that the expansive reading is treaty-consistent, or does it reflect a structural reluctance to constrain a technically complex, politically insulated institution regardless of the substantive merits?',
    'Comparative analysis of CJEU reasoning in ECB cases versus its reasoning in other technical-agency deference contexts (e.g., competition policy, banking supervision) to identify whether the deference pattern is mandate-specific or a general judicial posture toward expert agencies.',
    'If deference is mandate-specific and substantively grounded, the expansive reading''s suppression of the orthodox alternative is more durable and legitimate; if it reflects general judicial reluctance to second-guess technical agencies, the suppression is more contingent and could shift with a differently composed Court.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cjeu_deference_durability, conceptual, 'Whether judicial deference to this reading reflects substantive endorsement or generic institutional reluctance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 1999, 0.18).
narrative_ontology:measurement(ecb__tr_t2004, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(ecb__tr_t2010, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(ecb__tr_t2020, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 1999, 0.22).
narrative_ontology:measurement(ecb__be_t2004, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2004, 0.25).
narrative_ontology:measurement(ecb__be_t2010, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(ecb__be_t2020, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 1999, 0.25).
narrative_ontology:measurement(ecb__su_t2004, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2004, 0.27).
narrative_ontology:measurement(ecb__su_t2010, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2010, 0.34).
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(ecb__su_t2020, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2020, 0.37).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'the ECB's Article 127 mandate.' Each reading (orthodox_price_stability, expansive_secondary_objectives, climate_incorporation) is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle — the readings are not the same constraint viewed from different angles, they are structurally distinct claims about what the treaty text permits and to whom it distributes costs and benefits. All three link to each other via affects_constraints since institutional adoption of one reading structurally forecloses or pressures the operative status of the others within any given policy period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
