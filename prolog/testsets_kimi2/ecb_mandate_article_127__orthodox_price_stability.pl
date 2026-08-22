% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Article 127 Orthodox Price-Stability Exclusivity
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   The orthodox reading of Article 127 TFEU treats the ECB's price-stability
 *   mandate as lexicographic and exclusive, rendering secondary objectives
 *   (employment, growth, environmental integration) legally subordinate and
 *   operationally non-existent. This reading is contested by expansive and
 *   climate-incorporation interpretations that draw on the 'without
 *   prejudice' clause and Article 11 TFEU respectively. The constraint
 *   coordinates inflation expectations but asymmetrically extracts policy
 *   space from debtor states and climate-exposed economies, requiring active
 *   institutional enforcement to maintain the narrow boundary against
 *   mounting political resistance.
 *
 * KEY AGENTS:
 *   - ECB Governing Council (orthodox faction): agenda-setter (institutional/constrained) â operationalizes the narrow mandate and resists expansionary reinterpretation.
 *   - Court of Justice of the EU: observer (institutional/analytical) â adjudicates mandate-boundary challenges but historically defers to ECB technical discretion.
 *   - Creditor member states: primary beneficiary (powerful/constrained) â preserve real debt value and purchasing power through tight-money prioritization.
 *   - Saver households: beneficiary (moderate/constrained) â nominal purchasing power protected but no direct voice in mandate design.
 *   - Debtor member states: primary payer (powerful/constrained) â bear higher real debt burdens and austerity costs when monetary policy stays tight regardless of unemployment.
 *   - Climate-exposed economies: payer (moderate/trapped) â absorb uncompensated climate risks from ECB frameworks that exclude carbon pricing and transition financing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.72).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.81).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Article 127 Orthodox Price-Stability Exclusivity").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '8c439031-3b1c-492a-a0b9-310ed1f9e63b').
narrative_ontology:cs_kernel_codification('8c439031-3b1c-492a-a0b9-310ed1f9e63b', formalized).
narrative_ontology:cs_authority_grounding('8c439031-3b1c-492a-a0b9-310ed1f9e63b', lineage).
narrative_ontology:cs_interpretation_layer_present('8c439031-3b1c-492a-a0b9-310ed1f9e63b').
narrative_ontology:cs_reading_relation('8c439031-3b1c-492a-a0b9-310ed1f9e63b', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('8c439031-3b1c-492a-a0b9-310ed1f9e63b', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('8c439031-3b1c-492a-a0b9-310ed1f9e63b', foundational, price_stability_lexicographic_priority).
narrative_ontology:cs_axiom_status(price_stability_lexicographic_priority, holdable).
narrative_ontology:cs_axiom_grounding('8c439031-3b1c-492a-a0b9-310ed1f9e63b', price_stability_lexicographic_priority, conventional).
narrative_ontology:cs_axiom('8c439031-3b1c-492a-a0b9-310ed1f9e63b', foundational, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('8c439031-3b1c-492a-a0b9-310ed1f9e63b', secondary_objectives_non_operational, conventional).
narrative_ontology:cs_reference_frame('8c439031-3b1c-492a-a0b9-310ed1f9e63b', price_stability_primacy).
narrative_ontology:cs_drift_state('8c439031-3b1c-492a-a0b9-310ed1f9e63b', contemporary_climate_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8c439031-3b1c-492a-a0b9-310ed1f9e63b', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, saver_households).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, debtor_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_exposed_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operationalizes the ECB mandate under Article 127 TFEU. The orthodox majority treats the 2% inflation target as lexicographic, actively resisting the incorporation of employment, growth, or climate-risk targets into primary monetary policy design. Reinterpretation is legally possible but politically costly given creditor-state Council influence and domestic constitutional court threats.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Adjudicates challenges to ECB programs on mandate-boundary grounds. Has historically deferred to the Bank's technical discretion but faces escalating litigation demanding climate and employment integration. Its interpretive rulings can shift the legal feasibility of the orthodox reading without directly setting policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, court_of_justice_eu, observer,
    institutional, generational, analytical, continental).

% Benefit from price-stability prioritization that preserves the real value of sovereign and private debt claims. They advocate strict mandate construction in the European Council and ECB appointment processes, and would face wealth erosion if inflation targeting were relaxed or climate transition costs were monetized.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states, beneficiary,
    powerful, generational, constrained, national).

% Hold deposits and fixed-income assets whose real return depends on low and stable inflation. They are diffuse, lack institutional voice in ECB design, and benefit incidentally from the orthodox mandate's protection of nominal purchasing power.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, saver_households, beneficiary,
    moderate, biographical, constrained, national).

% Face elevated real debt burdens and constrained fiscal space when monetary policy remains tight regardless of domestic unemployment or growth needs. Their governments repeatedly argue for broader mandate interpretation in Eurogroup and Council forums but are outvoted or overruled by the ECB's operational framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, debtor_member_states, payer,
    powerful, generational, constrained, national).

% Bear the costs of a monetary framework that excludes climate-risk pricing from asset purchases and collateral policy. They suffer both physical climate impacts and financial stability risks from carbon-intensive asset portfolios held on the ECB balance sheet, without monetary-policy support for transition financing.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_exposed_economies, payer,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors inflation expectations across a heterogeneous monetary union by committing the central bank to a quantified price-stability target, solving the time-inconsistency problem in monetary policy and preventing monetization of government debt.
% TRANSFER_FUNCTION: Transfers institutional attention and policy priority away from employment, growth, and climate-risk considerations toward the preservation of nominal purchasing power, benefiting creditor states and saver households while debtor states and climate-exposed economies absorb the uncompensated risks and opportunity costs.
% ABSENT_VOICES: Advocates for full-employment targeting and climate-risk integration are structurally absent from ECB Governing Council voting procedures. Debtor-state finance ministers are present but outvoted in operational frameworks. Civil-society climate litigants must resort to ex-post judicial challenge rather than participation in policy design.
% DISAPPEARANCE_RATIONALE: If the orthodox exclusivity vanished overnight, the ECB would rebalance toward secondary objectives; inflation expectations might drift, but fiscal-debt sustainability and climate-transition financing would re-enter the monetary policy calculus. The legal and political architecture of the Monetary Union would face immediate redesign pressure from both creditor and debtor camps, and the ECB balance sheet would likely tilt toward green assets.
% FOUNDING_PROBLEM: The hyperinflation trauma of the interwar period and the need to credibly commit a newly independent central bank against political pressure to monetize debt or prioritize short-term employment over price stability in a monetary union lacking fiscal union.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians and the Bundesbank tradition attest the founding trauma. Independent macroeconomists and debtor-state finance ministries attest the founding inflation problem is structurally absent in current low-inflation conditions. The European Parliament has repeatedly questioned the narrow construction. No credible corroboration of the live-status claim exists outside the creditor-state and monetary-conservative beneficiary set.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the orthodox reading systematically subordinates legitimate secondary objectives that would benefit other agents, converting a genuine coordination anchor into an asymmetric transfer of policy priority. Suppression (0.81) is higher still: the constraint's persistence depends on actively excluding climate and employment considerations from operational design, not on spontaneous consensus. Theater ratio (0.48) reflects the increasing performative quality of legal formalism â repeated assertions of 'lexicographic' priority that mask political choices. Accessibility collapse (0.78) captures the legal-institutional entrenchment that makes treaty revision appear the only exit. Resistance (0.75) registers persistent political and litigious pushback from debtor states and climate advocates. The temporal series track rising extraction and suppression from 1999 to 2024 as unconventional monetary policy and climate emergencies exposed the rigidity of the orthodox frame.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (creditor states, savers) experience the constraint as a necessary institutional anchor that solves time-inconsistency and protects wealth. The payer seats (debtor states, climate-exposed economies) experience the identical legal text as an enforced extraction of policy autonomy, where the price-stability nominal anchor is maintained at the cost of their debt sustainability and ecological transition. The engine computes this divergence from the same structural data â the asymmetry is not a disagreement about facts but a structural property of the mandate's seat-specific directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Creditor member states and saver households sit at the beneficiary end: the constraint subsidizes their asset values and purchasing power by subordinating inflationary or climate-transition monetary tools. Debtor member states and climate-exposed economies sit at the target end: they bear the extraction through suppressed fiscal space and externalized climate risk. The ECB Governing Council sits as agenda-setter with constrained exit â it could reinterpret the mandate but faces severe political and legal costs from creditor-state coalitions and domestic constitutional courts if it does.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope, rather than snare, preserves the genuine coordination function: a monetary union without a nominal anchor would face expectations chaos and free-rider fiscal dynamics. The mandatrophy risk would be misclassifying the constraint as pure extraction (snare) and thereby erasing the coordination floor, or as pure coordination (rope) and thereby erasing the asymmetric victimization. The authored metrics claim independence: the metrics are descriptively high on extraction and suppression, while the claimed type acknowledges the coordination skeleton that justifies the constraint's origin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_liveness,
    'Is the original inflation-trauma time-inconsistency problem still live in the contemporary euro-area economy, or has the constraint outlived its founding coordination function and become primarily extractive?',
    'Comparative inflation volatility analysis across pre- and post-1999 eras, plus assessment of ECB independence threats from fiscal dominance episodes.',
    'If the founding problem is dead, the orthodox reading''s high extraction is not justified by live coordination; reclassification toward snare or piton would follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether the mandate''s founding inflation problem is still live').

omega_variable(
    climate_externalization_mechanism,
    'Are climate risks excluded from ECB asset-purchase and collateral frameworks because they are legally irrelevant to price stability, or because the orthodox reading structurally suppresses climate incorporation to protect creditor balance sheets from transition costs?',
    'ECJ ruling on the ECB''s obligation to consider climate under Article 11 TFEU, or independent carbon-stress testing of the ECB portfolio against its capital keys.',
    'If suppression is creditor-protection in legal disguise, the extraction asymmetry is deeper than the metrics suggest; if genuinely legal, the exclusion is a Mountain-like boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_externalization_mechanism, conceptual, 'Whether climate exclusion is legal boundary or creditor protection').

omega_variable(
    orthodox_reading_stability,
    'Can the orthodox exclusivity reading persist without active political enforcement by creditor-state coalitions, or would it collapse to an expansive reading absent that enforcement?',
    'Counterfactual analysis of ECB policy stances during periods of German political vacua or Franco-Italian Council majorities.',
    'If the reading is enforcement-dependent, its classification as tangled_rope is confirmed; if self-sustaining from treaty text alone, it leans toward rope or mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_reading_stability, empirical, 'Whether the orthodox reading is politically sustained or textually autonomous').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.24).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.3).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 15, 0.38).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.44).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 25, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 25, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is the orthodox reading of Article 127 TFEU, decomposed from the colloquial label 'ECB mandate' which conflates three structurally distinct interpretations. Each reading carries a different beneficiary/victim structure, epsilon, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
