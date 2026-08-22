% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: ECB Mandate (Article 127 TFEU) — Orthodox Price-Stability Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   Article 127(1) TFEU establishes price stability as the ECB's primary
 *   objective and states that, 'without prejudice' to price stability, the
 *   ECB shall support the general economic policies of the Union. This story
 *   instantiates the orthodox reading: exclusive operational focus on the ~2%
 *   inflation target, with employment, growth, and (post-2018) climate
 *   considerations treated as non-operational unless and until price
 *   stability is already achieved — a threshold the reading treats as rarely
 *   if ever satisfied in a way that activates the secondary clause. Two
 *   sibling constraints read the same treaty text differently:
 *   expansive_secondary_objectives holds the 'without prejudice' clause
 *   authorizes ongoing discretionary balancing, and climate_incorporation
 *   holds Article 11 TFEU's environmental integration obligation is binding
 *   on ECB operations even absent price stability. This story's ε (0.58)
 *   reflects only the orthodox reading's own operation — the concentration of
 *   protection on asset holders and creditor states and the diffuse cost
 *   borne by wage workers, debtor states, and excluded climate advocates —
 *   not any averaged or hedged value across readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.72).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate (Article 127 TFEU) — Orthodox Price-Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, 'f4c3fc9f-963e-4ad3-9f62-133c0fdec45a').
narrative_ontology:cs_kernel_codification('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', fixed_text).
narrative_ontology:cs_authority_grounding('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', lineage).
narrative_ontology:cs_interpretation_layer_present('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a').
narrative_ontology:cs_reading_relation('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', foundational, price_stability_lexical_priority).
narrative_ontology:cs_axiom_status(price_stability_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', price_stability_lexical_priority, conventional).
narrative_ontology:cs_axiom('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', foundational, central_bank_independence_requires_narrow_mandate).
narrative_ontology:cs_axiom_status(central_bank_independence_requires_narrow_mandate, holdable).
narrative_ontology:cs_axiom_grounding('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', central_bank_independence_requires_narrow_mandate, instrumental).
narrative_ontology:cs_reference_frame('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', bundesbank_style_monetary_orthodoxy).
narrative_ontology:cs_drift_state('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', post_2020_pandemic_and_climate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f4c3fc9f-963e-4ad3-9f62-133c0fdec45a', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, euro_area_savers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, sovereign_bondholders).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, orthodox_monetary_economists).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, euro_area_wage_workers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_transition_advocates).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, peripheral_eurozone_unemployed).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, central_bank_independence_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, price_stability_primacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads Article 127(1) TFEU as establishing price stability as the sole primary objective, with the 'without prejudice' clause for supporting general economic policies treated as strictly subordinate and non-operational unless price stability is already secured. Sets interest rates and asset-purchase criteria under this reading, and resists formal reinterpretation via legal opinion, public communication, and internal statute interpretation. Insulated from electoral accountability by treaty-guaranteed independence.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Hold financial assets whose real value is protected when inflation is suppressed near 2%. Benefit directly from the exclusive-focus reading because it prioritizes their asset-preservation interest over employment or transition objectives, and can shift savings across jurisdictions or asset classes if price stability weakens.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, euro_area_savers, beneficiary,
    organized, biographical, mobile, continental).

% Price eurozone debt on the credibility of strict inflation targeting; a narrow mandate reading lowers the risk premium they demand and increases the value of their holdings. Can reallocate globally if the mandate's credibility weakens, giving them leverage independent of any single member state.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, sovereign_bondholders, beneficiary,
    powerful, generational, arbitrage, global).

% Germany, the Netherlands, and similarly-positioned states benefit from a hard inflation anchor that protects their export competitiveness and low-inflation preferences, and have historically shaped ECB statute interpretation through governing council appointments and treaty negotiation leverage. Bound to the euro system but structurally favored by its orthodox reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states, agenda_setter).

% Bear the employment costs of rate decisions calibrated exclusively to inflation, without the mandate authorizing the ECB to weigh unemployment as an operational target except after price stability is achieved. Cannot exit the currency union or influence ECB governance directly; wage suppression and unemployment during disinflation episodes fall on this group first.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, euro_area_wage_workers, payer,
    powerless, biographical, trapped, national).

% Italy, Greece, and similarly-positioned states face higher borrowing costs and constrained fiscal space when the ECB's exclusive inflation focus limits accommodative monetary support during downturns. Formally sovereign but structurally dependent on ECB market operations to manage bond spreads, giving them no real exit from the mandate's interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_states, payer,
    moderate, generational, constrained, national).

% Argue that Article 11 TFEU environmental integration obligations should inform ECB collateral and asset-purchase frameworks. Under this reading, their claims are treated as outside the operational mandate entirely — not balanced against price stability, but excluded from the calculus unless price stability is already secured, which in practice never triggers activation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_transition_advocates, excluded,
    organized, civilizational, trapped, continental).

% Concentrated in southern and peripheral member states, this group bears the labor-market cost of monetary tightening decided without operational weight on employment. Emigration is the only individual exit available; collective political voice is filtered through national governments with no direct lever over ECB policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, peripheral_eurozone_unemployed, payer,
    powerless, biographical, trapped, national).

% Adjudicates challenges to ECB action (e.g., Gauweiler, Weiss) and has generally deferred to the ECB's own reading of its mandate's scope, applying proportionality review rather than substituting its own interpretation of Article 127's objective hierarchy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, sovereign_bondholders).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, credible nominal anchor across nineteen sovereign fiscal systems sharing one currency, preventing competitive inflation and stabilizing cross-border price expectations without a fiscal union to backstop it.
% TRANSFER_FUNCTION: Moves real income and employment risk from asset holders and creditor states (protected by low, stable inflation) to wage earners and debtor states (who absorb the output and employment costs of rate decisions made without operational regard for growth or unemployment).
% ABSENT_VOICES: Climate transition advocates and organized labor across debtor states would argue for a broader operational mandate weighing employment and environmental risk; they submit position papers and litigate at the margins but have no seat in the Governing Council's own interpretive process, which remains self-referential.
% DISAPPEARANCE_RATIONALE: If the orthodox reading were abandoned overnight in favor of expansive secondary objectives, creditor states and bondholders would face a credibility shock and higher risk premia on eurozone assets; wage workers and debtor states would gain policy space. Whether this counts as 'the world rearranging' or 'restoring the mandate's true balance' is precisely the interpretive dispute the kernel contest is about — this story's own reading holds that the exclusive-focus arrangement is the correct legal baseline, so its removal would be experienced as destabilizing by its beneficiaries and corrective by its victims.
% FOUNDING_PROBLEM: Post-Bundesbank design: prevent the political capture of monetary policy by national governments seeking short-term employment gains at the cost of long-run inflation credibility, a problem especially acute in a currency union without unified fiscal discipline.
% FOUNDING_PROBLEM_CORROBORATION: The ECB's own legal service and orthodox monetary economists (Issing, Weidmann-aligned commentary) attest the inflation-capture problem remains live given persistent fiscal indiscipline in several member states. Independent sources outside the beneficiary set — European Parliament economic affairs committee testimony, ECJ Advocate General opinions in Weiss, and academic critiques (e.g., from the European Trade Union Institute) — argue the capture-prevention problem has been substantially solved by four decades of independence norms, and that the exclusive-focus reading now functions primarily to insulate the ECB from accountability for its real-economy tradeoffs rather than to prevent inflationary capture.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, contested).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction and suppression both rise over the interval (0.42→0.58 and 0.55→0.72) as the ECB's post-2008 and post-2020 crisis responses hardened the institutional and legal precedent for the exclusive-focus reading — successive ECJ rulings (Gauweiler 2015, Weiss 2018) deferred to the ECB's own interpretation, closing off judicial correction and increasing the interpretive suppression required to hold the narrow reading against mounting political pressure for climate and employment integration. Theater ratio is moderate and rising (0.15→0.28) as public communication about 'symmetric' and 'medium-term' framing has grown without corresponding operational change — a performative softening of language around an unchanged operational hierarchy.
 *
 * PERSPECTIVAL GAP:
 *   From the Governing Council's seat, the exclusive-focus reading is simply what the treaty text requires — a matter of legal fidelity, not distributive choice. From the peripheral unemployed or excluded climate advocates' seats, the same reading is an active choice to externalize their concerns from the operational calculus, defended by institutional insulation from their political input. The engine's per-seat computation should reflect this asymmetry structurally, from the declared power/exit data, not from any narrative adjudication here.
 *
 * DIRECTIONALITY LOGIC:
 *   Savers, bondholders, and creditor states sit near the beneficiary end: the exclusive-focus reading directly protects their asset values and macroeconomic preferences, and several (bondholders especially) retain arbitrage-grade exit if credibility weakens, which paradoxically reinforces their stake in maintaining the reading. Wage workers and the peripheral unemployed sit near the full-target end: trapped inside a currency union whose central bank does not treat their employment outcomes as an operational input, with no exit short of emigration. Debtor member states are constrained rather than trapped — formally sovereign, but their market access depends on ECB goodwill, which ties their fiscal room to the mandate's interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing inflationary capture of monetary policy by governments chasing short-term employment gains — was real at Maastricht and is treated by this reading as still live (hence founding_problem_status: contested rather than dead). The classification as tangled_rope rather than snare reflects that the coordination function (a credible nominal anchor across a fiscal union) is genuine and valuable, not merely cover; the extraction is the asymmetric distribution of the costs and benefits of achieving that coordination, sustained through active enforcement (treaty independence, judicial deference) rather than pure fabrication.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    without_prejudice_clause_scope,
    'Does the ''without prejudice to price stability'' clause in Article 127(1) establish a strict lexical priority (secondary objectives are non-operational until price stability is fully achieved) or a proportionality-style balancing test (secondary objectives may be weighed continuously, more heavily as price stability risk recedes)?',
    'A definitive ECJ ruling directly interpreting the operational meaning of ''without prejudice'' in the context of ECB policy discretion, rather than the proportionality-of-means review the Court has applied to date in Gauweiler and Weiss.',
    'A lexical-priority reading vindicates this story''s orthodox classification as the legally correct baseline; a balancing-test reading would mean this story''s exclusive-focus operation is itself an extractive departure from a broader legally-authorized mandate, shifting weight toward the expansive_secondary_objectives sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(without_prejudice_clause_scope, conceptual, 'Whether Article 127''s secondary-objective clause is lexical or balancing.').

omega_variable(
    capture_prevention_vs_accountability_shield,
    'Does the exclusive-focus reading still function primarily to prevent inflationary political capture, or has that function been substantially achieved such that the reading now functions primarily to shield the ECB from accountability for real-economy distributive tradeoffs?',
    'Comparative institutional analysis of eurozone fiscal discipline outcomes since 1999 against counterfactual capture risk, cross-referenced with ECB internal communications on how the mandate''s scope is invoked in policy deliberation.',
    'If the capture-prevention function has been achieved, the founding_problem_status shifts from contested toward dead, strengthening a piton or snare reading of the persistence of strict exclusivity; if capture risk remains live, the tangled_rope classification''s coordination component is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capture_prevention_vs_accountability_shield, empirical, 'Whether the mandate''s exclusivity still solves its founding problem or has become self-protective.').

omega_variable(
    climate_externalization_naturalness,
    'Is the exclusion of climate risk from ECB operational calculus under this reading a neutral legal consequence of the treaty''s plain text, or a substantive policy choice with distributive winners (fossil-asset holders, carbon-intensive sectors whose collateral value is protected by market-neutrality) dressed as textual fidelity?',
    'Comparative analysis of ECB collateral framework composition against a counterfactual climate-adjusted framework, and examination of internal ECB legal opinions on whether market-neutrality itself constitutes a policy choice.',
    'If externalization is a substantive choice, the beneficiary set for this reading should be understood to include carbon-intensive incumbents implicitly, strengthening the case that the orthodox reading is not merely narrow but actively protective of specific interests beyond savers and creditors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_externalization_naturalness, conceptual, 'Whether climate risk exclusion is neutral textualism or substantive protection of carbon-intensive incumbents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1999, 0.15).
narrative_ontology:measurement(ecb__tr_t2004, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2004, 0.17).
narrative_ontology:measurement(ecb__tr_t2009, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(ecb__tr_t2014, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2014, 0.24).
narrative_ontology:measurement(ecb__tr_t2019, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1999, 0.42).
narrative_ontology:measurement(ecb__be_t2004, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2004, 0.46).
narrative_ontology:measurement(ecb__be_t2009, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2009, 0.5).
narrative_ontology:measurement(ecb__be_t2014, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2014, 0.53).
narrative_ontology:measurement(ecb__be_t2019, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(ecb__su_t2004, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2004, 0.58).
narrative_ontology:measurement(ecb__su_t2009, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2009, 0.63).
narrative_ontology:measurement(ecb__su_t2014, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2014, 0.67).
narrative_ontology:measurement(ecb__su_t2019, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__orthodox_price_stability, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, climate_incorporation).

% DUAL FORMULATION NOTE:
% This story is one of three readings decomposed from the ecb_mandate_article_127 kernel per the epsilon-invariance principle. orthodox_price_stability (this file) authors a narrow beneficiary set and high suppression of mandate expansion; expansive_secondary_objectives authors a broader coordination function permitting discretionary balancing; climate_incorporation authors a treaty-based climate integration obligation with a different victim set (carbon-intensive incumbents rather than wage workers as primary payers). Each reading carries its own stable epsilon and is not averaged with the others; the three are linked here for contamination-propagation analysis, not to imply a single unified constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
