% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Article 127 Mandate — Orthodox Price-Stability-Only Reading
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox reading of the ECB's Article
 *   127 TFEU mandate: that the Treaty's 'primary objective... to maintain
 *   price stability' language, combined with the 'without prejudice to the
 *   objective of price stability, the ESCB shall support the general economic
 *   policies in the Union' clause, establishes exclusive operational priority
 *   for the 2% inflation target, with employment, growth, and (post-2015)
 *   climate considerations legally present but non-binding on actual
 *   rate-setting and balance-sheet decisions. This is one of three linked
 *   constraint stories reading the same Treaty kernel differently: the
 *   expansive_secondary_objectives reading treats the 'without prejudice'
 *   clause as authorizing genuine discretionary balancing once price
 *   stability is not threatened, and the climate_incorporation reading treats
 *   Article 11 TFEU's environmental integration clause as imposing an
 *   affirmative treaty obligation on ECB collateral and asset-purchase
 *   frameworks. This story's ε (0.61) reflects substantial but not extreme
 *   extraction: the orthodox reading has a real coordination function
 *   (credible nominal anchor for a currency union with no single fiscal
 *   authority) but produces asymmetric costs concentrated on peripheral labor
 *   markets and externalizes climate risk. The 2009-2012 sovereign debt
 *   crisis is the inflection point in the extraction and suppression series —
 *   the orthodox reading hardened doctrinally as a defense against political
 *   pressure to use ECB tools for fiscal or employment relief during the
 *   crisis.
 *
 * KEY AGENTS:
 *   - ECB Governing Council — sets and enforces the exclusivity interpretation, institutional power, arbitrage-grade exit via legal insulation
 *   - Eurozone creditors and fixed-income savers — primary beneficiaries of disinflation-protected real returns
 *   - German-style price-stability constituencies and the independent central-bank technocracy — shape and defend the doctrine, benefit from prestige and professional autonomy it confers
 *   - Peripheral eurozone labor markets and high-unemployment member states — bear employment costs with no monetary lever of their own, trapped in currency union
 *   - Climate-transition-dependent regions — bear externalized climate and transition risk the orthodox reading does not internalize
 *   - European Court of Justice — analytical observer, has ratified ECB interpretive discretion in Gauweiler and Weiss without foreclosing rival readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.61).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.72).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.61).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Article 127 Mandate — Orthodox Price-Stability-Only Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '7cc8f832-7d7f-4c57-b94c-37ae5576a9e7').
narrative_ontology:cs_kernel_codification('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', fixed_text).
narrative_ontology:cs_authority_grounding('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', lineage).
narrative_ontology:cs_interpretation_layer_present('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7').
narrative_ontology:cs_reading_relation('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', ecb_mandate_article_127__expansive_secondary_objectives, forecloses).
narrative_ontology:cs_reading_relation('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', foundational, price_stability_lexical_exclusivity).
narrative_ontology:cs_axiom_status(price_stability_lexical_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', price_stability_lexical_exclusivity, conventional).
narrative_ontology:cs_axiom('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', foundational, central_bank_independence_requires_single_metric_discipline).
narrative_ontology:cs_axiom_status(central_bank_independence_requires_single_metric_discipline, holdable).
narrative_ontology:cs_axiom_grounding('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', central_bank_independence_requires_single_metric_discipline, instrumental).
narrative_ontology:cs_reference_frame('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', maastricht_bundesbank_credibility_settlement).
narrative_ontology:cs_drift_state('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', post_2012_sovereign_debt_crisis_doctrine_hardening, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cc8f832-7d7f-4c57-b94c-37ae5576a9e7', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, eurozone_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, fixed_income_savers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, german_style_price_stability_constituencies).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, independent_central_bank_technocracy).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, peripheral_eurozone_labor_markets).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, high_unemployment_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_transition_dependent_regions).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, highly_indebted_sovereigns).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127 TFEU as requiring exclusive operational focus on the 2% harmonized inflation target, treating employment, growth, and environmental objectives as legally subordinate and non-binding on rate-setting and asset-purchase decisions. Sets its own interpretive doctrine, is largely insulated from electoral or parliamentary override, and defends the narrow reading as the source of its independence and credibility.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hold nominal claims (bonds, savings, fixed-income instruments) whose real value is protected by aggressive inflation suppression. Benefit directly and predictably from every basis point of disinflation the mandate produces, with capital mobile enough to relocate if the mandate's stance shifted.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eurozone_creditors, beneficiary,
    organized, biographical, mobile, continental).

% Domestic savers, largely in northern member states, whose retirement and deposit wealth is preserved by low inflation. Politically vocal in favor of the orthodox reading; less able than institutional creditors to exit but well-represented in the mandate's founding constituency.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, fixed_income_savers, beneficiary,
    moderate, biographical, constrained, national).

% National central banks, finance ministries, and ordoliberal policy communities that authored and continue to police the exclusive-mandate interpretation, treating it as the design template inherited from the Bundesbank. They shape appointments and doctrine within the Governing Council to keep the reading stable.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, german_style_price_stability_constituencies, beneficiary,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, german_style_price_stability_constituencies, agenda_setter).

% The professional monetary-policy establishment whose expertise, prestige, and institutional autonomy are validated by a narrow, technically clean single-objective mandate. A single-target mandate is easier to defend against political interference and easier to claim success against; expansion of objectives would dilute both the metric and the professional monopoly on interpreting it.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, independent_central_bank_technocracy, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, independent_central_bank_technocracy, agenda_setter).

% Workers in Southern and peripheral member states bear the employment cost when rate policy is set purely against the inflation target regardless of regional unemployment divergence. Currency union removes the exchange-rate and independent monetary adjustment they would otherwise have; they cannot exit the euro without catastrophic transition costs.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, peripheral_eurozone_labor_markets, payer,
    powerless, biographical, trapped, national).

% National governments whose fiscal and social capacity to counter unemployment is structurally decoupled from monetary policy under the orthodox reading; they must manage the social consequences of ECB decisions in which employment carries no operational weight, without a monetary lever of their own.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, high_unemployment_member_states, payer,
    moderate, generational, trapped, national).

% Regions and sectors reliant on public and ECB-influenced capital flows for decarbonization see climate risk treated as outside the mandate's operational scope under this reading — collateral frameworks and asset purchases remain climate-blind, externalizing transition and physical climate risk that this reading does not internalize as a mandate cost.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_transition_dependent_regions, payer,
    powerless, civilizational, trapped, continental).

% Member states with high public debt loads face higher borrowing costs whenever the exclusive-inflation reading requires tightening regardless of fiscal or growth conditions; they cannot devalue or restructure the mandate unilaterally and depend on ECB discretion they cannot direct.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, highly_indebted_sovereigns, payer,
    moderate, biographical, constrained, national).

% Trade unions, employment ministries, and climate-policy coalitions argue Article 127's 'without prejudice to price stability' clause was drafted to permit balancing, not exclusivity. They lobby the European Parliament and submit amicus positions in Treaty litigation, but have no seat inside the Governing Council's interpretive process.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, employment_and_climate_advocates, excluded,
    organized, generational, constrained, continental).

% Adjudicates challenges to the ECB's interpretive discretion (e.g., Gauweiler, Weiss) and has so far granted the ECB wide latitude in defining its own mandate's operational content, effectively ratifying the exclusivity reading as legally defensible without foreclosing alternative readings outright.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legally stable, credible nominal anchor for the eurozone: one target, one instrument, insulated from national political cycles, solving the genuine coordination problem of a currency union with nineteen fiscal authorities and no single fiscal counterpart.
% TRANSFER_FUNCTION: Moves real income stability toward creditors and nominal-asset holders and away from labor markets and fiscally constrained member states, by treating employment and regional divergence as non-operational considerations in rate-setting and asset-purchase design.
% ABSENT_VOICES: Peripheral labor ministries, unions, and climate-policy coalitions would argue the 'without prejudice to price stability' clause permits — even requires — balancing against employment and environmental objectives once price stability is not threatened; they are not represented inside the Governing Council's own doctrinal interpretation of its mandate.
% DISAPPEARANCE_RATIONALE: If the exclusivity reading were abandoned overnight in favor of an expansive or climate-integrated interpretation, ECB rate paths, asset-purchase collateral eligibility, and forward guidance would shift materially — creditors would face reduced real-return protection, peripheral labor markets and climate-dependent regions would see monetary policy respond to their conditions for the first time, and the technocratic insulation the current reading provides would be diluted by explicit multi-objective balancing.
% FOUNDING_PROBLEM: The Maastricht Treaty drafters needed to convince fiscally conservative member states (principally Germany) to surrender national monetary sovereignty by guaranteeing that the new central bank would not be captured by inflationary political pressure the way some national central banks had been perceived to be.
% FOUNDING_PROBLEM_CORROBORATION: The ECB and price-stability constituencies attest the founding problem — credible commitment against inflationary capture — remains live and requires continued exclusivity. Independent legal scholars, the European Parliament's Economic and Monetary Affairs Committee, and academic monetary economists outside the ECB's own staff have published analyses arguing the credibility problem was largely resolved by the 1990s-2000s disinflation and that continued exclusivity now serves distributive rather than credibility functions; the IMF and OECD have also noted the mandate's secondary objectives remain legally present but operationally unused.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.61) reflects a genuine coordination function (single credible nominal anchor) coexisting with asymmetric cost concentration on peripheral labor markets and climate-exposed regions who have no monetary policy lever of their own inside the currency union. Suppression (0.72) is high because the orthodox reading is actively defended: legal doctrine (ECJ deference), institutional appointment control by price-stability constituencies, and technocratic framing all work to foreclose the expansive and climate readings from becoming operational, not merely to coordinate expectations. Theater ratio is comparatively low (0.28) because the mandate does perform real technical work — actual rate decisions are made and actual inflation outcomes are targeted — but is trending upward as post-2015 pressure to at least gesture toward secondary objectives (green QE tilting, forward-guidance references to employment) has grown without changing operational substance.
 *
 * PERSPECTIVAL GAP:
 *   From the Governing Council's own seat, exclusivity is what preserves independence and prevents the mandate from becoming a vehicle for whatever government is in power — a coordination good. From peripheral labor markets and climate-dependent regions, the identical structure is experienced as extraction: a rule that removes their conditions from consideration by design, enforced through legal doctrine they cannot contest and appointments they do not influence. The engine's per-seat computation should register both experiences from the same structural facts rather than requiring the story to average them into one perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Eurozone creditors and fixed-income savers sit near the full-beneficiary end: the constraint directly subsidizes the real value of their claims and neither group bears meaningful cost from continued exclusivity. Peripheral eurozone labor markets and climate-transition-dependent regions sit near the full-target end: trapped exit (cannot leave the currency union without catastrophic cost), and the mandate's operational content directly excludes their conditions from consideration. The independent central-bank technocracy and German-style price-stability constituencies are declared as dual agenda-setter/beneficiary because they both administer the interpretation and derive institutional/political benefit from its persistence — this is the structural core of the tangled-rope classification: those coordinated (via the nominal anchor) are not identical to those who pay (peripheral labor, climate-exposed regions), and active enforcement (legal doctrine plus appointment control) is required to hold the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible commitment against inflationary political capture — was largely a product of 1970s-1980s European inflation experience and pre-EMU national central bank politics. That problem is substantially resolved: the eurozone has run below-target inflation for long stretches (2013-2020) and the credibility mechanism that once required rigid exclusivity has arguably outlived its acute justification, while the mandate's exclusivity is now defended primarily on institutional-autonomy and distributive grounds rather than the original capture-prevention rationale. This is not framed as fully resolved (mandatrophy_resolved is not asserted) because whether the credibility problem could re-emerge if secondary objectives were operationalized is a genuinely contested empirical question, not a settled one — hence founding_problem_status is 'contested' rather than 'dead'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_problem_still_live,
    'Is the original 1990s inflationary-capture credibility problem that motivated exclusivity still a live risk today, or has it been resolved by three decades of low-inflation track record, such that continued exclusivity now serves distributive rather than credibility functions?',
    'Comparative analysis of central banks with dual or multi-objective mandates (e.g. the US Federal Reserve''s dual mandate) over the same period, testing whether operational balancing against employment produced measurably worse inflation credibility or anchoring than the ECB''s exclusive approach.',
    'If the credibility problem is resolved, the orthodox reading''s coordination justification weakens substantially and the constraint reads closer to pure distributive extraction (snare-adjacent); if still live, the tangled-rope characterization (genuine coordination function plus asymmetric cost) is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_problem_still_live, empirical, 'Whether the founding credibility rationale for exclusivity remains empirically justified or has become a legacy justification for a now-distributive arrangement.').

omega_variable(
    treaty_text_underdetermination,
    'Does Article 127(1) TFEU''s structure — ''primary objective... price stability'' followed by ''without prejudice to the objective of price stability, the ESCB shall support...'' — logically require exclusivity, or does it establish a lexical priority that permits balancing once price stability is not threatened, as the expansive_secondary_objectives reading holds?',
    'Comparative treaty-drafting history analysis (travaux préparatoires of Maastricht negotiations) and comparison with how similarly structured ''without prejudice'' clauses have been interpreted elsewhere in EU law.',
    'If the drafting history supports lexical priority with conditional balancing, the orthodox reading''s claim to being the singularly correct interpretation weakens, and the exclusivity practice looks more like an institutional choice than a treaty compulsion — sharpening the case that suppression (0.72) reflects doctrinal enforcement of a contestable reading rather than textual necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_text_underdetermination, conceptual, 'Whether the Treaty text itself determines exclusivity or merely permits it as one reading among defensible alternatives — the core of the kernel contest this story is one reading of.').

omega_variable(
    ecj_deference_as_ratification_or_abstention,
    'Does the ECJ''s pattern of granting the ECB wide interpretive latitude (Gauweiler, Weiss) constitute an affirmative legal ratification of the orthodox exclusivity reading, or merely judicial abstention from a political question that leaves all three readings equally available?',
    'Close doctrinal analysis of ECJ reasoning in mandate-adjacent cases, distinguishing holdings that affirmatively endorse exclusivity from holdings that merely decline to second-guess ECB discretion on proportionality grounds.',
    'If the ECJ''s deference is genuine ratification, this reading''s suppression of sibling readings gains additional legal entrenchment (harder for expansive or climate readings to gain traction through litigation); if abstention, the door remains open for future ECB leadership to shift the operational reading without new legal barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_deference_as_ratification_or_abstention, conceptual, 'Whether judicial deference forecloses sibling readings as a matter of law or leaves the kernel contest genuinely open.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1999, 0.15).
narrative_ontology:measurement(ecb__tr_t2004, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(ecb__tr_t2009, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(ecb__tr_t2014, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2014, 0.24).
narrative_ontology:measurement(ecb__tr_t2019, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1999, 0.42).
narrative_ontology:measurement(ecb__be_t2004, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2004, 0.46).
narrative_ontology:measurement(ecb__be_t2009, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement(ecb__be_t2014, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(ecb__be_t2019, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1999, 0.5).
narrative_ontology:measurement(ecb__su_t2004, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement(ecb__su_t2009, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement(ecb__su_t2014, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement(ecb__su_t2019, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2019, 0.71).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__orthodox_price_stability, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint, ecb_mandate_article_127__expansive_secondary_objectives, and ecb_mandate_article_127__climate_incorporation are three readings of the same Article 127 TFEU kernel. Each authors a distinct ε and beneficiary/victim structure rather than averaging across readings: this orthodox reading (ε=0.61) narrows the beneficiary set to creditors/savers/technocracy and externalizes climate risk with high suppression of alternative readings; the expansive reading is expected to show a broader, more diffuse beneficiary set and lower suppression (discretionary balancing is less actively policed than a bright-line exclusivity rule); the climate_incorporation reading is expected to show climate-exposed and future-generation beneficiaries newly internalized and correspondingly different victim sets (likely current fossil-asset holders and short-horizon creditors). All three should be read together as one contested kernel, not reconciled into a single value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
