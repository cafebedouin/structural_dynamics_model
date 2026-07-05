% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding Referee Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the binding referee reading of the contested WTO
 *   DSB authority kernel: DSB panels issue rulings grounded in
 *   covered-agreement treaty text that member states are legally obligated to
 *   implement, with non-compliance triggering authorized retaliation. Under
 *   this reading, sovereignty over WTO-covered domains was explicitly traded
 *   away at accession in exchange for market access, and a panel ruling is a
 *   treaty compliance obligation, not a negotiating recommendation a state
 *   can decline. This is one of three siblings sharing the DSB authority
 *   kernel: the advisory_coordination_reading treats the same panels as
 *   facilitating negotiated settlement with discretion retained, and the
 *   judicial_activism_reading treats the same panels as exceeding their
 *   mandate through interpretive overreach. Each is authored as its own
 *   constraint with its own epsilon; this file does not average across them.
 *
 * KEY AGENTS:
 *   - wto_secretariat_and_panelists: administers binding rulings, institutional/analytical
 *   - major_trading_powers: beneficiary and occasional payer, powerful/mobile
 *   - export_oriented_industries: beneficiary, organized/mobile
 *   - developing_country_respondents: payer, moderate/constrained
 *   - affected_domestic_industries: payer, powerless/trapped
 *   - domestic_regulatory_agencies: payer, institutional/constrained
 *   - domestic_legislatures: excluded, institutional/trapped
 *   - trade_law_academics: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.52).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.61).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Referee Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '7da05d3f-7a80-4b36-9648-ed8a61f3c9c1').
narrative_ontology:cs_kernel_codification('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', formalized).
narrative_ontology:cs_authority_grounding('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', lineage).
narrative_ontology:cs_interpretation_layer_present('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1').
narrative_ontology:cs_reading_relation('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', foundational, treaty_ratification_constitutes_binding_consent).
narrative_ontology:cs_axiom_status(treaty_ratification_constitutes_binding_consent, holdable).
narrative_ontology:cs_axiom_grounding('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', treaty_ratification_constitutes_binding_consent, conventional).
narrative_ontology:cs_axiom('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', foundational, covered_domain_discretion_was_traded_for_market_access).
narrative_ontology:cs_axiom_status(covered_domain_discretion_was_traded_for_market_access, holdable).
narrative_ontology:cs_axiom_grounding('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', covered_domain_discretion_was_traded_for_market_access, conventional).
narrative_ontology:cs_reference_frame('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', gatt_positive_consensus_baseline).
narrative_ontology:cs_drift_state('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', post_appellate_body_paralysis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7da05d3f-7a80-4b36-9648-ed8a61f3c9c1', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_oriented_industries).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, major_trading_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_panelists).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, developing_country_respondents).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, affected_domestic_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, major_trading_powers).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_supremacy_within_covered_domains).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, rules_based_multilateral_trading_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes panels, interprets covered agreements, and issues rulings that member states are bound to implement or face authorized retaliation. Administers the compliance mechanism, sets timelines for implementation, and certifies whether measures brought into conformity satisfy the ruling. Draws legitimacy and institutional continuity from the binding character of its output.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_panelists, agenda_setter,
    institutional, generational, analytical, global).

% Use the binding ruling system offensively to force open foreign markets and defensively to have adverse rulings against them absorbed with minimal domestic disruption, given deep negotiating capacity and the ability to sequence compliance with domestic politics. Can afford litigation costs and can credibly threaten retaliation of their own; occasionally lose rulings but retain the resources to manage the fallout.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, major_trading_powers, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, major_trading_powers, payer).

% Benefit directly when the binding ruling mechanism strikes down a trading partner's tariff, subsidy, or regulatory barrier that impeded their market access. Do not participate in disputes directly but lobby governments to bring cases and collect the market-access gains when rulings are enforced.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_oriented_industries, beneficiary,
    organized, biographical, mobile, global).

% Face binding rulings requiring them to withdraw domestic measures — industrial policy, agricultural support, public health regulation — that were adopted for legitimate domestic reasons, under threat of authorized trade retaliation they often cannot survive economically. Lack the legal capacity and negotiating leverage of major powers to slow-walk or reshape implementation; remaining a WTO member while contesting a ruling is not a real option given the trade-access stakes.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, developing_country_respondents, payer,
    moderate, biographical, constrained, national).

% Domestic firms and workers whose protective tariff, subsidy, or regulation is struck down by a binding ruling lose that protection abruptly once a state complies or is retaliated against. They were not party to the negotiation of the treaty commitments their government made and have no standing before the panel; they experience the ruling as an external mandate imposed on their livelihood.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, affected_domestic_industries, payer,
    powerless, biographical, trapped, national).

% Health, safety, environmental, and industrial regulators find that measures adopted through domestic democratic processes can be ruled treaty-inconsistent by a panel applying covered-agreement text, effectively subordinating domestic regulatory discretion to treaty compliance obligations. They must redesign or withdraw contested measures under binding compliance timelines set by the DSB, not by domestic legislative process.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Enacted the underlying domestic measures through ordinary democratic process but have no seat at the panel and no path to contest the ruling other than through their executive's trade delegation. Their policy choices are treated as treaty violations rather than legitimate domestic decisions once a panel rules against them.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_legislatures, excluded,
    institutional, generational, trapped, national).

% Study panel and Appellate Body jurisprudence, assess whether rulings track the treaty text or drift into new obligations, and produce the scholarship that both defenders and critics of the binding regime cite.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, trade_law_academics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, rule-bound forum for resolving trade disputes so that member states do not resort to unilateral tariff retaliation or trade wars each time a measure is contested — genuine coordination against a real collective-action problem in international trade.
% TRANSFER_FUNCTION: Moves policy discretion from domestic democratic and regulatory processes to treaty-interpreting panels, and moves market access and compliance costs from the winning party's competitors to the losing party's protected domestic industries and regulators.
% ABSENT_VOICES: Domestic legislatures and the workers and firms whose protections are struck down have no standing before the panel; only the member state's executive trade delegation appears, and it may have interests (broader trade relationship, reciprocal concessions) that diverge from the affected domestic constituency's interests.
% DISAPPEARANCE_RATIONALE: If binding DSB authority disappeared overnight, states would revert to unilateral retaliation and power-based bargaining over trade disputes; major trading powers would lose a forum that currently disciplines smaller states' policy choices with treaty language rather than raw leverage, and many currently-compliant domestic measures struck down under binding rulings would likely be reinstated.
% FOUNDING_PROBLEM: Pre-WTO dispute settlement under the GATT could be blocked by the losing party (positive consensus rule), making rulings toothless and inviting unilateral trade retaliation (e.g., Section 301 actions) that destabilized the trading system.
% FOUNDING_PROBLEM_CORROBORATION: Major trading powers and the WTO Secretariat attest the binding mechanism remains necessary to prevent a return to unilateral retaliation. Independent trade law academics and several developing-country trade ministries attest that the mechanism has drifted from resolving blockable disputes toward constraining domestic regulatory choice in ways the original GATT contracting parties did not anticipate — corroboration exists on both sides, which is itself the mark of a genuinely contested founding problem rather than a settled one.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects a genuine coordination function (avoiding unilateral trade wars) layered with real asymmetric cost: rulings systematically require withdrawal of domestic measures adopted through ordinary democratic process, and the cost of compliance or retaliation falls unevenly on states without negotiating leverage. Suppression (0.61) is high because the mechanism is not merely persuasive — authorized retaliation makes non-compliance economically costly, and there is no exit from covered obligations short of withdrawal from the WTO itself, which is not a live option for most economies given the trade-access stakes. Theater ratio is comparatively low (0.22) because panel review and appellate mechanisms are functioning legal processes, not primarily performative — though the number tracks slowly upward reflecting growing critique of the Appellate Body's functioning (particularly the AB's collapse from 2019, which some read as the system substituting the theater of a ruling for the substance of a functioning appeal). Accessibility collapse is moderate (0.58): once a state has acceded to the covered agreements, unilateral departure from a specific ruling is nearly impossible, but withdrawal from the WTO framework as a whole, however costly, remains a nominal alternative. Resistance (0.55) reflects the substantial and growing political resistance from respondent states and domestic constituencies who experience binding rulings as external override of domestic policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Major trading powers and export-oriented industries sit near the beneficiary end: they use the binding mechanism offensively to open markets and can absorb adverse rulings using deep negotiating capacity — d is low. Developing country respondents and affected domestic industries sit near the target end: rulings against them are enforced through a retaliation mechanism they cannot symmetrically wield, and their exit options are constrained or fully trapped — d is high. Domestic regulatory agencies and legislatures are structurally targets even though nominally sovereign, because the treaty obligations they signed subordinate domestically-enacted measures to panel interpretation of covered-agreement text.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (blockable, toothless GATT dispute settlement inviting unilateral retaliation) was real and is only partially resolved: the binding mechanism replaced one failure mode (no enforcement) with another (enforcement that outpaces the consent member states believe they gave). Classifying this as tangled_rope rather than snare or mountain avoids two mislabeling errors: treating the system as pure extraction would erase the real coordination gain (no return to 1980s-style trade wars); treating it as natural/inevitable (mountain) would hide that specific institutional design choices — panel composition, standard of review, remedy structure — determine how the extraction is distributed, and those choices remain contestable and have in fact been contested (the U.S. blockade of Appellate Body appointments since 2019 is direct evidence the arrangement is neither immutable nor uncontested).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_surrender_scope_ambiguity,
    'Did WTO accession constitute member states knowingly surrendering policy discretion across the full scope panels have since applied covered agreements to, or has panel and Appellate Body interpretation expanded the effective scope of surrendered discretion beyond what was negotiated at accession?',
    'Comparative analysis of accession-era negotiating history and travaux preparatoires against the actual universe of measures later found treaty-inconsistent; tracking whether the rate of adverse rulings against novel domestic regulatory categories (health, environment, industrial policy) has risen relative to the founding-era caseload composition.',
    'If scope has expanded beyond negotiated consent, this reading''s premise that discretion was ''surrendered'' becomes contested rather than settled, and elements of the judicial_activism_reading would need to be incorporated rather than treated as a wholly separate sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_surrender_scope_ambiguity, empirical, 'Whether the scope of surrendered discretion matches what was negotiated at accession or has expanded through interpretation.').

omega_variable(
    kernel_reading_selection_basis,
    'What determines which of the three sibling readings (advisory_coordination, binding_referee, judicial_activism) a given party or scholar adopts — is it a genuinely available interpretive choice under the treaty text, or does one reading better track the text while the others are motivated by outcome preference?',
    'Textual analysis of DSU Articles 3, 19, and 21-22 against the practice record; survey of which reading trade law scholars, WTO members, and domestic courts actually apply when the outcome is against their interest versus in their favor.',
    'If the binding_referee_reading is the textually dominant reading and the other two are largely adopted opportunistically depending on whether a party won or lost, that would support treating this reading as closer to the kernel''s stable center rather than one of three equally live options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether reading selection tracks genuine textual ambiguity or outcome-motivated reasoning across the three sibling readings.').

omega_variable(
    appellate_body_collapse_effect,
    'Does the ongoing non-functioning of the WTO Appellate Body (since 2019, due to blocked appointments) convert this reading''s ''binding'' character into something closer to advisory in practice, since rulings can now be appealed into a void?',
    'Track the rate at which losing parties file appeals into the non-functioning Appellate Body specifically to avoid compliance, versus rate of voluntary implementation of panel rulings absent appeal.',
    'If appeal-into-the-void has become a routine evasion mechanism, the binding_referee_reading''s compliance-obligation premise is empirically weakening even though the treaty text and formal doctrine remain unchanged — this would be a live drift signal for this specific reading, not a change in kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_collapse_effect, empirical, 'Whether Appellate Body paralysis is functionally converting binding rulings into advisory ones.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__binding_referee_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__binding_referee_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__binding_referee_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(wto__tr_t2019, wto_dsb_authority__binding_referee_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__binding_referee_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.39).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(wto__be_t2019, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(wto__su_t2019, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2019, 0.61).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints share the wto_dsb_authority kernel and decompose a single colloquial label ('WTO dispute settlement authority') into structurally distinct claims per the ε-invariance principle: advisory_coordination_reading (lower epsilon, discretion retained, closer to rope), binding_referee_reading (this file — treaty-grounded compliance obligation, tangled_rope), and judicial_activism_reading (contested legitimacy of the interpretive expansion itself, likely higher accessibility_collapse and resistance). Each carries its own epsilon and stakeholder structure; they are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
