% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Article 127 Mandate — Orthodox Price Stability Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   The ECB's mandate under Article 127 TFEU establishes price stability as
 *   the primary objective, with secondary objectives (supporting general
 *   economic policies, including full employment and environmental
 *   protection) subordinate 'without prejudice' to price stability. The
 *   orthodox reading treats this hierarchy as operational exclusion: the ECB
 *   may not trade off any inflation above 2% for secondary objectives, ever.
 *   This reading has hardened over time — from a credible anchor for a new
 *   currency (1999) to a structural barrier against mandate expansion (2025).
 *   The constraint now extracts from debtors, workers, and climate-vulnerable
 *   populations while coordinating expectations for creditors and financial
 *   incumbents. The claim/metric gap is deliberate: the constraint is CLAIMED
 *   as a coordination rope (the ECB's own framing) while the authored metrics
 *   describe a tangled rope — genuine coordination function plus asymmetric
 *   extraction maintained by active suppression of alternative
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.75).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Article 127 Mandate — Orthodox Price Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '7bcf5f62-4b46-450e-8df6-ed0789c23726').
narrative_ontology:cs_kernel_codification('7bcf5f62-4b46-450e-8df6-ed0789c23726', formalized).
narrative_ontology:cs_authority_grounding('7bcf5f62-4b46-450e-8df6-ed0789c23726', lineage).
narrative_ontology:cs_interpretation_layer_present('7bcf5f62-4b46-450e-8df6-ed0789c23726').
narrative_ontology:cs_reading_relation('7bcf5f62-4b46-450e-8df6-ed0789c23726', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('7bcf5f62-4b46-450e-8df6-ed0789c23726', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('7bcf5f62-4b46-450e-8df6-ed0789c23726', foundational, price_stability_lexical_priority).
narrative_ontology:cs_axiom_status(price_stability_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('7bcf5f62-4b46-450e-8df6-ed0789c23726', price_stability_lexical_priority, conventional).
narrative_ontology:cs_axiom('7bcf5f62-4b46-450e-8df6-ed0789c23726', foundational, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('7bcf5f62-4b46-450e-8df6-ed0789c23726', secondary_objectives_non_operational, conventional).
narrative_ontology:cs_axiom('7bcf5f62-4b46-450e-8df6-ed0789c23726', secondary, credibility_requires_absolute_inflation_ceiling).
narrative_ontology:cs_axiom_status(credibility_requires_absolute_inflation_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('7bcf5f62-4b46-450e-8df6-ed0789c23726', credibility_requires_absolute_inflation_ceiling, empirically_contingent).
narrative_ontology:cs_reference_frame('7bcf5f62-4b46-450e-8df6-ed0789c23726', maastricht_bundesbank_model).
narrative_ontology:cs_drift_state('7bcf5f62-4b46-450e-8df6-ed0789c23726', post_pandemic_green_deal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7bcf5f62-4b46-450e-8df6-ed0789c23726', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, financial_sector_incumbents).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, bondholder_classes).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, debtor_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_populations).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, central_bank_independence_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, ordoliberal_credibility_theory).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, inflation_targeting_anchoring).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the operational interpretation of Article 127. Controls the analytical framework, staff research agenda, and communication strategy that defines what counts as 'price stability' and whether secondary objectives receive operational weight. Collects institutional authority and legitimacy from maintaining the orthodox reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Net creditor governments (Germany, Netherlands, Finland, etc.) whose fiscal positions and bond portfolios benefit from low inflation and hard-currency credibility. They exercise influence through capital key subscriptions, Governing Council seats, and political pressure on national central bank governors. Exit is near-arbitrage: they could tolerate a modestly more expansive mandate but would lose the credibility premium they currently capture.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, creditor_member_states, beneficiary,
    powerful, biographical, arbitrage, continental).

% Major banks, asset managers, and insurers whose business models are calibrated to the orthodox framework — nominal anchor stability, predictable collateral rules, and absence of climate-risk haircuts on sovereign and corporate bonds. They benefit from the current operational regime and lobby to maintain it. Exit is mobile: they can reallocate portfolios globally but prefer the euro-area franchise under current rules.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, financial_sector_incumbents, beneficiary,
    organized, biographical, mobile, continental).

% Holders of euro-denominated sovereign and corporate debt who capture the real-value protection that a strict 2% ceiling provides. They are not a formal institution but their collective market discipline (selling pressure on any perceived mandate drift) functions as an enforcement mechanism. Exit is mobile across currency zones.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, bondholder_classes, beneficiary,
    organized, biographical, mobile, global).

% High-debt member states (Italy, Greece, France, Spain, etc.) that would benefit from marginally higher inflation tolerance, growth-weighted policy, or climate-transition financing flexibility. They bear the cost of the orthodox reading through higher real debt service, constrained fiscal space, and exclusion from ECB-supported green transition funding. Exit is constrained: leaving the euro is politically and economically prohibitive; coalition-building within the Governing Council is their only leverage.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, debtor_member_states, payer,
    moderate, biographical, constrained, continental).

% Workers in high-unemployment regions (especially Southern Europe youth, long-term unemployed) for whom the exclusive inflation focus means monetary policy never leans toward employment even when price stability is not threatened. They have no institutional voice in the Governing Council, no exit from the currency union, and no organized representation at the EU level. Exit is trapped: they bear the cyclical costs without representation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers, payer,
    powerless, immediate, trapped, continental).

% Populations disproportionately exposed to climate physical risks (flood zones, heat islands, agricultural disruption) and transition risks (carbon-intensive regions) who bear the cost of the ECB's refusal to integrate climate risk into collateral frameworks and asset purchases. The orthodox reading externalizes these risks onto the most vulnerable. Exit is trapped: they cannot leave the climate exposure or the currency zone.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_populations, payer,
    powerless, generational, trapped, continental).

% The only directly elected EU institution, with treaty-mandated oversight of the ECB (Article 284 TFEU). It regularly calls for broader mandate interpretation (employment, climate, social objectives) but has no formal power to change the ECB's operational framework — only moral suasion and confirmation hearings for Executive Board members. Its exclusion is structural: the Treaty places monetary policy outside democratic control.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_parliament, excluded,
    institutional, biographical, constrained, continental).

% Guardian of the Treaties and driver of EU climate policy (European Green Deal). Formally supports Article 11 TFEU environmental integration but cannot compel the ECB to operationalize it. The Commission's climate agenda is structurally dependent on ECB cooperation it cannot command. Exit is constrained: it must negotiate, not dictate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eu_commission, excluded,
    institutional, biographical, constrained, continental).

% Central banking scholars, legal theorists, and political economists who analyze the mandate interpretation contest. They provide the intellectual infrastructure for all three readings but hold no operational power. Their exit is analytical: they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, academic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a credible, predictable nominal anchor for the euro area by committing to a single, transparent inflation target — solving the time-inconsistency problem of discretionary monetary policy and coordinating private inflation expectations across 20 heterogeneous economies.
% TRANSFER_FUNCTION: Transfers real resources from debtors (member states, households, firms) to creditors (bondholders, savers, net creditor states) by maintaining a strict ceiling on inflation that protects nominal asset values at the expense of real debt burdens and employment stabilization. Also transfers climate adaptation costs from the financial sector (which would bear transition risk under climate-adjusted collateral rules) onto vulnerable populations and future generations.
% ABSENT_VOICES: The unemployed, climate-vulnerable communities, and future generations are structurally absent from the Governing Council. The European Parliament and Commission — the only institutions with democratic or treaty-wide mandates — are formally consulted but functionally excluded from operational decisions. National parliaments of debtor states have no direct channel.
% DISAPPEARANCE_RATIONALE: If the orthodox reading vanished overnight, the ECB would immediately face pressure to operationalize secondary objectives (employment, climate, financial stability) — likely adopting a flexible average-inflation framework, green TLTROs, climate-adjusted collateral haircuts, and explicit employment weighting. The euro area's monetary policy framework, fiscal-monetary interaction, and climate finance architecture would reorganize fundamentally.
% FOUNDING_PROBLEM: The Treaty of Maastricht (1992) established the ECB with a primary objective of price stability, modeled on the Bundesbank, to anchor the new currency's credibility and prevent the fiscal dominance that had plagued European monetary cooperation in the 1970s–80s. The 'without prejudice' clause on secondary objectives was a political compromise, not an operational grant.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars (e.g., German constitutional court jurisprudence, ordoliberal economists) attest the founding problem (credibility anchor for a new currency) remains live — the euro's fragility requires the same discipline. Critics (European Parliament resolutions, Commission Green Deal papers, heterodox economists, Southern European governments) attest the founding problem is substantially solved — the euro is established, credibility is secured, and the constraint now operates as rent protection for incumbents. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.68) reflects the growing gap between the 2% ceiling and the asymmetric costs it imposes: real debt burdens rise, employment stabilization is foregone, climate transition financing is blocked. Suppression (0.75) is high because the constraint's persistence depends on actively excluding rival readings — through Governing Council appointments, research agenda control, communication strategy, and legal resistance to Treaty-based challenges (Article 11 TFEU, proportionality). Theater (0.42) is rising: the ECB increasingly performs 'strategy reviews' and 'climate action plans' that change nothing operationally. Accessibility collapse (0.6) is moderate — alternative readings exist and are intellectually coherent but institutionally blocked. Resistance (0.55) is significant: Parliament resolutions, Commission pressure, court cases, academic critique, and political movements all contest the reading, but none have forced operational change.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB/creditor seat, this is a Mountain — the Treaty text, the credibility logic, and the institutional design all converge on a single non-negotiable anchor. From the debtor/worker/climate seat, this is a Snare — the coordination story is cover for extraction that persists only because alternatives are suppressed. From the analytical observer seat, this is a Tangled Rope — genuine coordination (nominal anchor) coexists with asymmetric extraction (creditor rent, climate externalization) maintained by active enforcement (institutional, legal, communicative). The engine computes this seat divergence from the structural data; the authored claim (tangled_rope) reflects the analytical observer's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council is the agenda-setter with analytical exit — it controls the interpretation but could change it. Creditor states and financial incumbents are beneficiaries with arbitrage/mobile exit — they capture the credibility premium and would lose it under mandate expansion but can reallocate. Bondholders are beneficiaries with mobile exit. Debtor states are payers with constrained exit — trapped in the euro but able to coalition-build. Unemployed workers and climate-vulnerable populations are payers with trapped exit — no voice, no exit. Parliament and Commission are excluded institutional voices. The directionality derivation from beneficiary/victim declarations plus exit options produces the structural asymmetry the engine computes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credibility anchor for a new currency) is contested — creditors and ordoliberals say it remains live; Parliament, Commission, and debtor states say it is solved. The constraint persists not because the founding problem is universally acknowledged as live, but because the benefiting parties (creditor states, financial incumbents, ECB institution) control the interpretation machinery. This is classic mandatrophy: a coordination arrangement whose original justification is contested but whose extraction function has been captured by beneficiaries who block revision. The classification as tangled_rope (not snare) preserves the genuine coordination function — the nominal anchor IS real and valuable — while naming the extraction and suppression that have layered onto it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_vs_practice_gap,
    'Does the Treaty text (''without prejudice to the objective of price stability'') structurally permit operational weight on secondary objectives, or does the hierarchical wording forbid any trade-off?',
    'European Court of Justice ruling on a referral asking whether the ECB''s exclusive 2% focus violates Article 11 TFEU environmental integration or the proportionality principle.',
    'If the Court rules the orthodox reading exceeds Treaty authorization, the constraint''s legal foundation collapses and mandate expansion becomes legally compelled — reclassifying toward scaffold/rope. If the Court upholds the reading, the extraction is Treaty-legitimated and suppression is legally authorized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_text_vs_practice_gap, conceptual, 'Whether the Treaty text itself forecloses expansive readings or the ECB''s interpretation is a self-serving construction.').

omega_variable(
    credibility_premium_measurement,
    'How much of the euro area''s low borrowing costs (vs. counterfactual) is attributable to the orthodox mandate vs. other factors (currency depth, ECB backstops, global savings glut)?',
    'Counterfactual modeling using structural macro models with mandate regime as a variable; event studies around mandate-relevant communications.',
    'If the credibility premium is small or zero, the extraction (creditor rent protection) has no coordination justification — the constraint is snare. If the premium is large and mandate-dependent, the coordination function is substantial — supporting tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_premium_measurement, empirical, 'Whether the coordination benefit (credibility) is real and mandate-dependent or a post-hoc justification.').

omega_variable(
    committer_frame_structural_delta,
    'This reading structurally narrows beneficiaries to savers/creditors, externalizes climate risks, and suppresses mandate expansion. How would the sibling readings'' ε and beneficiary structures differ?',
    'Author the sibling constraint stories (expansive_secondary_objectives, climate_incorporation) and compare their base_properties extractiveness, beneficiary/victim declarations, and claimed_type.',
    'The kernel''s ε-invariance requires each reading to author its own ε over the shared referent (the standing Article 127 arrangement). If sibling readings author substantially lower ε with broader beneficiary sets, this reading''s high ε is confirmed as a reading-specific choice, not a kernel property.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_structural_delta, conceptual, 'Commitment-system framing: how the three readings of the same kernel instantiate different constraints with different extraction profiles.').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression (0.75) primarily structural (Treaty hierarchy, Governing Council voting rules, appointment processes) or performative (communication strategy, research agenda control, strategy reviews that change nothing)?',
    'Decompose suppression into: (a) hard legal/institutional barriers to mandate change, (b) soft enforcement through communication and personnel, (c) internalized suppression (market participants self-censoring mandate-expansion advocacy).',
    'If suppression is mostly structural, the constraint is harder to change — tangled_rope persists. If mostly performative/internalized, a political shift could rapidly reduce suppression — potential scaffold transition. The theater ratio (0.42 rising) suggests growing performative component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural vs. performative vs. internalized composition of the constraint''s suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1999, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1999, 0.15).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t2005, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t2010, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t2015, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t2020, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t2025, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(ecb_mandate_orthodox_be_t1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1999, 0.35).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t2005, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t2010, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t2015, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t2020, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t2025, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb_mandate_orthodox_su_t1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t2005, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t2010, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t2015, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t2020, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t2025, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_fiscal_rules_stability_growth_pact).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_green_deal_financing_architecture).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the ecb_mandate_article_127 kernel. The orthodox reading claims the Treaty hierarchy as operational exclusion; the expansive reading claims the 'without prejudice' clause as operational authorization; the climate reading claims Article 11 TFEU as compulsory integration. They share the same Treaty text but instantiate different constraints with different ε, beneficiaries, and victims. The orthodox reading (this story) structurally influences the siblings by setting the institutional baseline they must contest — its enforcement machinery (Governing Council control, legal resistance, communication dominance) is what the other readings must overcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, institutional, 0.15).
constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, powerful, 0.2).
constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, organized, 0.25).
constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, moderate, 0.75).
constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
