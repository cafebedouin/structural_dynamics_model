% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__effects_jurisdiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__effects_jurisdiction_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__effects_jurisdiction_reading
 *   human_readable: GDPR Article 3(2) Extraterritorial Effects-Jurisdiction Reading
 *   domain: technology governance / international law / privacy regulation
 *
 * SUMMARY:
 *   This story instantiates the effects-jurisdiction reading of GDPR Article
 *   3(2): jurisdiction follows the effects of processing on EU residents (via
 *   the targeting-of-goods/services or monitoring-of-behavior tests), not the
 *   physical location or nationality of the controller. Under this reading,
 *   extraterritorial reach is a legitimate exercise of protective
 *   jurisdiction over harms occurring within EU territory (the harm is the
 *   processing's effect on an EU-located data subject), analogous to
 *   effects-based jurisdiction in competition and securities law. This is one
 *   of three sibling constraints reading the same kernel —
 *   territorial_sovereignty_reading treats the same extraterritorial reach as
 *   exceeding legitimate regulatory authority, and market_access_reading
 *   treats it as a conditional-access standard-setting mechanism (the
 *   'Brussels Effect') rather than a jurisdictional assertion at all. Each
 *   sibling has a distinct beneficiary structure, distinct victim set, and
 *   distinct epsilon — they are not the same constraint measured differently;
 *   per the epsilon-invariance principle they are three separate stories
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - eu_data_subjects: beneficiary of extended protection (organized/constrained)
 *   - eu_domestic_controllers: beneficiary of competitive leveling, also incumbent payer under Art 3(1) (organized/constrained)
 *   - eu_data_protection_authorities: agenda-setter administering the targeting/monitoring test (institutional/analytical)
 *   - non_eu_smes_targeting_eu_market: primary payer, disproportionate compliance burden relative to EU revenue (moderate/constrained)
 *   - non_eu_ad_tech_and_analytics_firms: primary payer, core business model directly targeted by the monitoring test (powerful/constrained)
 *   - third_country_governments: excluded from EU rulemaking despite extraterritorial effect on their domiciled firms (institutional/trapped)
 *   - international_trade_and_legal_scholars: analytical observers assessing jurisdictional legitimacy (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__effects_jurisdiction_reading, 0.62).
domain_priors:theater_ratio(gdpr_article_3_scope__effects_jurisdiction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gdpr_article_3_scope__effects_jurisdiction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__effects_jurisdiction_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__effects_jurisdiction_reading, "GDPR Article 3(2) Extraterritorial Effects-Jurisdiction Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__effects_jurisdiction_reading, "technology governance / international law / privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__effects_jurisdiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__effects_jurisdiction_reading, 'd93339a0-87ab-459e-a4e0-136b7e45a267').
narrative_ontology:cs_kernel_codification('d93339a0-87ab-459e-a4e0-136b7e45a267', formalized).
narrative_ontology:cs_authority_grounding('d93339a0-87ab-459e-a4e0-136b7e45a267', extraction).
narrative_ontology:cs_interpretation_layer_present('d93339a0-87ab-459e-a4e0-136b7e45a267').
narrative_ontology:cs_reading_relation('d93339a0-87ab-459e-a4e0-136b7e45a267', gdpr_article_3_scope__territorial_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('d93339a0-87ab-459e-a4e0-136b7e45a267', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('d93339a0-87ab-459e-a4e0-136b7e45a267', foundational, harm_location_grounds_jurisdiction).
narrative_ontology:cs_axiom_status(harm_location_grounds_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('d93339a0-87ab-459e-a4e0-136b7e45a267', harm_location_grounds_jurisdiction, conventional).
narrative_ontology:cs_axiom('d93339a0-87ab-459e-a4e0-136b7e45a267', secondary, resident_protection_overrides_controller_situs).
narrative_ontology:cs_axiom_status(resident_protection_overrides_controller_situs, holdable).
narrative_ontology:cs_axiom_grounding('d93339a0-87ab-459e-a4e0-136b7e45a267', resident_protection_overrides_controller_situs, deontological).
narrative_ontology:cs_reference_frame('d93339a0-87ab-459e-a4e0-136b7e45a267', effects_based_protective_jurisdiction).
narrative_ontology:cs_drift_state('d93339a0-87ab-459e-a4e0-136b7e45a267', post_schrems_ii_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d93339a0-87ab-459e-a4e0-136b7e45a267', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__effects_jurisdiction_reading, eu_domestic_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_smes_targeting_eu_market).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_ad_tech_and_analytics_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__effects_jurisdiction_reading, eu_domestic_controllers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals located in the EU whose personal data is now protected regardless of where the processing entity is incorporated, so long as goods/services are offered to them or their behavior is monitored. Receive rights (access, erasure, portability) enforceable against foreign firms that would otherwise be beyond reach of EU law. Cannot themselves reach outside the EU market to negotiate different terms, but the constraint reaches out on their behalf.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_subjects, beneficiary,
    organized, generational, constrained, continental).

% EU-based firms already bear full GDPR compliance costs under Article 3(1). Article 3(2)'s extraterritorial reach levels the competitive field by binding foreign competitors targeting the same EU customers to the same rules, removing a compliance-cost arbitrage that would otherwise favor non-EU rivals.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_domestic_controllers, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__effects_jurisdiction_reading, eu_domestic_controllers, payer).

% National DPAs and the European Data Protection Board interpret and enforce the targeting/monitoring test, issue guidelines on Article 3(2) scope, investigate non-EU controllers, and can impose fines up to 4% of global turnover. They administer the extraterritorial reach and could, through guidance or referral to the CJEU, narrow or widen its application.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Small and mid-sized firms outside the EU (US, Asia-Pacific, Latin America) that sell to or monitor EU residents online must build full GDPR compliance programs — data protection officers, EU representatives, breach notification systems — despite lacking any physical EU presence. Exiting the EU market entirely is the only clean way out, at direct cost to revenue; remaining means absorbing compliance costs disproportionate to their EU revenue share.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_smes_targeting_eu_market, payer,
    moderate, biographical, constrained, global).

% Large foreign advertising, tracking, and analytics companies whose business model depends on behavioral monitoring of EU users. The monitoring test captures them squarely, forcing consent architecture changes, data localization considerations, and exposure to landmark fines. They have resources to litigate and lobby but cannot easily exit the EU's large consumer market.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, non_eu_ad_tech_and_analytics_firms, payer,
    powerful, biographical, constrained, global).

% Governments of the jurisdictions where the payer firms are domiciled have no formal seat in EU rulemaking or DPA enforcement decisions, despite the extraterritorial reach affecting firms subject to their own domestic law and taxation. They can negotiate adequacy decisions but cannot directly contest individual enforcement actions against their firms.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, third_country_governments, excluded,
    institutional, generational, trapped, global).

% Study whether the effects-based jurisdictional test is consistent with customary international law principles on prescriptive jurisdiction, and whether it represents a legitimate evolution or an overreach analogous to (or distinguishable from) US extraterritorial securities and antitrust enforcement.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__effects_jurisdiction_reading, international_trade_and_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures EU residents receive uniform data protection regardless of where the controller processing their data is located, closing the loophole where a foreign-incorporated firm serving EU customers could evade rules that domestic competitors must follow.
% TRANSFER_FUNCTION: Moves compliance burden (legal, technical, administrative cost) from EU data subjects and EU-domiciled firms onto non-EU controllers and processors who target the EU market or monitor EU residents' behavior, in exchange for continued access to that market.
% ABSENT_VOICES: Non-EU governments and standard-setting bodies whose firms are bound by a jurisdictional test they did not negotiate; small foreign firms without resources to participate in EU consultation processes or challenge enforcement actions before the CJEU.
% DISAPPEARANCE_RATIONALE: If Article 3(2)'s extraterritorial reach were repealed, foreign controllers targeting EU residents without an EU establishment would fall outside GDPR entirely; EU data subjects' protections would depend solely on the (often weaker or absent) privacy law of the controller's home jurisdiction, and EU domestic controllers would face renewed cost-arbitrage pressure from unregulated foreign competitors.
% FOUNDING_PROBLEM: Pre-GDPR, EU data protection law (the 1995 Directive) was tied to establishment or use of equipment within the EU, allowing foreign online services with no EU physical presence to process EU residents' data essentially unregulated, undermining the protection regime for a rapidly growing share of EU residents' actual data exposure.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholarship (e.g., analyses cited in CJEU jurisprudence and OECD/Council of Europe cross-border data flow reports) and non-EU regulators negotiating adequacy status corroborate that cross-border data flows to entities without EU establishment remain the dominant vector of EU resident data exposure, sustaining the founding problem outside the EU's own institutions.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__effects_jurisdiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__effects_jurisdiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__effects_jurisdiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__effects_jurisdiction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__effects_jurisdiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__effects_jurisdiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects real, rising compliance transfer from non-EU controllers to the EU protection regime, but it is moderate rather than severe because compliance is triggered only by deliberate targeting or monitoring of EU residents — firms with no EU-directed activity are unaffected. Suppression (0.62) is meaningfully high: the test leaves foreign controllers with essentially two options (comply fully or withdraw from the EU market), and DPA enforcement (fines, potential processing bans) backs the requirement with real coercive force. Theater ratio is low-moderate (0.28) because enforcement actions (e.g., major fines against foreign ad-tech firms) demonstrate the mechanism is functionally, not merely performatively, applied. Accessibility collapse (0.5) and resistance (0.55) are moderate: alternatives exist (geo-blocking EU users, restructuring data flows) but are commercially costly, and resistance is active and organized (industry lobbying, litigation, adequacy negotiations) rather than symbolic.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data subjects and EU domestic controllers are the structural beneficiaries — the former receive protection they would otherwise lack against foreign processors, the latter receive competitive parity. Non-EU SMEs and ad-tech/analytics firms are the structural targets: their exit options are constrained by market dependence on EU consumers, and the extraction (compliance cost, enforcement exposure) attaches directly and asymmetrically to them while the coordination gain accrues to EU-side actors. Third-country governments are excluded rather than coordinated or extracted from directly — they experience second-order effects on their domiciled firms without a formal seat in the rulemaking or enforcement process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection gap for EU residents dealing with non-EU-established controllers) remains empirically live — cross-border data flows to entities without EU establishment are, if anything, a larger share of EU residents' data exposure now than in 2018, corroborated by scholarship and adequacy-negotiation records outside the EU's own institutions. This blocks a mandatrophy read: the mandate has not outlived its function under this reading. The tangled-rope classification captures that the mechanism is simultaneously solving a genuine coordination problem (uniform protection regardless of controller location) AND imposing asymmetric extraction on a specific payer class (non-EU firms lacking EU-scale legal/compliance resources) that must be actively enforced to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effects_jurisdiction_customary_law_status,
    'Does effects-based jurisdiction over data processing satisfy customary international law''s requirements for prescriptive jurisdiction (as effects-based jurisdiction does in antitrust and securities law), or does GDPR Article 3(2) extend beyond precedents established in those domains?',
    'Comparative analysis of state practice and opinio juris regarding effects-based jurisdiction claims, and tracking whether third-country courts or international bodies formally contest EU enforcement actions against their domiciled firms as ultra vires.',
    'If effects jurisdiction is validated as consistent with customary international law, this reading''s legitimacy strengthens and the tangled_rope classification''s coordination component gains weight. If found to exceed established precedent, the territorial_sovereignty_reading''s foreclosure claim strengthens and this reading''s classification could shift toward snare (extraction lacking legitimate jurisdictional basis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effects_jurisdiction_customary_law_status, conceptual, 'Whether effects-based extraterritorial jurisdiction for data protection is legally grounded or an overreach.').

omega_variable(
    compliance_cost_proportionality,
    'Is the compliance burden imposed on non-EU SMEs proportionate to the actual EU-resident data protection benefit, or does it function as a de facto barrier disproportionately affecting smaller foreign firms relative to large incumbents who can absorb fixed compliance costs?',
    'Empirical study comparing per-firm compliance cost as a share of EU-derived revenue across firm size bands, and market exit/entry data for non-EU SMEs targeting EU consumers post-GDPR.',
    'If costs are disproportionately regressive by firm size, this strengthens the payer-side reading of asymmetric extraction and supports classifying the SME-facing dimension as closer to snare; if roughly proportionate, it supports the coordination-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_proportionality, empirical, 'Whether extraterritorial compliance costs fall disproportionately on smaller non-EU firms.').

omega_variable(
    kernel_reading_selection_evidence,
    'What structural or textual signals justify selecting the effects-jurisdiction reading over the market-access or territorial-sovereignty readings as the operative interpretation of Article 3(2)?',
    'This reading is grounded in the CJEU''s own jurisprudential language (e.g., Google Spain, Schrems lineage cases) treating the provision as establishing jurisdictional scope conditioned on targeting/monitoring effects, and in DPA guidance documents that speak explicitly in terms of ''scope of application'' rather than ''market access conditions.'' The market_access_reading is instead grounded in economic/political-science literature on the Brussels Effect (Bradford) that reframes the same legal text functionally rather than doctrinally.',
    'If EU courts increasingly frame Article 3(2) explicitly as a market-access condition rather than jurisdictional assertion, this reading''s classification would need revision toward the market_access sibling''s structure, with lower compliance-cost framing and more emphasis on voluntary standard adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Which textual and jurisprudential signals justify treating Article 3(2) as jurisdictional rather than access-conditional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__effects_jurisdiction_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(gdpr_tr_t2019, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(gdpr_tr_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(gdpr_tr_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(gdpr_tr_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2024, 0.27).
narrative_ontology:measurement(gdpr_tr_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(gdpr_be_t2019, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2019, 0.46).
narrative_ontology:measurement(gdpr_be_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2020, 0.49).
narrative_ontology:measurement(gdpr_be_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2022, 0.53).
narrative_ontology:measurement(gdpr_be_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2024, 0.56).
narrative_ontology:measurement(gdpr_be_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t2018, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(gdpr_su_t2019, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2019, 0.47).
narrative_ontology:measurement(gdpr_su_t2020, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(gdpr_su_t2022, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2022, 0.57).
narrative_ontology:measurement(gdpr_su_t2024, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement(gdpr_su_t2026, gdpr_article_3_scope__effects_jurisdiction_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__effects_jurisdiction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__effects_jurisdiction_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gdpr_article_3_scope kernel, decomposed per the epsilon-invariance principle because the three readings assign structurally different beneficiary/victim sets and different epsilon values to the same textual provision. effects_jurisdiction_reading (this story) treats Article 3(2) as protective jurisdiction over harm-to-EU-residents, with EU data subjects and domestic controllers as beneficiaries and non-EU targeting/monitoring firms as victims, epsilon=0.58 (tangled_rope). territorial_sovereignty_reading treats the same provision as exceeding legitimate territorial jurisdiction, structurally a snare-leaning claim from the excluded foreign-sovereign perspective. market_access_reading treats it as a voluntary standard-setting condition (Brussels Effect), structurally closer to a rope with lower coercive framing since firms 'choose' to meet the standard to access the market rather than being subject to an extraterritorial legal command.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
