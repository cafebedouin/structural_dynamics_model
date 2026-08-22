% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 as Conditional Market Access Requirement (Brussels Effect Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This story authors the market-access reading of GDPR Article 3's
 *   territorial scope: the view that Article 3(2)'s targeting/monitoring test
 *   functions as a condition on accessing the EU market and its residents'
 *   attention, not as a jurisdictional claim over conduct occurring wholly
 *   abroad. Under this reading the extraterritorial effect is a byproduct of
 *   standard diffusion (the Brussels Effect) rather than an assertion of
 *   sovereign authority beyond EU borders. Sibling readings of the same
 *   kernel (gdpr_article_3_scope) treat the same textual provision
 *   differently: the effects_jurisdiction_reading grounds extraterritorial
 *   reach in protecting EU residents wherever effects land, and the
 *   territorial_sovereignty_reading treats any extraterritorial application
 *   as exceeding legitimate regulatory authority. Each reading is authored as
 *   its own constraint story with its own ε; this file's ε (0.42) reflects a
 *   moderate, largely voluntary-adoption extraction profile appropriate to
 *   the market-access framing, distinctly lower than what an
 *   effects-jurisdiction reading would likely author, because this reading
 *   treats compliance as a strategic choice tied to a benefit (market access)
 *   rather than a coercive extension of law.
 *
 * KEY AGENTS:
 *   - eu_data_protection_authorities: agenda_setter (institutional/analytical) — administer the targeting test as a market-access toll
 *   - large_multinational_platforms: payer/beneficiary (powerful/mobile) — adopt GDPR globally as a strategic compliance-cost optimization
 *   - non_eu_smes_targeting_eu_market: payer (moderate/constrained) — bear compliance costs disproportionate to their capacity to absorb them
 *   - non_eu_firms_outside_natural_export_markets: payer (powerless/trapped) — swept in by the monitoring test without ever seeking EU market access
 *   - third_country_governments_asserting_sovereignty: excluded (institutional/constrained) — sovereignty objection has no forum under this reading's framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.42).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.38).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 as Conditional Market Access Requirement (Brussels Effect Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, 'be767b4d-ea88-4c0b-b560-fefa159c682a').
narrative_ontology:cs_kernel_codification('be767b4d-ea88-4c0b-b560-fefa159c682a', fixed_text).
narrative_ontology:cs_authority_grounding('be767b4d-ea88-4c0b-b560-fefa159c682a', extraction).
narrative_ontology:cs_interpretation_layer_present('be767b4d-ea88-4c0b-b560-fefa159c682a').
narrative_ontology:cs_reading_relation('be767b4d-ea88-4c0b-b560-fefa159c682a', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('be767b4d-ea88-4c0b-b560-fefa159c682a', gdpr_article_3_scope__territorial_sovereignty_reading, influences).
narrative_ontology:cs_axiom('be767b4d-ea88-4c0b-b560-fefa159c682a', foundational, market_participation_is_conditional_privilege).
narrative_ontology:cs_axiom_status(market_participation_is_conditional_privilege, holdable).
narrative_ontology:cs_axiom_grounding('be767b4d-ea88-4c0b-b560-fefa159c682a', market_participation_is_conditional_privilege, conventional).
narrative_ontology:cs_axiom('be767b4d-ea88-4c0b-b560-fefa159c682a', foundational, extraterritorial_effect_is_diffusion_not_assertion).
narrative_ontology:cs_axiom_status(extraterritorial_effect_is_diffusion_not_assertion, holdable).
narrative_ontology:cs_axiom_grounding('be767b4d-ea88-4c0b-b560-fefa159c682a', extraterritorial_effect_is_diffusion_not_assertion, instrumental).
narrative_ontology:cs_reference_frame('be767b4d-ea88-4c0b-b560-fefa159c682a', eu_internal_market_conditionality_framework).
narrative_ontology:cs_drift_state('be767b4d-ea88-4c0b-b560-fefa159c682a', post_2018_global_compliance_diffusion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('be767b4d-ea88-4c0b-b560-fefa159c682a', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_domiciled_compliant_firms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_residents).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_smes_targeting_eu_market).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_firms_outside_natural_export_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, large_multinational_platforms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, large_multinational_platforms).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_regulatory_diffusion_thesis).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, single_market_conditionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer Article 3(2)'s targeting/monitoring test as a condition of touching the EU market, not as a claim to govern conduct occurring entirely outside the EU. They issue guidance, levy fines against firms that choose to offer goods or services to EU residents, and treat compliance as the toll for market entry rather than an assertion of sovereign reach over foreign territory.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Already built to GDPR standard as a baseline cost of doing business in their home market. When foreign competitors must match that standard to sell into the EU, the competitive advantage of operating on a lower compliance cost base disappears, leveling the field in the firms' favor without new investment on their part.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_domiciled_compliant_firms, beneficiary,
    organized, generational, mobile, continental).

% Receive a uniform floor of data protection regardless of where the service provider is headquartered, because any firm that wants their business or attention must meet the standard. They did not have to organize or litigate to get this protection extended to services designed abroad.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_residents, beneficiary,
    organized, generational, arbitrage, continental).

% Choose to build one global compliance architecture to GDPR standard rather than segment by jurisdiction, because the EU market is large enough to make segmentation costlier than uniform compliance. This is presented as a voluntary strategic choice (the standard becomes their global default, extending EU-shaped rules to non-EU users as a byproduct), but the underlying driver is that opting out of the EU market is the only true exit, and for firms of this scale that exit is rarely taken.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, large_multinational_platforms, payer,
    powerful, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, large_multinational_platforms, beneficiary).

% Want to sell into the EU market but lack the legal and technical resources of large platforms to absorb compliance costs painlessly. For them the 'market access toll' framing is accurate in structure but expensive in practice: they can technically walk away from EU customers, but doing so forecloses a market often necessary for growth, so the choice functions closer to coercion than to free strategic adoption.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_smes_targeting_eu_market, payer,
    moderate, biographical, constrained, national).

% Get swept into monitoring-test liability because their analytics, advertising, or tracking technology incidentally profiles EU residents even without deliberate market targeting. From this seat the 'market access' framing understates their exposure — they never sought EU market access at all, yet face the same compliance bar as firms that did.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_firms_outside_natural_export_markets, payer,
    powerless, immediate, trapped, national).

% Watch firms in their own jurisdictions voluntarily adopt GDPR-equivalent standards to preserve EU market access, then often codify similar rules domestically once the compliance infrastructure already exists. They neither administer nor pay the constraint but their own regulatory agendas are structurally reshaped by its diffusion.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, third_country_regulators, observer,
    institutional, generational, analytical, global).

% Object in principle to a foreign regulator's rules determining compliance obligations for firms domiciled and operating in their own territory, but have no forum in which that objection is adjudicated as a jurisdictional dispute — the EU's own framing as 'market access, not extraterritorial assertion' forecloses the objection from being heard on sovereignty grounds within EU institutions.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, third_country_governments_asserting_sovereignty, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets one data protection floor for anyone who wants to transact with or monitor EU residents, so EU residents, EU regulators, and EU-domiciled firms need not negotiate protection standards separately with every foreign counterparty; a single conditional standard substitutes for thousands of bilateral or firm-specific negotiations.
% TRANSFER_FUNCTION: Moves compliance cost from EU-domiciled firms (who already bear it) toward foreign firms seeking EU market access or monitoring EU residents; moves regulatory-design influence toward EU institutions, whose standard becomes the default architecture adopted globally rather than negotiated jurisdiction by jurisdiction.
% ABSENT_VOICES: Third-country governments who view their domestic firms' compliance obligations as properly a matter of their own sovereign regulatory choice are not party to the EU's rulemaking or enforcement process; their objection is structurally unaddressable within the market-access framing, which treats the whole question as private commercial choice rather than an inter-state jurisdictional dispute.
% DISAPPEARANCE_RATIONALE: If Article 3(2)'s targeting/monitoring test vanished, foreign firms would face no EU-standard obligation merely from serving or monitoring EU residents; the Brussels Effect's primary transmission mechanism (uniform compliance for market access) would collapse, non-EU firms would likely re-segment services by jurisdiction, and the EU-domiciled firms' current competitive parity with foreign entrants would erode as those entrants reverted to home-jurisdiction standards.
% FOUNDING_PROBLEM: Before GDPR's extraterritorial scope, firms outside the EU could collect and monetize EU residents' data under lighter home-jurisdiction rules, undermining the protections EU-domiciled firms and residents were subject to and creating a competitiveness gap that punished EU compliance rather than rewarding it.
% FOUNDING_PROBLEM_CORROBORATION: Independent competition economists and comparative-law scholars outside the EU institutions (e.g. analyses of the Brussels Effect literature, notably Bradford's own work and subsequent independent replication studies of regulatory diffusion) corroborate that a real competitiveness/protection gap existed pre-2018 and that Article 3(2) measurably closed it for firms that chose to serve the EU market; third-country regulators who adopted similar frameworks afterward provide independent (if consequence-driven) corroboration that the underlying problem was real rather than merely EU-beneficiary-asserted.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).
:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) rather than high because, under this reading's own lights, the arrangement is structured as a conditional toll tied to a genuine benefit (market access) rather than a coercive extension of jurisdiction — firms retain a formal exit (decline to target/monitor EU residents). Suppression (0.38) and resistance (0.40) are moderate: enforcement exists and is real, but the story's own logic holds that most compliance is voluntary strategic adoption by firms seeking the EU market's benefits, not coerced submission to a foreign sovereign's direct command. Accessibility collapse (0.45) reflects that alternatives (segmenting services by jurisdiction) remain technically available but grow costlier as global compliance-by-default becomes normalized. Theater ratio stays low and rises only slightly (0.15 to 0.22) — the coordination function (a uniform protection floor) remains substantially real throughout the interval; there is little indication of hollowed-out performative compliance under this reading.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (EU regulators) and the eu_domiciled/eu_resident beneficiary seats experience this as legitimate conditional access-setting with a real coordination payoff. The powerless payer seat (firms swept in by the monitoring test without ever targeting the EU market) experiences something structurally closer to unbounded jurisdictional reach, because for them there was no market-access bargain to accept or decline — the engine's per-seat computation should register this asymmetry even though the story-level claim treats the arrangement as market-access conditionality throughout.
 *
 * DIRECTIONALITY LOGIC:
 *   EU data protection authorities and EU-domiciled firms/residents sit near the beneficiary end: the standard was built around their existing baseline and its export costs them little while raising rivals' costs. Large multinational platforms sit closer to symmetric — they pay compliance costs but capture a beneficiary-like position by exporting the resulting global standard as their own default, cementing incumbency advantage over smaller entrants who must catch up. Non-EU SMEs targeting the EU market sit toward the target end: real costs, constrained exit (abandoning EU customers is costly but possible). Firms swept in without ever targeting the EU market sit nearest full target: they receive no market-access benefit at all yet bear the compliance exposure, which is why an override may be warranted for that seat rather than relying purely on the beneficiary/victim + exit derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The market-access reading resists mislabeling the arrangement as pure extraction by insisting on the coordination half of the bargain: EU residents get a uniform protection floor, and firms that voluntarily seek EU market access get predictable compliance obligations instead of a patchwork of national and case-by-case negotiated terms. But the reading also resists treating the arrangement as pure coordination, because the beneficiary/victim asymmetry is real and structural — EU-domiciled incumbents did not have to change to benefit, while foreign entrants and especially firms with no EU market ambitions bear costs without a comparable payoff. The tangled_rope claim captures both halves: genuine coordination function (uniform floor, real reduction in negotiation costs) coexisting with asymmetric extraction (costs concentrated on non-EU firms, benefits concentrated on EU incumbents and regulators) that requires active enforcement (DPA fines, monitoring-test litigation) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_vs_coercion_boundary,
    'Is the compliance choice genuinely voluntary strategic adoption (as the market-access framing holds), or is it functionally coerced given the practical impossibility of segmenting global digital services by jurisdiction at scale?',
    'Empirical study of firms that actually chose jurisdictional segmentation over global GDPR-standard compliance post-2018, and the cost differential they faced relative to firms that adopted globally — a large gap would suggest coercion-in-practice rather than free strategic choice.',
    'If segmentation was genuinely a viable low-cost alternative for most affected firms, the market-access reading''s low suppression score is well-supported. If segmentation was prohibitively costly for all but the largest firms, effective suppression under this reading is understated and the constraint functions closer to the effects_jurisdiction sibling''s coercive framing even while retaining this reading''s formal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_coercion_boundary, empirical, 'Whether market-access framing''s voluntariness claim survives scrutiny of actual firm behavior.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the market-access reading the operative legal doctrine (as EU institutions and the European Data Protection Board''s own guidance frame it), or is it a post-hoc characterization that understates what is functionally an assertion of extraterritorial jurisdiction over foreign firms with no EU presence?',
    'Comparative analysis of enforcement actions against firms with zero EU market-entry intent (pure monitoring-test cases) versus firms actively seeking EU customers — if enforcement against the former group is substantial and treated identically to the latter, the market-access framing does not track actual doctrine and the effects_jurisdiction reading better describes enforcement practice.',
    'If enforcement practice does not distinguish market-seeking firms from purely-monitored-incidentally firms, this reading''s claimed_type and low suppression score would not survive contact with the effects_jurisdiction sibling''s evidence, and the two readings would need to be understood as competing legal theories with the effects_jurisdiction reading better fitting observed enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading actual EU enforcement practice more closely tracks.').

omega_variable(
    sovereignty_objection_forum_absence,
    'Does the absence of a forum for third-country sovereignty objections (documented in absent_voices) reflect the market-access reading''s structural correctness (there is genuinely no sovereignty question because compliance is voluntary commercial choice), or does it reflect institutional foreclosure of a legitimate objection that the territorial_sovereignty_reading would recognize?',
    'Track whether any third-country government successfully raises a formal sovereignty-based challenge to Article 3(2) application in an international forum (WTO, bilateral treaty arbitration) and whether that forum accepts the market-access characterization or treats it as jurisdictional overreach.',
    'A successful sovereignty-based challenge would validate the territorial_sovereignty_reading''s core premise and suggest the market-access reading''s framing has been serving as a legal shield against a substantively live objection rather than dissolving it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_objection_forum_absence, conceptual, 'Whether the absent sovereignty forum reflects genuine non-issue or institutional foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__market_access_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__market_access_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__market_access_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__market_access_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__market_access_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gdpr_tr_t24, gdpr_article_3_scope__market_access_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__market_access_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__market_access_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__market_access_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__market_access_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__market_access_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(gdpr_be_t24, gdpr_article_3_scope__market_access_reading, base_extractiveness, 24, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gdpr_article_3_scope__market_access_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__market_access_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gdpr_article_3_scope kernel. The effects_jurisdiction_reading authors extraterritorial reach as the doctrinal point (protecting EU residents wherever effects land), yielding a higher ε and higher suppression score appropriate to a coercive-jurisdiction framing. The territorial_sovereignty_reading authors the same textual provision as exceeding legitimate regulatory authority, with sovereignty itself as the contested value and non-EU sovereign states as the primary victim class rather than individual firms. All three share the same underlying legal text (Article 3(2) GDPR) but diverge in beneficiary/victim structure, extraction level, and claimed type because each reading fixes a different referent for what the arrangement is fundamentally doing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__market_access_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
