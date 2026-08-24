% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope as Conditional Market Access (Brussels Effect Reading)
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story captures the 'market access reading' of GDPR
 *   Article 3 territorial scope: the Regulation operates as a conditional
 *   entry requirement for the EU digital market, not as a jurisdictional
 *   assertion over foreign sovereign territory. Compliance is a rational
 *   market strategy for firms seeking EU users; the extraterritorial reach is
 *   the Brussels Effect — unilateral regulatory diffusion via market power.
 *   This reading produces lower enforcement tension than the effects
 *   jurisdiction reading because firms self-select into compliance; the
 *   constraint persists because the EU market is too valuable to exit, not
 *   because enforcement machinery hunts down non-compliant actors globally.
 *   The beneficiary is EU regulatory influence; the extraction falls on
 *   non-EU firms bearing compliance costs as the price of market access.
 *
 * KEY AGENTS:
 *   - eu_data_protection_authorities: Primary agenda_setter (institutional/analytical) — enforces, coordinates, sets interpretive agenda
 *   - eu_regulatory_institutions: Primary beneficiary (institutional/arbitrage) — gains global standard-setting power without treaties
 *   - eu_tech_firms: Beneficiary/payer (organized/constrained) — home advantage, competitive moat, but also bears costs
 *   - non_eu_large_tech_firms: Primary payer (powerful/constrained) — absorbs compliance as market entry fee, lobbies for friction reduction
 *   - non_eu_sme_firms: Primary payer (moderate/trapped) — disproportionate burden, often exits via geo-fencing
 *   - global_data_subjects: Beneficiary (organized/mobile) — receives privacy rights as positive externality
 *   - non_eu_governments: Excluded (institutional/constrained) — sovereignty concerns, no voice in standard-setting
 *   - privacy_activists_civil_society: Observer (moderate/analytical) — strategic litigation, advocacy, no direct stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.42).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.35).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope as Conditional Market Access (Brussels Effect Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, 'de5d81e5-3934-4751-8121-1eca225f46e1').
narrative_ontology:cs_kernel_codification('de5d81e5-3934-4751-8121-1eca225f46e1', formalized).
narrative_ontology:cs_authority_grounding('de5d81e5-3934-4751-8121-1eca225f46e1', lineage).
narrative_ontology:cs_interpretation_layer_present('de5d81e5-3934-4751-8121-1eca225f46e1').
narrative_ontology:cs_reading_relation('de5d81e5-3934-4751-8121-1eca225f46e1', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('de5d81e5-3934-4751-8121-1eca225f46e1', gdpr_article_3_scope__territorial_sovereignty_reading, influences).
narrative_ontology:cs_axiom('de5d81e5-3934-4751-8121-1eca225f46e1', foundational, brussels_effect_standard_diffusion).
narrative_ontology:cs_axiom_status(brussels_effect_standard_diffusion, holdable).
narrative_ontology:cs_axiom_grounding('de5d81e5-3934-4751-8121-1eca225f46e1', brussels_effect_standard_diffusion, conventional).
narrative_ontology:cs_axiom('de5d81e5-3934-4751-8121-1eca225f46e1', foundational, market_access_conditionality_as_coordination).
narrative_ontology:cs_axiom_status(market_access_conditionality_as_coordination, holdable).
narrative_ontology:cs_axiom_grounding('de5d81e5-3934-4751-8121-1eca225f46e1', market_access_conditionality_as_coordination, instrumental).
narrative_ontology:cs_reference_frame('de5d81e5-3934-4751-8121-1eca225f46e1', gdpr_market_access_baseline_2018).
narrative_ontology:cs_drift_state('de5d81e5-3934-4751-8121-1eca225f46e1', post_schrems_ii_adequacy_evolution, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('de5d81e5-3934-4751-8121-1eca225f46e1', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_institutions).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_tech_firms).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, global_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_large_tech_firms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_sme_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, eu_tech_firms).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_standard_diffusion).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, market_access_conditionality_as_coordination).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, regulatory_interoperability_via_unilateral_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce GDPR against any entity processing EU residents' data, regardless of location. Issue fines up to 4% global revenue. Coordinate through EDPB to harmonize interpretation. Their authority derives from EU treaties; they gain global regulatory influence when non-EU firms comply voluntarily to access the EU market.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% The European Commission and Parliament gain standard-setting power without treaty negotiation. Other jurisdictions adopt GDPR-like laws (Brazil LGPD, California CCPA, India DPDP) creating de facto EU regulatory hegemony. They collect no direct revenue but accrue geopolitical influence and legislative first-mover advantage.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Home-field advantage: EU firms internalize compliance costs earlier and shape the standard through lobbying. They pay compliance costs but gain competitive moat against non-EU rivals who face higher adaptation costs. Their exit is constrained by being EU-incorporated.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_tech_firms, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, eu_tech_firms, payer).

% Meta, Google, Amazon, Microsoft, etc. Bear massive compliance costs (DPOs, data mapping, transfer mechanisms, fines risk). But EU market is too large to exit; compliance is a market entry fee. They lobby for adequacy decisions and transfer frameworks to reduce friction. Their power lets them absorb costs smaller firms cannot.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_large_tech_firms, payer,
    powerful, biographical, constrained, global).

% Small non-EU firms face disproportionate compliance burden: legal counsel, technical measures, EU representative appointment. Many simply block EU users (geo-fencing) rather than comply — effectively excluded from the market. No lobby presence in Brussels. Exit means abandoning EU revenue entirely.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_sme_firms, payer,
    moderate, biographical, trapped, global).

% Receive enhanced privacy rights (access, erasure, portability) globally because firms apply GDPR standards universally for operational simplicity. But enforcement against non-EU violators remains difficult; rights are real but remedial access is uneven. Can switch services but face network effects.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, global_data_subjects, beneficiary,
    organized, biographical, mobile, global).

% Sovereignty concerns: foreign regulation effectively governs domestic firms' data practices. Some retaliate (China PIPL, Russia localization) or negotiate adequacy (Japan, UK, Canada). Not consulted in GDPR drafting; their firms bear costs without representation. Structural exclusion from the standard-setting process.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_governments, excluded,
    institutional, generational, constrained, national).

% Advocate for stronger enforcement, broader territorial scope, and replication globally. Monitor DPAs, file strategic complaints (noyb, EPIC), shape public discourse. Neither collect rents nor pay compliance costs; their leverage is reputational and litigation-based.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, privacy_activists_civil_society, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, high-baseline global privacy standard that reduces regulatory fragmentation and transaction costs for cross-border data flows. Firms comply once for EU market access and extend compliance globally (Brussels Effect), creating de facto harmonization without treaty negotiation.
% TRANSFER_FUNCTION: Moves compliance costs (legal, technical, operational) from non-EU firms — especially SMEs — to EU regulatory institutions in the form of standard-setting power and geopolitical influence. EU firms gain competitive moat. Data subjects receive privacy protections as positive externality.
% ABSENT_VOICES: Non-EU SMEs that geo-fence EU users rather than comply — they are structurally excluded from the market and the policy conversation. Developing country digital economies that lose investment when firms avoid GDPR-like regulatory burdens. Their absence is not accidental: the market access mechanism selects for firms with compliance capacity.
% DISAPPEARANCE_RATIONALE: If the conditional market access mechanism vanished, global privacy standardization would fragment. Jurisdictions would compete on laxity (race to bottom) or erect incompatible walls (splinternet). The Brussels Effect — unilateral regulatory projection via market power — would collapse as a governance model. Cross-border data flows would face higher legal uncertainty and compliance multiplicity.
% FOUNDING_PROBLEM: Pre-GDPR: fragmented national implementations of the 1995 Data Protection Directive created regulatory arbitrage, inadequate protection for cross-border flows, and enforcement gaps. EU needed a unified regulation with direct effect that could project a single standard globally via its market size — solving coordination failure in transatlantic and global data governance.
% FOUNDING_PROBLEM_CORROBORATION: Bradford (2020) 'The Brussels Effect' documents the market access mechanism from outside EU institutions. IAPP annual surveys show 70%+ of non-EU firms extend GDPR compliance globally. European Commission adequacy decisions (Japan, UK, Canada, Korea) confirm third countries adopt EU standards for market access. These sources are not EU DPAs or EU firms.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects real compliance costs transferred from non-EU firms to EU regulatory influence — but lower than effects jurisdiction reading because firms voluntarily incur costs for market access. Suppression (0.35) is moderate: geo-fencing is a real exit option (unlike effects jurisdiction where jurisdiction follows the person), but the EU market size makes exit costly. Theater ratio (0.18) is low: the privacy standard has genuine coordination function (interoperability, adequacy decisions, global convergence). Accessibility collapse (0.45) is partial: alternatives exist (don't serve EU, build separate stack) but are economically painful. Resistance (0.48) is moderate: industry lobbying (Schrems II, transfer mechanisms) but also widespread voluntary adoption. The claimed_type 'tangled_rope' captures the hybrid: genuine coordination (global privacy baseline) + asymmetric extraction (non-EU firms pay, EU gains influence) + active enforcement (fines, EDPB coordination).
 *
 * PERSPECTIVAL GAP:
 *   From the EU DPA seat: the constraint is a rope — genuine coordination solving a real collective action problem (fragmented privacy laws), minimal coercion because firms want EU market. From the non-EU SME seat: it is a snare — the coordination story is cover; the cost of compliance is existential, exit is geo-fencing (market exclusion), and they had no voice in the standard. From the non-EU large tech seat: it is a tangled rope — they coordinate (global privacy baseline reduces their own fragmentation costs) but extract (they pay disproportionate compliance costs that cement their moat against smaller rivals). The engine computes these divergences from the structural data: power/exit asymmetry + beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   EU DPAs and EU institutions are structural beneficiaries (d ~ 0.1-0.2): they gain influence, collect fines, set global agenda. EU firms are near-symmetric beneficiaries with payer secondary role (d ~ 0.35): they pay compliance but gain competitive advantage. Non-EU large tech are payers with constrained exit (d ~ 0.7): they pay the transfer but cannot credibly exit the EU market. Non-EU SMEs are trapped payers (d ~ 0.9): compliance cost exceeds their capacity, exit is market abandonment. Global data subjects are beneficiaries with mobile exit (d ~ 0.2): they gain rights but can switch services. Non-EU governments are excluded (no d derivation — they are not governed by the constraint but by its spillovers). The derivation chain: beneficiary/victim declarations + power atoms + exit options → directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented privacy laws, regulatory arbitrage) remains live — cross-border data flows still need coordination, and regulatory fragmentation persists outside the EU adequacy network. The arrangement has not outlived its function; if anything, the coordination need has grown with AI training data flows. However, the extraction asymmetry (SME exclusion, large tech moat) has intensified since 2018. This is not mandatrophy (function persists) but a tangled_rope whose extraction component has grown while coordination remains genuine. The mandate is not resolved; the tension is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_effects_jurisdiction_boundary,
    'Is the market access reading structurally distinct from the effects jurisdiction reading, or do they converge in practice on the same compliance obligations?',
    'Compare CJEU case law (Google LLC v. CNIL, Meta v. Bundeskartellamt) for language: does the Court ground extraterritoriality in market access (Brussels Effect) or in effects jurisdiction (targeting test)? Track legislative drafting history of Article 3(2) recitals.',
    'If readings converge, this constraint story duplicates effects_jurisdiction_reading with different framing — ε-invariance violation. If distinct, market access reading has lower suppression (voluntary compliance) and different beneficiary structure (EU influence vs. EU resident protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_effects_jurisdiction_boundary, conceptual, 'Whether the two readings instantiate different constraints or the same constraint with different framings.').

omega_variable(
    sme_exclusion_severity,
    'What proportion of non-EU SMEs geo-fence EU users vs. comply, and does this exclusion constitute a structural market distortion?',
    'Empirical studies of geo-fencing rates (e.g., ''GDPR and the Death of the Small Business'' literature), European Commission SME compliance cost surveys, WTO trade in services data on digital market access.',
    'If exclusion is widespread and systematic, the constraint operates as a snare for SMEs (coordination story is cover for market closure). If rare, the tangled_rope classification holds — coordination function dominates for most actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sme_exclusion_severity, empirical, 'Whether SME exclusion is a marginal edge case or a structural feature of the market access mechanism.').

omega_variable(
    enforcement_tension_measurement,
    'How much lower is enforcement tension really under the market access reading vs. effects jurisdiction reading?',
    'Compare DPA enforcement actions against non-EU firms: frequency, fine amounts, cooperation requests. Track Schrems II aftermath — did firms treat SCCs as market access compliance or jurisdictional submission?',
    'If enforcement tension is not meaningfully lower, the ''lower tension'' claim in this reading is narrative, not structural. The constraint would be more extractive (higher effective suppression) than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_tension_measurement, empirical, 'Whether the market access framing actually reduces coercive enforcement pressure on non-EU firms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 2016, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_a3_market_tr_t2016, gdpr_article_3_scope__market_access_reading, theater_ratio, 2016, 0.05).
narrative_ontology:measurement(gdpr_a3_market_tr_t2018, gdpr_article_3_scope__market_access_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(gdpr_a3_market_tr_t2019, gdpr_article_3_scope__market_access_reading, theater_ratio, 2019, 0.15).
narrative_ontology:measurement(gdpr_a3_market_tr_t2020, gdpr_article_3_scope__market_access_reading, theater_ratio, 2020, 0.16).
narrative_ontology:measurement(gdpr_a3_market_tr_t2021, gdpr_article_3_scope__market_access_reading, theater_ratio, 2021, 0.17).
narrative_ontology:measurement(gdpr_a3_market_tr_t2022, gdpr_article_3_scope__market_access_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(gdpr_a3_market_tr_t2023, gdpr_article_3_scope__market_access_reading, theater_ratio, 2023, 0.18).
narrative_ontology:measurement(gdpr_a3_market_tr_t2024, gdpr_article_3_scope__market_access_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(gdpr_a3_market_be_t2016, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2016, 0.15).
narrative_ontology:measurement(gdpr_a3_market_be_t2018, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(gdpr_a3_market_be_t2019, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2019, 0.38).
narrative_ontology:measurement(gdpr_a3_market_be_t2020, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(gdpr_a3_market_be_t2021, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2021, 0.41).
narrative_ontology:measurement(gdpr_a3_market_be_t2022, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2022, 0.42).
narrative_ontology:measurement(gdpr_a3_market_be_t2023, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2023, 0.42).
narrative_ontology:measurement(gdpr_a3_market_be_t2024, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_a3_market_su_t2016, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2016, 0.1).
narrative_ontology:measurement(gdpr_a3_market_su_t2018, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2018, 0.28).
narrative_ontology:measurement(gdpr_a3_market_su_t2019, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2019, 0.3).
narrative_ontology:measurement(gdpr_a3_market_su_t2020, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(gdpr_a3_market_su_t2021, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2021, 0.33).
narrative_ontology:measurement(gdpr_a3_market_su_t2022, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2022, 0.34).
narrative_ontology:measurement(gdpr_a3_market_su_t2023, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2023, 0.35).
narrative_ontology:measurement(gdpr_a3_market_su_t2024, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__market_access_reading, 0.03).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, eu_adequacy_decision_framework).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, global_data_transfer_mechanisms).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, national_privacy_law_convergence).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, eu_digital_markets_act_interoperability).

% DUAL FORMULATION NOTE:
% This constraint (market_access_reading) and effects_jurisdiction_reading form a constraint family decomposing the kernel 'GDPR Article 3 territorial scope.' The market access reading has lower ε (0.42 vs. estimated 0.55+ for effects jurisdiction) because compliance is framed as voluntary market strategy. The territorial_sovereignty_reading is a rejection constraint (ε ≈ 0 for its proponents) that exerts countervailing pressure. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__market_access_reading, institutional, 0.15).
constraint_indexing:directionality_override(gdpr_article_3_scope__market_access_reading, powerful, 0.68).
constraint_indexing:directionality_override(gdpr_article_3_scope__market_access_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
