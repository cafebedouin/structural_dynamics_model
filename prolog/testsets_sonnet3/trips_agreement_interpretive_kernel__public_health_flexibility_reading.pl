% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Agreement — Public Health Flexibility Reading (Compulsory Licensing / Parallel Imports)
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   In 1994 the TRIPS Agreement bound all WTO members, rich and poor, to a
 *   common floor of patent protection for pharmaceuticals, a floor most
 *   developing countries had not previously enforced domestically. The
 *   HIV/AIDS crisis of the late 1990s exposed the gap between that floor and
 *   the ability of low- and middle-income states to afford patented
 *   antiretrovirals, producing sustained pressure that crystallized in the
 *   2001 Doha Declaration on TRIPS and Public Health: an authoritative
 *   political statement that the Agreement 'does not and should not prevent'
 *   members from taking measures to protect public health, and that each
 *   member has the right to determine what constitutes a national emergency.
 *   This reading treats that declaration, and the subsequent 2003/2005
 *   Article 31bis amendment permitting export-oriented compulsory licensing,
 *   as confirming that the original text always contained this broad
 *   flexibility rather than creating a new one.
 *
 * KEY AGENTS:
 *   - developing_country_health_ministries: agenda-setter and beneficiary — invokes the flexibility
 *   - generic_pharmaceutical_manufacturers: beneficiary — captures the license-enabled market
 *   - patients_in_low_and_middle_income_countries: beneficiary — receives affordable medicines but has no direct voice
 *   - originator_pharmaceutical_patent_holders: payer — loses exclusivity-based returns
 *   - pharmaceutical_industry_investors: payer — bears compressed expected returns
 *   - wto_dispute_settlement_body: observer — adjudicates individual disputes without settling the reading contest
 *   - originator_state_trade_representatives: excluded — pressures externally via bilateral channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.58).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.52).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Agreement — Public Health Flexibility Reading (Compulsory Licensing / Parallel Imports)").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e').
narrative_ontology:cs_kernel_codification('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', fixed_text).
narrative_ontology:cs_authority_grounding('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', distributed).
narrative_ontology:cs_reading_relation('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', foundational, sovereign_public_health_policy_space_primacy).
narrative_ontology:cs_axiom_status(sovereign_public_health_policy_space_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', sovereign_public_health_policy_space_primacy, conventional).
narrative_ontology:cs_axiom('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', secondary, flexibility_is_original_textual_feature_not_exception).
narrative_ontology:cs_axiom_status(flexibility_is_original_textual_feature_not_exception, holdable).
narrative_ontology:cs_axiom_grounding('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', flexibility_is_original_textual_feature_not_exception, conventional).
narrative_ontology:cs_reference_frame('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', doha_declaration_sovereign_flexibility_baseline).
narrative_ontology:cs_drift_state('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', post_covid_vaccine_access_contest, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3716c9a4-1dc4-4ae7-b3a0-8e713cb5694e', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developing_country_health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_low_and_middle_income_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_industry_investors).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_public_health_primacy_doctrine).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_flexibility_sovereign_policy_space_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes Article 31 compulsory licensing and Article 6 parallel import exhaustion to authorize domestic or imported generic production during health emergencies (HIV/AIDS, COVID-19, hepatitis). Administers the notification and compensation procedures TRIPS requires, and faces bilateral pressure (Special 301 listings, trade threats) from patent-holding states when it invokes these flexibilities aggressively.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developing_country_health_ministries, agenda_setter,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developing_country_health_ministries, beneficiary).

% Gains legal cover to manufacture and export patented medicines under compulsory license once a government invokes the flexibility, capturing market share and margin that would otherwise flow to the originator. Can relocate production across jurisdictions with favorable compulsory-licensing regimes; benefits directly from the reading's broad construal of 'national emergency' and 'other circumstances of extreme urgency.'
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Gains access to essential medicines at generic prices instead of originator prices, often the difference between treatment and no treatment. Has no direct voice in TRIPS negotiation or dispute settlement; benefits entirely through the health ministry's invocation of the flexibility on their behalf, and bears the consequences (continued high prices, rationed access) when a ministry declines to invoke it under political or trade pressure.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_low_and_middle_income_countries, beneficiary,
    powerless, biographical, trapped, national).

% Holds the patent that a compulsory license overrides, losing exclusivity-based pricing power and a portion of expected returns in the licensed market. Can lobby home-state trade representatives to pressure the licensing government, litigate at the WTO if flexibility use is contested as exceeding Article 31/31bis limits, or accept negotiated royalty compensation — but cannot unilaterally block a properly noticed compulsory license under this reading of the text.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders, payer,
    institutional, biographical, constrained, global).

% Prices in expected exclusivity-period returns when funding drug development; broad compulsory-licensing practice compresses realized returns in markets where flexibilities are invoked, which this reading treats as an accepted policy tradeoff rather than a defect. Can reallocate capital toward markets or therapeutic areas less exposed to compulsory licensing risk.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_industry_investors, payer,
    powerful, biographical, mobile, global).

% Adjudicates disputes over whether a given compulsory license or parallel import exceeds the text's flexibility allowances. Its rulings do not resolve which reading of TRIPS is correct in the abstract; they resolve individual disputes under whichever reading the panel applies, and its case-by-case rulings are the terrain on which this reading and the strong_exclusivity_reading compete for ongoing legitimacy.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_body, observer,
    institutional, generational, analytical, global).

% Represents patent-holding-industry home states in bilateral trade relationships; would prefer the strong_exclusivity_reading be the operative interpretation but has no formal seat inside the flexibility-invocation procedure itself — its influence is exercised externally, through bilateral pressure and trade-agreement side-letters that narrow flexibility use without amending the TRIPS text this reading is grounded in.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_state_trade_representatives, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, diffuse).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared floor of patent protection across WTO members while reserving explicit policy space (Articles 31, 31bis, 6) for governments to override that protection domestically when public health requires it — solving the problem of how a single multilateral IP treaty can bind rich and poor member states with radically different capacities to pay for patented medicines.
% TRANSFER_FUNCTION: Moves pricing power and expected patent-period returns from originator patent holders (and their investors) to generic manufacturers and, ultimately, to patients and health systems in states that invoke compulsory licensing or parallel import — the transfer is triggered by a state's own declaration of necessity, not by unilateral originator concession.
% ABSENT_VOICES: Originator-state trade representatives and industry associations are structurally outside the compulsory-licensing procedure itself (it is a domestic administrative act by the licensing government); they respond by shifting the contest to bilateral trade pressure and TRIPS-plus provisions in regional agreements, which this reading treats as extrinsic to the text rather than as evidence against the flexibility reading.
% DISAPPEARANCE_RATIONALE: If this reading's flexibilities disappeared and only the strong_exclusivity_reading operated, the wave of compulsory licenses that expanded HIV/AIDS antiretroviral access in the 2000s and enabled COVID-era vaccine and treatment scale-up would have been legally foreclosed; health ministries would lose their primary lawful lever against patent-holder pricing, and generic manufacturers' single largest legal justification for cross-border production would vanish.
% FOUNDING_PROBLEM: The original TRIPS text (1994) was drafted primarily around industrial-country IP-harmonization interests; the flexibility reading crystallized as a corrective response to the demonstrated failure of strict patent exclusivity to deliver affordable medicines during the HIV/AIDS crisis, culminating in the 2001 Doha Declaration on TRIPS and Public Health.
% FOUNDING_PROBLEM_CORROBORATION: WHO and UNAIDS technical reporting, independent public-health economists, and the Doha Declaration's own negotiating record (endorsed by consensus of the full WTO membership, not only generic-producing or low-income states) attest that access-to-medicines gaps remain a live problem this reading continues to address; this corroboration comes from parties outside the direct beneficiary set (generic manufacturers, health ministries) and includes multilateral health agencies with an institutional mandate independent of TRIPS itself.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored moderate-high, not low: this reading redistributes real economic value away from patent holders toward generic manufacturers and health systems, and that redistribution is the reading's explicit point, not an incidental byproduct — hence tangled_rope rather than rope. Suppression (0.52) reflects that invoking the flexibility still requires overcoming bilateral trade pressure, Special 301 listing risk, and TRIPS-plus provisions in regional trade agreements that narrow the practical space the text formally grants; the flexibility is real but not frictionless. Theater ratio (0.28) is moderate-low: most compulsory licenses that are formally issued do result in real generic production and real price reductions, though a subset of threatened-but-unused licenses function mainly as negotiating leverage against originators (a genuine but partial theatrical layer). Accessibility collapse (0.40) is authored low-moderate because alternatives to invoking the flexibility remain visible and used — voluntary licensing negotiations, tiered pricing deals — so the constraint has not closed off all other paths. Resistance (0.72) is high because originator interests actively contest nearly every high-profile invocation through litigation threats, diplomatic pressure, and industry lobbying for narrower interpretation in future trade agreements.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing-country health ministries and generic manufacturers sit near the beneficiary end: they gain negotiating leverage and market access respectively, and their exit options (constrained but self-directed for ministries; mobile for manufacturers who can relocate production) keep directionality low. Patients are structurally powerless beneficiaries with trapped exit — they cannot invoke the flexibility themselves and depend entirely on ministry action, which is why their situation notes the asymmetry even though their role is beneficiary. Originator patent holders and their investors sit near the target end: the compulsory license is imposed on them by a government decision they cannot unilaterally block, and their only recourse (WTO litigation, bilateral pressure) operates outside the licensing procedure itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored live (not dead), which is the mandatrophy-relevant fact here: unlike a scaffold whose transitional justification has expired, this reading's coordination function — reconciling a uniform IP floor with divergent national capacity to pay for medicines — remains actively invoked (COVID-19 vaccine and treatment access fights extended rather than resolved it). The classification as tangled_rope rather than snare reflects that this is not disguised extraction from patients or ministries; it is a real coordination structure (a shared multilateral IP floor) that also produces genuine, asymmetric extraction from patent holders when invoked — both halves of the tangled_rope gate are structurally present and neither should be argued away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_public_health_vs_strong_exclusivity,
    'Does the TRIPS text itself embed broad public-health flexibility as an original feature, or does the flexibility exist only insofar as the Doha Declaration and Article 31bis amendment added a permissive gloss to a text that was originally drafted for high uniform protection?',
    'This is a live interpretive dispute internal to the kernel, not resolvable by external data alone; the sibling story strong_exclusivity_reading holds the opposing premise (the flexibilities are narrow exceptions bolted onto a fundamentally protective text). Resolution would require either a definitive WTO Appellate Body ruling squarely addressing original textual intent (unlikely given Appellate Body paralysis since 2019) or a change in dominant state practice that one reading or the other could claim as confirming custom.',
    'If the strong_exclusivity_reading is the structurally correct one, this story''s beneficiary/victim assignment inverts in significance: the flexibility becomes a contested carve-out rather than a core feature, and its extractiveness score would be read as measuring deviation from the treaty''s true baseline rather than the treaty''s true operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_public_health_vs_strong_exclusivity, conceptual, 'Whether public-health flexibility is original to TRIPS or a later permissive gloss — the core kernel contest.').

omega_variable(
    bilateral_trips_plus_erosion,
    'To what extent do bilateral and regional trade agreements (TRIPS-plus provisions in US and EU free trade agreements) functionally narrow this reading''s flexibilities below what the multilateral text formally permits?',
    'Comparative analysis of compulsory-licensing and data-exclusivity provisions across a sample of US/EU bilateral trade agreements with developing-country partners, measured against baseline TRIPS/Doha flexibility, tracking whether signatory states'' actual invocation rates of Article 31 declined post-agreement.',
    'If TRIPS-plus provisions substantially narrow practical flexibility, the effective suppression this reading experiences is higher than the multilateral text alone suggests, and the true locus of the strong_exclusivity_reading''s operative power may lie in bilateral instruments rather than in TRIPS interpretation itself — a separate constraint that this story''s network links should capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_trips_plus_erosion, empirical, 'Whether bilateral TRIPS-plus agreements erode this reading''s flexibilities in practice.').

omega_variable(
    compulsory_license_use_vs_threat_ratio,
    'What proportion of the flexibility''s practical effect comes from actual compulsory licenses issued and executed, versus the credible threat of compulsory licensing used as leverage in voluntary price negotiations with originators?',
    'Comparative dataset of formally issued compulsory licenses (WHO/WTO TRIPS notifications) against publicly reported voluntary licensing and tiered-pricing agreements reached under explicit threat of compulsory licensing, coded for temporal proximity and negotiating context.',
    'A high threat-to-use ratio would support a higher theater_ratio reading than authored here — much of the flexibility''s real-world effect would be a bargaining chip rather than executed extraction — while a low ratio confirms the moderate theater_ratio currently authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compulsory_license_use_vs_threat_ratio, empirical, 'Whether the flexibility operates mainly through execution or through negotiating threat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language label 'the TRIPS Agreement' per the ε-invariance principle: public_health_flexibility_reading (this story — moderate-high ε from the flexibility-invoking state's own lights, tangled_rope), strong_exclusivity_reading (a sibling story authoring high uniform protection as the textual baseline, expected higher ε against generic manufacturers and lower against patent holders), and dispute_settlement_interpretive_authority (a structurally distinct claim about who authoritatively interprets the text, not what the text says — kept as a separate story because conflating interpretive authority with interpretive content would violate the single-claim-per-story discipline). All three are linked bidirectionally via affects_constraints because each reading's practical force depends partly on how WTO dispute panels rule in individual cases, and panel rulings in turn shape which reading gains ground in subsequent state practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
