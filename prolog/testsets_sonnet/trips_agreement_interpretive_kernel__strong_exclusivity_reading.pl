% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading — Uniform Patent Protection as Innovation Incentive
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the strong-exclusivity reading of the TRIPS
 *   interpretive kernel: the position that the treaty text mandates high,
 *   uniform patent protections and that the compulsory-licensing and
 *   parallel-import provisions should be read as narrow exceptions rather
 *   than broad public-health tools. This is the reading favored by originator
 *   pharmaceutical firms and the high-income states whose trade delegations
 *   drafted and continue to defend it, including through TRIPS-plus
 *   provisions in subsequent bilateral agreements that narrow flexibilities
 *   beyond what the base text requires. The sibling reading —
 *   public_health_flexibility_reading — reads the same text (particularly as
 *   clarified by the 2001 Doha Declaration) as embedding broad
 *   compulsory-licensing and parallel-import rights whenever public health is
 *   at stake. A third sibling, dispute_settlement_interpretive_authority,
 *   concerns which body has the binding power to resolve the gap between
 *   these two readings. Each is authored as a separate constraint with its
 *   own ε per the ε-invariance principle; this file does not average across
 *   them.
 *
 * KEY AGENTS:
 *   - originator_pharmaceutical_firms: primary beneficiary (institutional/arbitrage) — collects monopoly rents under the exclusivity term
 *   - high_income_state_trade_negotiators: agenda-setter (institutional/arbitrage) — drafts and defends the strong reading, extends it via bilateral TRIPS-plus terms
 *   - low_income_state_health_ministries: primary payer (moderate/constrained) — bears procurement costs, faces retaliation risk if it tests the narrow flexibility gate
 *   - patients_without_treatment_access: ultimate victim (powerless/trapped) — bears the terminal cost of the price the constraint sets
 *   - public_health_advocacy_coalitions: excluded voice — objects but holds no interpretive vote
 *   - wto_dispute_settlement_body: co-administrator — determines in practice how narrow 'narrow' is
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading — Uniform Patent Protection as Innovation Incentive").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'fe1fd87f-0b6b-4fb8-8512-52a213c9f06b').
narrative_ontology:cs_kernel_codification('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', fixed_text).
narrative_ontology:cs_authority_grounding('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', extraction).
narrative_ontology:cs_interpretation_layer_present('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b').
narrative_ontology:cs_reading_relation('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', foundational, uniform_strong_exclusivity_maximizes_global_innovation).
narrative_ontology:cs_axiom_status(uniform_strong_exclusivity_maximizes_global_innovation, holdable).
narrative_ontology:cs_axiom_grounding('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', uniform_strong_exclusivity_maximizes_global_innovation, empirically_contingent).
narrative_ontology:cs_axiom('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', foundational, flexibilities_are_narrow_exceptions_not_embedded_rights).
narrative_ontology:cs_axiom_status(flexibilities_are_narrow_exceptions_not_embedded_rights, holdable).
narrative_ontology:cs_axiom_grounding('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', flexibilities_are_narrow_exceptions_not_embedded_rights, conventional).
narrative_ontology:cs_reference_frame('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', post_uruguay_round_uniform_minimum_standards).
narrative_ontology:cs_drift_state('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', post_doha_declaration_and_covid_waiver_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('fe1fd87f-0b6b-4fb8-8512-52a213c9f06b', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_state_trade_negotiators).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, biotech_investors).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_state_health_ministries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_without_treatment_access).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_exclusivity_drives_pharmaceutical_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold patent portfolios protected under the 20-year uniform minimum term and narrow compulsory-licensing exceptions. Collect monopoly-priced revenue in markets worldwide, including in states where per-capita income makes the drug price a multiple of median annual earnings. Can relocate R&D investment and lobby through home-state trade delegations; face no meaningful exit pressure from the constraint itself since it is built to their specification.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Drafted and continue to defend the strong-exclusivity text at WTO renegotiation rounds and in bilateral trade agreements (TRIPS-plus provisions) that narrow flexibilities further than the base text requires. Use market access as leverage to extract stricter enforcement from trading partners. Bear none of the health-access costs directly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_state_trade_negotiators, agenda_setter,
    institutional, generational, arbitrage, global).

% Price pharmaceutical R&D investment on the assumption of long, strongly enforced exclusivity windows. Capital is mobile across jurisdictions and asset classes; the constraint underwrites expected returns but is not the only mechanism by which returns could be secured (data exclusivity, market-based pricing, prizes are substitutes debated in the literature).
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, biotech_investors, beneficiary,
    organized, biographical, mobile, global).

% Must procure patented medicines at prices set by monopoly holders or attempt compulsory licensing procedures that this reading construes narrowly — requiring case-by-case justification, subject to trade retaliation threats, and administratively burdensome relative to ministry capacity. National formularies are constrained by what the state can afford under the exclusivity regime; exit would mean violating treaty obligations and risking WTO dispute sanctions or bilateral trade consequences.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_state_health_ministries, payer,
    moderate, immediate, constrained, national).

% Barred from manufacturing and exporting bioequivalent versions of patented drugs during the exclusivity term except through the narrow compulsory-licensing gate. Lose the market-entry window that would let them compete on production efficiency. Some diversify into off-patent lines or biosimilars, but the core business model this reading forecloses is patent-window generic entry.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, payer,
    moderate, biographical, trapped, national).

% Face treatment unavailability or catastrophic out-of-pocket costs for patented medicines their national health system cannot afford at monopoly prices. Have no individual exit — cannot relocate to a jurisdiction with different pricing, cannot substitute an unpatented equivalent when none exists, and depend entirely on state-level compulsory licensing action they cannot themselves initiate.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_without_treatment_access, payer,
    powerless, immediate, trapped, local).

% Adjudicates disputes over whether a state's compulsory licensing or parallel import action complies with the text. Its rulings determine how narrowly or broadly the flexibilities are actually applied in practice, making it a co-administrator of this reading's real-world force even though it does not itself hold patents or set prices.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_body, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_body, agenda_setter).

% Argue the compulsory-licensing and parallel-import flexibilities the text already contains (Doha Declaration clarifications) should be read broadly, not narrowly. Participate in WTO ministerial side events and file amicus submissions but hold no vote in treaty interpretation or dispute panel composition; their objections are documented but structurally non-binding on this reading's administrators.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, originator_pharmaceutical_firms).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine cross-border problem: without some minimum patent protection, firms undertaking multi-billion-dollar, decade-long pharmaceutical R&D could see innovations copied immediately upon market entry, undercutting the incentive to invest in discovery at all. Uniform minimum standards prevent a race-to-the-bottom where states compete for generic manufacturing by refusing to recognize any foreign patents.
% TRANSFER_FUNCTION: Moves consumer surplus and public health budgets from drug purchasers (states, insurers, patients) to patent-holding firms and their shareholders, for the duration of the exclusivity term, at a rate determined by the patent holder's pricing power rather than by production cost or ability to pay.
% ABSENT_VOICES: Patients in the states most affected by high drug prices are not signatories to any treaty and have no seat at WTO ministerial negotiations or dispute panels. Public health advocacy coalitions submit commentary but do not vote on interpretation. Generic manufacturers in the Global South influence national negotiating positions only indirectly, through domestic political pressure that is itself constrained by trade-retaliation threats from high-income partners.
% DISAPPEARANCE_RATIONALE: If the strong-exclusivity reading were replaced overnight by the broad-flexibility reading, low-income states would issue compulsory licenses far more readily, generic manufacturers would enter patented-drug markets on a faster timeline, drug prices in affected markets would fall substantially within a few years, and originator firms would restructure pricing and R&D-financing models (potentially shifting toward tiered pricing, prize mechanisms, or advance market commitments) to preserve returns under a lower-exclusivity regime.
% FOUNDING_PROBLEM: In the 1980s-90s, pharmaceutical and other IP-intensive industries argued that weak or absent patent protection in many developing-country markets meant firms captured little value from innovations sold internationally, discouraging R&D investment and enabling free-riding by states that manufactured generics without contributing to development costs.
% FOUNDING_PROBLEM_CORROBORATION: Originator firms and their home-state trade delegations attest the incentive problem remains live and requires strong, narrowly-construed protections. Independent health economists (e.g., WHO Commission on Intellectual Property, Innovation and Public Health; MSF Access Campaign analyses) and multiple low-income state health ministries attest that the actual innovation-incentive effect of TRIPS-level protection in the poorest markets is empirically weak — those markets contribute negligibly to global pharmaceutical revenue — while the access-cost effect is severe and immediate, suggesting the strong-exclusivity construction serves rent extraction from middle-income markets more than it serves the stated innovation-incentive rationale in the markets where the flexibility fight is actually fought.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at 2025) and has risen steadily since 1995 because the strong-exclusivity reading has been reinforced over time by bilateral TRIPS-plus agreements that layer additional exclusivity (data exclusivity, patent-term extensions, evergreening tolerance) on top of the base treaty floor — a genuine accumulation, not a static extraction. Suppression is substantial (0.72) and has hardened over the interval: compulsory-licensing attempts by states (South Africa 1997-2001, Brazil, India, more recently several African states during COVID-19) have consistently triggered trade-pressure responses from patent-holder home states, which is the enforcement machinery that keeps this reading operative rather than merely rhetorical. Theater ratio is comparatively low (0.28) because the underlying enforcement (dispute panels, Section 301 pressure, bilateral trade leverage) is functionally real, not performative — this is a live, actively-defended reading, not a decayed one. Accessibility collapse (0.62) reflects that once a state accedes to WTO membership, alternatives to some baseline patent recognition are largely foreclosed, though the flexibility mechanisms (however narrowly read) remain a real, if constrained, alternative path — hence collapse well below mountain-level (0.85+). Resistance (0.71) is high: this reading is actively and continuously contested by health ministries, generic manufacturers, and advocacy coalitions, which is precisely why an enforcement apparatus is required to hold it (a genuine mountain would meet negligible resistance).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this reading is straightforward, necessary coordination: without strong exclusivity, R&D investment collapses and no one benefits, including eventual generic entrants who rely on originator discovery. From the payer seats — health ministries, generic manufacturers, patients — the identical structure operates as enforced extraction: a narrow gate is deliberately kept narrow by trade-retaliation threat, and the coordination story (innovation incentive) does not evidently apply to the markets where the fight over flexibility is actually fought, since those markets contribute negligibly to originator firm revenue in the first place. The engine computes these divergent per-seat classifications from the structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Originator firms and biotech investors sit near the full-beneficiary end: they collect the transfer, control enforcement leverage through home-state trade delegations, and hold mobile or arbitrage-grade exit relative to the constraint. High-income state trade negotiators are structurally aligned with beneficiaries as agenda-setters, bearing none of the access costs. Low-income state health ministries and generic manufacturers sit toward the target end: constrained or trapped exit, real costs imposed through the same enforcement structure that makes the reading operative. Patients without treatment access sit at the extreme target end — powerless, trapped, immediate time horizon, bearing the terminal cost with zero capacity to invoke the treaty's own flexibility provisions on their own behalf.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (weak patent protection discouraging pharmaceutical R&D investment in globally-traded markets) retains partial validity for large, wealthy markets but is contested as applied to the low-income markets where the flexibility disputes actually occur — those markets are not where originator firms' revenue or R&D-financing decisions are made at the margin. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (some IP protection likely does support some R&D investment globally) while registering that the narrow-flexibility construction, as applied to public-health emergencies in low-income states, extracts asymmetrically from parties who receive negligible benefit from the incentive the constraint claims to protect. Calling it a pure mountain (natural, inevitable, no beneficiaries) would launder an actively negotiated, actively defended, historically contested treaty text into physics; calling it a pure snare would erase the real (if geographically misallocated) coordination function the strong-protection regime serves for R&D-intensive firms in wealthy markets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_trips_exclusivity_vs_flexibility,
    'Is the TRIPS text''s authoritative reading the strong-exclusivity construction (narrow flexibilities, high uniform protection) or the public-health-flexibility construction (broad compulsory licensing, parallel import as embedded right)? The same text supports both readings depending on which interpretive tradition (trade-law textualism favoring uniformity vs. Doha-Declaration-informed public-health purposivism) is applied.',
    'Would require either a definitive WTO Appellate Body ruling resolving the ambiguity as a matter of binding law (though the Appellate Body has been non-functional since 2019, itself a contested fact), or a supermajority treaty amendment codifying one reading. Absent either, the ambiguity is structurally irreducible at the level of legal text.',
    'If the flexibility reading is adopted as authoritative, low-income states gain a much stronger legal footing for compulsory licensing without retaliation risk, which would substantially lower the extraction this constraint currently measures. If the strong-exclusivity reading is entrenched further (e.g. via more TRIPS-plus agreements), extraction and suppression both continue rising as measured here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_trips_exclusivity_vs_flexibility, conceptual, 'Which of the two substantive kernel readings governs TRIPS in practice, and by what mechanism the contest is resolved.').

omega_variable(
    innovation_incentive_causal_link_low_income_markets,
    'Does strong patent protection in low-income and middle-income markets specifically (as opposed to protection in the US/EU/Japan markets that generate the overwhelming majority of pharmaceutical revenue) causally contribute to originator firms'' R&D investment decisions at the margin?',
    'Comparative analysis of originator firm R&D budget allocation and revenue geography pre- and post-TRIPS accession in low-income states; natural experiments from states that delayed TRIPS compliance under transition periods.',
    'If the causal link is weak or absent for low-income markets, the strong-exclusivity reading''s coordination justification does not extend to the markets where its costs are most severe, sharpening the tangled_rope classification toward snare for that subset of enforcement. If the link is real even at low-income market scale, the coordination function is more genuinely shared than the victim-side narrative suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_causal_link_low_income_markets, empirical, 'Whether the stated innovation-incentive rationale actually operates in the markets where the flexibility fight occurs.').

omega_variable(
    compulsory_licensing_narrow_construction_stability,
    'Is the narrow construction of compulsory licensing an artifact of the current dispute-settlement and trade-pressure equilibrium (and thus reversible as that equilibrium shifts, e.g. post-Appellate-Body paralysis, post-COVID TRIPS waiver precedent) or is it a stable, self-reinforcing feature of the treaty architecture?',
    'Track post-2022 TRIPS waiver implementation (the COVID-19 vaccine IP waiver) as a natural experiment: if broad flexibility use becomes normalized without triggering proportionate trade retaliation, the narrow construction was equilibrium-contingent, not architecturally fixed.',
    'If equilibrium-contingent, the suppression metric measured here (0.72, rising) could reverse rather than continue climbing, and reclassification pressure would shift this reading''s real-world force toward the flexibility reading''s territory over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compulsory_licensing_narrow_construction_stability, empirical, 'Whether the narrow-flexibility construction is a durable structural feature or a contingent equilibrium of enforcement pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(trip_tr_t1995, observed).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement_basis(trip_tr_t2001, observed).
narrative_ontology:measurement(trip_tr_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2007, 0.21).
narrative_ontology:measurement_basis(trip_tr_t2007, observed).
narrative_ontology:measurement(trip_tr_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement_basis(trip_tr_t2013, observed).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement_basis(trip_tr_t2019, observed).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(trip_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement_basis(trip_be_t1995, observed).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement_basis(trip_be_t2001, observed).
narrative_ontology:measurement(trip_be_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2007, 0.68).
narrative_ontology:measurement_basis(trip_be_t2007, observed).
narrative_ontology:measurement(trip_be_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2013, 0.72).
narrative_ontology:measurement_basis(trip_be_t2013, observed).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2019, 0.76).
narrative_ontology:measurement_basis(trip_be_t2019, observed).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(trip_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement_basis(trip_su_t1995, observed).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement_basis(trip_su_t2001, observed).
narrative_ontology:measurement(trip_su_t2007, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2007, 0.63).
narrative_ontology:measurement_basis(trip_su_t2007, observed).
narrative_ontology:measurement(trip_su_t2013, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2013, 0.67).
narrative_ontology:measurement_basis(trip_su_t2013, observed).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement_basis(trip_su_t2019, observed).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(trip_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the TRIPS agreement' per the ε-invariance principle: strong_exclusivity_reading (this file, tangled_rope, high extraction from low-income states/patients), public_health_flexibility_reading (the substantive counter-reading, expected lower extraction and different beneficiary/victim structure), and dispute_settlement_interpretive_authority (the procedural question of which body's interpretation binds, a distinct structural claim from either substantive reading). All three must remain linked via affects_constraints; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
