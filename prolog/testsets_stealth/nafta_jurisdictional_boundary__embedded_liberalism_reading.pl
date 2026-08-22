% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: Trade Framework Jurisdictional Boundary — Embedded Liberalism Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   A trade agreement operates as a jurisdictional boundary between
 *   international market-access obligations and domestic regulatory
 *   authority. Under the reading instantiated here, the text is a balanced
 *   framework: states gain enforceable access to each other's markets while
 *   retaining a defended zone of domestic policy space, with
 *   non-discrimination as the policing principle and 'legitimate objectives'
 *   as the hinge. The arrangement genuinely coordinates (mutual
 *   liberalization becomes credible without harmonization) and genuinely
 *   extracts (every boundary test costs litigation money, and the
 *   anticipation of tests reshapes regulation before any case is filed).
 *   Extraction is moderate and takes the specific form of dispute-process
 *   costs rather than direct transfers. KEY AGENTS (by structural
 *   relationship): - major_power_trade_ministries: Agenda-setter
 *   (institutional/arbitrage) — selects disputes, shapes interpretation,
 *   shops venues - exporting_industries: Primary beneficiary
 *   (organized/mobile) — collects enforceable access -
 *   domestic_regulatory_agencies: Dual-positioned beneficiary/payer
 *   (institutional/constrained) — retains defensive authority, bears defense
 *   and chill costs - small_trading_states: Primary target
 *   (moderate/constrained) — bears disproportionate per-dispute costs -
 *   environmental_labor_standard_constituencies: Excluded constituency
 *   (organized/trapped) — outside the room, harmed by chill -
 *   specialized_trade_bar: Secondary beneficiary (organized/mobile) —
 *   receives the fee stream the boundary contests generate -
 *   dispute_settlement_panels: Analytical observer (institutional/analytical)
 *   — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.38).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "Trade Framework Jurisdictional Boundary — Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'c020b080-ee03-41f6-8c2c-ddba6ff38aa2').
narrative_ontology:cs_kernel_codification('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', fixed_text).
narrative_ontology:cs_authority_grounding('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', lineage).
narrative_ontology:cs_interpretation_layer_present('c020b080-ee03-41f6-8c2c-ddba6ff38aa2').
narrative_ontology:cs_reading_relation('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', foundational, nondiscriminatory_regulation_is_trade_compatible).
narrative_ontology:cs_axiom_status(nondiscriminatory_regulation_is_trade_compatible, holdable).
narrative_ontology:cs_axiom_grounding('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', nondiscriminatory_regulation_is_trade_compatible, conventional).
narrative_ontology:cs_axiom('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', foundational, policy_space_balanced_not_subordinate).
narrative_ontology:cs_axiom_status(policy_space_balanced_not_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', policy_space_balanced_not_subordinate, conventional).
narrative_ontology:cs_reference_frame('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', embedded_liberalism_balance).
narrative_ontology:cs_drift_state('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', contemporary_managed_trade_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c020b080-ee03-41f6-8c2c-ddba6ff38aa2', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, specialized_trade_bar).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, small_trading_states).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_labor_standard_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, embedded_liberalism_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, nondiscrimination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which foreign measures to challenge and which domestic defenses to mount; staff the delegations that negotiate texts and nominate panelists; can shift between multilateral and bilateral channels when one venue disappoints. They initiate most disputes and treat occasional losses as the operating cost of a usable instrument.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, major_power_trade_ministries, agenda_setter,
    institutional, generational, arbitrage, global).

% Sell into markets whose access terms are fixed by the agreement; petition their home ministry when a foreign measure raises their costs; almost never litigate directly. Their investment plans assume the access commitments hold, and they mobilize to defend the framework whenever renegotiation threatens it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries, beneficiary,
    organized, biographical, mobile, global).

% Write and enforce health, safety, environmental, and labor rules inside the boundary the agreement draws around legitimate objectives. When a measure is challenged they must defend it with evidence and legal argument; when drafting, they weigh how a rule will read to a panel — a discipline that shapes rules before any case exists. They keep the authority to regulate, and they pay, in staff time and defensive redrafting, for keeping it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer).

% Depend on the agreement for access they could not obtain bilaterally, but face per-dispute costs that scale badly with their size: thin specialist legal benches, consultation rounds they cannot sustain, and settlements reached to avoid attrition rather than on the merits.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, small_trading_states, payer,
    moderate, biographical, constrained, regional).

% Campaign for stronger protective standards and watch the boundary from outside the room: they are not parties to disputes, participate only as amici where panels permit, and see their preferred measures tested by other governments' legal teams rather than defended by their own.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_labor_standard_constituencies, excluded,
    organized, generational, trapped, global).

% Counsel both challengers and defenders across successive disputes; the more contested the boundary, the steadier the demand for their services. Expertise concentrates in a few firms and capitals, and personnel circulate between government service and private practice.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, specialized_trade_bar, beneficiary,
    organized, biographical, mobile, global).

% Adjudicate boundary cases under the agreement's rules; their reports become the working definition of legitimate objectives until superseded. They hear every seat's arguments and hold no stake in outcomes beyond the integrity of the process.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, dispute_settlement_panels, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, specialized_trade_bar).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes mutual market-opening credible among sovereigns that will not accept regulatory harmonization: each state binds its border measures while retaining domestic rule-making, and the non-discrimination discipline polices the line between disguised protection and legitimate regulation.
% TRANSFER_FUNCTION: Moves enforceable market access to export industries; moves the cost of policing the boundary — litigation, evidence-building, defensive redrafting — onto responding governments, disproportionately smaller ones; moves dispute fees to the specialized trade bar.
% ABSENT_VOICES: Environmental, labor, and consumer constituencies and subnational governments sit outside the dispute system. They would contest where the boundary sits and how capacious 'legitimate objectives' is, but they enter only through amici briefs or the discretionary attention of ministries deciding which cases to bring.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, access commitments would unravel into bilateral power bargaining; regulatory chill would be replaced by overt diplomatic pressure and retaliatory tariff politics; the dispute economy would relocate to ad hoc arbitration and lobbying; smaller states would lose the one forum where their claims carry formal weight equal to larger ones.
% FOUNDING_PROBLEM: How to lock in trade liberalization against protectionist backsliding without dismantling the domestic regulatory state — the postwar bargain later named embedded liberalism: openness abroad purchased with policy space at home.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the international-political-economy literature articulated the openness-versus-policy-space tension decades before the agreement; negotiating-history records show drafters explicitly weighing defensive exceptions; and contemporaneous legislative testimony from labor and environmental organizations demanded the policy-space guarantee. No attestation rests solely on export interests.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the arrangement's costs are real but indirect: no tariff-style transfer, instead litigation budgets, evidence-building burdens, and anticipatory defensive redrafting concentrated on responding governments. Suppression is moderate-low (0.38): enforcement runs through authorized retaliation and adverse panel reports, but exit remains open — states can withdraw, negotiate carve-outs, or shift to bilateral channels — so the constraint coerces less than it obliges. Theater ratio (0.29) reflects real market-access work done by panels alongside performative trade-and-environment committees and periodic reviews that produce communiques rather than boundary clarification. Accessibility collapse is moderate (0.42): unilateral protectionism, managed bilateralism, and withdrawal persist as alternatives, each with known costs, so understanding the constraint does not close the option set. Resistance (0.52) is sustained: interpretive declarations, carve-out negotiation, delayed compliance, and appellate-body paralysis are all active pushback from states the boundary binds. The temporal series run on one shared grid (t=0,6,12,18,24,30) with every tracked metric authored at every point. The suppression series is deliberately near-flat: aggregate enforcement intensity stayed roughly constant while its venue shifted from multilateral adjudication toward bilateral leverage — a relocation, not a ratchet — which is why suppression_requirement is tracked at all despite the shallow slope. Base extractiveness climbs steadily as the dispute economy matured and chill accumulated: rent layered onto a functioning coordination core, the signature the interval was chosen to expose.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the ministry seat the arrangement is a usable instrument they administer and occasionally lose with; from the small-state seat it is a cost-asymmetric tribunal they cannot afford to fully use; from the agency seat it is a defensive discipline that preserves authority while taxing its exercise; from the constituency seat it is a closed room; from the bar's seat it is steady demand. Same text, same panels, five different constraints experienced. The engine computes this divergence from the structural data — power, exit, and declared position — and the divergence is the finding, not noise to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Exporting industries and the trade bar are declared beneficiaries with mobile exit, deriving directionality near the beneficiary end: the arrangement subsidizes them. Small trading states are declared victims with constrained exit, deriving directionality near the full-target end: they pay the boundary-policing costs and cannot arbitrage away. Domestic regulatory agencies are the deliberate complication: declared beneficiaries (they retain defensive authority the siblings would strip) but carrying payer costs through their secondary role — defense litigation and pre-emptive redrafting place them mid-range rather than at the subsidized end. Major-power ministries are undeclared in the arrays; their agenda-setter role and arbitrage exit position them near the beneficiary end without a formal declaration. Constituencies are excluded and harmed, sitting near the target end despite having no formal standing. No directionality overrides were needed: the beneficiary/victim declarations plus exit options differentiate every seat the derivation would otherwise conflate, and the one dual-positioned agent is carried by its secondary_role rather than by an override that would misapply to other institutional-atom agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetric mislabels. Calling this a rope (pure coordination) erases the extraction stream: the same structure that makes access credible generates a litigation economy whose costs fall asymmetrically on smaller responders, and whose anticipatory shadow disciplines regulators who never see a courtroom. Calling it a snare erases the coordination achievement: access gains are real, exit is genuinely available, the founding problem is live, and no seat captures the gains exclusively — the fee stream lands with the bar, but the access value disperses across export sectors. Mandatrophy is not resolved: the founding problem (reconciling openness with policy space) persists, so the arrangement is neither a scaffold awaiting sunset nor a piton performing a dead mandate. The measurement series supports this: theater rises but from a low base, and extractiveness rises without decoupling from the coordination function — degradation in progress, not completion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the nafta_jurisdictional_boundary kernel governs the standing arrangement — does this story''s epsilon describe the framework as the embedded-liberalism reading holds it, or has operative practice shifted toward a sibling?',
    'Track which framing panels, parties, and renegotiation texts actually invoke in operative disputes; if capital-supremacy or sovereignty-primacy framing dominates outcomes, this story''s epsilon and victim set no longer describe the arrangement under contest.',
    'Adopting capital_supremacy_reading raises epsilon sharply (protective standards become challengeable defaults) and reassigns victims to domestic regulators wholesale; adopting sovereignty_primacy_reading drops epsilon toward pure-coordination levels and dissolves the litigation-cost extraction this story measures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon.').

omega_variable(
    boundary_location_contest,
    'Where exactly does the legitimate-objectives boundary sit — at bare non-discrimination, or at stricter proportionality or least-restrictive-means tests?',
    'Panel and appellate jurisprudence over successive disputes; comparative analysis of measures upheld versus struck under differing formulations.',
    'A stricter test raises per-case extraction for defending governments and widens the chill zone; a deferential test compresses extraction toward pure coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_location_contest, empirical, 'The boundary''s location is settled case-by-case, not by the text, making per-case extraction sensitive to doctrinal drift.').

omega_variable(
    chill_vs_realized_cost_split,
    'What share of measured extraction is realized litigation cost versus anticipatory self-censorship by regulators who soften or abandon rules before any challenge?',
    'Compare trajectories of challenged measures against matched unchallenged measures in the same agencies; survey regulatory drafting practice for trade-proofing behavior.',
    'If chill dominates, extraction exceeds what the dispute record shows and effective epsilon is understated; if realized costs dominate, procedural remedies (fee-shifting, legal aid for small states) address most of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chill_vs_realized_cost_split, empirical, 'Splitting the extraction stream between courtroom costs and the shadow they cast.').

omega_variable(
    climate_measure_boundary_stress,
    'Will carbon-border adjustment and climate-motivated trade measures be absorbed within the balance as legitimate objectives, or do they force a choice between the sibling readings?',
    'Outcomes of the first generation of climate-measure disputes and the interpretive language parties accept for them.',
    'Absorption confirms the reading''s flexibility claim; rejection pushes operative practice toward capital_supremacy_reading and dates this reading''s obsolescence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_measure_boundary_stress, empirical, 'Whether the balance survives its largest incoming stress test.').

omega_variable(
    cost_asymmetry_origin,
    'Is the small-state cost disadvantage designed into the dispute procedure (burden allocation, timelines, standing) or does it emerge from general legal-market scale effects?',
    'Comparative study of dispute costs across similarly sized litigants inside and outside the trade regime; procedural-history analysis of burden rules.',
    'Designed asymmetry implicates the constraint itself and supports structural remedy; scale-effect asymmetry implicates the surrounding legal economy and suggests coalition pooling or third-party funding as the fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_asymmetry_origin, empirical, 'Whether the extraction''s unequal incidence is intrinsic to the arrangement or incidental to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(naft_tr_t0, observed).
narrative_ontology:measurement(naft_tr_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(naft_tr_t6, observed).
narrative_ontology:measurement(naft_tr_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement_basis(naft_tr_t12, observed).
narrative_ontology:measurement(naft_tr_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement_basis(naft_tr_t18, observed).
narrative_ontology:measurement(naft_tr_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(naft_tr_t24, observed).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(naft_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(naft_be_t0, observed).
narrative_ontology:measurement(naft_be_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 6, 0.41).
narrative_ontology:measurement_basis(naft_be_t6, observed).
narrative_ontology:measurement(naft_be_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement_basis(naft_be_t12, observed).
narrative_ontology:measurement(naft_be_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement_basis(naft_be_t18, observed).
narrative_ontology:measurement(naft_be_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(naft_be_t24, observed).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(naft_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(naft_su_t0, observed).
narrative_ontology:measurement(naft_su_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement_basis(naft_su_t6, observed).
narrative_ontology:measurement(naft_su_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement_basis(naft_su_t12, observed).
narrative_ontology:measurement(naft_su_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 18, 0.36).
narrative_ontology:measurement_basis(naft_su_t18, observed).
narrative_ontology:measurement(naft_su_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement_basis(naft_su_t24, observed).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(naft_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the trade agreement does to domestic regulation' decomposes into three structurally distinct constraints by hierarchy premise. This file instantiates the middle reading (balance with retained defensive authority, moderate litigation-cost extraction); capital_supremacy_reading instantiates the override reading (high epsilon, regulators as victims); sovereignty_primacy_reading instantiates the subordination reading (epsilon near coordination floor, minimal extraction). The readings share a kernel but differ in epsilon, beneficiary structure, and failure modes; each file documents its own values and none averages across the set. Edges run both directions because each reading's viability conditions the others' rhetorical position in renegotiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
