% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive Market Freedom (Integration-Primary Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   Within the federation's founding membership treaty, the
 *   integration-primary reading holds free movement of workers as a
 *   constitutive freedom of the single market: a worker's qualifications,
 *   social-security position, and family rights are valid in every member
 *   state by operation of the founding text, and national restrictions on
 *   labor-market access are presumptively illegitimate unless narrowly
 *   justified. Enforcement runs through the Commission's infringement powers
 *   and the Court's preliminary-ruling doctrine, under which national
 *   restriction statutes are disapplied on contact with the principle. This
 *   story instantiates ONE reading of the contested membership-treaty kernel;
 *   the sovereignty-primary and subsidiarity-balance siblings are separate
 *   constraints with different beneficiary and victim sets, linked through
 *   the network section. The claim/metric relationship is deliberately
 *   unreconciled: the reading is CLAIMED here as tangled_rope — a genuine
 *   coordination function (continental labor allocation, social-security
 *   coordination, mutual recognition) carrying asymmetric, uncompensated
 *   costs concentrated on destination-area low-wage workers, welfare
 *   administrators, and member-state autonomy — while the authored metrics
 *   describe the arrangement's actual operation; the engine computes per-seat
 *   classifications from the structural data. KEY AGENTS (by structural
 *   relationship): - mobile_workers: primary beneficiary (moderate/mobile) —
 *   receive federation-wide labor-market access by treaty right rather than
 *   state consent - multinational_employers: concentrated beneficiary
 *   (institutional/arbitrage) — receive the cross-border labor-supply
 *   externality and wage moderation while household costs land on public
 *   budgets - labor_exporting_member_states: secondary beneficiary
 *   (moderate/constrained) — receive remittances and unemployment relief from
 *   out-migration - domestic_low_wage_workers: primary target
 *   (powerless/trapped) — bear wage competition and housing-cost pressure
 *   with no relocation exit - member_state_welfare_administrators: target
 *   (institutional/trapped) — absorb entry, family-benefit, and
 *   in-work-benefit costs under coordination rules they cannot close -
 *   member_state_governments: dual-positioned target and co-agenda-setter
 *   (institutional/constrained) — bear autonomy loss and fiscal cost while
 *   staffing the Council that amends the founding text - european_commission:
 *   agenda-setter (institutional/arbitrage) — enforces the presumption
 *   against national restrictions - court_of_justice: agenda-setter
 *   (institutional/identity_locked) — converts migration politics into
 *   proportionality doctrine; its authority is constituted by the guarantor
 *   role - third_country_nationals: excluded (powerless/trapped) — work under
 *   a harsher parallel regime with no seat in the arrangement -
 *   national_parliaments: excluded (moderate/trapped) — lost migration
 *   authority without an enforcement seat -
 *   independent_migration_researchers: analytical observer
 *   (analytical/analytical) — measure flows, wage effects, and fiscal
 *   incidence cited by all sides
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.58).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.74).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive Market Freedom (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4').
narrative_ontology:cs_kernel_codification('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', fixed_text).
narrative_ontology:cs_authority_grounding('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', lineage).
narrative_ontology:cs_interpretation_layer_present('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4').
narrative_ontology:cs_reading_relation('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', foundational, free_movement_constitutive_not_concessional).
narrative_ontology:cs_axiom_status(free_movement_constitutive_not_concessional, holdable).
narrative_ontology:cs_axiom_grounding('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', free_movement_constitutive_not_concessional, conventional).
narrative_ontology:cs_axiom('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', foundational, restriction_burden_on_restricting_state).
narrative_ontology:cs_axiom_status(restriction_burden_on_restricting_state, holdable).
narrative_ontology:cs_axiom_grounding('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', restriction_burden_on_restricting_state, instrumental).
narrative_ontology:cs_reference_frame('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', single_market_constitutive_freedom).
narrative_ontology:cs_drift_state('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', post_exit_referendum_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7c6017b8-76f5-46ae-bd0e-4f5f11d6b5e4', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, labor_exporting_member_states).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, domestic_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_welfare_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work in a member state other than the one that issued their qualifications; their credentials, social-security records, and family rights are recognized across the federation by operation of the founding text rather than bilateral agreement. Their access to any member state's labor market does not depend on that state's consent. What they carry is the cost of relocation, language, and recognition friction; their exit from any single labor market is the mobility the arrangement itself guarantees.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Recruit across the whole federation without negotiating with each national labor administration, and staff operations where labor is available rather than where it is resident. They receive an elastic cross-border labor supply and the wage moderation that competition among origin regions produces, while the schooling, healthcare, and benefit costs of their workforce's households land on destination-area public budgets. Their exit is the strongest in the arrangement: they can relocate operations and recruitment among jurisdictions at will.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_employers, beneficiary,
    institutional, generational, arbitrage, global).

% Origin states whose residents work abroad; they receive remittance inflows, relief from structural unemployment, and return-migration skills. Their bargaining position inside the federation depends on the mobility regime they benefit from, which limits their appetite to back destination-state restriction demands even where out-migration hollows out their own regions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, labor_exporting_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Compete for local jobs with incoming workers willing to accept lower reservation wages, in housing and rental markets where the same competition raises costs. They cannot relocate to follow the wage structure — family, housing tenure, local ties, and region-specific skills hold them in place — and the labor supply that sets their wage floor is one they have no voice in setting. The market-level gain from mobility is real, but it is not paid to them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, domestic_low_wage_workers, payer,
    powerless, immediate, trapped, local).

% Administer benefit, healthcare, and social-insurance systems that must admit mobile workers and their families on coordinated terms; they absorb labor-market entry costs, family benefits exported to origin-state households, and in-work benefits that subsidize low-wage employment. Coordination regulations and case law define what they may ask of a claimant's migration history; they cannot close the system to new entrants, and their budgets are set by legislatures bound by the same rules.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_welfare_administrators, payer,
    institutional, generational, trapped, national).

% Signed the founding text and staff the Council that amends it, yet bear the arrangement's running costs: infringement judgments against their statutes, fiscal outlays their budgets did not plan, and a migration authority ceded to the federal level. Leaving is possible — one large member state has done it — but the negotiation cost, market loss, and institutional disentanglement make exit a once-in-a-generation decision rather than an available lever. In day-to-day operation they are respondents, not authors.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, agenda_setter).

% Initiates infringement proceedings against member-state restrictions, negotiates transitional controls, and determines in practice which national measures count as narrowly justified. Its institutional project — market integration — is the arrangement's declared end; it selects enforcement priorities and gives the presumption its operational content. It is bound by the text it enforces, but it chooses what enforcement means.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, european_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Interprets the constitutive status of movement rights through preliminary rulings; its docket converts political disputes about migration into proportionality questions. Its authority is constituted by its role as guarantor of the founding text — stepping outside that role would dissolve the source of its own legitimacy — and its doctrine has made national restriction statutes disapplied on contact with the principle.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, court_of_justice, agenda_setter,
    institutional, generational, identity_locked, continental).

% Live and work inside the federation under a separate, far more restrictive permission regime; they staff many of the same sectors mobile citizens enter, without the recognition rights the arrangement guarantees. They would object that federation mobility is a privilege of its own membership, extended to them only as individual states concede — but they are not parties to the arrangement and hold no seat in it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, third_country_nationals, excluded,
    powerless, biographical, trapped, continental).

% Lost authority over labor-market access and migration when the principle was federalized; they retain subsidiarity-monitoring formalities but cannot legislate the restrictions their constituents demand. Their objection is procedural and standing — a constitutive commitment of this scale, they argue, required their direct consent rather than ratification by executives — and they have no enforcement seat.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_parliaments, excluded,
    moderate, generational, trapped, national).

% Measure mobility flows, wage effects, and fiscal incidence across member states; they produce the evidence every side of the contest cites and hold no stake in which reading governs.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, independent_migration_researchers, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, multinational_employers).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the continental labor-allocation problem that bilateral agreements cannot: qualifications, social-security records, and family rights are recognized everywhere at once, employers recruit across the whole market without per-state permission, and labor moves toward scarcity without case-by-state negotiation. Social-security coordination and mutual recognition are solved once, federally, instead of in hundreds of bilateral treaties.
% TRANSFER_FUNCTION: Moves labor, and the fiscal claims attached to workers and their households, from labor-abundant regions to labor-scarce regions; moves regulatory authority over labor-market access from member states to the federal enforcement institutions; moves the wage-competition pressure of an enlarged labor supply onto destination-area low-wage workers and the entry, family-benefit, and in-work-benefit costs onto destination welfare budgets.
% ABSENT_VOICES: Third-country nationals perform much of the same work under a harsher parallel permission regime and would object that federation mobility is a membership privilege denied to them; national parliaments lost migration authority without an enforcement seat; destination-area communities absorb service costs through governments that are themselves bound respondents. The enforcement institutions' unanimity around the constitutive reading is partly an artifact of who was in the room when the presumption was set: executives and courts, not the payers.
% DISAPPEARANCE_RATIONALE: Sectors built on cross-border staffing — care, agriculture, construction, logistics — would lose their workforce within weeks; social-security coordination would collapse into bilateral renegotiation; origin regions would absorb return flows they cannot employ; destination states would re-erect restriction regimes and the labor side of the single market would fragment into managed bilateralism; the treaty order itself would require renegotiation.
% FOUNDING_PROBLEM: The founding problem was the national closure of labor markets — the interwar pattern in which states restricted factor movement, deepened depression, and converted economic friction into political hostility. The treaty's authors built labor mobility as a market right rather than a state concession so that no member government could again wall off its labor market as an instrument of national economic policy.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians attest the founding problem from outside the arrangement — the interwar fragmentation record is standard scholarship, not the federation's own account. Labor-exporting member states attest that demand for mobility persists (remittance flows and employment depend on it); destination-area public-service unions attest the cost side the founding design left unpriced. The recurring national restriction attempts, each struck down and several re-enacted, are themselves external evidence that the founding problem remains live — no beneficiary-seat attestation is required to establish it.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but not dominant: the market-integration function is real — labor does move toward scarcity, social security is coordinated once federally, mutual recognition works — but the arrangement's costs land on identifiable seats without compensation: destination-area low-wage workers face wage competition they cannot exit, welfare administrators absorb entry and family-benefit costs under rules they cannot close, and member states ceded migration authority they cannot unilaterally reclaim. Suppression (0.74) is high because the reading's operative mechanism is precisely the foreclosure of national restriction: infringement actions, disapplication of statutes, and a justification burden placed on the restricting state. Theater (0.22) is low-moderate: enforcement is functional, though proportionality language grows formulaic as the doctrine settles. Accessibility collapse (0.62): the alternative — national restriction regimes — is largely foreclosed once the principle operates, but derogation space (public policy, public security, public health, transitional controls) remains narrow yet real. Resistance (0.66): sustained member-state pushback — opt-out demands, emergency-brake proposals, repeated re-enactment of struck-down welfare rules, and one full exit. All three measurement series run on ONE shared time grid (T=0..60, decade steps mapping the treaty era to the post-exit decade) so every metric is authored at every examined point; the trajectories show enforcement maturation (suppression 0.35 to 0.74), post-enlargement scale effects (extraction 0.30 to 0.58), and modest theater drift as doctrine settles. Suppression is authored as a raw structural property — it is the enforcement machinery's intensity, unscaled by scope or power; only extractiveness is scaled downstream.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and the dual-positioned government seat should compute as both. From mobile workers' and employers' positions the arrangement is market freedom — access no state may withhold. From the domestic low-wage worker's position the same structure is a wage floor set by a labor supply they have no voice in, with no exit (local housing, family, region-specific skills). Welfare administrators experience unfunded mandates with closed boundaries. Member-state governments experience both sides at once: they defend the treaty order in Council and against other respondents while being respondents themselves — their directionality sits mid-high, not at the full-target end, because they also collect from the market the principle constitutes. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the beneficiary end: mobile workers (the mobility IS their benefit, and their exit is the guarantee itself), multinational employers (arbitrage-grade exit; they recruit where labor is cheap), and labor-exporting member states (remittances and unemployment relief; their federation bargaining position depends on the regime they benefit from). Targets sit near the full-target end: domestic low-wage workers (trapped by housing, family, and local skills) and welfare administrators (trapped by coordination law they administer but cannot close). Member-state governments are the structural complication: declared victims bearing autonomy loss and fiscal cost, with a secondary agenda-setter role — they signed and can amend the founding text and staff the Council. I author no directionality override because overrides key to power atoms, and this story's institutional seats diverge too widely (employers near-beneficiary, administrators near-target, governments mid-high, Commission and Court near-beneficiary as carriers of the enforcement project) for any single atom-level correction to be honest; the dual position is carried by the secondary_role declaration instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — national closure of labor markets as an instrument of economic nationalism — is live, not dead: restriction attempts recur in every decade of the interval and each is litigated, which is itself evidence that the problem the arrangement was built for persists. Mandatrophy is therefore not resolved, and the live-status x world-rearranges pairing raises no zombie flag. The tangled_rope classification guards both misreadings: reading the arrangement as pure coordination would erase the uncompensated concentration of costs on seats that never individually consented; reading it as pure extraction would erase the real coordination — the market does clear labor continent-wide, and social-security coordination solved once what bilateralism would solve badly hundreds of times. The leading degradation risk is demographic: if origin-destination wage differentials converge and mobility flows thin, the coordination function atrophies while enforcement theater persists — the theater_ratio series is the leading indicator. If the welfare-cost attribution omega resolves small, the victim set narrows and the arrangement drifts rope-ward; if it resolves large, snare-direction pressure rises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the federation_membership_treaty kernel (reading: integration_primary). Do the treaty''s structure and enforcement practice instantiate this reading, or one of its siblings — sovereignty_primary (movement conditional on state consent) or subsidiarity_balance (proportionality-bounded mobility with national interests as co-original limits)?',
    'Constitutional practice over time: which reading the enforcement institutions apply when a member state restricts, and whether member-state practice converges on the presumption or reverts to consent-based management.',
    'Under sovereignty_primary, mobile workers leave the beneficiary set, national restriction becomes the default, and measured suppression collapses toward zero; under subsidiarity_balance, the victim set narrows to costs exceeding proportionate bounds and the presumption weakens to a tiebreaker. Epsilon and classification are properties of the reading, not of the topic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Which reading of the membership-treaty kernel this constraint instantiates, and what the sibling readings would change structurally.').

omega_variable(
    welfare_cost_attribution,
    'What share of destination welfare-system cost is attributable to free movement itself, as against population aging, benefit-design choices, and macroeconomic conditions?',
    'Administrative data linking claimant origin and household composition to cost, plus natural experiments from transitional-control episodes that varied access by cohort and date.',
    'A small movement-attributable share means the welfare-administrator victim declaration overstates extraction and the arrangement drifts toward pure coordination; a large share means measured extraction is understated and the arrangement faces snare-direction pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_cost_attribution, empirical, 'Whether welfare-system costs are caused by mobility or merely co-occur with it.').

omega_variable(
    constitutive_status_basis,
    'Is ''constitutive'' an economic-functional claim (the single market cannot clear without labor mobility) or a political-constitutional choice (the founding text enacted it and could have enacted otherwise)?',
    'Counterfactual institutional analysis: whether managed bilateral labor agreements could deliver comparable market integration, and what the founding negotiations reveal about alternatives considered.',
    'If functional, suppressing national restrictions is coordination-necessary cost and epsilon falls toward the coordination floor; if political, the presumption is a choice with identifiable winners and epsilon rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutive_status_basis, conceptual, 'Whether the constitutive status of free movement is necessity or enactment.').

omega_variable(
    suppression_structural_or_doctrinal,
    'Is the high suppression of national restrictions structural (treaty supremacy and direct effect operating as external barriers) or doctrinal (national administrations have internalized the presumption and no longer attempt restrictions)?',
    'Observe restriction attempts in member states where enforcement capacity or political will weakens: if attempts recur and are struck down, suppression is structural; if attempts do not recur even without enforcement threat, the presumption has been internalized.',
    'If internalized, suppression persists even as enforcement machinery atrophies — the arrangement is more stable than its enforcement budget suggests, and a shrinking infringement docket would not signal decay of the arrangement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_doctrinal, empirical, 'Whether restriction-suppression lives in enforcement machinery or in settled doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_integration_primary_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fmt_integration_primary_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.13).
narrative_ontology:measurement(fmt_integration_primary_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fmt_integration_primary_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.17).
narrative_ontology:measurement(fmt_integration_primary_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.19).
narrative_ontology:measurement(fmt_integration_primary_tr_t50, federation_membership_treaty__integration_primary, theater_ratio, 50, 0.21).
narrative_ontology:measurement(fmt_integration_primary_tr_t60, federation_membership_treaty__integration_primary, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(fmt_integration_primary_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fmt_integration_primary_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(fmt_integration_primary_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(fmt_integration_primary_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(fmt_integration_primary_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(fmt_integration_primary_be_t50, federation_membership_treaty__integration_primary, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(fmt_integration_primary_be_t60, federation_membership_treaty__integration_primary, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fmt_integration_primary_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fmt_integration_primary_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(fmt_integration_primary_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(fmt_integration_primary_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(fmt_integration_primary_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(fmt_integration_primary_su_t50, federation_membership_treaty__integration_primary, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(fmt_integration_primary_su_t60, federation_membership_treaty__integration_primary, suppression_requirement, 60, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% The colloquial label 'free movement in the federation' covers three structurally distinct constraints (epsilon-invariance decomposition): this integration-primary story (movement constitutive, restrictions presumptively illegitimate; mobile workers and employers beneficiaries; destination labor markets and welfare systems victims), the sovereignty-primary story (movement conditional on state consent; national labor-market protection as the operative norm), and the subsidiarity-balance story (proportionality-bounded mobility with national interests as standing constraints). Each has its own epsilon, beneficiary/victim structure, and classification. This file links both siblings because the integration-primary reading is the upstream claim the others define themselves against — its case law sets the narrow-justification standard that determines how much room the balance reading's national interests retain, and its constitutive premise is what the sovereignty reading denies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
