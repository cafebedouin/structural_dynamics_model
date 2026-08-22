% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Regulation of Drug Markets as Legal Commerce (Legalization Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the
 *   substance_control_authority kernel: the state's authority over drug
 *   markets exercised as regulation of legal commerce — licensing, product
 *   quality standards, age-gated access, and excise taxation — rather than
 *   criminalization (the prohibition reading) or clinical harm-minimization
 *   (the harm reduction reading). The ε referent is the legalized-regulated
 *   arrangement itself, assessed by this reading's own lights: the reading
 *   endorses the arrangement, and its honest assessment still finds
 *   substantial extraction — excise taxes well above the regulatory
 *   function's administrative cost, licensing regimes that concentrate market
 *   access, and a criminalized unlicensed periphery that persists wherever
 *   taxation leaves a price gap. The three readings are separate constraints
 *   with separate ε values, beneficiaries, and victims, linked through the
 *   network as one constraint family; the decomposition follows the
 *   ε-invariance principle because 'state authority over drugs' measures
 *   differently — and structures victims differently — under each reading.
 *
 * KEY AGENTS:
 *   - state_regulatory_agency: agenda setter (institutional/arbitrage) — authors the licensing, tax, and access-control framework; collects excise revenue; polices the boundary against unlicensed supply
 *   - licensed_producers: primary beneficiary (powerful/constrained) — legal market access and enforceable property rights; gains from licensing scarcity
 *   - licensed_retailers: secondary beneficiary (moderate/constrained) — point-of-sale gatekeepers; enforce age checks; collect and remit taxes
 *   - adult_consumers: dual beneficiary-payer (moderate/constrained) — legal tested access; bear excise premiums
 *   - unlicensed_sellers: primary target (powerless/trapped) — criminalized at the licensing boundary; barred from licensure by record and capital
 *   - low_income_users: target (powerless/trapped) — regressive tax burden; dispensing deserts; residual illicit supply as price escape
 *   - public_health_agencies: mixed beneficiary (organized/constrained) — earmarked revenue and surveillance mandate; absorb treatment burden scaling with use
 *   - international_narcotics_control_bodies: excluded voice (institutional/identity_locked) — treaty-based objection outside domestic rulemaking
 *   - independent_drug_policy_analysts: analytical observer (analytical/analytical) — track use, health, and market-structure outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.5).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.42).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Regulation of Drug Markets as Legal Commerce (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '4672b739-eda7-410a-85e1-8f26ec99974e').
narrative_ontology:cs_kernel_codification('4672b739-eda7-410a-85e1-8f26ec99974e', formalized).
narrative_ontology:cs_authority_grounding('4672b739-eda7-410a-85e1-8f26ec99974e', expertise).
narrative_ontology:cs_interpretation_layer_present('4672b739-eda7-410a-85e1-8f26ec99974e').
narrative_ontology:cs_reading_relation('4672b739-eda7-410a-85e1-8f26ec99974e', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('4672b739-eda7-410a-85e1-8f26ec99974e', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('4672b739-eda7-410a-85e1-8f26ec99974e', foundational, drug_markets_are_regulable_commerce).
narrative_ontology:cs_axiom_status(drug_markets_are_regulable_commerce, holdable).
narrative_ontology:cs_axiom_grounding('4672b739-eda7-410a-85e1-8f26ec99974e', drug_markets_are_regulable_commerce, conventional).
narrative_ontology:cs_axiom('4672b739-eda7-410a-85e1-8f26ec99974e', foundational, regulated_access_protects_third_parties).
narrative_ontology:cs_axiom_status(regulated_access_protects_third_parties, holdable).
narrative_ontology:cs_axiom_grounding('4672b739-eda7-410a-85e1-8f26ec99974e', regulated_access_protects_third_parties, empirically_contingent).
narrative_ontology:cs_reference_frame('4672b739-eda7-410a-85e1-8f26ec99974e', regulated_commerce_framework).
narrative_ontology:cs_drift_state('4672b739-eda7-410a-85e1-8f26ec99974e', contemporary_regime_operations, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4672b739-eda7-410a-85e1-8f26ec99974e', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_retailers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, adult_consumers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_agencies).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unlicensed_sellers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, low_income_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, adult_consumers).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, legalization_displacement_thesis).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, regulatory_capacity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the licensing rules, tax rates, and access controls governing the legal drug market; issues and revokes licenses; funds inspection and testing from earmarked revenue; directs enforcement against unlicensed sellers. Exit for this seat is redesign: the same statute-making power that built the framework can restructure rates, licensing breadth, or the scope of permitted commerce.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agency, agenda_setter,
    institutional, generational, arbitrage, national).

% Grow and manufacture product under license, paying licensing fees and excise taxes and meeting testing and tracking requirements. Legal status gives them enforceable contracts, property rights, and access to banking and capital markets. Licensing rules that cap entry protect their market share; sunk compliance investment makes relocation or exit costly.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_producers, beneficiary,
    powerful, generational, constrained, national).

% Operate the storefront layer of the legal market: verify age, sell tested and labeled inventory, collect and remit taxes, and depend on license renewal for continued operation. They compete with one another and with the residual unlicensed supply on price.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_retailers, beneficiary,
    moderate, biographical, constrained, regional).

% Buy tested, labeled product through age-gated retail without criminal exposure. They pay excise taxes and licensing-driven price premiums above unregulated-market prices; a lower-priced unlicensed alternative remains available but carries quality and legal risk.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adult_consumers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, adult_consumers, payer).

% Bear the tax burden as a larger share of income, often live in areas where retail siting rules leave few licensed outlets, and face dependence-related limits on reducing consumption. The residual unlicensed market is their price escape, at the cost of product uncertainty and continued criminal exposure for buying from unlicensed sellers.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, low_income_users, payer,
    powerless, immediate, trapped, local).

% Sold in the pre-legalization market or continue selling outside the licensing boundary wherever taxes leave a price gap. Criminal records and licensing capital requirements bar most from converting to licensed status; enforcement against unlicensed supply is directed at them. Their sector-specific skills and inventory have no legal redeployment.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, unlicensed_sellers, payer,
    powerless, immediate, trapped, local).

% Receive earmarked shares of excise revenue and gain surveillance and treatment mandates over the legal market. Their costs scale with use volume — treatment demand, poisoning response, youth-prevention programs — while their funding scales with sales, a mixed position inside the same revenue stream.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_agencies, beneficiary,
    organized, generational, constrained, national).

% Treaty-based bodies whose mandate is constituted by the international drug-control conventions this regime departs from. They object to commercial legalization and sit outside the domestic rulemaking process that produced it; their standing depends on the control framework they defend.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, international_narcotics_control_bodies, excluded,
    institutional, generational, identity_locked, global).

% Academic and think-tank researchers tracking use prevalence, health outcomes, market concentration, and illicit-market displacement across legalization regimes. They hold no stake in licensing or revenue and publish findings that cut both for and against the regime's design.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, independent_drug_policy_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, state_regulatory_agency).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an untracked, sometimes violent supply chain into a licensed, inspectable market: product testing and labeling, age verification, traceable inventory, and tax collection are performed once, centrally, through rules every participant can know in advance.
% TRANSFER_FUNCTION: Moves excise taxes and licensing-fee pass-throughs from consumers to the state; moves market share and legal protections to licensed operators; moves product-quality risk from consumers to licensed producers; moves enforcement burden from users to unlicensed sellers.
% ABSENT_VOICES: Unlicensed sellers have no seat in the licensing rulemaking that defines their criminal exposure; international treaty bodies object to the framework but sit outside the domestic process; residents of areas zoned away from retail bear siting externalities without licensing standing.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, supply would revert to unlicensed channels within weeks, quality assurance and age controls would disappear, licensed operators would unwind or go underground, and earmarked revenue streams would close; enforcement would either rebuild as criminalization or abstain, and the retail landscape, tax structure, and criminal-justice caseload would all reorganize.
% FOUNDING_PROBLEM: Uncontrolled illicit drug markets: violent supply chains, untested and adulterated product, underage access with no gate, and the fiscal and enforcement costs of treating the whole market as criminal.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiologists and drug-market researchers document persistent illicit supply and adulteration harms in mature legalization regimes, and law-enforcement seizure and arrest data — produced outside the licensed-industry and state-revenue beneficiary set — attest that the founding problem persists at meaningful scale.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50 at interval end: excise taxes and licensing-driven premiums in mature legalization regimes commonly exceed the full administrative cost of the regulatory function by a wide margin, while remaining bounded by genuine consumer value (tested product, legal access) and by price competition from the residual unlicensed tier. Suppression is 0.42 and structural: enforcement targets unlicensed supply and underage access rather than use itself — far below the prohibition reading's baseline, but non-trivial and rising as the licensing boundary is policed more actively. Theater is 0.30: testing, age verification, and tax collection are functional, while seed-to-sale tracking, packaging compliance, and reporting paperwork grow faster than their safety yield. Accessibility collapse is 0.38 — the unlicensed market persists in every mature regime, so alternatives do not fully close. Resistance is 0.45: price-motivated illicit competition, retail-siting opposition, and tax avoidance are continuous. All three metric series share one time grid (interval 0–14 ≈ 2012–2026, the first decade of mature cannabis legalization regimes), with a value authored at every point for every metric. The trajectory is a ratchet: early regimes set low taxes and open licensing to displace illicit supply, then fiscal dependence and industry consolidation raise taxes, fees, and compliance burdens — extractiveness and suppression climb together as the unlicensed periphery persists. The two powerless payer groups are natural coalition partners (price reform plus licensing access), but criminalization of the unlicensed periphery prevents open organization while consumer advocacy concentrates on access rather than licensing structure, so no effective coalition has formed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setter seat the framework is market-building: it converted a violent supply chain into a taxable, inspectable industry. From the licensed seats it is property and market access. From the consumer seats it is legal access at a premium. From the unlicensed-seller and low-income-user seats, the same licensing boundary that coordinates the legal market is the surface that criminalizes them and prices them toward residual unlicensed supply. The engine computes these per-seat classifications from power, exit, and role data; this story's claim does not adjudicate between them. The structural asymmetry is that the coordination surface and the extraction surface are the same object — the licensing boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensed producers, licensed retailers, and the state seat sit near the beneficiary end of d: the framework subsidizes them with legal market access, enforceable property rights, and revenue. Adult consumers are declared beneficiary with a payer secondary role — legal access and tested product against excise premiums — placing them near symmetric. Unlicensed sellers and low-income users are declared victims with trapped or constrained exit, placing them near the full-target end; trapped exit amplifies their effective extraction. Public health agencies are the one overridden seat: they occupy the organized power atom alone, and the derivation from their beneficiary declaration would read their earmarked funding as a near-pure beneficiary position, but their cost side (treatment, surveillance, prevention) scales with the use volume the legal market generates — a mixed position authored at d=0.4. Gain receipt: excise revenue is the dominant designed flow and accrues to the state seat; licensing scarcity rents to licensees are real but secondary and partially competed away by the residual unlicensed tier.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — violent, adulterated, uncontrolled illicit supply plus the fiscal and enforcement costs of criminalization — is still live: unlicensed markets persist in every mature regime and adulteration risk persists outside the licensed boundary. No mandatrophy declaration is authored. The tangled_rope claim holds both sides the classification needs to keep visible: the coordination function (quality assurance, age-gated access, violence displacement, revenue) is genuine, so a snare label would erase real gains; the extraction through the same structure (taxes above cost, licensing scarcity, a criminalized periphery) is real, so a rope label would launder it. The drift to watch is the tax-and-licensing ratchet in the measurement series: if illicit displacement fails entirely while extraction keeps climbing, the structure drifts toward snare-flavored operation, and the engine's per-seat computation would register it before the claim does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the substance_control_authority kernel — the legalization reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative structural analysis across the three family files: prohibition_reading assigns users to a criminalized victim set and protects third parties through deterrence; harm_reduction_reading treats users as patients and protects third parties through clinical intervention; this reading assigns an unlicensed periphery as victim set and protects third parties through market regulation. The disagreement is located at the legal status of the market itself and at the primary protective mechanism.',
    'If the kernel were instead read as a single constraint averaged across readings, ε would be unmeasurable (each reading structures different victims over the same market); the family decomposition is what keeps each ε invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    illicit_market_displacement_failure,
    'Does the legalized-regulated regime actually eliminate illegal markets, or do taxation and licensing barriers recreate a persistent illicit tier?',
    'Post-legalization enforcement seizure and arrest series, and price-gap analysis between licensed and unlicensed product in each mature regime.',
    'If the illicit tier persists at scale, the reading''s structural delta (''illegal markets eliminated'') fails: the regime operates as a legal core plus a criminalized periphery, effective extraction on the periphery rises, and the tangled_rope reading strengthens — with drift toward snare-flavored operation if the gap widens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(illicit_market_displacement_failure, empirical, 'Whether the regime displaces or merely relocates and re-criminalizes illicit supply.').

omega_variable(
    use_volume_elasticity,
    'Does legal access raise population use volume, as the expected structural delta flags?',
    'Longitudinal population surveys spanning pre- and post-legalization windows, with age-cohort breakdowns to separate initiation from prevalence effects.',
    'Higher use volume raises third-party and public-health costs, shifts the beneficiary/payer balance against the regime, strengthens the harm reduction reading''s empirical footing, and raises the effective cost-bearing of the public_health_agencies seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_elasticity, empirical, 'Whether commercial legalization increases total consumption.').

omega_variable(
    licensing_concentration_capture,
    'Does the licensing regime consolidate into oligopoly capture — large operators shaping fee structures, entry caps, and tax policy through lobbying?',
    'Market-concentration series, lobbying expenditure records, and licensing-fee trajectories relative to administrative cost.',
    'High capture converts the coordination framework into licensed-oligopoly extraction: the licensee seats'' derived directionality would need upward revision, the theater ratio would rise as compliance burdens entrench incumbents, and the classification would drift toward snare features with coordination as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_concentration_capture, empirical, 'Whether licensing structure serves coordination or entrenches a licensed oligopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t2, substance_control_authority__legalization_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement_basis(subs_tr_t2, observed).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(subs_tr_t4, observed).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__legalization_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(subs_tr_t6, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t14, substance_control_authority__legalization_reading, theater_ratio, 14, 0.3).
narrative_ontology:measurement_basis(subs_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t2, substance_control_authority__legalization_reading, base_extractiveness, 2, 0.34).
narrative_ontology:measurement_basis(subs_be_t2, observed).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(subs_be_t4, observed).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__legalization_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(subs_be_t6, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t14, substance_control_authority__legalization_reading, base_extractiveness, 14, 0.5).
narrative_ontology:measurement_basis(subs_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t2, substance_control_authority__legalization_reading, suppression_requirement, 2, 0.24).
narrative_ontology:measurement_basis(subs_su_t2, observed).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement_basis(subs_su_t4, observed).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__legalization_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement_basis(subs_su_t6, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t14, substance_control_authority__legalization_reading, suppression_requirement, 14, 0.42).
narrative_ontology:measurement_basis(subs_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug policy' or 'state authority over drugs' covers three structurally distinct constraints: criminalization (prohibition_reading), regulated legal commerce (this file), and clinical harm minimization (harm_reduction_reading). Each reading assigns different legal status to the same market, different victim sets (criminalized users vs. taxed consumers and an unlicensed periphery vs. patients), and different ε. This file authors ε only for the legalized-regulated arrangement; the family is linked so drift propagates visibly across readings — e.g., the tax ratchet modeled here re-creates a criminalized periphery that strengthens the prohibition reading's empirical footing, while earmarked revenue flows feed the harm reduction reading's resource base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__legalization_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
