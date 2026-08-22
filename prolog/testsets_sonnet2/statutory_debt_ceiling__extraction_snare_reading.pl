% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Weaponized Hostage Mechanism
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling caps the total amount the U.S. Treasury may
 *   legally borrow to fund spending Congress has already authorized through
 *   separate appropriations. Because the ceiling requires periodic
 *   legislative renewal, and because breach threatens sovereign default, it
 *   creates a recurring veto point disconnected from the appropriations
 *   process itself. This reading treats the ceiling as a weaponized boundary:
 *   a legislative minority (or a chamber majority under divided government)
 *   can withhold the routine renewal vote to extract policy concessions
 *   unrelated to the debt itself, under threat of imposing default costs on
 *   parties who have no say in the negotiation. This is the
 *   extraction_snare_reading of the statutory_debt_ceiling kernel — a sibling
 *   coordination_scaffold_reading treats the same statute as a genuine
 *   Treasury-operations convenience with negligible extraction, and a sibling
 *   constitutional_nullity_reading treats the statute as void ab initio under
 *   the 14th Amendment's public-debt clause. All three are separate
 *   constraints sharing one kernel text; this file authors only the snare
 *   reading, at its own stable ε.
 *
 * KEY AGENTS:
 *   - debt_ceiling_holdout_faction: Primary beneficiary (organized/mobile) — extracts concessions under threat of default
 *   - treasury_department: Institutional payer (institutional/trapped) — absorbs operational cost of crisis management
 *   - federal_benefit_recipients: Primary target (powerless/trapped) — bears direct payment-disruption risk
 *   - treasury_bondholders: Diffuse target (powerful/mobile) — bears rating and yield-premium cost
 *   - general_taxpayers: Diffuse generational target (powerless/trapped) — bears compounding borrowing-cost increase
 *   - credit_rating_agencies: Analytical observer — measures and transmits governance-risk premium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.81).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.72).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Hostage Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, 'fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1').
narrative_ontology:cs_kernel_codification('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', formalized).
narrative_ontology:cs_authority_grounding('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', extraction).
narrative_ontology:cs_interpretation_layer_present('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1').
narrative_ontology:cs_reading_relation('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', foundational, legislative_leverage_over_fiscal_execution_is_legitimate_procedural_tool).
narrative_ontology:cs_axiom_status(legislative_leverage_over_fiscal_execution_is_legitimate_procedural_tool, holdable).
narrative_ontology:cs_axiom_grounding('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', legislative_leverage_over_fiscal_execution_is_legitimate_procedural_tool, conventional).
narrative_ontology:cs_axiom('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', secondary, default_threat_credibility_is_the_source_of_negotiating_power).
narrative_ontology:cs_axiom_status(default_threat_credibility_is_the_source_of_negotiating_power, holdable).
narrative_ontology:cs_axiom_grounding('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', default_threat_credibility_is_the_source_of_negotiating_power, empirically_contingent).
narrative_ontology:cs_reference_frame('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', post_1917_treasury_operational_convenience).
narrative_ontology:cs_drift_state('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', post_2011_downgrade_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('fce5f21d-93ee-4c8d-bf2d-0f2c97b5c0c1', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, debt_ceiling_holdout_faction).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, opposition_party_leadership).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_benefit_recipients).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_contractors_and_employees).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, general_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_party_leadership).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, congressional_power_of_the_purse_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A minority bloc within the majority party (or a chamber majority facing a divided government) that withholds votes to raise the statutory limit until specific unrelated policy concessions are granted. Their leverage exists only because the vote is periodically required and default is catastrophic; they can walk away from negotiations without personally bearing the costs of a breach, and their electoral base often rewards the brinkmanship.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, debt_ceiling_holdout_faction, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, debt_ceiling_holdout_faction, beneficiary).

% Must use extraordinary measures (disinvesting trust funds, delaying payments, accounting maneuvers) to avoid breach while Congress negotiates. Has no authority to raise the limit itself and cannot unilaterally prioritize payments without violating other statutes. Bears the operational and reputational cost of managing a crisis it did not create and cannot resolve alone.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_department, payer,
    institutional, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, treasury_department, observer).

% Social Security recipients, veterans, and other beneficiaries whose payments are threatened with delay or suspension each time the ceiling nears breach. They have no seat in the negotiation, no ability to hedge against a missed payment, and no recourse beyond eventually voting — long after any specific crisis resolves.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_benefit_recipients, payer,
    powerless, immediate, trapped, national).

% Federal workers and contractors face furlough, delayed pay, or halted contract disbursement during standoffs. Some can seek other employment over time, but many are tied to federal-specific skills, pensions, or security clearances that make exit costly.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_contractors_and_employees, payer,
    moderate, biographical, constrained, national).

% Domestic and foreign holders of U.S. Treasury securities face default or technical-default risk each cycle, along with credit rating downgrade exposure (as occurred in 2011 and again with rating agency warnings in later standoffs). Large institutional holders can diversify away over time; the risk still imposes a persistent volatility premium priced into yields.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders, payer,
    powerful, biographical, mobile, global).

% Bear the diffuse, compounding cost of elevated borrowing rates that persist after each standoff is resolved (a documented effect of the 2011 crisis alone), plus the opportunity cost of legislative time and credibility spent on brinkmanship rather than governance. No individual taxpayer can exit the tax base or negotiate around the cost.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, general_taxpayers, payer,
    powerless, generational, trapped, national).

% Must ultimately supply the votes to raise the ceiling and is blamed politically regardless of outcome — either for capitulating to the holdout faction's demands or for the economic damage of a breach. Cannot simply ignore the holdout bloc without losing the votes needed to pass any raise at all.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_party_leadership, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, majority_party_leadership, agenda_setter).

% Monitor the standoff and issue warnings or downgrades (S&P's 2011 downgrade of U.S. sovereign debt from AAA being the paradigm case) based on assessed governance risk, independent of who wins the substantive policy fight. Their assessments become inputs into bondholder pricing and thus a transmission mechanism for the extraction's cost.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, biographical, analytical, global).

% Inherit the precedent that the ceiling is a usable hostage mechanism once any faction has successfully extracted concessions through brinkmanship, and inherit the elevated baseline borrowing costs left by prior standoffs. They have no voice in the current cycle's negotiation despite bearing its structural legacy.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, future_congresses, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, debt_ceiling_holdout_faction).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a single aggregate borrowing authorization could let Treasury manage debt issuance without a fresh vote for every appropriation Congress has already made — sparing repeated floor votes on operational financing decisions.
% TRANSFER_FUNCTION: Moves policy concessions (spending cuts, program riders, unrelated legislative wins) from the negotiating majority and from beneficiaries/contractors/bondholders/taxpayers who bear default-proximity costs, to the holdout faction that extracts the concessions and to political actors who gain electoral capital from the brinkmanship.
% ABSENT_VOICES: Federal benefit recipients, federal employees, and future Congresses have no seat in the negotiation despite bearing the concentrated or inherited costs; bondholders exert pressure only indirectly through pricing, well after the political damage is done.
% DISAPPEARANCE_RATIONALE: If the statutory ceiling vanished (e.g. through repeal, permanent suspension, or a 14th Amendment nullity ruling), the periodic hostage-taking opportunity would disappear: Treasury would issue debt against appropriations already enacted without a separate authorization veto point, eliminating the specific leverage moment the holdout faction currently exploits. Rating-agency governance-risk premiums tied to this specific mechanism would likely recede.
% FOUNDING_PROBLEM: The ceiling was originally enacted (1917, expanded through the 20th century) to give Treasury flexibility to manage debt issuance without requiring Congress to authorize each individual bond issuance — a coordination convenience, not a spending check, since it does not control appropriations.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists (including former Treasury officials and the Congressional Research Service) and the Government Accountability Office attest the ceiling exercises no independent control over spending or deficits — spending and revenue decisions are made in separate appropriations and tax legislation — and that its sole contemporary function is as a periodic veto point exploited for unrelated policy leverage. This assessment is corroborated from outside the holdout faction that benefits from the mechanism's persistence.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.81) is authored high because the mechanism's contemporary function — under this reading — has no relationship to controlling deficits or spending; it is purely a leverage point exploited episodically for unrelated concessions, evidenced by the 2011 downgrade and repeated crisis cycles. Suppression (0.72) reflects that alternatives (repeal, automatic-adjustment mechanisms, discharge-petition workarounds) are structurally suppressed by the political cost of appearing 'soft' on fiscal discipline, not by any functional necessity. Theater ratio (0.40) captures that a meaningful share of the brinkmanship is performative — many standoffs resolve with minimal substantive concession relative to the crisis theater generated, though the 2011 and 2023 cycles extracted real policy wins (spending caps, sequestration triggers) for the holdout factions. Accessibility collapse is moderate (0.45): alternatives to the current binary ceiling-or-breach structure are well understood and have been proposed repeatedly (discharge petitions, the 'Gephardt rule', platinum-coin gambits, 14th Amendment arguments) but remain politically unexercised, not logically foreclosed — this is a live political fight, not a mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the holdout faction's own seat, the mechanism looks like principled fiscal restraint successfully exercised through legitimate procedural leverage — a Rope story. From the payer seats (benefit recipients, bondholders, taxpayers), the same structure computes as a Snare: coerced payment under threat, with no coordination benefit reaching them. The engine's per-seat computation should reproduce this divergence directly from the declared power/exit/scope data without any need to average the two readings into one number.
 *
 * DIRECTIONALITY LOGIC:
 *   The holdout faction is the structural beneficiary: it collects policy concessions without bearing electoral or economic responsibility for near-breach conditions, and can walk from the table because default costs fall elsewhere. Federal benefit recipients, contractors, bondholders, and taxpayers are targets: they bear concentrated near-term costs (recipients, contractors), price-transmitted costs (bondholders via downgrade risk), or diffuse compounding costs (taxpayers via elevated yields) without any negotiating position. Treasury and majority leadership sit as institutional payers forced to manage or resolve the crisis without holding the leverage themselves — they pay in reputational and operational terms even when they are not the extraction's intended target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored 'dead': the ceiling's original coordination rationale (sparing Treasury from per-issuance authorization votes) does not require the ceiling's contemporary hostage function, and independent analysts outside the holdout faction corroborate that spending control already happens elsewhere (appropriations, tax law). This is precisely the mismatch the R5 interview is built to surface: a founding problem long resolved (dead) combined with a disappearance_verdict of world_rearranges signals capture — the mechanism persists not because its coordination function is live, but because a party benefits from its persistence as a leverage instrument. The classification as snare rather than tangled_rope follows from the coordination_scaffold_reading's absence from THIS file: this reading asserts no live coordination function offsets the extraction, which is a substantive, contestable claim — hence its status as one reading among three, not the single truth of the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_function_still_alive,
    'Does the debt ceiling retain any residual coordination function (e.g., forcing periodic fiscal accountability moments) that offsets its extraction, or has that function been fully supplanted by hostage dynamics?',
    'Compare legislative outcomes in ceiling-raise cycles that produced substantive fiscal reform versus cycles that produced pure brinkmanship with no policy change; a preponderance of the latter across the historical record would support pure-extraction over hybrid coordination.',
    'If a genuine residual coordination function is found, this reading would need revision toward tangled_rope (requiring active enforcement + beneficiary + victim, all already present) rather than snare; if no such function exists, snare remains the structurally accurate classification for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_still_alive, conceptual, 'Whether any coordination function survives alongside the extraction this reading documents.').

omega_variable(
    sibling_reading_adjudication,
    'Which of the three kernel readings (coordination_scaffold, extraction_snare, constitutional_nullity) should govern practical treatment of the ceiling going forward, and is that a legal question, a political question, or both?',
    'A definitive Supreme Court ruling on the 14th Amendment argument would resolve the constitutional_nullity_reading''s legal status; absent that, the political branches'' continued treatment of the ceiling as operative and negotiable is itself evidence favoring the extraction_snare_reading''s continued applicability.',
    'A nullity ruling would eliminate the extraction mechanism''s legal basis entirely, converting the constraint from live snare to historical artifact; absent such a ruling, the snare persists as the operative reading in practice regardless of its constitutional standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_adjudication, conceptual, 'The unresolved contest among the three sibling readings and its practical stakes.').

omega_variable(
    beneficiary_electoral_versus_policy_gain,
    'Is the holdout faction''s benefit primarily electoral (signaling fiscal toughness to a base) or substantive (actual policy concessions with lasting fiscal effect)?',
    'Track whether concessions extracted in past standoffs (e.g., 2011 sequestration, 2023 spending caps) persisted, were reversed, or had measurable long-run fiscal impact versus being largely symbolic.',
    'If gains are mostly electoral/symbolic, the theater_ratio should be authored higher in future revisions; if gains are substantive and durable, the extraction is more clearly a real transfer rather than performative brinkmanship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_electoral_versus_policy_gain, empirical, 'Whether the extracted value is substantive policy change or largely symbolic political capital.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1979, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1979, 0.15).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(stat_tr_t2013, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(stat_tr_t2019, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(stat_tr_t2023, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement(stat_tr_t2025, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1979, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1979, 0.25).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2011, 0.78).
narrative_ontology:measurement(stat_be_t2013, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2013, 0.7).
narrative_ontology:measurement(stat_be_t2019, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(stat_be_t2023, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2023, 0.82).
narrative_ontology:measurement(stat_be_t2025, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2025, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1979, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1979, 0.3).
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(stat_su_t2013, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2013, 0.63).
narrative_ontology:measurement(stat_su_t2019, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(stat_su_t2023, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2023, 0.74).
narrative_ontology:measurement(stat_su_t2025, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, sovereign_credit_rating_governance_risk).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposed from the natural-language concept 'the debt ceiling' per the epsilon-invariance principle: the same statutory text supports a low-extraction coordination_scaffold_reading, a high-extraction extraction_snare_reading (this file), and a constitutional_nullity_reading under which the statute has no valid legal force at all. Each reading carries its own stable epsilon and its own beneficiary/victim structure; they are linked here rather than merged because measuring 'the debt ceiling' one way (as Treasury bookkeeping) yields negligible extraction while measuring it another way (as legislative leverage under default threat) yields substantial extraction — the epsilon-invariance test requires decomposition rather than one averaged number.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
