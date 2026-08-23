% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free-Movement Priority over National Welfare Boundaries (Integration-Primary Reading)
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   This story instantiates one reading, integration_primary, of the
 *   contested kernel federation_membership_obligations. The standing
 *   arrangement under contest: treaty-enshrined mobility rights (worker,
 *   self-employed, student, and economically inactive resident after lawful
 *   presence) that member-state welfare eligibility rules must accommodate;
 *   enforcement runs through CJEU preliminary rulings and Commission
 *   infringement action, with primacy doctrine rendering national closure
 *   legislation legally void where it collides with the rights. The
 *   arrangement coordinates genuinely (a continental labor market with
 *   portable rights, mutual recognition of qualifications, crisis-time
 *   reallocation capacity) while transferring concentrated costs onto seats
 *   that never set the terms: non-mobile workers and residents of gateway
 *   regions, the public systems of sending states that train staff who leave,
 *   national welfare administrations whose statutory boundaries bind less
 *   each decade, and member-state governments whose social-policy competence
 *   is adjudicated elsewhere. Claim and metrics are authored independently:
 *   claimed_type states tangled_rope as the structure I believe true (both a
 *   real coordination function and asymmetric extraction, actively enforced);
 *   the metrics describe operation as this reading honestly assesses it. Per
 *   the epsilon-referent rule, epsilon is authored for the standing
 *   integration-primary arrangement as seen through THIS reading's lights,
 *   never for the sibling readings' endorsed arrangements, which are separate
 *   files. KEY AGENTS (by structural relationship): -
 *   intra_eu_mobile_workers: primary beneficiary (moderate/mobile) — enters
 *   receiving-state welfare beneficiary sets - multinational_employers:
 *   concentrated gain recipient (powerful/arbitrage) -
 *   court_of_justice_of_the_eu: agenda setter via case law, authority
 *   accumulator (institutional/analytical) - european_commission: enforcement
 *   arm, competence accumulator (institutional/constrained) -
 *   displaced_local_labor: primary cost bearer (powerless/trapped) -
 *   gateway_region_residents: diffuse cost bearer with service offsets
 *   (moderate/constrained) - sending_state_public_services: structural loser
 *   of trained staff (organized/trapped) - national_welfare_administrations:
 *   administered boundary-loser (institutional/constrained) -
 *   member_state_governments: dual seat — domestic administrator and
 *   sovereignty-cost payer (institutional/constrained) -
 *   third_country_nationals: excluded voice (powerless/trapped) -
 *   eu_mobility_research_community: analytical observer
 *
 * KEY AGENTS:
 *   - - intra_eu_mobile_workers: primary beneficiary (moderate/mobile) — treaty rights to work, reside, and access host welfare after qualifying periods
 *   - - multinational_employers: concentrated gain recipient (powerful/arbitrage) — continental staffing pool, wage moderation, posting agility
 *   - - court_of_justice_of_the_eu: agenda setter via case law (institutional/analytical) — each ruling extends or bounds the rights; docket and doctrine accumulate
 *   - - european_commission: enforcement arm (institutional/constrained) — infringement actions, enlargement transition bargains, coordination regulations
 *   - - displaced_local_labor: primary cost bearer (powerless/trapped) — wage and scheduling pressure, housing competition in gateway trades
 *   - - gateway_region_residents: diffuse cost bearer with service offsets (moderate/constrained) — rents and queues paid, staffing received
 *   - - sending_state_public_services: structural loser (organized/trapped) — trains staff whose returns accrue abroad, barred from retaining them
 *   - - national_welfare_administrations: administered boundary-loser (institutional/constrained) — eligibility discretion settled in Luxembourg
 *   - - member_state_governments: dual seat — domestic administrator and sovereignty-cost payer (institutional/constrained)
 *   - - third_country_nationals: excluded voice (powerless/trapped) — inside the territory, outside the rights settlement
 *   - - eu_mobility_research_community: analytical observer (analytical/analytical) — supplies the evidence both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.58).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free-Movement Priority over National Welfare Boundaries (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '81d1749e-7047-4902-b2a1-770825ee8095').
narrative_ontology:cs_kernel_codification('81d1749e-7047-4902-b2a1-770825ee8095', fixed_text).
narrative_ontology:cs_authority_grounding('81d1749e-7047-4902-b2a1-770825ee8095', lineage).
narrative_ontology:cs_interpretation_layer_present('81d1749e-7047-4902-b2a1-770825ee8095').
narrative_ontology:cs_reading_relation('81d1749e-7047-4902-b2a1-770825ee8095', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('81d1749e-7047-4902-b2a1-770825ee8095', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('81d1749e-7047-4902-b2a1-770825ee8095', foundational, mobility_rights_constitutive_of_membership).
narrative_ontology:cs_axiom_status(mobility_rights_constitutive_of_membership, holdable).
narrative_ontology:cs_axiom_grounding('81d1749e-7047-4902-b2a1-770825ee8095', mobility_rights_constitutive_of_membership, conventional).
narrative_ontology:cs_axiom('81d1749e-7047-4902-b2a1-770825ee8095', foundational, welfare_boundaries_subordinate_to_mobility).
narrative_ontology:cs_axiom_status(welfare_boundaries_subordinate_to_mobility, holdable).
narrative_ontology:cs_axiom_grounding('81d1749e-7047-4902-b2a1-770825ee8095', welfare_boundaries_subordinate_to_mobility, deontological).
narrative_ontology:cs_axiom('81d1749e-7047-4902-b2a1-770825ee8095', secondary, worker_status_prerequisite_for_residence).
narrative_ontology:cs_axiom_status(worker_status_prerequisite_for_residence, overridden).
narrative_ontology:cs_axiom_grounding('81d1749e-7047-4902-b2a1-770825ee8095', worker_status_prerequisite_for_residence, conventional).
narrative_ontology:cs_reference_frame('81d1749e-7047-4902-b2a1-770825ee8095', mobility_constitutive_membership_order).
narrative_ontology:cs_drift_state('81d1749e-7047-4902-b2a1-770825ee8095', contemporary_post_enlargement_backlash, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('81d1749e-7047-4902-b2a1-770825ee8095', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, intra_eu_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, court_of_justice_of_the_eu).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, european_commission).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, gateway_region_residents).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, sending_state_public_services).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, national_welfare_administrations).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, gateway_region_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move, take up employment, and reside in any member state under treaty rights; after qualifying periods they access the host state's social benefits on terms approaching those of nationals. Gains include wage differentials over origin-country levels, portable pension rights, and family life spanning borders. Exit is their defining capacity: they can return or move onward, though re-establishing benefit entitlements restarts waiting periods wherever they land.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, intra_eu_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Work in gateway regions and exposed trades (construction, food processing, warehousing, residential care) where incoming labor concentrates. Face wage moderation at the lower deciles, intensified scheduling competition, and crowded rental markets. Moving elsewhere within the country means chasing the same inflows; retraining pathways are slow and unevenly funded; many are anchored in place by tenancies, mortgages, and family care obligations.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, trapped, regional).

% Live in the districts receiving the largest inflows. Experience school-place shortages, longer GP waits, and escalating rents, while clinics, care homes, and food-processing lines stay open because mobile workers staff them. Household budgets absorb the housing costs; household care consumes the services the arrivals deliver.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, gateway_region_residents, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, gateway_region_residents, beneficiary).

% Operate the health, eldercare, and education systems of the main sending states, which trained professionals who then exercised mobility rights westward. Replace experienced nurses and physicians with short-contract staff or vacancies, and finance training whose returns accrue abroad. Retention through pay is out of reach while wage convergence lags decades behind, and the same rights regime they fund makes barring departure unlawful.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, sending_state_public_services, payer,
    organized, generational, trapped, national).

% Administer benefit systems whose eligibility boundaries are settled in Luxembourg as often as in national statute. Process coordination paperwork (S1 registrations, posted-worker declarations), bring or defend test cases they expect will narrow their discretion, and absorb verification burdens for residence-linked claims. Their statutory categories survive on paper but bind less each decade.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_welfare_administrations, payer,
    institutional, generational, constrained, national).

% Co-legislate the framework yet administer its daily application. Negotiate opt-outs and transitional controls at each accession, invoke safeguard clauses in downturns, and answer electorates that blame the framework for local pressures. Full departure has been demonstrated possible once, at ruinous cost, and is not a working option for the remainder; their leverage runs through unanimity votes and infringement defenses rather than through setting the terms.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, agenda_setter).

% Staff operations across borders from a single continental labor pool: smooth shortages by recruiting where skills sit, moderate wage growth with credible access to alternative supply, and relocate functions between member states when labor costs shift. Posting arrangements and mutual recognition of qualifications make multi-country deployment routine rather than exceptional.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, multinational_employers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Decides preliminary references that define how far mobility rights reach into national social systems; each ruling extends or bounds the acquis and becomes precedent no member state voted on directly. Its docket, doctrinal legacy, and institutional weight grow with every social-policy reference it answers. It exits nothing and answers to the member states collectively rather than to any affected class of individuals.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, court_of_justice_of_the_eu, agenda_setter,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, court_of_justice_of_the_eu, beneficiary).

% Opens infringement proceedings against member states that restrict mobile workers' equal treatment, negotiates transition periods at each enlargement, and proposes the coordination regulations that govern benefit aggregation across borders. Its portfolio and initiative monopoly over the mobility file expand with each competence the case law confirms.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, european_commission, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, european_commission, beneficiary).

% Reside, work, and pay taxes inside member states while remaining outside the citizenship-mobility settlement: no automatic cross-border residence rights, benefit access gated by national schemes and long regularization paths. They watch a border-free zone operate for others and hold no seat in the forum that defines it.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, third_country_nationals, excluded,
    powerless, biographical, trapped, continental).

% Produces the longitudinal microdata and natural-experiment literature (wage-compression estimates, fiscal-balance panels, posting-regime audits) that every other seat cites when convenient; publishes findings regardless of which seat they embarrass.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_mobility_research_community, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, multinational_employers).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation of twenty-seven bounded labor markets: allocates labor toward where it prices highest, gives citizens an enforceable cross-border legal status, prevents beggar-thy-neighbor welfare closure during asymmetric shocks, and makes the goods, services, and capital freedoms credible by guaranteeing the labor freedom reciprocally.
% TRANSFER_FUNCTION: Moves labor and human capital from lower-wage to higher-wage member states; moves welfare-system access rights from nationally-bounded residence pools to qualifying mobile workers; moves adjudicative authority over social-policy boundaries from national parliaments and courts to Luxembourg; and leaves the adjustment costs (wage pressure, housing competition, emptied training pipelines) concentrated on the non-mobile and the sending side.
% ABSENT_VOICES: Third-country nationals living and working inside the settlement's territory but excluded from its rights architecture would object that a mobility regime is being constitutionalized over their heads on citizenship grounds they can never satisfy. Non-mobile low-income residents of gateway regions register their objection chiefly through populist electoral vehicles rather than any seat in the process-design forum. Sending-state rural communities losing care staff have no Union-level representation of their specific loss. None of the three holds a seat in the configuration that defines this arrangement.
% DISAPPEARANCE_RATIONALE: Overnight repeal would strand millions of settled cross-border households mid-entitlement, break employer staffing models built on posting and recognition, damage the single market's credibility (if labor can be closed, so can the rest), vacate a large share of the Court's social docket, and force twenty-seven welfare systems to redraw residence tests against one another simultaneously. Rearrangement would be violent and slow; nothing approaches equilibrium quickly.
% FOUNDING_PROBLEM: After 1945, Western European states sought to make interstate war materially impossible by entangling coal, steel, trade, and eventually labor so tightly that national economic closure would amount to a hostile act. Free movement of workers entered the common market at its founding so that no member state could weaponize labor barriers against another; Maastricht later made mobility the constitutive content of a new Union citizenship.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the accession applications of formerly neutral states (Austria, Finland, Sweden) cited the security-and-interdependence rationale in official memoranda; successive national security strategies treat economic interdependence as a war deterrent; integration historiography documents the founding motive in the drafting record of the treaties themselves. Stated plainly: the welfare-boundary-yield component enjoys no equivalent external corroboration. National social-law scholars dispute that deep welfare harmonization follows from the security founding problem, and that silence about necessity is itself signal.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: from this reading's own seat the arrangement delivers its constitutional purpose, but three cost channels are real and uncompensated (concentrated wage and housing pressure on trapped local labor, training-return drain from sending-state systems, adjudicative-authority rents accruing to Luxembourg); the reading judges the overall bargain net-positive without denying the debit side of the ledger. Suppression 0.58: persistence depends on active legal machinery (primacy, infringement penalties, precedent discipline) applied against member-state closure attempts; it is structural coercion aimed at governments, with minimal force ever applied at the level of individuals. Suppression is authored as a raw structural property and is not scaled by power or scope in the story; only extractiveness is scaled, by the engine, through directionality and scope. Theater 0.28: solidarity rhetoric and pre-enlargement assurances that workers will not come perform inclusion, while services-mode carve-outs and safeguard clauses preserve exits for capital that labor lacks. Accessibility collapse 0.62: once primacy is understood, wholesale national welfare closure is foreseeable as legally void, yet partial instruments persist (proportionality-bounded residence tests, transitional controls, safeguard invocation), so alternatives dim without vanishing. Resistance 0.72: sustained and occasionally decisive — one member state exited, opt-out protocols persist, national constitutional courts contest primacy's reach, and safeguard clauses are invoked in every downturn. All three temporal series run on ONE shared seven-point grid; every tracked metric carries an authored value at every examined point. The suppression_requirement series is authored deliberately rather than defaulted: this story's enforcement-capacity arc is part of its meaning — machinery ratcheted upward through the post-enlargement decade, peaked around the backlash years, then partially receded as the Court itself bounded jobseekers' access (the Dano-line turn). The trajectories are monotone with one doctrinal inflection rather than cyclical; no intermittent-reinforcement mechanism is claimed, and the scalar base_properties values are the interval-end states of the same series.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply different per-seat types. From displaced_local_labor and sending_state_public_services (trapped or bound payers at powerless and organized power), the arrangement computes snare-leaning: costs arrive without consent and exit is theoretical. From intra_eu_mobile_workers (mobile) and multinational_employers (arbitrage), it computes rope-leaning: a rights regime that simply works. The Luxembourg seats compute coordination-with-authority-rents: the Court experiences its rulings as elaborating a text the states freely ratified, while payer seats experience the same rulings as rules nobody they elected wrote. Identity-lock binds the integration elite: for Court and Commission careers the acquired-body-of-law worldview is professional identity, the frame in which yielding welfare boundaries reads as constitutional maturity rather than dispossession; if that frame broke, the institutional seats would compute far less coordination weight and the authority-rent channel would surface plainly. On the suppression-mechanism ambiguity: this constraint's suppression is predominantly structural (legal primacy binding national legislatures and administrators); a minority is internalized, as national officials socialized into conformity continue treating boundaries as void even where enforcement slackens — roughly a large structural share and a smaller internalized residue.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations map onto real flows. Mobile workers receive wage differentials and benefit access; their defining mobility pushes their derived directionality firmly toward the beneficiary end. Multinational employers collect the largest concentrated surplus (continental staffing option value, wage-trajectory control, posting agility) and arbitrage-grade exit places them nearest the beneficiary pole. The Luxembourg seats derive low directionality from their beneficiary declarations, though their agenda_setter roles mean the derivation should not read them as pure subsidy-recipients; the dual roles carry that nuance. Trapped payers sit at the target end: displaced local labor cannot relocate faster than inflows arrive, sending-state systems are bound by the very rights they finance training for, and national administrations and governments bear competence loss with only constrained exit (departure demonstrated once, at ruinous cost). No directionality_overrides are used: the derivation chain from declared roles plus exit options reproduces these positions faithfully, and an override keyed to the institutional power atom would wrongly homogenize seats this story exists to distinguish (national_welfare_administrations on the target side versus the Court on the beneficiary side share a power atom but hold opposite structural relationships). Receipt surface: gain_flow names multinational_employers because receipt-of-gain and beneficiary-role are distinct facts — the workers' gains are broad but individually bounded and partly returned through taxes, while the employers' seat demonstrably accrues the arrangement's transferable surplus; the Court's gain (doctrine, docket, weight) is real but not transferable in the same ledger, so it is not the receipt seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making interstate war materially impossible by entangling economies until closure equals aggression) remains live and is corroborated from outside the beneficiary set, so no mandatrophy resolution is declared and none should be inferred. The tangled_rope classification does double preventive work. Against the snare mislabel: a pure-extraction reading would erase the coordination the regime actually delivers (portable pensions, qualification recognition, pandemic-era redistribution of essential workers to where beds and shelves needed filling) — coordination that would persist regardless of anyone defending it rhetorically. Against the rope whitewash: a pure-coordination reading would dissolve the named victims into transition costs, ignoring that the payer seats are precisely those with the least exit and the weakest voice where the rules are written. Mandatrophy drift is watched rather than resolved: if the security rationale decays while boundary-yield intensifies, the founding-problem status flips and the arrangement slides toward piton-or-snare territory; the temporal series (extractiveness rising through 2016 before easing) is the kind of accumulating-extraction record that abductive triggers feed on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the federation_membership_obligations kernel is operative: mobility-right priority (this file), sovereign closure authority (member_sovereignty_primary), or contribution-tiered access (selective_solidarity)?',
    'Not resolvable inside this file: each reading is a separate constraint with its own epsilon and victim set. Resolution arrives only through treaty-amendment politics, accumulated CJEU doctrine shifts, or corpus-level comparison across the three sibling stories.',
    'If member_sovereignty_primary prevailed, the victim set inverts: mobile workers facing lawful exclusion become the targets and this file''s payer seats move toward beneficiary positions; if selective_solidarity prevailed, non-contributory mobiles become the target class and displaced-local-labor costs attenuate. Every seat''s classification moves with the answer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer structure: this constraint is the integration_primary reading of one contested kernel; sibling readings instantiate rival arrangements with different victim sets.').

omega_variable(
    net_fiscal_contribution_ambiguity,
    'Are mobile EU workers net fiscal contributors or net recipients in typical receiving states over a working lifetime?',
    'Longitudinal administrative microdata linking tax contributions and benefit receipts per arrival cohort, as produced by the research-community seat.',
    'If broadly net-contributory, the taxpayer-side victim channel collapses, epsilon falls, and the arrangement drifts toward rope with costs concentrated on displaced labor alone; if net-recipient in gateway strata, an additional victim seat hardens and epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_fiscal_contribution_ambiguity, empirical, 'Fiscal balance of the mobile population: decides whether one declared victim channel is structural or rhetorical.').

omega_variable(
    labor_displacement_vs_complementarity,
    'Does incoming mobile labor compress wages and displace non-mobile workers in exposed local markets, or complement them?',
    'Replicated natural-experiment designs (admission-wave discontinuities, settlement-pattern instruments) reconciling the conflicting area-based and individual-panel literatures.',
    'If displacement is marginal, displaced_local_labor is closer to a rhetorically constructed victim, epsilon drops, and the classification tilts toward rope; if displacement is concentrated and durable, that victim seat hardens and the arrangement skews snare-ward at the trapped seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_vs_complementarity, empirical, 'Whether the primary human cost channel is structural or narrated.').

omega_variable(
    ecj_expansion_accountability_deficit,
    'Is the Court''s case-law-driven extension of mobility rights into social policy an accountability deficit extracting an authority rent, or ordinary judicial elaboration of a text the states freely ratified?',
    'Comparative constitutional analysis of delegation breadth at ratification versus the doctrine reached; intergovernmental conference records on intended social-policy scope.',
    'If deficit, the institutional beneficiary seats carry capture weight and the arrangement skews snare-ward at those seats; if ordinary elaboration, the authority-rent channel shrinks and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_expansion_accountability_deficit, conceptual, 'Legitimacy character of the Luxembourg seats'' expanding role in social-policy boundary-setting.').

omega_variable(
    sending_state_adjustment_permanence,
    'Will sending-state public systems'' losses persist, or do wage convergence and return migration retire this victim seat within a generation?',
    'Cohort tracking of return-migration rates and sectoral staffing recovery in the principal sending states.',
    'If losses retire, the sending-side seat behaves as a transitional cost with a natural sunset, softening the arrangement''s extraction profile at that seat; if losses persist, the seat calcifies as a permanent structural payer and the asymmetry hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_adjustment_permanence, empirical, 'Permanence question for the sending-side victim channel.').

omega_variable(
    coercion_grid_level_resolution,
    'Are the authored level-resolved grid judgments (individual freedom expanding while structural coercion hardens) robust, or artifacts of conservative fill where the story underdetermines a level?',
    'Level-disaggregated evidence: individual-level mobility-friction measures, organizational compliance-cost surveys, class-level electoral-pressure indices, and structural infringement-and-penalty records.',
    'If the gradient fails plausibility audit, the cross-level drift conclusions report OPEN rather than confirmed; the scalar classification stands unaffected, but any level-gradient claim withdraws.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_grid_level_resolution, conceptual, 'Uncertainty flag on the authored coercion grid''s conservative level-resolved judgments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_obligations__integration_primary, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__integration_primary, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_obligations__integration_primary, theater_ratio, 2004, 0.22).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_obligations__integration_primary, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_obligations__integration_primary, theater_ratio, 2016, 0.32).
narrative_ontology:measurement(fede_tr_t2021, federation_membership_obligations__integration_primary, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(fede_tr_t2026, federation_membership_obligations__integration_primary, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_obligations__integration_primary, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__integration_primary, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(fede_be_t2004, federation_membership_obligations__integration_primary, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(fede_be_t2010, federation_membership_obligations__integration_primary, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(fede_be_t2016, federation_membership_obligations__integration_primary, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(fede_be_t2021, federation_membership_obligations__integration_primary, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement(fede_be_t2026, federation_membership_obligations__integration_primary, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_obligations__integration_primary, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__integration_primary, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(fede_su_t2004, federation_membership_obligations__integration_primary, suppression_requirement, 2004, 0.5).
narrative_ontology:measurement(fede_su_t2010, federation_membership_obligations__integration_primary, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(fede_su_t2016, federation_membership_obligations__integration_primary, suppression_requirement, 2016, 0.66).
narrative_ontology:measurement(fede_su_t2021, federation_membership_obligations__integration_primary, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement(fede_su_t2026, federation_membership_obligations__integration_primary, suppression_requirement, 2026, 0.58).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1992, tn=2026
narrative_ontology:measurement(fede_grid_01, federation_membership_obligations__integration_primary, accessibility_collapse(class), 1992, 0.3).
narrative_ontology:measurement(fede_grid_02, federation_membership_obligations__integration_primary, accessibility_collapse(class), 2026, 0.35).
narrative_ontology:measurement(fede_grid_03, federation_membership_obligations__integration_primary, accessibility_collapse(individual), 1992, 0.3).
narrative_ontology:measurement(fede_grid_04, federation_membership_obligations__integration_primary, accessibility_collapse(individual), 2026, 0.22).
narrative_ontology:measurement(fede_grid_05, federation_membership_obligations__integration_primary, accessibility_collapse(organizational), 1992, 0.35).
narrative_ontology:measurement(fede_grid_06, federation_membership_obligations__integration_primary, accessibility_collapse(organizational), 2026, 0.38).
narrative_ontology:measurement(fede_grid_07, federation_membership_obligations__integration_primary, accessibility_collapse(structural), 1992, 0.55).
narrative_ontology:measurement(fede_grid_08, federation_membership_obligations__integration_primary, accessibility_collapse(structural), 2026, 0.72).
narrative_ontology:measurement(fede_grid_09, federation_membership_obligations__integration_primary, resistance(class), 1992, 0.3).
narrative_ontology:measurement(fede_grid_10, federation_membership_obligations__integration_primary, resistance(class), 2026, 0.55).
narrative_ontology:measurement(fede_grid_11, federation_membership_obligations__integration_primary, resistance(individual), 1992, 0.25).
narrative_ontology:measurement(fede_grid_12, federation_membership_obligations__integration_primary, resistance(individual), 2026, 0.2).
narrative_ontology:measurement(fede_grid_13, federation_membership_obligations__integration_primary, resistance(organizational), 1992, 0.35).
narrative_ontology:measurement(fede_grid_14, federation_membership_obligations__integration_primary, resistance(organizational), 2026, 0.42).
narrative_ontology:measurement(fede_grid_15, federation_membership_obligations__integration_primary, resistance(structural), 1992, 0.45).
narrative_ontology:measurement(fede_grid_16, federation_membership_obligations__integration_primary, resistance(structural), 2026, 0.65).
narrative_ontology:measurement(fede_grid_17, federation_membership_obligations__integration_primary, stakes_inflation(class), 1992, 0.25).
narrative_ontology:measurement(fede_grid_18, federation_membership_obligations__integration_primary, stakes_inflation(class), 2026, 0.45).
narrative_ontology:measurement(fede_grid_19, federation_membership_obligations__integration_primary, stakes_inflation(individual), 1992, 0.2).
narrative_ontology:measurement(fede_grid_20, federation_membership_obligations__integration_primary, stakes_inflation(individual), 2026, 0.18).
narrative_ontology:measurement(fede_grid_21, federation_membership_obligations__integration_primary, stakes_inflation(organizational), 1992, 0.3).
narrative_ontology:measurement(fede_grid_22, federation_membership_obligations__integration_primary, stakes_inflation(organizational), 2026, 0.38).
narrative_ontology:measurement(fede_grid_23, federation_membership_obligations__integration_primary, stakes_inflation(structural), 1992, 0.4).
narrative_ontology:measurement(fede_grid_24, federation_membership_obligations__integration_primary, stakes_inflation(structural), 2026, 0.55).
narrative_ontology:measurement(fede_grid_25, federation_membership_obligations__integration_primary, suppression(class), 1992, 0.2).
narrative_ontology:measurement(fede_grid_26, federation_membership_obligations__integration_primary, suppression(class), 2026, 0.25).
narrative_ontology:measurement(fede_grid_27, federation_membership_obligations__integration_primary, suppression(individual), 1992, 0.15).
narrative_ontology:measurement(fede_grid_28, federation_membership_obligations__integration_primary, suppression(individual), 2026, 0.12).
narrative_ontology:measurement(fede_grid_29, federation_membership_obligations__integration_primary, suppression(organizational), 1992, 0.25).
narrative_ontology:measurement(fede_grid_30, federation_membership_obligations__integration_primary, suppression(organizational), 2026, 0.28).
narrative_ontology:measurement(fede_grid_31, federation_membership_obligations__integration_primary, suppression(structural), 1992, 0.4).
narrative_ontology:measurement(fede_grid_32, federation_membership_obligations__integration_primary, suppression(structural), 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% Constraint family: the label 'free movement versus welfare boundaries' decomposes into three readings of one kernel with different victim sets and different epsilon. This reading's victims are displaced local labor, gateway-region residents, sending-state systems, national welfare administrations, and member-state governments; member_sovereignty_primary's victims would instead be mobile workers facing lawful exclusion; selective_solidarity's victims would be non-contributory mobiles denied tiered access. Family members link via network.affects_constraints. Pressure runs asymmetrically: this reading's case-law expansion creates structural downstream pressure on the selective_solidarity proposal space (every citizenship-based extension enlarges the constituency for contributory tiering), while coexisting indefinitely with member_sovereignty_primary as opposing defaults held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
