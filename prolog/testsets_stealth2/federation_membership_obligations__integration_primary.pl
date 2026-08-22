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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: Integration-Primary Reading: Free Movement Constitutive of Union Citizenship, Welfare Boundaries Subordinate
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   Under the integration-primary reading, free movement is not a concession
 *   states grant but the constitutive activity of Union citizenship itself,
 *   and member-state welfare boundaries must accommodate whoever exercises
 *   it. The standing arrangement this story is ABOUT — the ε referent — is
 *   that mobility-first regime as it actually operates: mobile workers
 *   entering full host welfare beneficiary sets, displaced local labor and
 *   host contributor pools bearing the adjustment, sending regions bleeding
 *   working-age members, and the Court of Justice expanding its membership
 *   jurisprudence case by case. The reading presents the arrangement as a
 *   constitutive good; the structural data show a genuine coordination
 *   function carrying asymmetric, actively enforced costs — hence the
 *   tangled-rope claim. This file is one reading of the kernel
 *   federation_membership_obligations; the sibling readings
 *   (member_sovereignty_primary, selective_solidarity) are separate
 *   constraints with their own epsilon values and victim sets, linked through
 *   the network block.
 *
 * KEY AGENTS:
 *   - - mobile_eu_workers: Primary beneficiary (moderate/arbitrage) — enter full host welfare beneficiary set; best exit position in the story
 *   - - destination_state_employers: Concentrated beneficiary (organized/arbitrage) — receive elastic labor supply and bottom-end wage moderation while externalizing absorption costs
 *   - - ecj_integrationist_judiciary: Agenda-setter and authority beneficiary (institutional/identity_locked) — expands membership obligations through preliminary rulings; administrates the boundary it benefits from moving
 *   - - displaced_local_labor: Primary payer (moderate/constrained) — bears bottom-segment wage, housing, and service-adjustment costs regionally
 *   - - host_state_contributors: Payer (moderate/trapped) — fund early-year welfare absorption before arrivals' contributions mature
 *   - - sending_state_communities: Payer (moderate/constrained) — lose working-age members, tax base, and care capacity over generational time
 *   - - member_state_governments: Payer-administrator (institutional/constrained) — deliver welfare under boundaries they no longer control; litigate at the margins
 *   - - third_country_nationals: Excluded (powerless/trapped) — outside the rights tier entirely; the arrangement's silent comparator
 *   - - national_constitutional_courts: Observer (institutional/analytical) — police how far integration may reach into national welfare systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.62).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "Integration-Primary Reading: Free Movement Constitutive of Union Citizenship, Welfare Boundaries Subordinate").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '3a295c2f-69eb-4bda-951a-7cbe94b1b81e').
narrative_ontology:cs_kernel_codification('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', formalized).
narrative_ontology:cs_authority_grounding('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', lineage).
narrative_ontology:cs_interpretation_layer_present('3a295c2f-69eb-4bda-951a-7cbe94b1b81e').
narrative_ontology:cs_reading_relation('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', foundational, free_movement_constitutes_union_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutes_union_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', free_movement_constitutes_union_citizenship, conventional).
narrative_ontology:cs_axiom('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', foundational, welfare_boundaries_yield_to_mobility_rights).
narrative_ontology:cs_axiom_status(welfare_boundaries_yield_to_mobility_rights, holdable).
narrative_ontology:cs_axiom_grounding('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', welfare_boundaries_yield_to_mobility_rights, instrumental).
narrative_ontology:cs_reference_frame('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', mobility_constitutive_citizenship_framework).
narrative_ontology:cs_drift_state('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', post_brexit_repudiation_wave, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3a295c2f-69eb-4bda-951a-7cbe94b1b81e', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, destination_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, ecj_integrationist_judiciary).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, host_state_contributors).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, sending_state_communities).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, union_citizenship_fundamental_status_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, primacy_and_direct_effect_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, single_market_four_freedom_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work and reside in member states other than their own under equal-treatment rules. Once economically active they enter the host welfare system's beneficiary set — health coverage, family benefits, pension accrual — with cross-border coordination so contributions and entitlements follow them. Their exit is unusually good: they chose their destination and can choose another, or return home, carrying accrued rights.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, arbitrage, continental).

% Recruit from a continent-wide labor pool without visa machinery. Mobility keeps bottom-of-market wages moderated and staffing elastic in agriculture, care work, construction, logistics, and hospitality. They bear little of the welfare-absorption cost that falls on public budgets during workers' early resident years.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, destination_state_employers, beneficiary,
    organized, biographical, arbitrage, continental).

% Answers preliminary references on what member states may demand of mobile citizens before granting benefits. Each ruling narrows what national welfare boundaries may exclude and widens the doctrine the Court administers. Its docket, precedent stock, and constitutional role grow with every extension; stepping back from the integration project would dissolve the role the institution has become.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj_integrationist_judiciary, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, ecj_integrationist_judiciary, beneficiary).

% Competes for bottom-segment jobs and scarce rental housing against an elastic incoming workforce. Adjustment shows up as wage stagnation in exposed occupations, longer social-housing queues, and pressure on local schools and clinics. Moving away, retraining, or organizing are possible but slow and individually costly; most absorb the adjustment where they stand.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    moderate, biographical, constrained, regional).

% Pay taxes and social contributions that fund benefit uptake by newly arrived workers and their families before those households' own contributions mature. They cannot opt out of the arrangement and express dissent mainly through elections, which is where the backlash politics concentrates.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, host_state_contributors, payer,
    moderate, biographical, trapped, national).

% Lose working-age members to destination markets — nurses, builders, seasonal workers — along with their tax base and informal care capacity. Remittances and returned unemployment liabilities offset part of the loss; rural regions and care sectors feel the depletion most, over decades.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, sending_state_communities, payer,
    moderate, generational, constrained, national).

% Deliver welfare, schooling, and housing under eligibility rules they can no longer set unilaterally; the boundary decisions sit with treaty law and the Court. They retain day-to-day administration and bear the fiscal and political costs of absorption. Leaving the arrangement entirely means treaty exit at existential cost, so they litigate at the margins instead.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, agenda_setter).

% Live and work in the EU without the mobility-and-welfare package that citizens of member states carry. They would contest the tiering that grants newcomers from inside the union rights denied to longer-settled residents from outside it, but they hold no procedural seat in the conversation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, third_country_nationals, excluded,
    powerless, biographical, trapped, continental).

% Review what the integration project may demand of national welfare systems and budgets. They argue the member-state side in their judgments, delimit how far mobility obligations reach, and provide the institutional voice for the closure-authority position without administering anything themselves.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, destination_state_employers).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates labor across twenty-seven-plus national labor markets inside a single market: makes qualifications mutually recognized, bars nationality discrimination against workers exercising mobility, gives firms a continent-wide recruitment pool, and gives workers enforceable rights to move, reside, and work abroad. Welfare-system accommodation (equal treatment once economically active) removes the benefit cliff that would otherwise deter take-up of cross-border jobs.
% TRANSFER_FUNCTION: Moves working-age labor from higher-unemployment, lower-wage member states to destination markets; moves welfare entitlements and school, health, and housing capacity from host-state contributor pools to incoming workers and their families during their early resident years; moves interpretive authority over membership boundaries from national parliaments and ministries to the Court of Justice through preliminary references.
% ABSENT_VOICES: Third-country nationals resident in the EU sit outside the rights tier entirely and have no seat in the jurisprudential conversation. Displaced local workers appear only indirectly, filtered through their governments' litigation positions; they are never parties. Sending-region municipalities that lose working-age residents have no procedural role in host-state welfare design.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand millions of cross-border workers and their families in legal limbo, collapse staffing in sectors dependent on intra-EU recruitment (health care, agriculture, construction, hospitality), force immediate renegotiation of social-security coordination, and delete the Court's citizenship jurisprudence wholesale — the single market's labor dimension would reorganize around bilateral treaties on roughly the Swiss model.
% FOUNDING_PROBLEM: After 1945, Western European states sought to make interstate war materially impossible and to rebuild industrial capacity: pooling coal and steel, then guaranteeing that no member could seal its labor market or discriminate against another's workers, so that economic interdependence would lock in political peace. Maastricht (1992) later attached a citizenship status to that mobility so that rights would follow persons, not just factors of production.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: national constitutional courts (notably the German Federal Constitutional Court) attest the original market-integration mandate while disputing that it licenses open-ended welfare accommodation; independent labor economics and OECD/IMF analyses corroborate both the allocative gains and the localized adjustment costs; the UK's 2016 withdrawal vote stands as external attestation that a large electorate judged the mobility settlement's terms unacceptable. No attestation is available exclusively from within the beneficiary set.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.58: the arrangement carries real asymmetric costs — host welfare absorption during arrivals' early resident years, bottom-segment wage and housing pressure, origin-region human-capital depletion — set against a genuine allocative coordination function that keeps a continental labor market liquid. Suppression 0.62 reflects legal compulsion, not participant preference: primacy and direct effect foreclose member-state closure options, which is precisely what this reading requires ('boundaries must yield'). Suppression here is structural (treaty law and case law), not internalized; no interpersonal mechanism applies. Theater 0.33: the functional core (actual mobility, actual benefit portability, actual rulings) is real, but a growing share of activity is ceremonial defense of the citizenship frame as its political support narrows — citizenship rhetoric thickening as substance becomes contested. Accessibility_collapse 0.45: workable alternatives are demonstrated, not hypothetical — Swiss bilateralism, Danish opt-outs, the Norwegian EEA adjacency, and one completed member exit all show the arrangement is not the only way to run a European economy. Resistance 0.70: sustained member-state litigation, restrictive benefit legislation, emergency-brake demands, and one outright repudiation. The temporal series run on one shared seven-point grid (1993-2025) so every metric is authored at every examined time point; the enforcement arc builds through the enlargement decade, peaks around 2014 (maximal citizenship jurisprudence meeting peak political backlash), and eases slightly afterward as the Dano-line trims jobseeker access and Brexit removes the most vocal objector. End-state values match the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and the engine derives that divergence from the structural data. From the mobile worker's seat the arrangement is almost pure enablement (arbitrage-grade exit, full beneficiary set); from the displaced local worker's and contributor's seats it is imposed cost without consent or exit. The sharpest divergence is same-level: member_state_governments all hold identical formal institutional power, yet net-receiving governments (absorbing fiscal and political costs, gaining labor) and net-sending governments (exporting unemployment, losing workers, collecting remittances) sit at nearly opposite directionalities — differentiation comes from labor-flow position, not global standing. The Court's seat adds an identity-lock dynamic: its institutional identity has fused with the integration project (the organization has become its function), so its agenda-setting cannot be evaluated as neutral administration; a Court that reframed Union citizenship as derivative of national membership would be a different actor, and the classification of the whole arrangement would shift with it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-d seats: mobile_eu_workers (declared beneficiary, arbitrage exit — nearest the full-beneficiary end), destination_state_employers (declared beneficiary, organized power, arbitrage — captures the labor-market margin while externalizing absorption costs), and ecj_integrationist_judiciary (secondary beneficiary whose authority stock grows with each ruling). Victim declarations drive the high-d seats: displaced_local_labor (constrained exit, regional scope — bears the adjustment where it stands), host_state_contributors (trapped — cannot exit the tax-contribution system), sending_state_communities (generational losses), and member_state_governments (bear the imposed boundary loss while retaining administration). No directionality_overrides are authored: the derivation chain from declared roles, power, and exit options captures the asymmetries, and the override surface keys on power_atom alone — an override at 'moderate' would collide across seats sharing that atom (mobile workers and sending communities are both moderate) and drag beneficiaries toward target positions. Where a seat is genuinely dual-positioned (the Court, the member governments), the dual position is carried in secondary_role rather than forced through a single scalar.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring the arrangement as the reading presents it — a pure constitutive good — would launder the adjustment costs onto seats with no procedural voice; authoring it as pure extraction would erase the coordination function that actually allocates labor across twenty-seven systems and that no plausible alternative replicates at scale. The tangled-rope authoring keeps both halves structurally visible: the coordination gate is satisfied (a real collective-action problem, solved centrally) and the extraction gate is satisfied (named payers through the same structure, actively enforced). The mandatrophy risk sits in the genealogy: the war-prevention rationale that founded the mobility project is aging while the arrangement compounds, and if the market-making phase is complete the remaining structure persists partly on momentum. Authoring founding_problem_status as contested — rather than resolving it by fiat in either direction — keeps that question load-bearing: the mismatch consumer can test whether a dead founding problem is sustaining a world-rearranging arrangement, which is exactly the capture/zombie signature this corpus exists to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the integration_primary reading of the kernel federation_membership_obligations. Would instantiating a sibling reading — member_sovereignty_primary (national welfare states retain closure authority; mobility conditional on labor-market protection) or selective_solidarity (rights tiered by contribution history; access follows the contributory principle) — produce a structurally different constraint?',
    'Generate the sibling readings as separate stories and compare victim sets, directionality distributions, and computed types: member_sovereignty_primary restores national closure authority (the paying seats shift toward mobile workers denied entry); selective_solidarity re-keys access to contribution history (victims become non-contributory movers). The disagreement is located in the priority ordering between mobility rights and welfare-boundary authority, not in the existence of either element.',
    'If a sibling reading became the operative commitment, this reading''s beneficiary/victim structure inverts or re-tiers and the computed classification follows the new structure; this file''s epsilon is authored only for the mobility-first arrangement, not averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of a contested membership-obligations kernel; sibling adoption would restructure the beneficiary/victim sets.').

omega_variable(
    lifecycle_fiscal_incidence,
    'Do mobile workers repay their early-year welfare absorption over their working lives, or does net fiscal transfer from host contributor pools persist at current flow levels?',
    'Longitudinal administrative data matching cohorts of intra-EU movers to lifetime contribution and receipt records in host states, separated by skill band and family composition.',
    'If repayment holds, the arrangement''s extraction is transitional adjustment cost and the coordination half of the structure strengthens; if net transfer persists, extraction is structural rent and the payer seats'' effective burden rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lifecycle_fiscal_incidence, empirical, 'Whether the welfare transfer to mobile workers is a lifecycle loan or a standing subsidy.').

omega_variable(
    displacement_attribution,
    'How much of bottom-segment wage stagnation and housing pressure in destination regions is attributable to intra-EU mobility rather than automation, minimum-wage design, or land-use policy?',
    'Difference-in-differences designs exploiting transitional-control episodes (the 2004 and 2007 enlargements, the 2014 expiry of UK controls) to isolate mobility shocks from concurrent domestic trends.',
    'High mobility attribution raises the payer seats'' effective extraction and validates the adjustment-cost framing; low attribution shifts the burden story to domestic policy choices and lowers measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_attribution, empirical, 'Attribution of destination-region adjustment costs to mobility versus confounding domestic causes.').

omega_variable(
    jurisprudential_durability,
    'Can the Court''s expansive citizenship line survive member-state counter-pressure delivered through treaty revision, unanimity politics, and further exits?',
    'Track Article 48 TEU convention activity, changes in infringement patterns, and whether post-Dano restrictiveness stabilizes or deepens across successive rulings.',
    'Sustained reversal would convert this reading from operative commitment to historical position and transfer classification weight to whichever sibling replaces it; consolidation would entrench the current structure and its extraction profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jurisprudential_durability, empirical, 'Durability of the expansive jurisprudential line under political counter-pressure.').

omega_variable(
    court_identity_lock,
    'Is the Court''s maintenance of the expansive line a product of institutional identity fusion with the integration project, and would a reframing of Union citizenship as derivative of national membership break that lock?',
    'Compare rulings across judge-generation cohorts and against explicit member-state consensus signals; observe whether the Court sustains the line when political opposition is open and unified.',
    'If the identity frame broke, enforcement of this reading would weaken sharply and the arrangement would drift toward the member_sovereignty_primary configuration without any formal treaty change — a classification shift driven by the enforcer''s identity, not by the rules'' text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_identity_lock, conceptual, 'Whether the enforcing institution''s identity fusion, rather than doctrine, sustains the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1993, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmo_integration_primary_tr_t1993, federation_membership_obligations__integration_primary, theater_ratio, 1993, 0.2).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t1993, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2000, federation_membership_obligations__integration_primary, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2000, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2004, federation_membership_obligations__integration_primary, theater_ratio, 2004, 0.24).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2004, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2008, federation_membership_obligations__integration_primary, theater_ratio, 2008, 0.26).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2008, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2014, federation_membership_obligations__integration_primary, theater_ratio, 2014, 0.3).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2014, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2019, federation_membership_obligations__integration_primary, theater_ratio, 2019, 0.34).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2019, observed).
narrative_ontology:measurement(fmo_integration_primary_tr_t2025, federation_membership_obligations__integration_primary, theater_ratio, 2025, 0.33).
narrative_ontology:measurement_basis(fmo_integration_primary_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(fmo_integration_primary_be_t1993, federation_membership_obligations__integration_primary, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t1993, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2000, federation_membership_obligations__integration_primary, base_extractiveness, 2000, 0.47).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2000, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2004, federation_membership_obligations__integration_primary, base_extractiveness, 2004, 0.52).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2004, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2008, federation_membership_obligations__integration_primary, base_extractiveness, 2008, 0.56).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2008, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2014, federation_membership_obligations__integration_primary, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2014, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2019, federation_membership_obligations__integration_primary, base_extractiveness, 2019, 0.57).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2019, observed).
narrative_ontology:measurement(fmo_integration_primary_be_t2025, federation_membership_obligations__integration_primary, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(fmo_integration_primary_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(fmo_integration_primary_su_t1993, federation_membership_obligations__integration_primary, suppression_requirement, 1993, 0.4).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t1993, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2000, federation_membership_obligations__integration_primary, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2000, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2004, federation_membership_obligations__integration_primary, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2004, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2008, federation_membership_obligations__integration_primary, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2008, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2014, federation_membership_obligations__integration_primary, suppression_requirement, 2014, 0.66).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2014, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2019, federation_membership_obligations__integration_primary, suppression_requirement, 2019, 0.63).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2019, observed).
narrative_ontology:measurement(fmo_integration_primary_su_t2025, federation_membership_obligations__integration_primary, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(fmo_integration_primary_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, selective_solidarity).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial debate 'free movement versus the welfare state' covers three structurally distinct claims, not one. This file authors the integration_primary instance (mobility constitutive, welfare boundaries subordinate; epsilon 0.58 over the standing mobility-first arrangement). member_sovereignty_primary authors the closure-authority instance (different victim set: mobile workers refused entry; different epsilon). selective_solidarity authors the contributory-tiering instance (victim set: non-contributory movers; different epsilon again). Measuring 'the free movement constraint' with observables drawn from a sibling reading changes epsilon — which is the signal that these are different constraints sharing a label. This reading sits upstream: its case-law expansions are cited as settled fact in the siblings' disputes and structurally pressure both (every citizenship-based grant erodes the space in which closure authority or contribution-tiering can be defended), without logically eliminating either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
