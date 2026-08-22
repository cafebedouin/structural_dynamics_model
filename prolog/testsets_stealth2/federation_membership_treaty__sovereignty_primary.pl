% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Conditional Free Movement under Member-State Consent Primacy
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   Within the federation's membership treaty, each member state retains
 *   authority to condition other members' nationals' access to its labor
 *   market and welfare system: work-authorization regimes, registration
 *   requirements, eligibility waiting periods, and safeguard clauses invoked
 *   during shocks. This story instantiates the sovereignty_primary reading of
 *   the membership-treaty kernel — movement access is a consented permission,
 *   not a held right — and authors the structure as that reading sees it: a
 *   managed-mobility compact that genuinely coordinates a single labor area
 *   while placing its adjustment costs on the movers. The claim and metrics
 *   are independent authored facts: the reading holds the arrangement
 *   substantially legitimate, and the authored metrics still record real
 *   cost-bearing by mobile workers and real enforcement machinery. KEY AGENTS
 *   (by structural relationship): - member_state_governments: Agenda setter
 *   (institutional/constrained) — writes and enforces the consent conditions;
 *   collects fiscal headroom and electoral credit -
 *   domestic_incumbent_workforce: Primary beneficiary (organized/constrained)
 *   — shielded from direct wage competition -
 *   national_welfare_administrations: Co-administrator and secondary
 *   beneficiary (institutional/constrained) — runs the eligibility gates -
 *   mobile_workers: Primary target (powerless/mobile) — access conditioned,
 *   bears uncertainty and second-class terms - cross_border_employers:
 *   Secondary target with offsetting benefit (powerful/arbitrage) — thinner
 *   labor pools against wider market access - sending_state_governments:
 *   Excluded party (institutional/constrained) — objects from outside the
 *   room where conditions are written - federation_judicial_review_body:
 *   Analytical observer (institutional/analytical) — adjudicates disputes
 *   with uneven practical bite Family note: this story belongs to the
 *   federation_membership_treaty constraint family; the sibling stories
 *   instantiate other readings of the same treaty text and author different
 *   epsilon values over the same referent arrangement (see kernel_context and
 *   the epsilon_reading_index_asymmetry omega).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.52).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.55).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Conditional Free Movement under Member-State Consent Primacy").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '71d17a08-aa57-4d0d-a7c1-488098bd5e84').
narrative_ontology:cs_kernel_codification('71d17a08-aa57-4d0d-a7c1-488098bd5e84', fixed_text).
narrative_ontology:cs_authority_grounding('71d17a08-aa57-4d0d-a7c1-488098bd5e84', distributed).
narrative_ontology:cs_reading_relation('71d17a08-aa57-4d0d-a7c1-488098bd5e84', federation_membership_treaty__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('71d17a08-aa57-4d0d-a7c1-488098bd5e84', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('71d17a08-aa57-4d0d-a7c1-488098bd5e84', foundational, movement_access_is_conditional_on_member_state_consent).
narrative_ontology:cs_axiom_status(movement_access_is_conditional_on_member_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('71d17a08-aa57-4d0d-a7c1-488098bd5e84', movement_access_is_conditional_on_member_state_consent, deontological).
narrative_ontology:cs_axiom('71d17a08-aa57-4d0d-a7c1-488098bd5e84', foundational, national_interests_justify_mobility_conditions).
narrative_ontology:cs_axiom_status(national_interests_justify_mobility_conditions, holdable).
narrative_ontology:cs_axiom_grounding('71d17a08-aa57-4d0d-a7c1-488098bd5e84', national_interests_justify_mobility_conditions, instrumental).
narrative_ontology:cs_reference_frame('71d17a08-aa57-4d0d-a7c1-488098bd5e84', interstate_consent_compact).
narrative_ontology:cs_drift_state('71d17a08-aa57-4d0d-a7c1-488098bd5e84', contemporary_post_enlargement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('71d17a08-aa57-4d0d-a7c1-488098bd5e84', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_incumbent_workforce).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_welfare_administrations).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, cross_border_employers).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, national_labor_market_protection_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, welfare_sustainability_precaution).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, consent_based_legitimacy_of_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National executives and legislatures write the consent conditions: work-authorization schemes, registration thresholds, welfare-eligibility waiting periods, and emergency safeguard clauses. They invoke conditions selectively during accession waves and downturns, collect the fiscal headroom of deferred welfare obligations and the electoral credit of visible protection, and bear the macroeconomic drag of tighter labor supply. Leaving the treaty framework would cost far more than adjusting its terms; tightening conditions is electorally rewarded, loosening them is punished.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, member_state_governments, beneficiary).

% Ministries and agencies administer eligibility tests, residence-duration rules, and contribution verification for arriving workers. Each tightening shifts prospective claimants off their books and back onto origin-state systems; each loosening adds caseload. They run the day-to-day gatekeeping and report caseload and fraud statistics upward, shaping the political case for further conditions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_welfare_administrations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, national_welfare_administrations, beneficiary).

% Workers already established in each national labor market face measurably less direct wage and hours competition while conditions hold, and vote on the governments that write them. Their protected position depends on the conditions remaining in place. They could themselves relocate abroad, but housing, family, and pension anchoring make that a theoretical rather than practiced option for most.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_incumbent_workforce, beneficiary,
    organized, biographical, constrained, national).

% Citizens of one member state seeking work or residence in another encounter authorization queues, duration limits, and welfare waiting periods that shape where moving is worth attempting at all. They can redirect to other member states with lighter conditions, but each redirection costs language acquisition, credential recognition, and rebuilt networks; repeated redirection compounds the cost. Those deterred from applying altogether never appear in any statistic.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, biographical, mobile, continental).

% Firms that recruit across borders face thinner applicant pools and longer vacancy times where conditions bind, and pay recruiting premia to compensate movers for the friction. The same treaty framework preserves their access to the wider market and their freedom to site operations where conditions suit them; large employers arbitrage jurisdictional differences, small ones absorb the friction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_employers, payer,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__sovereignty_primary, cross_border_employers, beneficiary).

% Governments of states whose nationals emigrate formally co-sign the treaty framework but hold no seat where admission conditions are written. Their concerns — remittance dependence on their citizens' success abroad, reputational stakes in how their nationals are treated — surface only in diplomatic notes and summit margins.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, sending_state_governments, excluded,
    institutional, generational, constrained, continental).

% The federation's court hears challenges brought by individual movers against national conditions and weighs them against treaty commitments. Its rulings bind unevenly in practice: member states comply, distinguish, or legislate around judgments, and the court's docket shapes which conditions face scrutiny at all.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_judicial_review_body, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables participation in a federated single market — mutual recognition, cross-border employment, portable contributions — while preserving each member state's capacity to absorb labor-market shocks and protect welfare solvency; converts an unmanaged mobility commons into consent-conditioned access that states can open, throttle, or close by decision.
% TRANSFER_FUNCTION: Moves labor-market access and welfare entitlement from a state-controlled permission register to individual movers (granted, withheld, delayed, or revoked); moves wage-competition risk off incumbent workforces and onto movers, who bear exclusion, waiting periods, and second-class terms; moves fiscal exposure off national budgets onto movers and their origin households during qualifying periods.
% ABSENT_VOICES: Sending-state governments hold a formal seat but no effective one where conditions are written; would-be movers deterred from ever applying are absent from every record and statistic; migrant households bearing waiting-period costs speak only through advocacy organizations with consultative standing.
% DISAPPEARANCE_RATIONALE: If consent conditions vanished overnight and movement became unconditional, labor allocation across the federation would reorganize within years: gateway-region wage floors would shift, welfare eligibility rules would be rewritten under fiscal pressure, employers would rebuild recruitment around open pools, and member-state electoral politics would reorganize around the new distributional outcomes rather than around defending the permission system itself.
% FOUNDING_PROBLEM: Reconcile creation of a common market and mobility area with preservation of each member's capacity to manage its own labor market and finance its welfare system — the tension between market integration and the national social contract.
% FOUNDING_PROBLEM_CORROBORATION: Mobile-worker associations and sending-state governments — both outside the benefiting set — attest the integration-versus-social-contract tension remains live at every enlargement and downturn; comparative federalism scholarship across multiple unions documents the same recurrence; no external source attests the problem resolved.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52 reflects the reading's own lights: mobile workers demonstrably bear conditioned access, waiting periods, and second-class terms (real cost-bearing), but this reading judges the majority of that bearing a legitimate price of national self-government and welfare solvency rather than appropriated surplus — hence moderate, not high. Suppression 0.55 records the enforcement reality (authorization schemes, eligibility testing, removal powers) operating within legal bounds and appeal structures; per the deterred_applicant_internalization omega, a minority share of total suppression is internalized (deterred applications) rather than structural. Accessibility collapse is low (0.35): alternatives persist everywhere — movers redirect to lighter-condition states, employers substitute or relocate, states adjust condition design — so understanding the constraint does not foreclose options. Resistance 0.55 is real and recurring: sending-state objections, mover litigation, employer pressure during shortages. Theater ratio 0.30: most administrative activity screens real applicants, but a visible minority of enforcement activity is announcement-driven symbolic tightening aimed at domestic audiences rather than at binding flows.
 *   
 *   Temporal series: all tracked metrics share one nine-point grid (1993–2026), every metric authored at every point. The trajectories oscillate rather than drift monotonically: extraction, suppression, and theater rise together through the enlargement waves and post-crisis tightening (2004–2016 peak, coinciding with transitional work-permit regimes and welfare-condition hardening), then ease through the post-2019 labor-shortage phase as states begin competing for mobile workers. The cycle is driven primarily by external factors — the accession calendar, business cycle, and demographic swings — though each restriction phase functions as a recurring extraction window during its tenure. Base properties are sampled at interval end (2026), during the easing phase; the end-point values match the scalar metrics by construction. The 2026 points are marked projected (generated mid-year).
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently from the same structural data. From the member_state_governments seat, the arrangement is a self-government instrument it built, uses deliberately, and answers for electorally — costs appear as modest macroeconomic drag, benefits as fiscal headroom and political credit. From the mobile_workers seat, the same structure is a permission regime that prices their careers in waiting periods and redirects. The incumbent-workforce seat experiences the constraint as background protection it did not choose and need not defend. The welfare-administration seat straddles: it enforces the gates and books the savings, making it structurally invested in continuation regardless of aggregate effect. Identity fusion is present on two seats: the incumbent workforce fuses protection with belonging ('our' labor market), an ideological lock that survives evidence of net cost; member states fuse the arrangement with institutional identity as guarantors of the national social contract. Were the identity frame to break — e.g., sustained shortage politics reframing movers as necessary rather than threatening — the incumbent seat's defense of conditions would weaken faster than the administrative seat's.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: member_state_governments, domestic_incumbent_workforce, and national_welfare_administrations derive low directionality (subsidized or near-beneficiary positions); mobile_workers derive high directionality, damped from the maximum by their genuine inter-state mobility — their exit is real but costly (language, credentials, networks), placing them well short of the trapped pole and short of arbitrage-grade exit. One override is declared: cross_border_employers. Victim declaration alone would push them near full-target, but their situation is genuinely dual — they bear recruitment friction while simultaneously drawing the wide-market benefits the same treaty maintains, and their capital mobility lets them arbitrage conditions rather than absorb them. The override to 0.65 encodes that net position. Receipt surface: gains were checked seat by seat — states collect diffuse fiscal savings, incumbents diffuse wage protection, administrators diffuse budget relief; no named seat concentrates the arrangement's gains into a captured stream, hence gain_flow is authored 'diffuse' as an affirmative finding, not a default. Fixing cost is prohibitive: for the states that could unilaterally drop their conditions, the electoral price of surrendering protective authority vastly exceeds the diffuse macroeconomic benefit of doing so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated, so no mandatrophy is resolved here and the arrangement is not running on inherited momentum. The hybrid claim earns its keep against two symmetrical mislabelings. Read as pure coordination (the arrangement's own apologetics), the asymmetric cost-bearing disappears and the mover seat's experience vanishes from the ledger. Read as pure extraction, the real coordination function disappears — managed mobility is what keeps the single labor area politically sustainable at all, and states demonstrably use their retained tools for genuine shock absorption, not only for rent-taking. Declaring both the coordination function and the victim set forces the engine to compute the arrangement as containing both, which is what forty years of accession-and-adjustment history shows. The measurement series guards against the adjacent failure mode: had extraction risen monotonically with theater climbing past 0.5, the coordination story would be decaying into cover; the observed easing after 2019 indicates the protective function still responds to real conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (sovereignty_primary) of the federation_membership_treaty kernel: would adopting a sibling reading (integration_primary or subsidiarity_balance) change the structural classification?',
    'Treaty revision, accession-basis change, or a doctrinal shift in the federation court''s movement-rights line; corpus comparison against the compiled sibling stories'' computed classifications.',
    'Under integration_primary, mobile_workers leave the victim set, the conditions become the condemned element, and the arrangement computes far more hostile from the mover seat; under subsidiarity_balance the victim set narrows to movers facing disproportionate conditions. Classification is reading-indexed until the contest resolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the membership-treaty kernel governs determines the beneficiary/victim structure and the resulting type.').

omega_variable(
    epsilon_reading_index_asymmetry,
    'Epsilon is a property of a reading, not a topic: the same standing arrangement assessed under sibling lights yields a materially different epsilon than this reading''s 0.52 — how large is the cross-reading gap over the identical referent?',
    'Compile and compare the sibling stories'' authored epsilon values over the same arrangement; the divergence quantifies the reading index.',
    'A large gap signals the corpus is measuring readings, not arrangements: cross-story comparison must join on kernel_id before comparing types or epsilon values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_index_asymmetry, conceptual, 'Cross-reading epsilon divergence over a shared referent arrangement.').

omega_variable(
    protection_efficacy_empirics,
    'Do consent conditions actually deliver the labor-market stabilization and welfare protection that justify them under this reading''s own warrant?',
    'Quasi-experimental evidence from liberalization and restriction episodes (accession transition expiries, bilateral mobility agreements) comparing wage, employment, and welfare-solvency outcomes in affected regions.',
    'If conditions fail to stabilize, the coordination justification thins and the extraction share of the measured profile grows — drift toward a purely extractive reading; if effective, the hybrid coordination-plus-cost-bearing structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_efficacy_empirics, empirical, 'Empirical efficacy of mobility conditions as protective instruments.').

omega_variable(
    mobile_exit_depth,
    'How deep is mobile workers'' exit option in practice — is relocation between member states genuine working exit, or pseudo-exit burdened by language barriers, credential non-recognition, and severed support networks?',
    'Migration-flow elasticity studies: do movers facing tightened conditions in one member state actually redirect to lighter-condition states, or drop out of movement entirely?',
    'Shallow exit pushes mobile_workers toward the trapped end of the directionality range, amplifying their effective cost-bearing and drifting the computed classification toward heavier enforcement dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobile_exit_depth, empirical, 'Real depth of inter-state relocation as exit for conditioned movers.').

omega_variable(
    deterred_applicant_internalization,
    'How much of the measured suppression is internalized — potential movers who never apply because they expect refusal or believe they are unwelcome — versus structural (permits, eligibility gates, removal powers)?',
    'Application-rate gaps between eligible cohorts before and after condition changes, controlling for underlying labor demand; survey evidence on perceived welcome among eligible non-applicants.',
    'Internalized suppression persists after formal liberalization, understating residual constraint force and biasing post-liberalization classifications toward the benign-coordination pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterred_applicant_internalization, empirical, 'Split of suppression into structural legal gates versus deterred-application internalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 1993, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_primary_tr_t1993, federation_membership_treaty__sovereignty_primary, theater_ratio, 1993, 0.18).
narrative_ontology:measurement_basis(sov_primary_tr_t1993, observed).
narrative_ontology:measurement(sov_primary_tr_t2000, federation_membership_treaty__sovereignty_primary, theater_ratio, 2000, 0.2).
narrative_ontology:measurement_basis(sov_primary_tr_t2000, observed).
narrative_ontology:measurement(sov_primary_tr_t2004, federation_membership_treaty__sovereignty_primary, theater_ratio, 2004, 0.3).
narrative_ontology:measurement_basis(sov_primary_tr_t2004, observed).
narrative_ontology:measurement(sov_primary_tr_t2007, federation_membership_treaty__sovereignty_primary, theater_ratio, 2007, 0.29).
narrative_ontology:measurement_basis(sov_primary_tr_t2007, observed).
narrative_ontology:measurement(sov_primary_tr_t2010, federation_membership_treaty__sovereignty_primary, theater_ratio, 2010, 0.28).
narrative_ontology:measurement_basis(sov_primary_tr_t2010, observed).
narrative_ontology:measurement(sov_primary_tr_t2016, federation_membership_treaty__sovereignty_primary, theater_ratio, 2016, 0.38).
narrative_ontology:measurement_basis(sov_primary_tr_t2016, observed).
narrative_ontology:measurement(sov_primary_tr_t2019, federation_membership_treaty__sovereignty_primary, theater_ratio, 2019, 0.36).
narrative_ontology:measurement_basis(sov_primary_tr_t2019, observed).
narrative_ontology:measurement(sov_primary_tr_t2022, federation_membership_treaty__sovereignty_primary, theater_ratio, 2022, 0.33).
narrative_ontology:measurement_basis(sov_primary_tr_t2022, observed).
narrative_ontology:measurement(sov_primary_tr_t2026, federation_membership_treaty__sovereignty_primary, theater_ratio, 2026, 0.3).
narrative_ontology:measurement_basis(sov_primary_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(sov_primary_be_t1993, federation_membership_treaty__sovereignty_primary, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement_basis(sov_primary_be_t1993, observed).
narrative_ontology:measurement(sov_primary_be_t2000, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement_basis(sov_primary_be_t2000, observed).
narrative_ontology:measurement(sov_primary_be_t2004, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2004, 0.53).
narrative_ontology:measurement_basis(sov_primary_be_t2004, observed).
narrative_ontology:measurement(sov_primary_be_t2007, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2007, 0.56).
narrative_ontology:measurement_basis(sov_primary_be_t2007, observed).
narrative_ontology:measurement(sov_primary_be_t2010, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement_basis(sov_primary_be_t2010, observed).
narrative_ontology:measurement(sov_primary_be_t2016, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement_basis(sov_primary_be_t2016, observed).
narrative_ontology:measurement(sov_primary_be_t2019, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2019, 0.59).
narrative_ontology:measurement_basis(sov_primary_be_t2019, observed).
narrative_ontology:measurement(sov_primary_be_t2022, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2022, 0.56).
narrative_ontology:measurement_basis(sov_primary_be_t2022, observed).
narrative_ontology:measurement(sov_primary_be_t2026, federation_membership_treaty__sovereignty_primary, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(sov_primary_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(sov_primary_su_t1993, federation_membership_treaty__sovereignty_primary, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement_basis(sov_primary_su_t1993, observed).
narrative_ontology:measurement(sov_primary_su_t2000, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2000, 0.36).
narrative_ontology:measurement_basis(sov_primary_su_t2000, observed).
narrative_ontology:measurement(sov_primary_su_t2004, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2004, 0.52).
narrative_ontology:measurement_basis(sov_primary_su_t2004, observed).
narrative_ontology:measurement(sov_primary_su_t2007, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2007, 0.53).
narrative_ontology:measurement_basis(sov_primary_su_t2007, observed).
narrative_ontology:measurement(sov_primary_su_t2010, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement_basis(sov_primary_su_t2010, observed).
narrative_ontology:measurement(sov_primary_su_t2016, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement_basis(sov_primary_su_t2016, observed).
narrative_ontology:measurement(sov_primary_su_t2019, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2019, 0.61).
narrative_ontology:measurement_basis(sov_primary_su_t2019, observed).
narrative_ontology:measurement(sov_primary_su_t2022, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement_basis(sov_primary_su_t2022, observed).
narrative_ontology:measurement(sov_primary_su_t2026, federation_membership_treaty__sovereignty_primary, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(sov_primary_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'free movement in the federation.' Per the epsilon-invariance principle, the label conflates three structurally distinct readings of one treaty kernel, each with its own stable epsilon, beneficiary/victim structure, and type: this story (sovereignty_primary — consent-conditional access; moderate epsilon from its own lights; tangled-rope structure), integration_primary (movement as constitutive right; restrictions read as the extractive element; substantially higher epsilon over the same referent), and subsidiarity_balance (proportionality-bounded mobility; intermediate victim set narrowed to disproportionate conditions). The stories are linked pairwise via affects_constraints; cross-story comparison must join on kernel_id because epsilon is reading-indexed. Upstream/downstream: integration_primary's jurisprudential claims are frequently cited as evidence against this reading's consent premise, and this reading's safeguard invocations create the factual record the subsidiarity_balance proportionality analysis adjudicates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
