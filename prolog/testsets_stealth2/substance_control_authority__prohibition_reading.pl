% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Criminal Prohibition of Drug Use and Possession (Third-Party Protection Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition reading of state
 *   substance-control authority: the arrangement in which use and possession
 *   of scheduled drugs are crimes, defended as protection of third parties
 *   from drug-related crime and social disorder. The arrangement has a
 *   genuine protective coordination function — residents of disorder-affected
 *   areas receive measurable relief where visible markets are suppressed —
 *   carried by the same machinery that imposes arrest, incarceration,
 *   records, and concentrated racial disparity on users and their
 *   communities, and that funds a large enforcement and corrections
 *   apparatus. KEY AGENTS (by structural relationship): -
 *   state_legislatures_and_executives: agenda setter
 *   (institutional/arbitrage) — writes schedules, appropriates budgets,
 *   appoints prosecutors - law_enforcement_agencies: primary collecting
 *   beneficiary (institutional/constrained) — receives appropriations and
 *   forfeitures, runs enforcement - private_prison_operators: secondary
 *   beneficiary (organized/arbitrage) — houses the sentenced -
 *   order_seeking_residents: intended protective beneficiary
 *   (moderate/constrained) - people_who_use_drugs: primary target
 *   (powerless/trapped) — bears criminalization directly -
 *   racially_disparate_enforced_communities: concentrated target
 *   (moderate/constrained) - families_of_incarcerated_users: collateral
 *   target (powerless/trapped) - public_health_researchers: analytical
 *   observer. The claim/metric gap is deliberate: the reading CLAIMS
 *   tangled_rope (genuine protection plus asymmetric cost-bearing) while the
 *   authored metrics describe heavily coercive operation with meaningful
 *   performative content — the engine measures the divergence per seat;
 *   nothing is reconciled here.
 *
 * KEY AGENTS:
 *   - state_legislatures_and_executives: agenda setter (institutional/arbitrage) — sets schedules, funding, and prosecutorial appointments
 *   - law_enforcement_agencies: primary collecting beneficiary (institutional/constrained) — budgets and forfeitures flow here; runs the enforcement machinery
 *   - private_prison_operators: secondary beneficiary (organized/arbitrage) — incarceration as primary mechanism fills beds
 *   - order_seeking_residents: intended protective beneficiary (moderate/constrained) — receives reduced visible disorder where deterrence holds
 *   - people_who_use_drugs: primary target (powerless/trapped) — enters the cost-bearing set through criminalization itself
 *   - racially_disparate_enforced_communities: concentrated target (moderate/constrained) — bear enforcement intensity far exceeding their share of use
 *   - families_of_incarcerated_users: collateral target (powerless/trapped)
 *   - public_health_researchers: analytical observer — compares regimes across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.5).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.8).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Criminal Prohibition of Drug Use and Possession (Third-Party Protection Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'e9bc2d47-3330-462d-95de-d9ffae4e1cf4').
narrative_ontology:cs_kernel_codification('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', formalized).
narrative_ontology:cs_authority_grounding('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', extraction).
narrative_ontology:cs_interpretation_layer_present('e9bc2d47-3330-462d-95de-d9ffae4e1cf4').
narrative_ontology:cs_reading_relation('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', foundational, criminalization_as_third_party_protection).
narrative_ontology:cs_axiom_status(criminalization_as_third_party_protection, holdable).
narrative_ontology:cs_axiom_grounding('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', criminalization_as_third_party_protection, instrumental).
narrative_ontology:cs_axiom('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', secondary, incarceration_legitimate_market_suppression).
narrative_ontology:cs_axiom_status(incarceration_legitimate_market_suppression, holdable).
narrative_ontology:cs_axiom_grounding('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', incarceration_legitimate_market_suppression, instrumental).
narrative_ontology:cs_reference_frame('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', full_criminal_prohibition_framework).
narrative_ontology:cs_drift_state('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', contemporary_post_legalization_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9bc2d47-3330-462d-95de-d9ffae4e1cf4', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, order_seeking_residents).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racially_disparate_enforced_communities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, families_of_incarcerated_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and amend controlled-substance statutes, set scheduling categories, appropriate enforcement and corrections budgets, and appoint prosecutors and agency heads. Respond to constituent demands for public order and to advocacy campaigns running in both directions. Can rewrite the arrangement by ordinary legislation, though international drug-control treaties raise the cost of full unilateral repeal.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_legislatures_and_executives, agenda_setter,
    institutional, biographical, arbitrage, national).

% Arrest, investigate, and refer for prosecution; operate specialized narcotics units; receive dedicated enforcement appropriations and, in many jurisdictions, forfeited assets that supplement budgets. Unit staffing, federal grant eligibility, and the existence of whole divisions depend on continued enforcement volume. Leaving the arrangement would require a legislative redefinition of their mandate.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter).

% Contract with governments to house sentenced and detained people; some contracts carry occupancy guarantees tying revenue to bed counts. If drug-sentence volume shrinks, they can bid for other detention streams such as immigration detention or federal marshal holdings.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, private_prison_operators, beneficiary,
    organized, biographical, arbitrage, national).

% Live in neighborhoods with visible street markets and associated disorder; report incidents, vote for order-focused candidates, and experience fewer open-air transactions and less public nuisance where enforcement suppresses visible dealing. Relocation is possible but costly, and attachment to home neighborhoods makes moving unattractive.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, order_seeking_residents, beneficiary,
    moderate, biographical, constrained, local).

% Buy and consume controlled substances; face arrest, prosecution, incarceration, fines, and lasting records that restrict housing, employment, licensing, and child custody. Physical dependence makes the conduct itself difficult to stop, and the accumulated record makes relocation and fresh starts harder. They are regulated extensively but rarely seated in the legislative process that defines their exposure.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, people_who_use_drugs, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, people_who_use_drugs, excluded).

% Predominantly Black and Latino urban communities whose arrest, conviction, and incarceration rates for drug offenses far exceed their share of reported use. Bear concentrated removal of working-age adults, elevated policing intensity, and widespread records among residents. Civil-rights organizations give these communities organized voice, but residential patterns keep day-to-day exposure high.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racially_disparate_enforced_communities, payer,
    moderate, biographical, constrained, national).

% Lose household income, caregiving capacity, and parental presence to sentences; carry court debts, commissary and phone costs, and long-distance visitation burdens. Kinship ties cannot be exited; the burden ends only when the sentence does.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, families_of_incarcerated_users, payer,
    powerless, biographical, trapped, national).

% Study overdose, infection, and crime outcomes across policy regimes; publish cross-jurisdiction comparisons (Portugal, Switzerland, US state-level experiments) that feed legislative debate. Hold no direct stake in enforcement budgets and can compare regimes freely.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses a real collective-action problem: individual third parties cannot privately protect themselves from spillover effects of open drug markets — street-level crime, public disorder, nuisance, and drug-associated predation. Criminalization coordinates a shared deterrent response through the state's monopoly on force, aiming to suppress visible market activity and incapacitate repeat participants.
% TRANSFER_FUNCTION: Moves liberty, years of life (incarceration), money (fines, fees, forfeited assets, court debt), and future prospects (records) from people who use drugs — concentrated further onto Black and Latino communities by enforcement patterns — into the enforcement and corrections apparatus; moves perceived safety and reduced visible disorder to residents of affected areas.
% ABSENT_VOICES: People who use drugs and people currently serving drug sentences are largely absent from legislative hearings; their objection — that the arrangement punishes them rather than protecting them — reaches policy mainly through advocacy intermediaries and formerly incarcerated spokespeople. Residents who prefer treatment-first responses in their own neighborhoods are also underrepresented relative to order-first constituencies.
% DISAPPEARANCE_RATIONALE: Overnight repeal would empty a large caseload from courts, probation, and prisons, eliminate dedicated narcotics units and their funding streams, reprice and restructure illicit markets, and force police reallocation. Third-party order expectations would not simply evaporate — jurisdictions would scramble to build successor instruments (civil citations, health referrals, regulated markets), and the shape of that scramble would differ by jurisdiction. The world rearranges around whatever successor each polity adopts.
% FOUNDING_PROBLEM: Early twentieth-century narcotic panics and, decisively, the late-century crack-era surge fused drug use with street crime and racialized disorder in public perception. Legislatures built criminalization to suppress visible markets and shield third parties from drug-associated crime and neighborhood decline.
% FOUNDING_PROBLEM_CORROBORATION: Criminological studies of drug-market violence, resident surveys in affected neighborhoods, and public-health data on drug-related homicide and nuisance all attest that the underlying problem — third-party harm from drug markets — persists. The same outside sources dispute whether criminalization is an effective remedy for it; corroboration of the problem comes from outside the enforcement beneficiary set.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.5 from this reading's own seat over the standing criminalization arrangement: the reading holds the instrument substantially justified, but concedes as real the mass-incarceration burden, the forfeiture incentive structure, and the disparity problem — costs even sympathetic defenders acknowledge rather than deny. Suppression is high (0.8) because coercion is the designed mechanism, not a side effect: arrest and imprisonment are how the arrangement operates; suppression is authored as a raw structural property and is not scaled by power or scope. Theater_ratio sits at 0.4: seizure press conferences, symbolic sentencing announcements, and quota-driven possession arrests coexist with genuinely functional patrol, interdiction, and prosecution. Accessibility_collapse is low (0.3) because live counterexample regimes (Portugal, Switzerland, US state legalization) keep alternative arrangements visible and operable — alternatives do not collapse on inspection. Resistance is substantial (0.6): a fifty-year reform movement, ballot measures, litigation, and defunding contests meet the arrangement continuously. The temporal series run on one shared grid (1971, 1980, 1990, 2000, 2010, 2023) with every tracked metric authored at every point: extractiveness escalates through the mandatory-minimum and mass-incarceration buildup, peaks circa 2000, and partially relaxes in the reform era; suppression_requirement traces the enforcement-infrastructure ratchet (militarized units, forfeiture expansion, sentencing severity) cresting around 2000 and easing modestly as legalization waves and prosecutorial discretion spread; theater_ratio climbs as interdiction visibly fails while seizure spectacle continues. The trajectory is monotone-rising-then-partially-relaxing, not cyclical.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administering seats should compute differently. From the legislature and enforcement seats the arrangement is a protective service they fund and operate, with costs that are the point of the design; from the user and family seats it is a machine that converts their conduct and kinship into years and records; from the community seat it is a differential-applied burden; from the resident seat it is conditional relief contingent on where they live. The engine computes these per-seat classifications from the structural data — the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for order_seeking_residents (genuine protective benefit, constrained exit), law_enforcement_agencies (collects appropriations and forfeitures), and private_prison_operators (bed-count revenue, arbitrage-grade exit into other detention streams). Victim declarations drive high directionality for people_who_use_drugs (trapped by dependence plus record — sits near the full-target end) and families_of_incarcerated_users (trapped by kinship). racially_disparate_enforced_commities carry high directionality through concentrated victimhood with only constrained exit. Public_health_researchers sit analytically neutral. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already differentiate every seat correctly, and adding overrides would duplicate what the derivation produces.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — third-party harm from drug markets — is still live, corroborated by criminological and public-health sources outside the beneficiary set, so mandatrophy is NOT resolved and none is declared. The tangled_rope classification is what prevents mislabeling in both directions: a pure-extraction reading would erase the genuine protective coordination that order-seeking residents verifiably receive where enforcement suppresses visible markets; a pure-coordination reading would erase the carceral cost asymmetry borne by users, families, and disproportionately policed communities. The drift path worth watching is the enforcement_apparatus_self_perpetuation omega: if budget dependence on arrest and seizure volumes hardens while the protective function stagnates, the arrangement slides toward snare-like persistence with the coordination story as cover; if reform hollows enforcement while statutes remain on the books, it degrades toward piton-like theatrical maintenance. The coalition check matters here: users, families, and affected communities have repeatedly formed reform coalitions (ballot initiatives, expungement campaigns) — their combined organized power is the main reason the post-2010 relaxation appears in the measurement series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (prohibition_reading) of the substance_control_authority kernel; would instantiating a sibling reading (harm_reduction_reading or legalization_reading) change the structural classification, and where exactly is the disagreement located?',
    'Author the sibling stories separately and compare computed per-seat classifications: the delta should localize to the victim set (users in or out) and to whether an enforcement apparatus retains a collection base. Cross-file comparison of the three readings resolves the location question.',
    'If sibling readings compute materially different types over the same population, the kernel contest is substantive rather than rhetorical; if they converge, the contest is about means within one stable structure. Either result recalibrates how much weight the prohibition reading''s claim carries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: this story is one reading of a contested kernel; siblings would move users out of the victim set and dissolve the enforcement collection base.').

omega_variable(
    deterrence_efficacy_uncertainty,
    'Does criminalization actually reduce third-party harm from drug markets, or does prohibition itself generate a substantial share of that harm through black-market turf conflict and unregulated supply?',
    'Natural experiments: measure third-party-harm trajectories (drug-market violence, overdose externality, neighborhood disorder) across jurisdictions before and after decriminalization (Portugal 2001), supervised-supply adoption (Switzerland), and state-level legalization, controlling for secular trends.',
    'If prohibition generates more third-party harm than it prevents, the protective coordination function collapses into cover and the arrangement drifts toward snare; if deterrence net-protects, the coordination leg of the tangled_rope holds firm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_uncertainty, empirical, 'Whether the arrangement''s protective function is real or whether it manufactures the harm it claims to prevent.').

omega_variable(
    disparity_attribution_ambiguity,
    'Are racial disparities in drug-law application intrinsic to the arrangement''s design (patrol allocation, sentencing structure, venue selection) or artifacts of socioeconomic confounders correlated with race?',
    'Within-jurisdiction comparison of enforcement rates against usage-rate surveys, matched on offense conduct and location; discontinuity analyses around policy changes that altered patrol allocation without altering statutes.',
    'If disparity is intrinsic, the extraction asymmetry is a designed feature and target-side directionality hardens; if confounded, part of the measured asymmetry belongs to adjacent housing and labor constraints rather than this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disparity_attribution_ambiguity, empirical, 'Whether the arrangement''s unequal application is designed-in or inherited from adjacent structures.').

omega_variable(
    enforcement_apparatus_self_perpetuation,
    'Has the enforcement apparatus become self-perpetuating — budgets, grant eligibility, and forfeiture revenues depending on arrest and seizure volumes — such that parts of the machinery now persist for their own sake?',
    'Budget-response analysis: do agency budget trajectories track drug-arrest volumes independent of crime rates? Compare jurisdictions where possession enforcement was deprioritized: did narcotics-unit funding shrink proportionally, or was it redirected to preserve headcount?',
    'Strong self-perpetuation shifts the arrangement''s persistence basis from participant benefit toward institutional inertia, pushing computed classifications toward snare (if extraction concentrates) or piton (if the function atrophies while performance continues).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_self_perpetuation, empirical, 'Whether the enforcement machinery maintains the arrangement for its own budgetary sake.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 1971, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1971, substance_control_authority__prohibition_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement_basis(subs_tr_t1971, observed).
narrative_ontology:measurement(subs_tr_t1980, substance_control_authority__prohibition_reading, theater_ratio, 1980, 0.27).
narrative_ontology:measurement_basis(subs_tr_t1980, observed).
narrative_ontology:measurement(subs_tr_t1990, substance_control_authority__prohibition_reading, theater_ratio, 1990, 0.34).
narrative_ontology:measurement_basis(subs_tr_t1990, observed).
narrative_ontology:measurement(subs_tr_t2000, substance_control_authority__prohibition_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(subs_tr_t2000, observed).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__prohibition_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement_basis(subs_tr_t2010, observed).
narrative_ontology:measurement(subs_tr_t2023, substance_control_authority__prohibition_reading, theater_ratio, 2023, 0.4).
narrative_ontology:measurement_basis(subs_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t1971, substance_control_authority__prohibition_reading, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement_basis(subs_be_t1971, observed).
narrative_ontology:measurement(subs_be_t1980, substance_control_authority__prohibition_reading, base_extractiveness, 1980, 0.44).
narrative_ontology:measurement_basis(subs_be_t1980, observed).
narrative_ontology:measurement(subs_be_t1990, substance_control_authority__prohibition_reading, base_extractiveness, 1990, 0.56).
narrative_ontology:measurement_basis(subs_be_t1990, observed).
narrative_ontology:measurement(subs_be_t2000, substance_control_authority__prohibition_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement_basis(subs_be_t2000, observed).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__prohibition_reading, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement_basis(subs_be_t2010, observed).
narrative_ontology:measurement(subs_be_t2023, substance_control_authority__prohibition_reading, base_extractiveness, 2023, 0.5).
narrative_ontology:measurement_basis(subs_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1971, substance_control_authority__prohibition_reading, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement_basis(subs_su_t1971, observed).
narrative_ontology:measurement(subs_su_t1980, substance_control_authority__prohibition_reading, suppression_requirement, 1980, 0.66).
narrative_ontology:measurement_basis(subs_su_t1980, observed).
narrative_ontology:measurement(subs_su_t1990, substance_control_authority__prohibition_reading, suppression_requirement, 1990, 0.77).
narrative_ontology:measurement_basis(subs_su_t1990, observed).
narrative_ontology:measurement(subs_su_t2000, substance_control_authority__prohibition_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement_basis(subs_su_t2000, observed).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__prohibition_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement_basis(subs_su_t2010, observed).
narrative_ontology:measurement(subs_su_t2023, substance_control_authority__prohibition_reading, suppression_requirement, 2023, 0.8).
narrative_ontology:measurement_basis(subs_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'drug policy' covers three structurally distinct arrangements of the same kernel (substance_control_authority), decomposed per the epsilon-invariance principle. This story (prohibition_reading) is the historically prior member: its documented costs — mass incarceration, disparity, forfeiture incentives — shape the legitimacy conditions under which the sibling readings argue, so the influence edge runs from this reading toward legalization_reading, while harm_reduction_reading coexists with it in blended regimes (criminalized possession alongside funded needle exchange). Each family member carries its own epsilon, beneficiary/victim sets, and claimed type; users sit inside the victim set here and largely outside it in the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
