% ============================================================================
% CONSTRAINT STORY: fifteenth_amendment__effective_franchise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifteenth_amendment_effective_franchise, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fifteenth_amendment__effective_franchise_reading
 *   human_readable: Fifteenth Amendment (Effective Franchise Reading)
 *   domain: constitutional_law/voting_rights
 *
 * SUMMARY:
 *   This constraint represents ONE reading of the Fifteenth Amendment kernel:
 *   the effective-franchise reading, which holds that the Amendment empowers
 *   Congress to dismantle any device that operates to deny the vote by race,
 *   including facially neutral mechanisms like literacy tests, poll taxes,
 *   and voter registration manipulation. This reading animated the Voting
 *   Rights Act of 1965 and its preclearance mechanism, which required states
 *   with histories of disfranchisement to obtain federal approval before
 *   changing voting procedures. The constraint exhibits high extractiveness
 *   (0.62) because the effective reading sustains federal override of state
 *   electoral autonomy, and moderate suppression (0.58) reflecting the
 *   ongoing barriers to minority voting despite formal legal protection. The
 *   theater ratio (0.45) reflects that Fifteenth Amendment doctrine
 *   articulates its principle clearly (disfranchisement in operation is
 *   unconstitutional) and the VRA supplied tangible enforcement mechanisms
 *   (preclearance, Section 2 effects tests), reducing performative gap
 *   relative to post-Shelby County doctrine. The effective-franchise reading
 *   coexists with the formal-franchise reading, which holds that only
 *   explicit racial conditions violate the Amendment. These readings compete
 *   in contemporary constitutional interpretation and cannot both prevail
 *   within a single legal framework, yet both remain live positions held by
 *   different judicial coalitions and constitutional scholars.
 *
 * KEY AGENTS:
 *   - Racial Minority Voters: Primary beneficiary (powerless/trapped) — target of federal protection; experience subordination through disfranchisement
 *   - Federal Congress/Executive: Primary institutional beneficiary (institutional/arbitrage) — holds enforcement authority and coordination power under effective reading
 *   - State Election Authorities: Primary victim (powerful/constrained) — subject to federal effects scrutiny and remedial override; retain legitimate electoral administration interests
 *   - Disenfranchisement Gatekeepers (local election officials, state legislatures): Structural beneficiary of formal reading (institutional/arbitrage) — facially neutral devices enable disfranchisement without explicit racial targeting; lose authority under effective reading
 *   - Supreme Court Doctrine: Institutional observer (institutional/constrained) — doctrine oscillates between effective and formal readings; post-Shelby County doctrine shows piton degradation
 *   - Voting Rights Advocacy Coalition: Organized observer (organized/mobile) — supports effective reading; sees enforcement as having temporary sunset logic (scaffold perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifteenth_amendment__effective_franchise_reading, 0.62).
domain_priors:suppression_score(fifteenth_amendment__effective_franchise_reading, 0.58).
domain_priors:theater_ratio(fifteenth_amendment__effective_franchise_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifteenth_amendment__effective_franchise_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(fifteenth_amendment__effective_franchise_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fifteenth_amendment__effective_franchise_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifteenth_amendment__effective_franchise_reading, tangled_rope).
narrative_ontology:human_readable(fifteenth_amendment__effective_franchise_reading, "Fifteenth Amendment (Effective Franchise Reading)").
narrative_ontology:topic_domain(fifteenth_amendment__effective_franchise_reading, "constitutional_law/voting_rights").

domain_priors:requires_active_enforcement(fifteenth_amendment__effective_franchise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifteenth_amendment__effective_franchise_reading, 'ce21d005-0451-4805-b053-0801e3e7918d').
narrative_ontology:cs_kernel_codification('ce21d005-0451-4805-b053-0801e3e7918d', formalized).
narrative_ontology:cs_authority_grounding('ce21d005-0451-4805-b053-0801e3e7918d', lineage).
narrative_ontology:cs_interpretation_layer_present('ce21d005-0451-4805-b053-0801e3e7918d').
narrative_ontology:cs_reading_relation('ce21d005-0451-4805-b053-0801e3e7918d', fifteenth_amendment__formal_franchise_reading, coexists_with).
narrative_ontology:cs_axiom('ce21d005-0451-4805-b053-0801e3e7918d', foundational, disfranchisement_in_operation_is_unconstitutional).
narrative_ontology:cs_axiom_status(disfranchisement_in_operation_is_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('ce21d005-0451-4805-b053-0801e3e7918d', disfranchisement_in_operation_is_unconstitutional, deontological).
narrative_ontology:cs_axiom('ce21d005-0451-4805-b053-0801e3e7918d', secondary, congress_enforcement_power_reaches_effects).
narrative_ontology:cs_axiom_status(congress_enforcement_power_reaches_effects, holdable).
narrative_ontology:cs_axiom_grounding('ce21d005-0451-4805-b053-0801e3e7918d', congress_enforcement_power_reaches_effects, conventional).
narrative_ontology:cs_reference_frame('ce21d005-0451-4805-b053-0801e3e7918d', federal_disfranchisement_prevention_authority).
narrative_ontology:cs_drift_state('ce21d005-0451-4805-b053-0801e3e7918d', contemporary_post_shelby_county, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('ce21d005-0451-4805-b053-0801e3e7918d', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(fifteenth_amendment__effective_franchise_reading, fifteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifteenth_amendment__effective_franchise_reading, racial_minorities_voters).
narrative_ontology:constraint_beneficiary(fifteenth_amendment__effective_franchise_reading, federal_enforcement_authority).
narrative_ontology:constraint_victim(fifteenth_amendment__effective_franchise_reading, disenfranchisement_gatekeepers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (MOUNTAIN) — From the subordinated position, the Fifteenth Amendment's promise of federal protection against disfranchisement appears as an absolute legal entitlement that should admit no exception or workaround. The voter cannot negotiate, cannot exit the jurisdiction to restore franchise, cannot adapt to literacy tests or poll taxes without self-denial. The constraint is immovable from this vantage: either the federal government enforces the Amendment's reach to all disfranchisement devices, or it does not. There is no middle ground.
constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RACIAL MINORITY COMMUNITIES (SNARE) — Across generations, the effective-franchise reading exposes the mechanism of vote dilution and gatekeeping. Without federal enforcement reaching facially neutral devices, minority voters experience systematic disfranchisement through suppression (literacy tests, poll taxes, registration obstacles) that the formal reading does not touch. The extraction runs persistently: the formal reading enables gatekeepers to evade Amendment compliance; minorities bear the cost of representation denial. High suppression (disfranchisement barriers), high extractiveness (gatekeepers retain political power through exclusion), minimal coordination benefit.
constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ELECTION AUTHORITIES (TANGLED ROPE) — States function under two conflicting mandates: administer elections under state law (coordination function — states have legitimate interests in voter qualification, fraud prevention) AND comply with federal constitutional constraints (extraction when those interests map to racial targeting). Under the effective-franchise reading, states face active enforcement: their facially neutral devices are subject to effects scrutiny. The constraint is hybrid: genuine coordination (how to set ballot access standards without fraud) embedded in asymmetric extraction (federal override of state preferences). States experience enforcement as coercive; federal observers see it as necessary to prevent subordination.
constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL ENFORCEMENT AUTHORITY (ROPE) — Congress and the federal executive experience the effective-franchise reading as a coordination mechanism: the Amendment supplies the authority and duty to dismantle disfranchisement devices that would otherwise persist. The Voting Rights Act (preclearance, Section 2 effects tests) is the coordination solution to a collective action problem: individual states lack incentive to enfranchise minorities over local gatekeeping preferences, so federal override substitutes. From this vantage, enforcement is coordination, not extraction. Beneficiary of the Amendment's grant of authority.
constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT DOCTRINE (PITON) — The Court's jurisprudence on the Fifteenth Amendment embodies substantial theater: bold pronouncements of constitutional principle (the Amendment reaches disfranchisement by results) coexist with narrow holdings that preserve state administrative discretion and limit remedial reach. Shelby County (2013) exemplifies the piton: the Court declared the preclearance formula obsolete while preserving the Amendment's formal authority, leaving states free to adopt new disfranchisement devices with less federal scrutiny. The doctrine persists in degraded form — the principle is asserted, but enforcement capacity has atrophied. Theater ratio high because the normative claim (disfranchisement is unconstitutional) stands disconnected from enforcement mechanism.
constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: VOTING RIGHTS ADVOCACY COALITION (SCAFFOLD) — Organized civil rights, voting rights, and democracy organizations experience the effective-franchise reading as a temporary governance structure whose sunset is contingent on state compliance evolution. The Voting Rights Act's preclearance and Section 2 effects tests were understood as transitional: once disfranchisement devices were dismantled and state practices reformed, federal oversight could recede. The coalition sees the constraint as having a built-in sunset logic: federal enforcement diminishes as state behavior complies. Since Shelby County, the advocacy coalition experiences the sunset as forced prematurely — preclearance ended before states had genuinely internalized the compliance norm.
constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational vantage, the effective-franchise reading embodies the permanent tension between two constitutional goods: federalism (state authority over electoral administration) and equal protection (federal guarantee against subordination by race). The constraint is the mechanism that holds both goods in tension: it reaches disfranchisement in operation (federal power) while preserving legitimate state discretion in ballot access standards (federalism). The effective-franchise reading generates this hybrid precisely because it refuses to reduce either good to the other. High extractiveness reflects the subordination cost to states; genuine coordination function (preserving federalism while enforcing equal protection) prevents classification as pure snare.
constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifteenth_amendment__effective_franchise_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fifteenth_amendment__effective_franchise_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifteenth_amendment__effective_franchise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fifteenth_amendment__effective_franchise_reading, TR),
    TR >= 0.70.

:- end_tests(fifteenth_amendment__effective_franchise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The effective-franchise reading sustains federal authority to override state voting rules, creating persistent extraction of state autonomy in the service of protecting minority franchise. Unlike a pure snare, the extraction is bounded: federal enforcement targets only disfranchisement by race, not all electoral administration. The reading preserves legitimate state discretion in ballot access standards (fraud prevention, citizenship verification, registration deadlines) provided those standards do not operate to deny by race. The extractiveness reflects the subordination cost to states and gatekeepers who lose the ability to use facially neutral devices for racial targeting. Suppression (0.58): Moderate-high. Measured suppression reflects ongoing barriers despite federal law: voter purges, voter ID requirements, polling place closures, registration obstacles, and redistricting all operate with documented racial disparities. The effective-franchise reading addresses suppression mechanism but does not eliminate it; suppression persists at lower levels after federal intervention. Theater ratio (0.45): Moderate-low. The effective reading supplies concrete enforcement mechanisms (preclearance in original VRA; Section 2 effects tests; statutory standards for vote dilution). The doctrine articulates its principle with clarity. Theater rises post-Shelby County (2013 measurement: 0.62) because preclearance ended without corresponding state compliance evolution, leaving the normative principle asserted but enforcement capacity degraded.
 *
 * PERSPECTIVAL GAP:
 *   The effective-franchise reading generates perspectival cleavage across all seven positions. From the disenfranchised voter (powerless/trapped), the Amendment's promise appears absolute — mountain: either enforced or not. From racial minority communities (moderate/trapped), disfranchisement operates as snare: gatekeepers extract political power through subordination. From states (powerful/constrained), the effective reading imposes tangled rope: genuine coordination interests (fraud prevention, ballot access) collide with federal override. From federal enforcement (institutional/arbitrage), the reading is rope: coordination mechanism solving a collective action problem. From the Supreme Court (institutional/constrained), doctrine appears degraded (piton) — the principle is asserted but enforcement capacity has atrophied post-Shelby County. From the advocacy coalition (organized/mobile), the reading supplies temporary enforcement with sunset logic (scaffold). From the analytical observer (analytical/analytical), the reading holds two constitutional goods in enduring tension: federalism and equal protection. No perspective sees the constraint identically. This perspectival range is diagnostic of tangled rope classification: genuine coordination function embedded in asymmetric extraction, experienced differently from each structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Minority voters as victims with trapped exit (d=0.95) experience maximum effective extraction; they cannot exit the jurisdiction to restore franchise. States as constrained institutional actors with victim status relative to federal override experience moderate-high d (~0.55-0.65). Federal enforcement authority as beneficiary with arbitrage options experiences low d (~0.15-0.25), treating the enforcement power as coordination benefit rather than extraction cost. The piton and scaffold perspectives at institutional/organized power levels experience different d values reflecting their different relationships to the constraint: the Supreme Court's doctrine appears degraded (high theater despite institutional power), while the advocacy coalition sees a transitional mechanism with exit-path logic. The engine computes chi from these d values; higher d for trapped minorities amplifies experienced extractiveness, while arbitrage options for federal authority compress it. The perspectival gap reflects these differential d values across the observation site.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effects_vs_intent_evidentiary_threshold,
    'What level of discriminatory effects evidence suffices to trigger Fifteenth Amendment enforcement without requiring proof of intentional targeting?',
    'Historical case law analysis (Thornburg v. Gingles, City of Mobile v. Bolden, Abbott v. Perez); empirical correlation studies between device adoption timing and minority voting percentage changes; expert testimony standards in vote dilution litigation',
    'If effects test is strict: states face high remedial burden; minority voters gain robust protection. If effects test is deferential: states retain substantial discretion; minority protection diminishes. The formal reading requires intent; the effective reading uses effects as proxy. This omega locates the doctrinal contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effects_vs_intent_evidentiary_threshold, empirical, 'Evidentiary threshold for discriminatory effects under Fifteenth Amendment').

omega_variable(
    facially_neutral_device_scope,
    'Which facially neutral voting devices (literacy tests, poll taxes, registration deadlines, voter ID requirements, purge protocols) fall within the Fifteenth Amendment''s reach?',
    'Doctrinal review of Supreme Court precedent (literacy tests prohibited in Louisiana v. United States; poll taxes prohibited in Harper v. Virginia; voter ID upheld in Crawford v. Washington); comparative analysis of effects on minority vs majority voters by device type',
    'If scope is broad: most disfranchisement mechanisms are amenable to federal challenge. If scope is narrow: many devices escape Amendment reach. This directly determines the extractiveness score — broader scope means stronger federal prevention of subordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facially_neutral_device_scope, conceptual, 'Scope of facially neutral devices reachable under the Fifteenth Amendment').

omega_variable(
    kernel_interpretive_contest,
    'Does the Fifteenth Amendment''s text (''deny or abridge...on account of race'') reach only devices that explicitly condition on race (formal reading) or any device that operates to deny by race regardless of facial neutrality (effective reading)?',
    'Original constitutional text and framing history; Supreme Court interpretive doctrines (textualism vs. purposivism); comparative constitutional law on equal protection reach in other jurisdictions',
    'CRITICAL: This is the kernel contest itself. The effective reading empowers Congress to reach effects; the formal reading confines reach to explicit conditions. Resolving this omega resolves the reading conflict — but only by adopting one framework over the other, not by empirical discovery. This is a conceptual/preference omega, not empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_interpretive_contest, conceptual, 'The kernel contest: whether the Fifteenth Amendment reaches disfranchisement in operation or only explicit racial conditions').

omega_variable(
    congressional_enforcement_power_scope,
    'Under the Fifteenth Amendment''s Section 2 enforcement clause, how broadly may Congress regulate voting practices to prevent racial disfranchisement by results?',
    'Supreme Court precedent on Fourteenth and Fifteenth Amendment enforcement power (City of Boerne framework; Shelby County; Garrett); legislative record and remedial tailoring analysis for Voting Rights Act reauthorizations',
    'If Congress has broad enforcement power: preclearance and effects tests are constitutional. If power is narrowly construed: preclearance and effects tests may exceed constitutional bounds. This determines whether federal enforcement capacity can persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_enforcement_power_scope, conceptual, 'Scope of congressional enforcement authority under the Fifteenth Amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifteenth_amendment__effective_franchise_reading, 1870, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_eff_theater_1870, fifteenth_amendment__effective_franchise_reading, theater_ratio, 1870, 0.8).
narrative_ontology:measurement(fa_eff_theater_1965, fifteenth_amendment__effective_franchise_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(fa_eff_theater_2000, fifteenth_amendment__effective_franchise_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(fa_eff_theater_2013, fifteenth_amendment__effective_franchise_reading, theater_ratio, 2013, 0.62).

% Extraction over time
narrative_ontology:measurement(fa_eff_extract_1870, fifteenth_amendment__effective_franchise_reading, base_extractiveness, 1870, 0.85).
narrative_ontology:measurement(fa_eff_extract_1965, fifteenth_amendment__effective_franchise_reading, base_extractiveness, 1965, 0.72).
narrative_ontology:measurement(fa_eff_extract_2000, fifteenth_amendment__effective_franchise_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(fa_eff_extract_2013, fifteenth_amendment__effective_franchise_reading, base_extractiveness, 2013, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fa_eff_suppress_1870, fifteenth_amendment__effective_franchise_reading, suppression_requirement, 1870, 0.92).
narrative_ontology:measurement(fa_eff_suppress_1965, fifteenth_amendment__effective_franchise_reading, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(fa_eff_suppress_2000, fifteenth_amendment__effective_franchise_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(fa_eff_suppress_2013, fifteenth_amendment__effective_franchise_reading, suppression_requirement, 2013, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifteenth_amendment__effective_franchise_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifteenth_amendment__effective_franchise_reading, fifteenth_amendment__formal_franchise_reading).
narrative_ontology:affects_constraint(fifteenth_amendment__effective_franchise_reading, voting_rights_act_preclearance).
narrative_ontology:affects_constraint(fifteenth_amendment__effective_franchise_reading, section_two_effects_test).
narrative_ontology:affects_constraint(fifteenth_amendment__effective_franchise_reading, shelby_county_authority_suspension).

% DUAL FORMULATION NOTE:
% The Fifteenth Amendment kernel decomposes into two constraint stories: the effective-franchise reading (this story) and the formal-franchise reading (sibling story). These readings have different epsilon values (effective=0.62, formal=0.45 estimated) reflecting different enforcement scopes and extractiveness profiles. The effective reading sustains broader federal override; the formal reading preserves state discretion. They are structurally distinct constraints sharing a common kernel. Downstream constraints include the Voting Rights Act's preclearance mechanism (enabled by effective reading), Section 2 effects tests (enabled by effective reading), and post-Shelby County constraint on federal authority (tension between formal and effective readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifteenth_amendment__effective_franchise_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
