% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine (Standing Graded by Capacity and Governance Legitimacy)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   Since the early 1990s, a doctrinal arrangement has treated sovereign
 *   standing as a variable rather than a constant: capacity indices,
 *   governance scorecards, Security Council determinations, and creditor
 *   assessments grade states along a spectrum, and deficiency findings
 *   license external administration, financing conditionality, and in the
 *   limit armed intervention. The arrangement solves a real problem - state
 *   collapse spills refugees, pirates, pathogens, and armed groups across
 *   borders, and someone must decide how response is authorized - while
 *   simultaneously concentrating the discretion to grade in institutions
 *   controlled by the states least likely ever to be graded down. This file
 *   instantiates ONLY the graduated_sovereignty reading of the
 *   westphalian_sovereignty kernel, per the epsilon-invariance discipline:
 *   the absolute and conditional readings are separate constraints in
 *   separate files with different victim sets and different epsilon values,
 *   and no averaging across readings occurs here. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   state-failure coordination function plus asymmetric extraction, actively
 *   enforced) while the authored metrics describe heavily extractive
 *   operation sitting close to the snare boundary - the engine measures that
 *   divergence; the claim is not reconciled to the metrics. Interval
 *   convention: T=0 corresponds to 1990 (dissolution of the Cold War
 *   settlement), T=35 to 2025; all measurement points are observed history,
 *   not projection.
 *
 * KEY AGENTS:
 *   - - great_power_interveners: Primary agenda-setter and principal collector (institutional/arbitrage) - controls the classification machinery and retains exit from it whenever binding
 *   - - creditor_country_directors: Concentrated beneficiary (institutional/arbitrage) - converts grading into policy alignment without treaty negotiation
 *   - - international_administrative_agencies: Mandate-dependent beneficiary (institutional/mobile) - inherits governing authority over reclassified territories
 *   - - emerging_regional_powers: Dual-positioned actor (organized/mobile) - wields the framework regionally while hedging precedent risk against themselves
 *   - - weak_state_governments: Primary target among states (moderate/constrained) - bears conditionality and reclassification risk with no opt-out from assessment
 *   - - weak_state_populations: Diffuse target (powerless/trapped) - carries austerity and externally designed programs with no vote in their grading
 *   - - administered_territory_residents: Maximal-exposure target (powerless/trapped) - lives under foreign executive, legislative, and judicial authority
 *   - - g77_united_nations_bloc: Excluded contestant (organized/constrained) - contests in venues where the operative decisions are not made
 *   - - comparative_politics_scholars: Analytical observer (analytical/analytical) - supplies the data and the critiques, sets and bears nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.66).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.66).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine (Standing Graded by Capacity and Governance Legitimacy)").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '7ce1b8ce-bab8-4bbc-b609-79ce02114d13').
narrative_ontology:cs_kernel_codification('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', fixed_text).
narrative_ontology:cs_authority_grounding('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', extraction).
narrative_ontology:cs_interpretation_layer_present('7ce1b8ce-bab8-4bbc-b609-79ce02114d13').
narrative_ontology:cs_reading_relation('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', foundational, sovereignty_graded_by_capacity_and_legitimacy).
narrative_ontology:cs_axiom_status(sovereignty_graded_by_capacity_and_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', sovereignty_graded_by_capacity_and_legitimacy, instrumental).
narrative_ontology:cs_axiom('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', secondary, external_classification_discretion_is_prerogative).
narrative_ontology:cs_axiom_status(external_classification_discretion_is_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', external_classification_discretion_is_prerogative, conventional).
narrative_ontology:cs_reference_frame('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', capacity_graded_stewardship_order).
narrative_ontology:cs_drift_state('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', post_libya_iraq_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ce1b8ce-bab8-4bbc-b609-79ce02114d13', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, great_power_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, creditor_country_directors).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_administrative_agencies).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, emerging_regional_powers).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_state_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, administered_territory_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, emerging_regional_powers).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, failed_state_spillover_thesis).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, external_stewardship_outperforms_nonintervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Permanent members of the Security Council and lead coalition states. They draft the resolutions and fund the assessment machinery that grades other states' standing, decide which deficiency findings become missions, and retain freedom to act outside the framework when authorization is withheld. When the framework widens their room to administer or intervene, they collect the widened prerogative; when it binds them, they route around it. Abandoning the framework would mean surrendering the legitimating vocabulary their operations currently borrow.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, great_power_interveners, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, great_power_interveners, beneficiary).

% Voting majorities on IMF and World Bank boards and donor-group steering committees. They set the governance benchmarks attached to financing, commission the country assessments, and approve or suspend disbursement against them. Benchmark compliance delivers policy alignment in borrowing countries without treaty negotiation. Their downside is limited to the cost of running the assessment machinery.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, creditor_country_directors, beneficiary,
    institutional, biographical, arbitrage, global).

% Transitional administrations, special political missions, and implementing contractors of the UNMIK-, OHR-, and UNAMA-style kind. They receive mandates, budgets, staffing pipelines, and career structures from the classification process; mandate renewal is their operating cycle. When a territory is found to need administration, they inherit executive, legislative, and judicial authority over it. Exit is rotation into the next mission.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_administrative_agencies, beneficiary,
    institutional, biographical, mobile, regional).

% Regional organizations and leading regional states, including ECOWAS, African Union mission contributors, and sub-regional hegemons. They invoke the same standing-gradation logic to authorize their own neighborhood operations and to attract external funding for them. At the same time, every precedent that lowers another state's standing lowers the bar applicable to themselves, and several carry internal fracture lines that outside assessors already cite. They hedge: championing the framework abroad while contesting its application at home.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, emerging_regional_powers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, emerging_regional_powers, payer).

% Governments of states carrying low index scores or active deficiency findings. Their borrowing terms, market access, and immunity from external administration all price in the classification. They can contest individual assessments, diversify creditors, or align with blocs, but they cannot opt out of being assessed, and a downgrade follows them across venues. Some convert compliance into regime survival; a few convert sustained reform into graduation out of the category.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_state_governments, payer,
    moderate, biographical, constrained, national).

% Residents of states under governance conditionality. They experience the arrangement as austerity packages tied to benchmarks they did not set, and as services delivered through externally designed programs with limited local accountability. They hold no vote in the indices or resolutions that determine their state's standing; their main recourse is emigration, which the classification itself makes harder.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_state_populations, payer,
    powerless, immediate, trapped, national).

% Populations of territories placed under direct transitional administration, such as Kosovo, Bosnia and Herzegovina under the OHR, Timor-Leste, and occupied Iraq 2003-2005. Executive, legislative, and judicial authority is exercised by appointed international officials, typically without local electoral accountability over the administrator, and final-status decisions rest with external bodies. Their exit is departure; the duration of the arrangement is set by the administering authority's timetable, not theirs.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, administered_territory_residents, payer,
    powerless, immediate, trapped, regional).

% Coalition of developing-country delegations (G77/NAM, African Group). They contest grading exercises in General Assembly debates, sponsor equality-affirming resolutions, and built the g7+ as a counter-platform for conflict-affected states. But the decisions that actually grade states are taken in Security Council chambers and board rooms where their votes are diluted or absent. They would subordinate classification to consent-based mechanisms; they are not seated where that proposal would be voted.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, g77_united_nations_bloc, excluded,
    organized, generational, constrained, global).

% Analysts of statehood, recognition, and intervention in universities and research institutes. They compile much of the data the assessments draw on, critique the indices' validity, and publish the genealogies connecting the framework to earlier civilizing-mission doctrines. They neither set nor bear the arrangement; their seat is evaluative.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, comparative_politics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedure for handling state collapse and its transboundary spillovers: it specifies when external administration or intervention is treated as legitimate, allocates response capacity (peace operations, financing, technical administration) toward states graded unable or unwilling to manage their territory, and gives third parties a common vocabulary for joining such responses.
% TRANSFER_FUNCTION: Moves classification discretion and decision rights from weak states to external authorities: fiscal-policy autonomy moves from debtor governments to creditor boards via conditionality; administrative and judicial authority moves from governed populations to mission leadership in administered territories; and in intervention cases, territorial control moves to external coalitions.
% ABSENT_VOICES: Populations of graded states have no vote in the indices, board decisions, or resolutions that determine their status. The G77 bloc contests in the General Assembly but the operative decisions sit in Security Council chambers and IFI board rooms where its weight is structurally diluted. Affected societies are rarely represented on the assessment teams that produce the findings about them.
% DISAPPEARANCE_RATIONALE: If the graduated framework vanished overnight, intervention legality would revert to strict non-intervention or to naked power politics with no legitimating vocabulary; conditionality regimes would lose their doctrinal cover and require renegotiation; the status of administered territories would be thrown open; and donor allocation systems built on fragility scoring would lose their organizing principle. Every named seat's position depends on the arrangement existing.
% FOUNDING_PROBLEM: After the Cold War settlement dissolved, state collapses in Somalia and the former Yugoslavia produced mass atrocity, refugee flows, and armed spillover while the existing rules offered no agreed doctrine for when outside action could lawfully replace a failing territorial authority. The graduated reading was built to supply criteria: standing would vary with capacity and governance legitimacy, and deficiency would license external stewardship.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: African Union Peace and Security Council communiques and UNHCR/OCHA operational reporting document the state-collapse spillover problem the arrangement answers; the g7+ group of conflict-affected states attests the problem is live while formally rejecting externally-set grading as the remedy; security-studies scholarship outside donor funding streams reaches the same problem statement. No corroborating source outside the beneficiary set attests that discretionary classification specifically is required - the corroboration covers the problem, not the remedy.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 because the arrangement's core yield - the discretion to classify - is decoupled from any service delivered: grading outputs move financing terms, immunity from administration, and intervention legitimacy, and the grading institutions face no symmetrical exposure. The temporal series shows accumulation through the 1990s, a peak around the transitional-administration era (Kosovo standards-before-status, the Iraq CPA, the trusteeship-debate high-water mark), partial retreat after the Iraq and Libya delegitimation cascade, and a rebound as fragility indexing became embedded in concessive-finance allocation. Suppression is authored at 0.62 as a RAW structural property - it is not scaled by power or scope in the way extractiveness is - reflecting that the arrangement's persistence requires continuous active maintenance against the entrenched juridical-equality norm (Charter Article 2(7)): every grading exercise must be defended in authorization battles, and enforcement has shifted from kinetic (early-1990s operations) toward procedural and budgetary leverage (index authority, disbursement suspension). Theater_ratio at 0.42 reflects a growing share of performative activity - benchmark workshops, indicator harmonization, review ceremonies - relative to shrinking field administration. Accessibility_collapse is LOW (0.38) because the alternatives remain fully articulable and institutionally embodied: categorical equality commands General Assembly majorities and sits in ICJ dicta, and the violation-threshold alternative is separately codified; understanding the graduated framework does not close these exits. Resistance is substantial (0.58): sustained bloc contestation, the g7+ counter-platform, and the post-2003 backlash that drove trusteeship language underground without displacing the practice. All three series run on one shared time grid (points 0, 5, 10, 15, 20, 25, 30, 35) so no metric row borrows another's end-state values. Coalition note: the powerless seats possess latent coalition power (General Assembly voting weight, debtor coordination, the g7+) but it is structurally diluted at the decision points where grading actually occurs - the arrangement survives precisely by keeping classification away from one-state-one-vote venues.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute sharply different types from identical structural data. From the great-power seat, the arrangement is prudential toolmaking: flexibility to respond to pathology that rigid equality rules would forbid, exercised case by case with regrettable imperfection. From the weak-government and administered-population seats, the same structure is juridical inequality: a standing hierarchy in which they appear only as objects of assessment, never as assessors, and in which the assessors' own domestic failures (financial crises, infrastructure collapse, mortality shocks inside strong states) never trigger equivalent findings. Same-level divergence: weak_state_governments and emerging_regional_powers hold adjacent nominal standing in the state system, yet the former appears in the framework only as a graded object while the latter wields it sub-regionally and hedges its precedents - differentiated exit options (constrained versus mobile) and dual positioning drive different computed directionalities despite comparable global rank. A further identity-lock dynamic runs through the professional carriers: assessment specialists and mission staff have career structures fused to the index-and-mandate cycle, so the framework's methodological premises are defended as professional competence, not merely as policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the low-d pole: great_power_interveners (agenda-setter plus collector, arbitrage exit) sit nearest the beneficiary end - the framework subsidizes them with discretion they can exercise or discard; creditor_country_directors and international_administrative_agencies collect directly and can rotate out. Victim declarations map to the high-d pole: weak_state_governments (constrained exit - assessment follows them across venues), weak_state_populations and administered_territory_residents (trapped - no vote, no exit except departure the classification itself impedes) sit nearest the full-target end, with administered residents maximal. Emerging_regional_powers derive a mid-spectrum directionality from their dual beneficiary/payer position: they collect regional operating license while absorbing precedent exposure. No directionality_overrides are authored: the derivation chain from declared roles, power atoms, and exit options already separates these seats correctly, and the one candidate correction (regional powers' net position nearer symmetric than their beneficiary role alone implies) is handled by the secondary_role declaration rather than an override. Larger spatial scopes on the global-seats amplify effective extraction modestly in the engine's computation; the trapped regional-scope targets carry the highest per-seat chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination/extraction decomposition prevents two opposite mislabels. Reading the arrangement as pure neo-colonial imposition misses the demonstrated coordination content: Somali relief logistics, the anti-piracy contact-group process, and Ebola-era cross-border response all ran through this framework's vocabulary with participation - sometimes initiated requests - from the very states the framework grades, which a pure cover-story account cannot explain. Reading it as benign capacity-coordination misses the rent: the grading discretion is the asset, it is held asymmetrically, and it survived every delegitimation episode intact because its holders control the venues. On the R5 genealogy: the founding problem (state-collapse spillover with no response doctrine) remains live and is corroborated by non-beneficiary sources, so no mandatrophy_resolved declaration is authored - the arrangement still does what it was built to do, badly and lopsidedly. The forward-looking risk is a piton trajectory: if regional response mechanisms mature and state-failure incidence falls, the classification bureaucracy could persist as inertial index-theater maintained by career structures rather than function - the theater_ratio series (rising through the interval, plateauing near 0.42-0.44) is the leading indicator to watch, and the mismatch consumer should treat any future dead-problem-plus-world-rearranges combination as the zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the westphalian_sovereignty kernel - what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Cross-file comparison of the three reading stories'' victim sets, beneficiary sets, and epsilon values; the disagreement is located in the criterion that conditions standing - none (absolute_sovereignty), a violation threshold (conditional_sovereignty), or continuous capacity-legitimacy grading (this reading).',
    'Under the absolute reading this constraint''s victim set vanishes entirely, since no external classification exists to bear; under the conditional reading the victim set narrows to violation-triggered cases and the standing-gradation discretion disappears. The classification authored here is valid only for the graduated reading and must not be averaged across the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: which reading of the sovereignty kernel this story instantiates and what siblings would change.').

omega_variable(
    classifier_interest_endogeneity,
    'Do grading outputs actually track capacity and governance legitimacy, or do they track the geopolitical interests of the grading institutions?',
    'Code historical classification decisions against blind capacity metrics and test the residual correlation with alliance membership, basing value, resource endowments, and patron relationships of the graded states.',
    'Interest-driven grading converts the remaining coordination share into cover and pushes the structure toward the snare boundary with epsilon above the authored 0.66; capacity-tracked grading supports the tangled_rope reading and leaves a reform path through fixed, contestable criteria.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classifier_interest_endogeneity, empirical, 'Whether the spectrum is a descriptive taxonomy or an instrument of the classifiers.').

omega_variable(
    stewardship_separability,
    'Is state-failure response separable from discretionary standing-gradation - could a consent-based, fixed-criteria protocol handle spillover without classification discretion?',
    'Compare outcomes of rule-bound regional mechanisms (AU and ECOWAS interventions, g7+ compacts, request-based administrations) against discretionary Security Council practice on matched state-failure cases.',
    'Demonstrated separability means the extractive component is removable without losing the coordination function, stabilizing the tangled_rope classification with a reform trajectory; inseparability means part of the measured extraction is the intrinsic price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_separability, conceptual, 'Whether the coordination function can be retained while the grading discretion is removed.').

omega_variable(
    downward_grading_symmetry,
    'Does the grading operate symmetrically - do high-capacity states ever get downgraded following domestic failure of their own?',
    'Longitudinal audit of index placements and deficiency findings against objective capacity shocks inside strong states: financial crises, infrastructure collapse, mortality events, and governance breakdowns in classifier states themselves.',
    'One-directional grading confirms the spectrum functions as a one-way instrument aimed at weak states, raising effective extraction on every weak-state seat and supporting the neo-colonial reading of the arrangement; symmetric grading would support a descriptive-taxonomy reading with materially lower chi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downward_grading_symmetry, empirical, 'Whether the capacity-legitimacy spectrum is applied upward as well as downward.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grad_sov_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(grad_sov_tr_t0, observed).
narrative_ontology:measurement(grad_sov_tr_t5, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(grad_sov_tr_t5, observed).
narrative_ontology:measurement(grad_sov_tr_t10, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(grad_sov_tr_t10, observed).
narrative_ontology:measurement(grad_sov_tr_t15, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(grad_sov_tr_t15, observed).
narrative_ontology:measurement(grad_sov_tr_t20, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(grad_sov_tr_t20, observed).
narrative_ontology:measurement(grad_sov_tr_t25, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(grad_sov_tr_t25, observed).
narrative_ontology:measurement(grad_sov_tr_t30, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(grad_sov_tr_t30, observed).
narrative_ontology:measurement(grad_sov_tr_t35, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(grad_sov_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(grad_sov_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(grad_sov_be_t0, observed).
narrative_ontology:measurement(grad_sov_be_t5, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(grad_sov_be_t5, observed).
narrative_ontology:measurement(grad_sov_be_t10, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(grad_sov_be_t10, observed).
narrative_ontology:measurement(grad_sov_be_t15, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(grad_sov_be_t15, observed).
narrative_ontology:measurement(grad_sov_be_t20, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(grad_sov_be_t20, observed).
narrative_ontology:measurement(grad_sov_be_t25, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(grad_sov_be_t25, observed).
narrative_ontology:measurement(grad_sov_be_t30, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 30, 0.64).
narrative_ontology:measurement_basis(grad_sov_be_t30, observed).
narrative_ontology:measurement(grad_sov_be_t35, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 35, 0.66).
narrative_ontology:measurement_basis(grad_sov_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(grad_sov_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(grad_sov_su_t0, observed).
narrative_ontology:measurement(grad_sov_su_t5, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(grad_sov_su_t5, observed).
narrative_ontology:measurement(grad_sov_su_t10, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(grad_sov_su_t10, observed).
narrative_ontology:measurement(grad_sov_su_t15, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(grad_sov_su_t15, observed).
narrative_ontology:measurement(grad_sov_su_t20, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(grad_sov_su_t20, observed).
narrative_ontology:measurement(grad_sov_su_t25, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(grad_sov_su_t25, observed).
narrative_ontology:measurement(grad_sov_su_t30, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(grad_sov_su_t30, observed).
narrative_ontology:measurement(grad_sov_su_t35, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(grad_sov_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the westphalian_sovereignty kernel. The colloquial label 'sovereignty' conflates three structurally distinct doctrines: the absolute reading (no external classification; victims are only of domestic predation; negligible external extraction), the conditional reading (violation-threshold gating; victims defined by atrocity commission; moderate extraction through selective triggering), and this graduated reading (continuous grading; victims are the low-capacity states as a class; high extraction through classification discretion). The upstream member is the absolute reading - the Charter-text baseline that all parties cite - and it feeds the other two as the doctrinal foil each defines itself against. This file links both siblings; each sibling file must link back. Epsilon differs across the family because the victim sets differ, not because one constraint is viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
