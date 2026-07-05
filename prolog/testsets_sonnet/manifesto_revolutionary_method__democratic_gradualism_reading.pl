% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism Reading of the Revolutionary Method Kernel
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This story instantiates the democratic-gradualism reading of the
 *   manifesto_revolutionary_method kernel: the claim that working-class
 *   emancipation can and should be pursued through winning electoral
 *   majorities and reforming existing liberal-democratic institutions from
 *   within, rather than through party-led insurrection (the
 *   vanguard_rupture_reading) or the construction of parallel council
 *   institutions (the council_communist_reading). Historically this is the
 *   Second International/social-democratic and later Eurocommunist
 *   trajectory: the SPD's post-Erfurt program orientation, Bernstein's
 *   revisionism, British Labourism, and their descendants. The reading
 *   commits the movement's organizational resources to legal, parliamentary
 *   channels, which produces genuine coordination benefits (avoiding
 *   repression, building durable mass organizations, embedding reforms in
 *   law) alongside a real, structurally asymmetric cost borne by militant
 *   factions whose tactics are treated as threats to the electoral
 *   coalition's legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism Reading of the Revolutionary Method Kernel").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '931b295d-eade-4606-8b9c-8d9a7b149c79').
narrative_ontology:cs_kernel_codification('931b295d-eade-4606-8b9c-8d9a7b149c79', fixed_text).
narrative_ontology:cs_authority_grounding('931b295d-eade-4606-8b9c-8d9a7b149c79', lineage).
narrative_ontology:cs_interpretation_layer_present('931b295d-eade-4606-8b9c-8d9a7b149c79').
narrative_ontology:cs_reading_relation('931b295d-eade-4606-8b9c-8d9a7b149c79', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('931b295d-eade-4606-8b9c-8d9a7b149c79', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('931b295d-eade-4606-8b9c-8d9a7b149c79', foundational, existing_state_institutions_are_transformable_vehicles).
narrative_ontology:cs_axiom_status(existing_state_institutions_are_transformable_vehicles, holdable).
narrative_ontology:cs_axiom_grounding('931b295d-eade-4606-8b9c-8d9a7b149c79', existing_state_institutions_are_transformable_vehicles, empirically_contingent).
narrative_ontology:cs_axiom('931b295d-eade-4606-8b9c-8d9a7b149c79', secondary, electoral_majority_constitutes_legitimate_working_class_power).
narrative_ontology:cs_axiom_status(electoral_majority_constitutes_legitimate_working_class_power, holdable).
narrative_ontology:cs_axiom_grounding('931b295d-eade-4606-8b9c-8d9a7b149c79', electoral_majority_constitutes_legitimate_working_class_power, conventional).
narrative_ontology:cs_reference_frame('931b295d-eade-4606-8b9c-8d9a7b149c79', erfurt_program_parliamentary_orientation).
narrative_ontology:cs_drift_state('931b295d-eade-4606-8b9c-8d9a7b149c79', contemporary_social_democracy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('931b295d-eade-4606-8b9c-8d9a7b149c79', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, established_trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_labor_representatives).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, wildcat_strike_organizers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_workers_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrine that socialism arrives through winning elections and passing legislation. Controls candidate selection, coalition strategy, and the party's disciplinary apparatus against factions favoring extra-parliamentary action. Collects institutional legitimacy, state resources, and access to governing coalitions as the price of committing the working-class movement to electoral timelines.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, beneficiary).

% Bargains within recognized labor law, receives dues checkoff and collective bargaining rights in exchange for accepting arbitration procedures and no-strike clauses. Union leadership's institutional survival is tied to the legality the gradualist framework confers; they lose recognized standing if the movement shifts toward extra-legal or insurrectionary tactics.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, established_trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Holds elected office premised on the theory that class power can be exercised through existing legislative structures. Career, salary, and political capital depend on the electoral-reform frame remaining the dominant working-class strategy; they can exit into other institutional roles if the coalition fractures, unlike rank-and-file militants.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_labor_representatives, beneficiary,
    powerful, biographical, mobile, national).

% Argues that capitalist state structures cannot be neutrally used to abolish capitalist property relations and that electoral timelines allow capital to reorganize and suppress gains between cycles. Routinely denounced by party leadership as 'adventurist' or 'ultra-left,' expelled from party organizations, and denied strike funds or legal defense when direct action tactics bring state repression.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, biographical, trapped, national).

% Organizes unauthorized strikes outside the bargaining framework the unions have committed to. Faces disavowal by official union leadership, loss of strike pay, and sometimes direct collaboration between party-aligned union officials and employers or police to end the action quickly and preserve the arbitration relationship.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, wildcat_strike_organizers, payer,
    powerless, immediate, trapped, regional).

% Favors workplace occupation, direct assemblies, and non-electoral organizing. Structurally locked out of resource allocation controlled by party and union apparatuses; their tactical repertoire is treated as a threat to the electoral coalition's respectability rather than as a legitimate complement to it.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_workers_movements, excluded,
    moderate, biographical, constrained, regional).

% Provides the electoral, legislative, and judicial machinery this reading commits itself to using. Sets the rules of legitimate contestation (ballot access, campaign finance, judicial review of legislation) that the gradualist strategy must operate within, and retains capacity to slow, dilute, or reverse reforms through courts, bureaucratic administration, or capital flight in response to electoral outcomes.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state_institutions, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state_institutions, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a mass working-class movement around a single, legally legible strategy — winning elections, building disciplined party and union organizations, and legislating reforms — which allows large numbers of participants with varying risk tolerance to act in concert without individually risking arrest, blacklisting, or violent state repression.
% TRANSFER_FUNCTION: Moves organizational legitimacy, state recognition, and access to bargaining/legislative channels toward parties and union leaderships that commit to electoral and legal methods, while moving material and reputational costs (expulsion, loss of strike funds, exposure to unmitigated state repression) onto militants and rank-and-file organizers who pursue extra-legal or insurrectionary tactics.
% ABSENT_VOICES: Revolutionary militants and wildcat organizers would argue that the gradualist framework systematically defangs the movement's leverage by ruling out disruption timed for maximum effect, and that the 'adventurist' label is deployed to protect institutional actors' standing rather than to serve working-class interests as a whole. They are formally inside the broader movement but structurally excluded from resource allocation and strategic decision-making controlled by party/union hierarchies.
% DISAPPEARANCE_RATIONALE: If the commitment to democratic-electoral gradualism vanished overnight, social democratic parties would lose their organizing rationale for restraining militant factions, unions would face internal pressure to abandon no-strike and arbitration commitments, and the working-class movement would likely fracture into competing strategic currents (electoral, insurrectionary, council-based) contesting for the same organizational infrastructure and resources.
% FOUNDING_PROBLEM: Late 19th and early 20th century socialist movements faced a strategic choice after early insurrectionary attempts (1848, the Paris Commune) were violently crushed: how to build durable working-class power without inviting catastrophic repression, given that industrializing states were extending suffrage and permitting (constrained) party and union organizing.
% FOUNDING_PROBLEM_CORROBORATION: Social democratic party historians and mainstream political scientists attest the strategy succeeded in embedding labor rights, welfare provisions, and working-class political representation durably into liberal-democratic states. Independent labor historians outside the party apparatus, and testimony from expelled left factions, attest that the same strategy has repeatedly demobilized militant capacity at moments of maximum leverage (e.g., the German SPD's role in suppressing the 1918-19 revolution) — corroboration for both the 'solved' and 'captured/demobilizing' readings exists, but no single account is uncontested by outside observers.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.40 (moderate, per the expected structural delta) reflecting that the constraint on rapid transformation is real but not severe — legislated reforms genuinely improve material conditions for organized workers even as the strategy forecloses faster or more thorough transformation. Suppression (0.45) captures the disciplinary machinery (expulsions, denial of strike funds, public denunciation as 'adventurist') that gradualist party/union leaderships have historically used against militant factions — this is a raw structural property, not scaled by scope. Theater ratio rises over the interval (0.15 to 0.42) reflecting the well-documented pattern in which parliamentary socialist parties increasingly substitute electoral messaging and legislative gesture for the class-transformative content of their original program (a Goodhart-style drift from transformation to institutional participation as its own end). Accessibility collapse (0.38) and resistance (0.55) are moderate: alternative strategies (council communism, insurrectionary organizing) remain visible and actively argued for throughout the interval, and militant resistance to gradualist discipline is a persistent, organized feature of the labor movement's history, not merely residual noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties and parliamentary representatives sit near the beneficiary end: they administer the strategy, and it converts into institutional power, salaries, and legitimacy for them specifically. Established unions are secondary beneficiaries whose institutional survival depends on the legal recognition the strategy secures. Revolutionary militants and wildcat organizers sit near the full-target end: trapped by immediate material stakes (strikes, arrests, blacklisting) with no institutional exit, they bear the disciplinary costs of the strategy without capturing its institutional rewards. The state institutions occupy a dual observer/agenda-setter role — they did not create this reading, but they set the rules of legitimate contestation the reading commits itself to operating within, which is why their exit option is 'analytical' rather than a stake in the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure rope or pure snare) prevents two mislabeling errors: (1) treating the gradualist strategy as pure benign coordination ignores the documented, repeated instances (Germany 1918-19, various Popular Front-era suppressions of wildcat action) where party/union discipline actively suppressed working-class militancy to protect electoral respectability — a real victim class exists; (2) treating it as pure extraction ignores the genuine, durable material gains (labor law, welfare states, suffrage-linked political power) the strategy secured for organized workers across a century, which a militant-only reading would erase. The coordination function (avoiding catastrophic repression, building mass legal organization) and the extraction function (suppressing more radical currents to protect institutional position) are both real and operate through the same structure — the defining tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reform_or_capture_of_working_class_power,
    'Does sustained electoral-gradualist participation genuinely accumulate working-class power toward eventual systemic transformation, or does it structurally convert that power into institutional stakeholding that forecloses transformation past a certain point (the ''parliamentary road'' as absorption rather than path)?',
    'Comparative historical analysis of cases where social democratic parties held sustained governing power (Sweden, post-war Britain, Germany) for evidence of continued momentum toward socialized ownership versus stabilization at a welfare-capitalist equilibrium; examine internal party archives for documented moments where transformative proposals were shelved specifically to preserve electoral coalitions.',
    'If the strategy structurally converts to stabilization at welfare capitalism, the coordination function is real but bounded and the tangled_rope classification understates a drift toward piton (a vestigial transformative mandate maintained mostly rhetorically); if genuine accumulation toward transformation is empirically supported in some cases, the tangled_rope characterization with moderate ε is well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_or_capture_of_working_class_power, empirical, 'Whether gradualist participation accumulates toward transformation or stabilizes as institutional capture.').

omega_variable(
    militant_suppression_necessity_or_pretext,
    'Is the suppression/expulsion of militant factions a necessary defensive measure protecting the mass movement from state repression triggered by adventurist tactics, or is ''adventurism'' a pretext deployed to protect the institutional position of party and union leaderships regardless of tactical merit?',
    'Case-by-case examination of specific expulsion/suppression episodes (e.g., SPD vs. Spartacists 1918-19, TUC vs. wildcat strikers in various national contexts) for whether the suppressed tactics demonstrably increased state repression risk to the broader movement, versus whether suppression occurred even when tactical risk was low but institutional embarrassment was high.',
    'If suppression tracks genuine risk-management, the victim classification for militants is partially offset by a real protective coordination function; if suppression tracks institutional self-preservation independent of risk, the extraction component is understated and the classification should weight closer to snare-adjacent for the militant-facing seat specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militant_suppression_necessity_or_pretext, conceptual, 'Whether militant suppression serves collective protection or institutional self-preservation.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the manifesto_revolutionary_method kernel itself coherently readable as having three mutually exclusive strategic answers, or does the underlying text license a more fluid, context-dependent combination of tactics (electoral participation AND council-building AND vanguard organization simultaneously, as many actual historical movements practiced) that the three-reading decomposition artificially separates?',
    'Textual and historical analysis of whether major socialist movements that are cited as exemplars of one reading (e.g., German SPD for gradualism) in fact combined tactics across the three readings simultaneously at different organizational levels, which would suggest the readings are analytically separable but not historically exclusive.',
    'If real movements routinely combined tactics, the three-reading decomposition remains analytically valid (each reading captures a distinct strategic logic and constituency) but the disappearance_verdict and founding_problem_status answers for any single reading should be read as partial rather than totalizing accounts of any actual historical movement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three kernel readings are analytically distinct but historically co-present rather than mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1875, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1875, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1875, 0.15).
narrative_ontology:measurement(mani_tr_t1905, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1905, 0.22).
narrative_ontology:measurement(mani_tr_t1935, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1935, 0.3).
narrative_ontology:measurement(mani_tr_t1965, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1965, 0.34).
narrative_ontology:measurement(mani_tr_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(mani_tr_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(mani_be_t1875, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1875, 0.22).
narrative_ontology:measurement(mani_be_t1905, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1905, 0.28).
narrative_ontology:measurement(mani_be_t1935, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1935, 0.35).
narrative_ontology:measurement(mani_be_t1965, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1965, 0.33).
narrative_ontology:measurement(mani_be_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1990, 0.37).
narrative_ontology:measurement(mani_be_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2020, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1875, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1875, 0.3).
narrative_ontology:measurement(mani_su_t1905, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1905, 0.38).
narrative_ontology:measurement(mani_su_t1935, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1935, 0.48).
narrative_ontology:measurement(mani_su_t1965, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement(mani_su_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(mani_su_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial label 'the Marxist revolutionary method' (or 'how the Manifesto says socialism is achieved') into three structurally distinct claims per the ε-invariance principle: democratic_gradualism_reading (this file, ε=0.40, tangled_rope — institutional continuity with liberal democracy, moderate extraction bounded by legal constraint), vanguard_rupture_reading (party seizure of state power, dictatorship of the proletariat as transitional form — expected higher ε and different victim set: political opposition broadly, not just militants), and council_communist_reading (federated workers' council democracy replacing both capitalist state and vanguard party — expected different extraction mechanism entirely, potentially majority-over-minority assembly dynamics rather than party/state coercion). Each reading is generated as its own file with its own ε, beneficiaries, victims, and classification; they are linked here rather than merged because measuring 'the' revolutionary method one way (electoral) versus another (insurrectionary) versus a third (council-based) produces incommensurable ε values — exactly the signal that these are three constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
