% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Internalization Requirement for Practice Displacement (Endogenous-Climb Reading)
 *   domain: political_history/state_formation
 *
 * SUMMARY:
 *   This story instantiates the endogenous_climb_reading of the
 *   legitimacy_of_imposed_practice kernel: practice displacement requires
 *   internalization, and imposed commitments fail without bottom-up adoption
 *   pathways. The standing arrangement under contest is the state's
 *   decree-based displacement machinery (calendar and dress statutes backed
 *   by inspection and penalty), assessed by this reading's own lights. The
 *   empirical delta the reading organizes: calendar displacement failed
 *   outright, with lunar-religious observance persisting for decades in
 *   worship and domestic life; dress displacement achieved partial surface
 *   adoption through urban prestige diffusion while private retention
 *   signaled incomplete internalization. Communities preserving autonomy sit
 *   at the beneficiary end of the structure; the state's modernization
 *   timeline sits at the target end, absorbing repeated expensive failure.
 *   Per the epsilon-invariance principle this is one of three linked family
 *   stories: the exogenous_override_reading and hybrid_scaffolding_reading
 *   instantiate different constraints from the same kernel, with their own
 *   epsilon values, beneficiary/victim structures, and classifications;
 *   nothing here averages over them. The claimed type is mountain because
 *   this reading asserts structural necessity (displacement cannot proceed
 *   without internalization, as a feature of how commitment works, not a
 *   policy choice); beneficiaries are declared deliberately, arming the
 *   false-summit probe, and the naturalness omega documents the irreducible
 *   ambiguity. Claim and metrics are authored independently: the metrics
 *   describe the mixed reality that the requirement forecloses the fast path
 *   completely while leaving the slow path open and meeting sustained
 *   modernizer resistance.
 *
 * KEY AGENTS:
 *   - - state_modernization_program: Primary target and agenda-setter (institutional/constrained) — sets and enforces the displacement statutes and absorbs the failed-timeline and enforcement costs
 *   - - provincial_inspection_corps: Enforcement arm (institutional/constrained) — converts statute into citations; its compliance statistics measure performance, not belief
 *   - - traditional_practice_communities: Primary beneficiary (organized/constrained) — sustains dual practice across the reform decades; nothing essential surrendered
 *   - - religious_authorities: Beneficiary and retention coordinator (organized/identity_locked) — custodial standing rises with each failed campaign
 *   - - urban_adapters: Partial beneficiary via diffusion (moderate/mobile) — adopts through prestige networks at a pace no inspector set; bears doubled-cost transition
 *   - - assimilation_seeking_youth: Excluded voice (powerless/mobile) — wants full assimilation, faces communal sanction sharper than state penalty, exits geographically
 *   - - comparative_modernization_scholars: Analytical observer (analytical/analytical) — assembles the cross-case failure record from outside both camps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.55).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.3).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, mountain).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Internalization Requirement for Practice Displacement (Endogenous-Climb Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation").

domain_priors:emerges_naturally(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'fc07fd6b-f4ff-49c8-a177-647a485fbbf5').
narrative_ontology:cs_kernel_codification('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', distributed).
narrative_ontology:cs_authority_grounding('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', distributed).
narrative_ontology:cs_reading_relation('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', foundational, internalization_precondition_for_displacement).
narrative_ontology:cs_axiom_status(internalization_precondition_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', internalization_precondition_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', secondary, decree_legitimacy_void_without_uptake).
narrative_ontology:cs_axiom_status(decree_legitimacy_void_without_uptake, holdable).
narrative_ontology:cs_axiom_grounding('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', decree_legitimacy_void_without_uptake, conventional).
narrative_ontology:cs_reference_frame('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', internalization_precondition_frame).
narrative_ontology:cs_drift_state('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', contemporary_comparative_record, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fc07fd6b-f4ff-49c8-a177-647a485fbbf5', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_practice_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adapters).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_program).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central reform state: ministries, drafting committees, and the political leadership behind the calendar and dress statutes. It legislated new timekeeping and dress from above, appropriated enforcement budgets, and set a consolidation timetable that assumed practice would follow statute within a decade. Across the reform decades its decrees produced observable public conformity while religious and domestic life continued on the old calendar; successive campaigns re-tightened penalties, blamed provincial laxity, and quietly extended deadlines. By the interval's end the statutes remained on the books, enforcement was dormant, and the original timetable had slipped past every planning horizon. Renouncing the project outright was politically unavailable because the regime's identity was staked on modernization, so it absorbed the costs of repeated failure without a formal exit.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_program, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_program, payer).

% The salaried enforcement layer: municipal inspectors, police details, and local registrars tasked with checking dress in streets and markets and recording calendar use in offices. They measured success in citations issued and hats removed, counts that rose even as private observance continued unchanged. Career advancement depended on reporting compliance, so reports converged on performance. As central attention faded, inspections thinned to token rounds; officers shifted back to routine policing and the citation apparatus became ceremonial.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, provincial_inspection_corps, agenda_setter,
    institutional, biographical, constrained, national).

% Rural and small-town communities whose shared life ran on the lunar-religious calendar and inherited dress. Under the statutes they complied where inspection reached, in offices, markets, and uniforms, and continued prior practice at home, in worship, and in ritual timekeeping, sustaining a dual rhythm maintained by households and congregations. Their internal networks transmitted the old practice across generations without needing the state's permission; over the reform decades nothing essential was surrendered. Some members, especially the young, chafed at the communal hold and left for cities; the communities themselves remained the unit through which time and dress retained their meanings.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_practice_communities, beneficiary,
    organized, generational, constrained, national).

% Clergy and custodial elites of the retained practices. They dated festivals, taught the old calendar, and defined proper dress within worship; every state campaign that failed raised their standing as guardians of what decree could not touch. They lost adherents to urban migration and voluntary assimilation but never lost control of ritual time. Their authority was inseparable from the practices they kept, and they treated each renewed government campaign as confirmation of their indispensability.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, religious_authorities, beneficiary,
    organized, civilizational, identity_locked, national).

% Merchants, civil servants, students, and urban professionals who took up the new calendar and dress through city fashion, workplace requirement, and prestige imitation rather than penalty. Adoption spread along trade routes and university networks at a pace no inspector commanded; many kept the old calendar for family rites while wearing the new dress to work. They bore the cost of doubled wardrobes and double timekeeping and collected the professional and social returns of appearing modern.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adapters, beneficiary,
    moderate, biographical, mobile, regional).

% Younger community members who wanted the new calendar and dress without reservation and found their own households and congregations holding them to the old ones. They faced communal sanction, including gossip, marriage barriers, and exclusion from ritual roles, that was sharper than anything the state's inspectors applied. Many answered with geographic exit: boarding school, city employment, migration. Neither the state's compliance statistics nor the communities' continuity narratives recorded their objection.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, assimilation_seeking_youth, excluded,
    powerless, immediate, mobile, regional).

% Historians and social researchers who assembled the cross-case record: calendar reforms that failed in one state after another, dress reforms that achieved street-level conformity while leaving kitchen and sanctuary untouched, and the rare shallow cases where decree did move behavior. Working from archives, memoirs, and village studies, they established the pattern this reading generalizes and catalogued its exceptions. They hold no stake in either the communities' continuity or the state's timetable.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, comparative_modernization_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the coupling between practiced commitment and the institutions that transmit it: shared timekeeping and dress continue to coordinate worship, kinship, and communal life because they are carried by internalized transmitters (clergy, households, congregations) rather than by statute. The requirement filters displacement attempts so that only practices with living carriers survive institutional shocks.
% TRANSFER_FUNCTION: Moves enforcement expenditure and political capital from the modernizing state into compliance-performance: observable conformity without corresponding transfer of practice loyalty. Moves legitimacy and continuity toward the communities and their custodial elites. The transfer the statutes aimed at, population-wide adoption of the new calendar and dress as internal commitment, never completes.
% ABSENT_VOICES: Assimilation-seeking youth inside the traditional communities, who wanted full adoption of the new practices and experienced communal retention as a barrier to their mobility, are absent from both the state's statistics and the autonomy-preservation narrative. Also under-recorded: ordinary practitioners whose private retention reflected lack of alternatives or habit rather than committed resistance, and whose voices would complicate both the victory narrative of the communities and the failure narrative of the state.
% DISAPPEARANCE_RATIONALE: If the internalization requirement ceased to bind, decree-based displacement would succeed wherever enforcement reached: minority and religious practice continuity would depend wholly on state tolerance, the recurring pattern of failed calendar and dress impositions would vanish from the comparative record, and custodial elites would lose the standing that accrues to guardians of what statute cannot touch. State cultural engineering would become a reliable instrument rather than a repeated embarrassment.
% FOUNDING_PROBLEM: State builders consolidating modern nationhood needed rapid standardization of population practice, including timekeeping, dress, and administrative habits, to run unified economies, armies, and bureaucracies. Imposed displacement by decree was the proposed instrument, and its recurring failure across cases posed the problem this reading answers: why legal mandate does not convert into lived practice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the community-beneficiary set by comparative historical sociology of calendar and dress reform across multiple states, and, notably, by the losing party's own records: state archives and retired officials' memoirs documenting enforcement expenditure against non-displacement outcomes. Community advocates attest the same pattern but from a benefiting seat; the modernizers' own concessions carry the independent weight.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legitimacy_of_imposed_practice__endogenous_climb_reading),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55 is the standing level the requirement imposes on any displacement attempt: the modernizer seat pays repeatedly for decrees that purchase compliance-performance and no internalization, while community seats are shielded rather than taxed. The measurement series traces the historical episode on one shared grid (1925-1975, every tracked metric authored at every point): realized extraction declines from 0.78 to 0.38 as enforcement decays and dual practice stabilizes into equilibrium; theater_ratio rises from 0.30 to 0.70 as the displacement regime's activity becomes performative (citation counts and official modernity displays substituting for the displacement the statutes named, textbook Goodhart drift); suppression_requirement falls from 0.80 to 0.22 as the enforcement machinery is built up, matures, and is quietly dismantled, an enforcement-decay trajectory rather than a ratchet. The scalar theater_ratio (0.20) describes the requirement itself, whose operation is real rather than staged; the rising series describes the referent regime's descent into ceremony, which is the story's central temporal finding. Suppression 0.30 is authored as a raw structural property, unscaled by power or scope: the requirement forecloses the decree path entirely but leaves the cultivation path (schooling, generational turnover, urban diffusion) open, slow, and only partly steerable, hence accessibility_collapse 0.55 rather than mountain-typical near-total collapse. Resistance 0.65 is high because the requirement's principal opponent is the modernizer coalition's refusal to accept the verdict: renewed campaigns, statistical denial, and blame-shifting onto provincial laxity. Boltzmann typing is identity_coordination: the function protected is identity-bounded practice (ritual timekeeping, dress as membership), with the voluntariness omega flagging the cover-story risk.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently, and the structural data explains why. From the state_modernization_program seat the requirement is an implacable ceiling: every escalation of enforcement produces the identical outcome, so the same structure reads as a hostile fixed limit on state capacity. From the traditional_practice_communities and religious_authorities seats the same structure is a floor: a guarantee that continuity survives statutory hostility, experienced not as a limit but as the normal condition of practice. The urban_adapters seat splits the difference, treating the requirement as background weather they navigate through mobility. Provincial inspectors experience yet another version: a mission whose success metric detached from its goal mid-career. The engine computes these per-seat classifications from power, exit, and role data; this story authors the structure, not the verdicts.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain runs from the beneficiary/victim declarations: traditional_practice_communities, religious_authorities, and urban_adapters are declared beneficiaries and derive low directionality (shielded, subsidized positions); state_modernization_program is the declared victim and derives high directionality. Two overrides correct places where the derivation would misread the structure. First, the state program: its agenda_setter status would drag its derived directionality toward the setter end, but its realized position is near-full target, it funds every failed attempt and collects no displacement, so the override sets institutional d to 0.8. Second, urban_adapters: declared beneficiary but materially mixed, they bore real adoption costs and constitute the very diffusion channel the state's timeline depends on, sitting nearer symmetric than shielded; the override sets moderate d to 0.4. Religious authorities warrant no override: identity_locked exit and civilizational horizon place them firmly at the beneficiary pole by derivation. Suppression enters the engine's arithmetic as a raw structural input, unscaled; only extractiveness is scaled by directionality and spatial scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Reading the requirement as pure mountain launders community benefit into naturality: the beneficiaries are declared precisely so the false-summit signature evaluates whether a benefiting coalition has theorized its protection into a law of social change, with the naturalness omega keeping that question open rather than pre-answered. Reading it as pure snare erases the genuine coordination function: transmission-coupled practice continuity is a real collective good that the requirement protects, and the identity_coordination typing with its conservative floor keeps that function on the books rather than letting relational framing excuse uncaptured extraction. On obsolescence: the founding problem (rapid cultural consolidation for state formation) is contested rather than dead, and the requirement's checking function reactivates whenever any state attempts displacement, so no sunset applies and mandatrophy is not resolved. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no zombie flag: the arrangement persists because the problem recurs, not because a corpse is being performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_regularity,
    'Is the internalization requirement a structural constant of social change, or a historically contingent pattern that autonomy-preserving communities and their custodial elites have theorized into inevitability?',
    'Systematic comparison across displacement attempts varying practice depth: shallow logistical practices (driving side, measurement units, currency) displaced by decree in prepared campaigns versus identity-laden practices (worship calendars, dress meaning) resistant across enforcement intensities. If depth rather than enforcement dose predicts outcome, the regularity behaves as structural.',
    'If structural, the mountain claim stands and community benefit is incidental to the mechanism; if contingent, the constraint is better read as a defended settlement benefiting incumbent practice-holders, and the classification shifts toward tangled_rope through the false-summit chain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_regularity, empirical, 'Whether practice-displacement failure reflects a natural limit on decree or a benefiting coalition''s constructed doctrine.').

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is the endogenous_climb_reading of the legitimacy_of_imposed_practice kernel: is the disagreement with the exogenous_override_reading located in the empirical record (whether decree ever suffices) or in the definition of displacement (observable compliance versus internalized commitment)?',
    'Operationalize displacement on both behavioral and attitudinal measures and re-adjudicate the cross-case record under each definition; test whether the readings'' verdicts converge once the measure is fixed.',
    'Under a behavioral definition the exogenous_override_reading gains ground, since prepared decree campaigns demonstrably move behavior; under an attitudinal definition the endogenous_climb_reading dominates. This story''s classification is stable either way, but the family''s epsilon ordering flips.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Where the kernel contest actually sits: in the evidence or in the definition of displacement.').

omega_variable(
    private_retention_meaning,
    'Does private retention of lunar timekeeping and inherited dress signal failed internalization, or a completed public/private settlement in which the state''s sphere was successfully transformed while the domestic sphere was conceded?',
    'Archival and oral-history work on how private practitioners interpreted their dual practice (resistance, habit, or compartmentalization), plus decay analysis of whether private observance eroded on its own once enforcement stopped.',
    'If compartmentalization, displacement partially succeeded and the state_modernization_program''s victimhood is overstated; effective extraction on the state seat falls and this reading''s verdict softens toward partial success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_retention_meaning, empirical, 'Whether private retention evidences defeat of the reform or its partial completion.').

omega_variable(
    enforcement_dose_confound,
    'Did displacement fail because internalization is required, or because enforcement dose was never sufficient, the confound the exogenous_override_reading exploits?',
    'Dose-response comparison across states and campaigns: relate enforcement intensity and duration to displacement outcomes, controlling for practice depth, urbanization, and regime stability.',
    'If a dose-response gradient exists, the requirement''s verdict is conditional rather than absolute and the hybrid and exogenous readings gain; if outcomes are flat across doses, the necessity claim strengthens and the mountain reading firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dose_confound, empirical, 'Enforcement-dose confound in the failure record.').

omega_variable(
    communal_retention_voluntariness,
    'Was retained practice within the communities voluntarily maintained, or enforced by communal sanction against members, especially the young, who wanted assimilation?',
    'Document sanction experiences of would-be assimilators (marriage barriers, ritual exclusion, gossip regimes) and compare exit and assimilation rates where communal sanction weakened.',
    'If retention is communally coerced, the beneficiary seat splits: communities protect some members while constraining others, excess extraction appears on the communal seat, and the identity_coordination typing risks laundering coercion as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_retention_voluntariness, empirical, 'Voluntariness of communal practice retention behind the autonomy-preservation benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 1925, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1925, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1925, 0.3).
narrative_ontology:measurement(legi_tr_t1935, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1935, 0.42).
narrative_ontology:measurement(legi_tr_t1945, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1945, 0.5).
narrative_ontology:measurement(legi_tr_t1955, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1955, 0.58).
narrative_ontology:measurement(legi_tr_t1965, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1965, 0.64).
narrative_ontology:measurement(legi_tr_t1975, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1975, 0.7).

% Extraction over time
narrative_ontology:measurement(legi_be_t1925, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1925, 0.78).
narrative_ontology:measurement(legi_be_t1935, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1935, 0.74).
narrative_ontology:measurement(legi_be_t1945, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1945, 0.66).
narrative_ontology:measurement(legi_be_t1955, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1955, 0.56).
narrative_ontology:measurement(legi_be_t1965, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1965, 0.46).
narrative_ontology:measurement(legi_be_t1975, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1975, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1925, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1925, 0.8).
narrative_ontology:measurement(legi_su_t1935, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1935, 0.76).
narrative_ontology:measurement(legi_su_t1945, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(legi_su_t1955, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1955, 0.48).
narrative_ontology:measurement(legi_su_t1965, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1965, 0.32).
narrative_ontology:measurement(legi_su_t1975, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1975, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'imposed practice reform' covers three structurally distinct claims about the same standing arrangement (decree-based displacement machinery). This story (endogenous_climb_reading) authors the internalization-necessity constraint with beneficiaries at the community seats and the modernization timeline at the target seat. The exogenous_override_reading authors the decree-sufficiency constraint, which inverts the beneficiary/victim structure (state benefits, communities bear displacement). The hybrid_scaffolding_reading authors the scaffolded-imposition constraint with partial displacement on both sides. The upstream/downstream structure runs from the shared empirical record: each reading cites the same calendar and dress episodes as evidence, so contamination propagates across the family if the underlying case archive is revised. Epsilon differs across the family by construction (OQ-26): same referent, reading-indexed values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, institutional, 0.8).
constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, moderate, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
