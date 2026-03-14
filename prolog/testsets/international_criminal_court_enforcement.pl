% ============================================================================
% CONSTRAINT STORY: international_criminal_court_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_criminal_court_enforcement, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: international_criminal_court_enforcement
 *   human_readable: International Criminal Court Enforcement Asymmetry
 *   domain: international_law/geopolitics
 *
 * SUMMARY:
 *   The International Criminal Court enforcement regime exhibits a structural
 *   asymmetry: the Court possesses authority to investigate and prosecute
 *   crimes against humanity across a global jurisdiction, yet enforcement
 *   capacity is radically constrained by state sovereignty, geopolitical
 *   protection of powerful nations, and resource limitations. This creates a
 *   tangled hybrid between genuine coordination (solving collective action
 *   problems of transnational accountability) and extraction (selective
 *   justice that favors geopolitically aligned states while leaving
 *   weak-state victims dependent on ICC discretion). The constraint
 *   demonstrates how international law institutions can simultaneously expand
 *   justice access while concentrating enforcement power in ways that
 *   reproduce global inequality. The theater ratio (0.65) reflects that ICC
 *   prosecution announcements often precede enforcement probability —
 *   indictments become symbolic outputs while actual conviction rates remain
 *   low. Theater has increased over the interval as the Court's enforcement
 *   gap has widened relative to its prosecutorial ambitions.
 *
 * KEY AGENTS:
 *   - Geopolitically Powerful Nations: Primary beneficiaries (institutional/arbitrage) — maintain sovereignty protection while selectively using ICC to prosecute rival factions. Veto power through Security Council and informal pressure over investigations.
 *   - Weak State Nationals: Primary victims (powerless/trapped) — depend entirely on ICC investigative selectivity for justice access; cannot exit or establish alternative accountability mechanisms.
 *   - ICC Institutional Authority: Institutional beneficiary (institutional/constrained) — maintains mandate and legitimacy from enforcement operations, but constrained by state cooperation requirements and resource limitations.
 *   - International Justice NGOs and Civil Society: Moderate victims (moderate/constrained) — provide case documentation and victim support but bear disproportionate burden of investigation groundwork while ICC retains prosecutorial discretion.
 *   - Weak State Governments: Intermediate victims (moderate/trapped) — nationals seek justice through ICC but governments face retaliation risk from accused allies or pressure from powerful state patrons.
 *   - Alternative Justice Mechanisms: Organized exit builders (organized/mobile) — regional courts, universal jurisdiction, transitional justice mechanisms represent structural alternatives that reduce ICC monopoly over international accountability.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing enforcement asymmetry as inevitable property of international law rather than recognizing it as contingent on specific power distributions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_criminal_court_enforcement, 0.58).
domain_priors:suppression_score(international_criminal_court_enforcement, 0.72).
domain_priors:theater_ratio(international_criminal_court_enforcement, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_criminal_court_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_criminal_court_enforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(international_criminal_court_enforcement, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_criminal_court_enforcement, tangled_rope).
narrative_ontology:human_readable(international_criminal_court_enforcement, "International Criminal Court Enforcement Asymmetry").
narrative_ontology:topic_domain(international_criminal_court_enforcement, "international_law/geopolitics").

domain_priors:requires_active_enforcement(international_criminal_court_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_criminal_court_enforcement, geopolitically_powerful_nations).
narrative_ontology:constraint_beneficiary(international_criminal_court_enforcement, icc_institutional_authority).
narrative_ontology:constraint_victim(international_criminal_court_enforcement, weak_state_nationals).
narrative_ontology:constraint_victim(international_criminal_court_enforcement, international_justice_accessibility).
narrative_ontology:constraint_victim(international_criminal_court_enforcement, non_signatory_jurisdictions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIMS FROM WEAK STATES (SNARE) — Nationals of states with weak enforcement capacity or international isolation face asymmetric access to ICC justice. The constraint traps both perpetrators and victims: weak-state nationals seeking justice depend entirely on ICC willingness to investigate their cases, while perpetrators from powerful states routinely escape prosecution through state immunity claims and geopolitical protection. Zero degrees of freedom for the trapped agent — ICC serves as selective justice mechanism.
constraint_indexing:constraint_classification(international_criminal_court_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL JUSTICE APPARATUS (TANGLED ROPE) — Non-state actors (NGOs, advocacy groups, truth commissions) coordinate accountability efforts and genuinely benefit from ICC existence while simultaneously bearing disproportionate burden of case documentation and victim support. Mixed coordination (providing evidence, supporting witnesses) and extraction (ICC selectivity creates cascading investigation workload for civil society).
constraint_indexing:constraint_classification(international_criminal_court_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GEOPOLITICALLY POWERFUL NATIONS (ROPE) — Institutional actors (US, Russia, China, permanent Security Council members) experience ICC enforcement as coordination mechanism without effective extraction: they have structural veto power through state sovereignty and Security Council dynamics. Enforcement asymmetry functions as coordination that solves collective action problems (prosecuting 'enemy' faction leaders) while protecting beneficiary nations through selective investigation patterns.
constraint_indexing:constraint_classification(international_criminal_court_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING INTERNATIONAL JUSTICE ARCHITECTURE (SCAFFOLD) — Regional courts (African Union, European, Latin American), national courts applying universal jurisdiction, and transitional justice mechanisms represent alternative accountability pathways. These are building exit routes from ICC dependence with sunset characteristics: as complementarity strengthens (national courts assume primary role), ICC enforcement bottleneck loses extractive power. Organized agents see low effective extraction because exit options are crystallizing.
constraint_indexing:constraint_classification(international_criminal_court_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ICC INSTITUTIONAL MAINTENANCE (PITON) — The Court itself maintains performative legitimacy rituals (high-profile indictments, symbolic trials) despite structural constraints that prevent enforcement against protected states. Theater ratio (0.65) reflects that ICC prosecution announcements often precede realistic enforcement probability — the indictment is the functional output, not conviction. Institutional inertia preserves the Court despite recognition that its enforcement mechanism is partially degraded.
constraint_indexing:constraint_classification(international_criminal_court_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY CONSTRAINT (MOUNTAIN) — From a civilizational perspective, international enforcement faces an immutable structural problem: any supranational enforcement mechanism must either (a) gain authority from state delegations (creating security dilemma where states limit enforcement against allies) or (b) operate without state authority (losing legitimacy and enforcement capacity). This appears as an unchangeable feature of international law. However, the engine may identify this as false naturalization — enforcement asymmetry is contingent on specific power distributions, not inherent to sovereignty itself.
constraint_indexing:constraint_classification(international_criminal_court_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_criminal_court_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_criminal_court_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_criminal_court_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_criminal_court_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_criminal_court_enforcement, TR),
    TR >= 0.70.

:- end_tests(international_criminal_court_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ICC creates genuine coordination benefits (collective accountability for transnational crimes) but the structure of enforcement systematically favors geopolitically powerful states while imposing asymmetric costs on weak-state nationals who are trapped within the ICC's discretionary jurisdiction. The extractiveness reflects this hybrid: coordination exists but is captured and weaponized. Over the 16-year interval, extractiveness increased from 0.42 to 0.58 as geopolitical polarization intensified (permanent Security Council members blocking investigations into allied states, withdrawal of major powers threatening the institution). Suppression (0.72): High. Multiple mechanisms suppress alternatives: states without extradition treaties with other ICC signatories cannot access alternative justice pathways; weak states lack resources for independent transitional justice; universal jurisdiction (alternative to ICC) faces legal and political barriers; victims in non-signatory jurisdictions are entirely dependent on ICC discretion. The suppression reflects structural barriers to exit, not just ICC-imposed constraints. Theater ratio (0.65): High and rising. ICC indictments function as symbolic outputs — announcement of prosecution against protected state nationals rarely precedes actual enforcement. The Court maintains legitimacy through prosecution announcements even when conviction probability is low. Theater has risen over the interval as the enforcement gap between indictment and conviction has widened, and as symbolic prosecution has become the primary mechanism through which ICC signals alignment with international norms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a maximal perspectival gap. The powerful nation sees coordination (Rope) — ICC serves their interests by prosecuting rival faction leaders while protecting their own nationals through de facto immunity. Weak-state victims see pure extraction (Snare) — they depend entirely on ICC discretion with no exit options. International justice NGOs see mixed effects (Tangled Rope) — they coordinate with ICC while bearing burden of documentation work, yet benefit from access to transnational accountability. Regional and alternative courts see a temporary problem being solved (Scaffold) — universal jurisdiction, African Union courts, and transitional justice mechanisms are building exit routes that reduce ICC monopoly. The ICC institution itself recognizes its own degradation (Piton) — the Court maintains performative legitimacy rituals despite recognizing that enforcement against protected states is structurally impossible. The analytical observer risks false naturalization (Mountain) — interpreting the enforcement asymmetry as inherent to sovereignty rather than recognizing it as contingent on power distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to extraction flow. Powerful nations have arbitrage options (exit to alternative accountability mechanisms, Security Council veto, diplomatic immunity) — low d value, beneficiary status. Weak-state nationals have zero exit options (trapped) and victim status — high d value (approaching 1.0). ICC institutional actors have constrained exit (dependent on state cooperation) but beneficiary status through mandate authority — moderate d. NGOs have constrained exit (dependent on ICC access for case investigation) but also provide essential inputs — moderate-high d with victim partial-status. Regional courts represent exit optionality for weak states — mobile exit options raise their d downward from the trapped baseline. The gap between powerful-nation and weak-state d values is maximal for this constraint type.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the hybrid classification (Tangled Rope) reflects genuine structural properties rather than ambiguity. The coordination function is real — ICC solves collective action problems around transnational accountability. The extraction is also real — enforcement asymmetry concentrates benefits on geopolitically aligned states while imposing costs on weak-state victims. The mandatrophy is avoided by recognizing that international institutions can simultaneously expand justice access (genuine coordination good) while concentrating enforcement power (extraction mechanism). The false mountain perspective (viewing enforcement asymmetry as inevitable) is revealed as a false summit by noting that other international institutions (regional courts, universal jurisdiction, transitional justice) operate with different enforcement patterns — the asymmetry is not inherent to international law but contingent on specific institutional design choices and power distributions. The piton perspective (ICC theater maintenance) is validated by the rising theater ratio and declining enforcement gap, confirming that symbolic prosecution has become a primary institutional output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_mechanism,
    'Is selective ICC enforcement a structural property of international justice or a contingent outcome of geopolitical power distribution?',
    'Comparative analysis of enforcement patterns across time horizons: if selectivity correlates with power shifts (e.g., rising China/India produces investigation patterns toward Western personnel), it is contingent; if selectivity persists independent of power distribution, it is structural',
    'If contingent: snare classification weakens as power equilibrium changes. If structural: snare persists regardless of geopolitical shifts — international law embeds asymmetry as feature, not bug.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether enforcement selectivity is structural or power-contingent').

omega_variable(
    complementarity_effectiveness,
    'Does the ICC complementarity principle (preference for national courts) actually shift enforcement burden to weak states or strengthen their domestic justice capacity?',
    'Longitudinal study of national court capacity before/after ICC presence; measurement of resource flows to transitional justice mechanisms; tracking of cases shifted from ICC to national prosecution',
    'If strengthens capacity: scaffold perspective validated, sunset is real. If creates burden-shifting: complementarity is extractive mechanism disguised as coordination, snare classification intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_effectiveness, empirical, 'Whether complementarity strengthens national capacity or shifts burden').

omega_variable(
    state_cooperation_incentive_structure,
    'What drives state cooperation with ICC arrest warrants against their own nationals or allies? Is it norm internalization, reputational cost avoidance, or constraint imposed by other state veto power?',
    'Case-level analysis of arrest warrant compliance: correlate with state''s external security dependence, UN voting patterns, and trade relationships; interview state officials on decision drivers',
    'If norm-driven: enforcement legitimacy is higher, snare classification weakens. If reputational/veto-driven: enforcement is extraction mechanism protecting aligned states, snare classification confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_cooperation_incentive_structure, empirical, 'Mechanism driving state cooperation with ICC arrest warrants').

omega_variable(
    performance_legitimacy_gap,
    'Does ICC symbolic prosecution (high-profile indictments with low conviction rates) generate legitimacy that reduces demand for effective enforcement, or does it generate backlash that undermines legitimacy?',
    'Polling/survey of victim populations and international law scholars on ICC legitimacy; correlation between indictment patterns and subsequent victim satisfaction; media analysis of ''justice theater'' perception',
    'If legitimacy-generating: piton classification confirmed — theater sustains institution. If backlash: theater erodes legitimacy, forcing either effectiveness or collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_legitimacy_gap, empirical, 'Whether ICC theater generates or erodes legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_criminal_court_enforcement, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, international_criminal_court_enforcement, theater_ratio, 0, 0.48).
narrative_ontology:measurement(inte_tr_t8, international_criminal_court_enforcement, theater_ratio, 8, 0.58).
narrative_ontology:measurement(inte_tr_t16, international_criminal_court_enforcement, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, international_criminal_court_enforcement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inte_be_t8, international_criminal_court_enforcement, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(inte_be_t16, international_criminal_court_enforcement, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_criminal_court_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(international_criminal_court_enforcement, state_sovereignty_veto).
narrative_ontology:affects_constraint(international_criminal_court_enforcement, universal_jurisdiction_competing_mechanism).
narrative_ontology:affects_constraint(international_criminal_court_enforcement, transnational_crime_accountability).

% DUAL FORMULATION NOTE:
% ICC enforcement is downstream of geopolitical power distributions but represents a distinct structural constraint. The upstream state sovereignty veto and permanent Security Council structure shape what enforcement becomes possible; the ICC enforcement constraint reflects how those upstream structures manifest in international criminal justice outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_criminal_court_enforcement, powerful, 0.08).
constraint_indexing:directionality_override(international_criminal_court_enforcement, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
