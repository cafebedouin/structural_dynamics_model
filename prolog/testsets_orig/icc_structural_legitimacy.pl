% ============================================================================
% CONSTRAINT STORY: icc_structural_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_icc_structural_legitimacy, []).

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
 *   constraint_id: icc_structural_legitimacy
 *   human_readable: ICC Structural Legitimacy and Selective Accountability
 *   domain: international_law/governance
 *
 * SUMMARY:
 *   The International Criminal Court presents a constraint that combines
 *   genuine coordination benefits (reducing interstate war through
 *   prosecutorial threat) with asymmetric extraction (selective
 *   accountability protecting powerful states). The structural mechanism is
 *   the UN Security Council veto: permanent members can block investigations
 *   into their own nationals or allies, while investigations into
 *   weaker-state perpetrators proceed unchallenged. This creates a
 *   multilayered extraction system where legitimacy is maintained through
 *   performative high-profile prosecutions (African warlords, regional
 *   strongmen) while structural asymmetry remains opaque. The constraint
 *   exemplifies how international institutions can coordinate important
 *   functions (deterring interstate conflict) while embedding selective
 *   extraction (asymmetric accountability). Theater ratio has risen from 0.45
 *   to 0.68 over the Court's 24-year history as prosecutorial activity has
 *   become increasingly decoupled from the institution's stated universal
 *   justice mission. Extractiveness has risen correspondingly as the
 *   asymmetry has become more visible without institutional adaptation. The
 *   constraint exhibits all six classification types across different
 *   observer positions, making it a diagnostic exemplar for how power
 *   asymmetry maps onto legitimacy perception.
 *
 * KEY AGENTS:
 *   - Weaker States: Primary victims (powerless/trapped) — face asymmetric prosecution risk; cannot veto unfavorable referrals; exit through withdrawal damages regional legitimacy and diplomatic standing
 *   - Mid-Tier Regional Powers: Secondary victims/beneficiaries (moderate/constrained) — benefit from ICC deterrence framework for neighbors; constrained by prosecution risk for own nationals; can exit at high cost
 *   - Permanent Security Council Members (USA, UK, France, Russia, China): Primary beneficiaries (institutional/arbitrage) — veto blocks investigations into own actions; set prosecution agenda through referral control; can withdraw or block costlessly
 *   - Global Justice Coalition: Organized agents (organized/constrained) — civil society, human rights organizations, prosecutorial networks; see ICC as temporary framework being superseded by hybrid and universal jurisdiction courts
 *   - The International Criminal Court Itself: Institutional actor (institutional/mobile) — maintains legitimacy through performative prosecution; faces reputational risk from selective accountability visibility; maintains theater through procedural complexity narratives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing power asymmetry as inherent to international law rather than contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(icc_structural_legitimacy, 0.58).
domain_priors:suppression_score(icc_structural_legitimacy, 0.62).
domain_priors:theater_ratio(icc_structural_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(icc_structural_legitimacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(icc_structural_legitimacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(icc_structural_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(icc_structural_legitimacy, tangled_rope).
narrative_ontology:human_readable(icc_structural_legitimacy, "ICC Structural Legitimacy and Selective Accountability").
narrative_ontology:topic_domain(icc_structural_legitimacy, "international_law/governance").

domain_priors:requires_active_enforcement(icc_structural_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(icc_structural_legitimacy, permanent_security_council_states).
narrative_ontology:constraint_beneficiary(icc_structural_legitimacy, powerful_nations).
narrative_ontology:constraint_victim(icc_structural_legitimacy, weaker_states).
narrative_ontology:constraint_victim(icc_structural_legitimacy, global_justice_constituency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAKER STATES (SNARE) — Nations without permanent UNSC representation face asymmetric accountability. They cannot veto ICC investigations or referrals. When their nationals commit war crimes, prosecution proceeds; when powerful nations commit identical acts, Security Council protection blocks investigation. Exit options are nonexistent — membership obligation is binding; withdrawal triggers diplomatic isolation. Maximum experienced extraction with high suppression.
constraint_indexing:constraint_classification(icc_structural_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER STATES (TANGLED ROPE) — Regional powers face genuine coordination benefit from ICC jurisdiction (predictable accountability framework for interstate disputes, deterrence against warlord prosecution of neighbors) alongside asymmetric extraction (nationals can face prosecution if conflict with powerful state allies). Constrained exit — leaving the ICC damages regional legitimacy but is materially feasible.
constraint_indexing:constraint_classification(icc_structural_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PERM-5 STATES (ROPE) — Benefit from selective accountability. UNSC veto blocks investigations into their actions; they set prosecution agenda for competitors. Coordination function: predictable legal framework reduces interstate war risk through threat of prosecution for enemies. Pure beneficiary with arbitrage exit — can withdraw or block referrals costlessly.
constraint_indexing:constraint_classification(icc_structural_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL JUSTICE COALITION (SCAFFOLD) — Organized civil society (Human Rights Watch, Amnesty, prosecutorial networks) sees ICC as a temporary institutional framework with a sunset: universal jurisdiction expansion and hybrid courts in national systems are building decentralized accountability pathways. Current extraction is tolerated because the coalition perceives an exit path and generational endpoint for ICC-dependent accountability.
constraint_indexing:constraint_classification(icc_structural_legitimacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ICC INSTITUTIONAL THEATER (PITON) — The Court maintains legitimacy through performative international law while core function (universal accountability) remains unrealized. High-profile prosecutions of African warlords and weaker-state nationals create appearance of universal justice; blocking of investigations into powerful states is presented as procedural complexity rather than structural asymmetry. Theater ratio reflects performative legitimacy maintenance despite degraded functional capacity. Institutional inertia sustains the Court past the point where its stated mission is achievable under current structure.
constraint_indexing:constraint_classification(icc_structural_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, ICC asymmetry appears as an immutable feature of international law: powerful states cannot be subordinated to external judges; sovereignty prevents universal enforcement. This perspective naturalizes power asymmetry as inherent to the international system. However, structural data contradicts this — the asymmetry is contingent on UNSC veto design and ratification politics, not on irreducible features of international relations. Engine false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(icc_structural_legitimacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(icc_structural_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(icc_structural_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(icc_structural_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(icc_structural_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(icc_structural_legitimacy, TR),
    TR >= 0.70.

:- end_tests(icc_structural_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The ICC extracts legitimacy and resources from weaker states while providing selective protection to powerful states. However, extraction is not maximal because genuine coordination benefits exist — the threat of prosecution does reduce interstate war risk, and even weaker states benefit from predictable legal frameworks for interstate disputes. The extraction is embedded in coordination, not pure overhead. Suppression (0.62): Moderate-high. Weaker states cannot exit (diplomatic/geopolitical costs are insurmountable), cannot block referrals (UNSC veto is exclusive to five states), and cannot appeal prosecutorial decisions (ICC has final authority). Suppression reflects structural entrapment within the system, but suppression is not total because some weaker states can contest investigations through procedural channels. Theater ratio (0.68): High and rising. ICC prosecutorial activity is increasingly visible (high-profile trials of regional figures) while structural asymmetry (protection of powerful-state actors) remains procedurally opaque. The theater serves legitimacy maintenance — the appearance of universal justice without functional universality. Theater ratio has risen as the asymmetry has become more visible without institutional redesign, forcing increased performative justification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a fundamental legitimacy divergence: powerful states perceive the ICC as rope (coordination mechanism for reducing interstate war), while weaker states perceive it as snare (asymmetric prosecution risk with no escape). The global justice coalition perceives it as scaffold (temporary institution being superseded by hybrid courts), while the ICC itself maintains piton performance (theater-driven legitimacy without functional universality). The analytical observer risks naturalizing this asymmetry as mountain (inherent to international relations), but the structural data reveals it as chosen design (UNSC veto, ratification selectivity, prosecutorial discretion). The perspectival gap between beneficiary and victim is maximal: identical institutional structures are experienced as pure coordination by protected states and pure extraction by unprotected states. This gap is the engine's diagnostic signal that extraction is embedded in the institution's core design, not incidental.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each agent's structural position relative to extraction flow. Permanent Security Council members are pure beneficiaries: they block investigations into their actions (d ≈ 0.08 → negative f(d)) while prosecuting rivals (they set extraction agenda). Weaker states are pure targets: they cannot veto unfavorable referrals (d ≈ 0.95 → high f(d)) and face asymmetric prosecution risk (maximum experienced extraction). Mid-tier states are mixed: they benefit from interstate deterrence (d ≈ 0.45) but face prosecution risk for own nationals (d partially offsets benefit). The global justice coalition has high organizational power but constrained exit (they perceive a scaffold sunset through hybrid court expansion but cannot immediately exit). The analytical observer with its mountain perspective assumes power asymmetry is unchangeable, but the structural data shows the asymmetry is contingent on UNSC veto design — revealing the false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The ICC exhibits all six types across perspectives, but the analytical tension is between rope (pure coordination for powerful states) and snare (pure extraction for weaker states). The tangled rope classification resolves this by showing that the institution simultaneously coordinates (reduces interstate war through prosecutorial threat) and extracts (asymmetric accountability). Both functions are real and embedded in the same structural mechanism (UNSC veto enables both deterrence and protection). The constraint is not a false positive (mistaken rope) because weaker states genuinely pay extraction costs; it is not a snare for everyone because powerful states experience genuine coordination benefits. The mandatrophy is resolved by showing that the constraint's type depends on observer position — tangled rope at the analytical level captures that both rope and snare functions coexist in the same institution. The false mountain perspective (natural law view) is flagged by the engine as naturalization of a contingent design choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_redesign_feasibility,
    'Is UNSC veto over ICC referrals a structural immutability or a contingent design choice reversible through diplomatic change?',
    'Historical analysis of UNSC veto usage patterns; feasibility studies of veto reform proposals; comparison with other multilateral institutions'' governance structures',
    'If immutable: ICC asymmetry is mountain-adjacent, legitimacy gap is inherent. If reversible: entire classification shifts toward snare/tangled_rope — the asymmetry is chosen extraction, not structural necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_redesign_feasibility, conceptual, 'Whether UNSC veto design is immutable or contingently chosen').

omega_variable(
    performance_vs_function_boundary,
    'At what prosecution rate does ICC performance (high-profile trials) become separable from function (deterring war crimes across all belligerents)?',
    'Comparative analysis of prosecution rates by perpetrator nationality; deterrence studies correlating ICC prosecution presence with conflict behavior in weaker vs powerful states',
    'If performance > 60% of observable activity: piton classification is robust (theater-driven). If performance < 30%: function is real despite asymmetry, piton drops to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_function_boundary, empirical, 'Ratio of performative to functional ICC activity').

omega_variable(
    hybrid_court_capacity_substitution,
    'Can hybrid and universal jurisdiction courts credibly replace ICC prosecution capacity for conflicts involving powerful-state actors?',
    'Jurisdictional analysis of hybrid courts (Cambodia, Lebanon, Sierra Leone); enforcement capacity studies; correlation of powerful-state prosecution success in national vs international forums',
    'If credible: scaffold sunset is real, justice coalition''s exit path is functional. If not credible: coalition is aspirational, constraint persists indefinitely at high extraction levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_court_capacity_substitution, empirical, 'Whether hybrid courts provide credible ICC substitutes').

omega_variable(
    legitimacy_perception_divergence,
    'Do weaker states experience ICC selectivity as delegitimizing the institution or as normalizing asymmetric international law?',
    'Survey data on ICC legitimacy perception by state power ranking; exit behavior analysis (withdrawals, non-cooperation); domestic litigation over ICC cooperation ratification',
    'If delegitimizing: constraint may collapse through reputational failure (mountain-to-snare reversal). If normalizing: asymmetry becomes self-sustaining institutional feature (piton-to-rope drift).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_perception_divergence, empirical, 'Legitimacy perception divergence between powerful and weaker states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(icc_structural_legitimacy, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icc_tr_t0, icc_structural_legitimacy, theater_ratio, 0, 0.45).
narrative_ontology:measurement(icc_tr_t8, icc_structural_legitimacy, theater_ratio, 8, 0.58).
narrative_ontology:measurement(icc_tr_t16, icc_structural_legitimacy, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(icc_be_t0, icc_structural_legitimacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(icc_be_t8, icc_structural_legitimacy, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(icc_be_t16, icc_structural_legitimacy, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(icc_structural_legitimacy, enforcement_mechanism).
narrative_ontology:affects_constraint(icc_structural_legitimacy, unsc_structural_veto).
narrative_ontology:affects_constraint(icc_structural_legitimacy, universal_jurisdiction_expansion).

% DUAL FORMULATION NOTE:
% ICC structural legitimacy is downstream of UNSC veto design (which determines prosecution selectivity) and upstream of universal jurisdiction expansion (which represents the justice coalition's perceived scaffold sunset). The three constraints form a family: UNSC veto (institutional constraint on referral authority), ICC selectivity (extraction constraint derived from veto design), universal jurisdiction (institutional alternative with lower extraction). Each has its own epsilon value reflecting different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(icc_structural_legitimacy, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
