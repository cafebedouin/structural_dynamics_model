% ============================================================================
% CONSTRAINT STORY: legislative_supremacy_alternative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legislative_supremacy_alternative, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legislative_supremacy_alternative
 *   human_readable: Legislative Supremacy as Hybrid Coordination-Extraction
 *   domain: political_philosophy/constitutional_governance
 *
 * SUMMARY:
 *   Legislative supremacy — the doctrine that a popularly elected legislature
 *   possesses unlimited authority to make and unmake law — creates a
 *   structural tension between coordination efficiency and minority
 *   protection. From one perspective, supremacy solves the coordination
 *   problem of determining which institution resolves constitutional
 *   disputes; from another, it enables majoritarian extraction at the cost of
 *   constitutional minorities. This constraint exhibits tangled rope
 *   properties at the macro level: genuine coordination function (efficient
 *   collective decision-making) coupled with asymmetric extraction
 *   (majorities overriding minorities without constitutional restraint). The
 *   indexical classification reveals why supremacy generates persistent
 *   political conflict: different agents perceive the same doctrine as either
 *   essential coordination (legislatures) or pure extraction (minorities).
 *   Theater ratio measures the gap between formal democratic procedures and
 *   actual minority protection — as democracies mature, formal protections
 *   accumulate while substantive minority power remains constrained,
 *   increasing the performative content of the supremacy doctrine.
 *
 * KEY AGENTS:
 *   - Constitutional Minorities: Primary victim (powerless/trapped) — ethnic, religious, or political minorities lacking majoritarian support. No exit option within jurisdiction or biological timeframe.
 *   - Majoritarian Legislatures: Primary beneficiary (institutional/arbitrage) — elected bodies capturing supremacy authority. Can arbitrage through constitutional design or exit through federalism.
 *   - Concentrated Interest Groups: Secondary beneficiary (powerful/constrained) — lobbying organizations exploiting majoritarian rule to capture legislative coalitions. Constrained by democratic rotation.
 *   - Constitutional Reform Movements: Organized agents (organized/constrained) — civil rights, federalist, and participatory democracy advocates building alternative institutional pathways with generational timescale.
 *   - Post-Supremacy Institutional Remnants: Institutional actor (institutional/arbitrage) — formal constitutional structures in jurisdictions that nominally limit supremacy but where legislatures maintain dominance through practice.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choice as structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legislative_supremacy_alternative, 0.52).
domain_priors:suppression_score(legislative_supremacy_alternative, 0.65).
domain_priors:theater_ratio(legislative_supremacy_alternative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legislative_supremacy_alternative, extractiveness, 0.52).
narrative_ontology:constraint_metric(legislative_supremacy_alternative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legislative_supremacy_alternative, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legislative_supremacy_alternative, tangled_rope).
narrative_ontology:human_readable(legislative_supremacy_alternative, "Legislative Supremacy as Hybrid Coordination-Extraction").
narrative_ontology:topic_domain(legislative_supremacy_alternative, "political_philosophy/constitutional_governance").

domain_priors:requires_active_enforcement(legislative_supremacy_alternative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legislative_supremacy_alternative, majoritarian_elected_bodies).
narrative_ontology:constraint_beneficiary(legislative_supremacy_alternative, concentrated_interest_groups).
narrative_ontology:constraint_victim(legislative_supremacy_alternative, constitutional_minorities).
narrative_ontology:constraint_victim(legislative_supremacy_alternative, long_term_institutional_checks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL MINORITIES (SNARE) — Trapped within a jurisdiction governed by majoritarian supremacy with no structural exit. Cannot exit the constraint; voting minority has minimal influence. Bears full extraction cost of majoritarian overreach. No organized alternative pathway visible within biographical horizon.
constraint_indexing:constraint_classification(legislative_supremacy_alternative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAJORITARIAN LEGISLATURES (ROPE) — Experiences legislative supremacy as coordination mechanism enabling efficient collective decision-making. Can arbitrage to other jurisdictions or exit coordination through constitutional reform. Benefits from constraint through concentration of decision-making authority. Sees the doctrine as natural and necessary for democratic governance.
constraint_indexing:constraint_classification(legislative_supremacy_alternative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CONCENTRATED INTEREST GROUPS (TANGLED ROPE) — Powerful but constrained by legislative processes and public accountability. Benefit from majoritarian rule through ability to capture legislative majorities. Also face extraction through democratic rotation of power and legislative oversight. Mixed coordination-extraction: leverage legislative process for coordination while extracting via lobbying and rent-seeking.
constraint_indexing:constraint_classification(legislative_supremacy_alternative, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENTS (SCAFFOLD) — Organized agents (civil rights movements, federalist reformers, decentralization advocates) perceive legislative supremacy as a temporary institutional form with sunset logic. See alternative governance structures (constitutional courts, multi-level federalism, deliberative democracy) as replacing pure supremacy. Low effective extraction because organized agents have generational agency and identified alternative pathways.
constraint_indexing:constraint_classification(legislative_supremacy_alternative, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-SUPREMACY INSTITUTIONAL REMNANTS (PITON) — In jurisdictions that have formally abandoned legislative supremacy (EU member states, India's constitutional federalism), the doctrine persists as institutional theater through continued legislative dominance in practice. Formal constitutional limits exist but are substantially performative. Theater derives from the gap between constitutional text and legislative practice. Piton classification: maintains the 'supremacy' frame through inertia despite alternative institutional structures.
constraint_indexing:constraint_classification(legislative_supremacy_alternative, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of supreme decision-making authority is inherent to governance: every system requires a final arbiter of constitutional meaning. This perspective risks naturalizing the specific institutional choice (legislative rather than judicial/executive/mixed) as an immutable feature of political systems. Engine's false summit detector will identify this as naturalization of a contingent choice rather than a structural necessity.
constraint_indexing:constraint_classification(legislative_supremacy_alternative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legislative_supremacy_alternative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legislative_supremacy_alternative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legislative_supremacy_alternative, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legislative_supremacy_alternative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legislative_supremacy_alternative, TR),
    TR >= 0.70.

:- end_tests(legislative_supremacy_alternative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Legislative supremacy generates measurable extraction: majorities override minority preferences without binding constitutional constraint. The extraction is not total because legislatures face political costs to overreach, international norms constrain extreme extraction, and some jurisdictions have developed quasi-constitutional checks. Empirically, extractiveness varies significantly by context — proportional representation systems show lower extractiveness (0.35-0.40) than first-past-the-post majoritarian systems (0.55-0.65). The 0.52 value represents a time-averaged mid-range jurisdiction. Suppression (0.65): Significant. Minorities face structural barriers to exit including citizenship costs, relocation expenses, language/cultural ties, and economic dependency. Supermajority requirements for secession or significant constitutional change create high suppression. Formal voting rights exist but deliver minimal effective power to powerless minorities. Theater ratio (0.58): Moderate. Formal democratic procedures (voting, legislative debate, public hearings) constitute performative elements that create legitimacy claims while majority preference remains dispositive. Over the measurement interval, theater has increased as formal minority protections have been adopted (committee procedures, minority legislative time, constitutional courts) while substantive minority power remains constrained — the gap between procedure and outcome has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival gap: beneficiaries perceive coordination (Rope), while victims perceive pure extraction (Snare). This gap is not a measurement error but a structural feature. Legislatures genuinely coordinate collective action — supreme decision-making authority is efficient for solving multi-party coordination problems. Constitutional minorities genuinely bear extraction costs — they have no structural mechanism to prevent majoritarian overreach. The tangled rope classification captures that both are true simultaneously: supremacy IS coordination AND extraction, measured from different positions in the structural hierarchy. The analytical observer risks collapsing this gap by naturalizing the majoritarian perspective as the primary one, treating supremacy as a law of governance rather than a contingent institutional choice that other systems (constitutional courts, federalism, mixed constitutionalism) have abandoned.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) measure each agent's structural position relative to the extraction flow. Majoritarian legislatures: d ≈ 0.10 (beneficiaries with arbitrage options). Concentration of authority flows toward them; they can exit through constitutional design. Constitutional minorities: d ≈ 0.95 (trapped victims). Extraction flows toward them; they cannot exit. Concentrated interest groups: d ≈ 0.55 (symmetric mixed): they benefit from legislative access but face extraction through democratic rotation and competing lobbies. Constitutional reform movements: d ≈ 0.40 (constrained targets working toward alternatives): they bear some extraction costs but have organizational agency and identified exit pathways. The theater ratio compounds experienced extractiveness by measuring how much formal procedure masks substantive power asymmetry — high theater reduces perceived extraction even when structural extraction remains constant.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that supremacy is BOTH coordination and extraction, not one or the other. The beneficiary's rope classification is their genuine experience: they are solving the multi-party coordination problem of final authority. The victim's snare classification is their genuine experience: they face majoritarian extraction with no structural exit. The tangled rope classification is the correct aggregate type because the constraint simultaneously exhibits (1) genuine coordination function — efficient collective decision-making that no alternative currently exceeds and (2) asymmetric extraction — majoritarian overrides that concentrated benefits flow toward majorities and costs flow toward minorities. The mandate does not dissolve: supremacy is properly classified as tangled rope, and the indexical perspectival gap is the diagnostic signal that reveals why political systems struggle with it. Some jurisdictions (EU, India, Canada, Australia) have formally rejected supremacy in favor of mixed constitutionalism, generating the piton classification in those systems: the supremacy frame persists as theater despite institutional alternatives. This supports the tangled rope diagnosis — if supremacy were pure coordination, its replacement by constitutional courts and federalism should have degraded coordination. Instead, coordination persists while extraction mechanisms change, proving extraction was separable from coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supremacy_definition_boundary,
    'What distinguishes legislative supremacy (maximal legislature authority) from legislative primacy (legislature as first-among-equals)?',
    'Comparative constitutional analysis: degree of override authority, ability to amend fundamental rights, power to dissolve other branches. Jurisdictions vary on where the line sits.',
    'If boundary is permeable: many systems exhibit partial supremacy (Tangled Rope from organizational perspective). If boundary is discrete: supremacy is present-or-absent categorical (may force classification away from Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supremacy_definition_boundary, conceptual, 'Definition boundary between supremacy and primacy').

omega_variable(
    constitutional_check_effectiveness,
    'Do constitutional courts, supermajority requirements, or other formal checks on legislatures actually constrain majoritarian extraction, or are they performative theater?',
    'Empirical analysis of constitutional court invalidation rates, supermajority passage frequency, amendment difficulty over time. Compare stated constitutional protections vs actual legislative overreach patterns.',
    'If checks are effective: extractiveness should be lower (0.35-0.40 range, rope rather than tangled_rope). If performative: extractiveness increases (0.55-0.65 range, snare/tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_check_effectiveness, empirical, 'Whether constitutional checks on legislatures are effective or performative').

omega_variable(
    exit_option_sufficiency,
    'For minority populations subject to legislative supremacy, does federalism, emigration, or deliberative alternatives constitute meaningful exit, or do structural costs make exit illusory?',
    'Analysis of actual migration/secession rates, cost-benefit calculation for federalism as exit mechanism. Do minorities perceive federalism as genuine exit or as nested supremacy?',
    'If exit is meaningful: trapped classification should downgrade to constrained. If exit is illusory: confirms snare classification (trapped perspective) at biographical horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_sufficiency, empirical, 'Whether exit options are structurally meaningful for minorities').

omega_variable(
    coordination_necessity_counterfactual,
    'Does legislative supremacy provide essential coordination benefits, or would distributed decision-making (federalism, subsidiarity, sortition) achieve equivalent coordination with lower extraction?',
    'Comparative institutional analysis: coordination failure rates under supremacy vs alternatives. Transaction cost analysis of legislative vs decentralized decision-making.',
    'If supremacy is uniquely efficient: rope classification strengthens (genuine coordination benefit). If alternatives achieve equivalent coordination: tangled_rope classification confirmed (extraction without coordination necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_counterfactual, conceptual, 'Whether legislative supremacy is necessary for coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legislative_supremacy_alternative, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legsupr_tr_t0, legislative_supremacy_alternative, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legsupr_tr_t30, legislative_supremacy_alternative, theater_ratio, 30, 0.5).
narrative_ontology:measurement(legsupr_tr_t60, legislative_supremacy_alternative, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(legsupr_be_t0, legislative_supremacy_alternative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legsupr_be_t30, legislative_supremacy_alternative, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(legsupr_be_t60, legislative_supremacy_alternative, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legislative_supremacy_alternative, enforcement_mechanism).
narrative_ontology:affects_constraint(legislative_supremacy_alternative, constitutional_court_constraint).
narrative_ontology:affects_constraint(legislative_supremacy_alternative, federalism_as_exit).
narrative_ontology:affects_constraint(legislative_supremacy_alternative, majoritarian_lock_in).

% DUAL FORMULATION NOTE:
% Legislative supremacy decomposes into two structurally distinct constraints: (1) supremacy as coordination mechanism (how to resolve final authority disputes — ε ≈ 0.08, Rope) and (2) supremacy as majoritarian extraction (how to ensure minority power without constitutional checks — ε ≈ 0.72, Snare). This story aggregates both into the tangled rope type. Upstream: the logically prior constraint is whether final constitutional authority is necessary (accessibility_collapse analysis would reveal this as mountain or false summit). Downstream: constitutional court establishment and federalism adoption represent institutional alternatives that materially change extractiveness values by distributing authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
