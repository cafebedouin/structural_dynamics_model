% ============================================================================
% CONSTRAINT STORY: french_local_elections_march_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_local_elections_march_2026, []).

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
 *   constraint_id: french_local_elections_march_2026
 *   human_readable: March 2026 French Municipal Elections
 *   domain: political/electoral_institutions
 *
 * SUMMARY:
 *   The March 15 and 22, 2026, French municipal elections represent a
 *   temporal constraint structure that functions as a Scaffold: a temporary
 *   coordination mechanism enabling voice and preference aggregation, built
 *   with an explicit sunset clause (the 6-year electoral cycle). The
 *   elections embody a paradox characteristic of democratic scaffolds — they
 *   simultaneously expand and restrict political agency. For voters, they are
 *   a rare moment of meaningful power to direct local governance; for
 *   marginalized constituencies, they are resource-intensive participation
 *   rituals with suppressive barriers; for incumbent mayors and established
 *   parties, they are validation mechanisms with institutional advantage; for
 *   the far-right National Rally, they represent an insurgent entry point
 *   with structural obstacles; for the administrative state, they are an
 *   elaborate machinery whose primary function (ballot security in analog
 *   registration systems) has partially atrophied. The constraint's low
 *   extractiveness (0.28) and moderate suppression (0.42) reflect that the
 *   election is genuinely a coordination mechanism — it solves a real
 *   collective action problem (aggregating preferences into municipal
 *   leadership) — rather than pure extraction. The theater ratio (0.58)
 *   captures the significant performative content (campaign spectacle, public
 *   debates, campaign finance signaling) alongside genuine substantive
 *   choice. The sunset clause is constitutional and enforceable: the next
 *   election is guaranteed in 2032, creating a defined reform window.
 *
 * KEY AGENTS:
 *   - Incumbent Mayors: Institutional beneficiaries (organized/arbitrage) — gain legitimacy extension and continued resource control; experienced electoral coordination as Rope
 *   - Established Parties (Socialist, Republicans, Ensemble, Green): Organized beneficiaries (organized/arbitrage) — benefit from candidate nomination control, voter mobilization infrastructure, existing council seats; experience as Rope coordination
 *   - Marginalized Constituencies: Primary victims (powerless/trapped) — face language barriers, transportation costs, document requirements, electoral intimidation; experience as Snare
 *   - Reform-Minded Voters: Secondary victims (moderate/constrained) — constrained by inability to change policy direction between elections, but experience election as temporary coordination mechanism enabling voice; experience as Scaffold
 *   - Far-Right National Rally: Powerful insurgent (powerful/mobile) — mobile in organizing/media strategies, but structurally constrained by mainstream party gatekeeping and media access bias; benefits from franchise legitimacy but faces suppression; experience as Tangled Rope
 *   - Electoral Commission & Administrative State: Institutional actors (institutional/arbitrage) — maintain elaborate voting machinery with increasingly degraded functional purpose; experience as Piton
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes electoral systems as provisional human constructs with built-in renewal points; grounds Scaffold classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_local_elections_march_2026, 0.28).
domain_priors:suppression_score(french_local_elections_march_2026, 0.42).
domain_priors:theater_ratio(french_local_elections_march_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_local_elections_march_2026, extractiveness, 0.28).
narrative_ontology:constraint_metric(french_local_elections_march_2026, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(french_local_elections_march_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_local_elections_march_2026, scaffold).
narrative_ontology:human_readable(french_local_elections_march_2026, "March 2026 French Municipal Elections").
narrative_ontology:topic_domain(french_local_elections_march_2026, "political/electoral_institutions").

domain_priors:requires_active_enforcement(french_local_elections_march_2026).
narrative_ontology:has_sunset_clause(french_local_elections_march_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_local_elections_march_2026, incumbent_mayors).
narrative_ontology:constraint_beneficiary(french_local_elections_march_2026, established_parties).
narrative_ontology:constraint_beneficiary(french_local_elections_march_2026, electoral_commission).
narrative_ontology:constraint_victim(french_local_elections_march_2026, local_policy_innovation).
narrative_ontology:constraint_victim(french_local_elections_march_2026, marginalized_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED CONSTITUENCIES (SNARE) — Trapped in electoral cycle with resource-intensive participation requirements. Cannot exit the franchise system without fundamental disenfranchisement. Faces suppression through language barriers, transportation costs, intimidation. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.32.
constraint_indexing:constraint_classification(french_local_elections_march_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REFORM-MINDED VOTERS (SCAFFOLD) — Constrained by lack of viable alternatives between elections, but the election itself is a temporary coordination mechanism enabling voice and potential change. Theater_ratio=0.58 reflects significant procedural theater (candidate campaigns, debates) but also genuine choice and stakes. d≈0.55, f(d)≈0.75, σ=0.8 → χ≈0.14. Low extraction because the sunset (next 6-year cycle) creates hope for course correction.
constraint_indexing:constraint_classification(french_local_elections_march_2026, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: INCUMBENT MAYOR (ROPE) — Benefits from electoral machinery; experiences election as coordination mechanism that validates/extends their mandate. Has arbitrage options (party switching, coalition building, post-election negotiation). d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.03. Negative effective extraction = net institutional beneficiary.
constraint_indexing:constraint_classification(french_local_elections_march_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ESTABLISHED PARTIES (ROPE) — Electoral system is pure coordination mechanism for parties: aggregating voter preferences, allocating candidacies, mobilizing supporters. Parties have arbitrage options (coalition shifts, recruitment strategies, issue framing). d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.01. Near-zero effective extraction; system serves their coordination interests.
constraint_indexing:constraint_classification(french_local_elections_march_2026, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FAR-RIGHT NATIONAL RALLY / POPULIST CHALLENGER (TANGLED ROPE) — Powerful insurgent with mobile options (street organizing, alternative platforms, coalition pivots). Electoral system both constrains (mainstream party gatekeeping, media access bias) and enables (universal suffrage amplifies their message). Benefits from coordination mechanism (elections legitimize their participation) but also bears suppression through establishment resistance. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.17. Active enforcement required: establishment parties coordinate against RN to maintain exclusion.
constraint_indexing:constraint_classification(french_local_elections_march_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL COMMISSION / ADMINISTRATIVE STATE (PITON) — Maintains elaborate electoral machinery (voter rolls, polling locations, observer protocols, counting procedures) whose primary function has atrophied: ensuring ballot security in the age of digital identity and online political mobilization. Much of the theater (physical polling stations, paper ballots, signature verification) persists through institutional inertia and constitutional tradition rather than functional necessity. theater_ratio=0.58 approaching piton threshold; machinery persists because formal alternatives haven't legally superseded it, not because it uniquely solves the coordination problem. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Administrative beneficiary.
constraint_indexing:constraint_classification(french_local_elections_march_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SCAFFOLD WITH CIVILIZATIONAL SUNSET) — From a historical/global perspective, the March 2026 election represents a temporary coordination mechanism with a built-in sunset: French municipal elections are constitutionally fixed to 6-year terms with explicit renewal points. The constraint (electoral cycle, campaign finance, incumbent advantage) has a defined termination that enables reform between cycles. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.18. The civilizational view recognizes that electoral systems themselves are provisional human constructs, not immutable laws — this grounds the scaffold classification.
constraint_indexing:constraint_classification(french_local_elections_march_2026, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_local_elections_march_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_local_elections_march_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_local_elections_march_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_local_elections_march_2026, TR),
    TR >= 0.70.

:- end_tests(french_local_elections_march_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The electoral system does extract political control from voters (they cannot direct local policy between elections) and concentrates it in incumbent mayors and party structures. However, the extraction is not severe because elections genuinely aggregate preferences and create stakes for responsiveness. The 6-year term creates a meaningful constraint (powerless voters cannot exit), but it is transparent and predictable. The low extractiveness (vs. historical autocratic systems without elections) reflects that elections ARE a real coordination mechanism solving the genuine problem of aggregating preferences. Suppression (0.42): Moderate. Significant barriers exist: language requirements for ballot materials disadvantage migrants, transportation barriers affect elderly and disabled voters, voter registration complexity, intimidation/family pressure in some communities, campaign finance asymmetry. However, suppression is not extreme because France has universal suffrage, formal nondiscrimination law, and established voter protection institutions. Theater ratio (0.58): Moderate. Campaign spectacle (media coverage, candidate debates, political advertising) constitutes a substantial portion of electoral activity, but substantive choice remains. The performative content has increased from historical norms (0.35 at interval start) as media-driven campaigning has intensified. Claimed type (Scaffold): The election is a temporary coordination mechanism with constitutional sunset. The mechanics (voting procedures, counting, observer protocols) include redundancy and ritual, but the underlying function (aggregating preferences into local leadership) remains genuine. Active enforcement is required because suppressive barriers must be maintained against universal suffrage expansion, and incumbent advantage requires structural protection.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. Incumbent mayors and established parties experience it as pure Rope (coordination serving their interests). Marginalized constituencies experience it as Snare (suppressive, extractive, inescapable). Reform-minded voters experience it as Scaffold (temporary, renewable, enabling voice). The far-right National Rally experiences it as Tangled Rope (simultaneous enablement via universal franchise and suppression via establishment gatekeeping). The electoral commission experiences it as Piton (elaborate machinery maintained by institutional inertia as much as functional necessity). The analytical observer sees Scaffold (provisional human construct with built-in sunset). These are not competing interpretations of the same phenomenon — they are structural realities from different positions within the constraint. The gap is not resolvable by 'correct' measurement; it is inherent to the electoral system's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized constituencies: Victim + trapped → d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.32. High extraction. Reform-minded voters: Victim + constrained → d≈0.55, f(d)≈0.75, σ=0.8 → χ≈0.14. Moderate extraction tempered by scaffold sunset logic. Incumbent mayors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.03. Net institutional benefit; Rope classification. Established parties: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.01. Near-zero effective extraction; pure coordination. National Rally: Neither pure beneficiary nor victim; powerful but suppressed. Powerful + mobile → d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.17. Tangled Rope: genuine coordination benefit (universal franchise enables their entry) combined with active suppression (establishment gatekeeping). Electoral commission: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Administrative beneficiary; Piton classification driven by theater gate, not by extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The March 2026 election resolves the mandatrophy through the Scaffold classification: it is a genuine coordination mechanism (aggregating voter preferences into local leadership) with a built-in sunset clause (6-year cycle guarantees renewal and reform opportunity). The mandatrophy threat would be: 'Is this really coordination (Rope) disguised as temporary (Scaffold), or pure extraction (Snare) disguised as coordination?' The Scaffold classification answers: it is coordination (benefits all agents by solving preference aggregation), but temporally bounded (the 6-year cycle means marginalized groups can eventually mobilize for reform, incumbent advantage is reset every cycle, suppressive barriers can be legislatively addressed). The low extractiveness (0.28) and genuine presence of beneficiaries (incumbent mayors, established parties do legitimately benefit from voter preference aggregation) confirms coordination function. The sunset clause (constitutionally enforceable 6-year term) confirms temporality. The active enforcement requirement (barriers must be maintained against universal suffrage expansion) confirms that suppression is structural but not foundational — it persists because laws allow it, not because the voting mechanism inherently requires it. The theater ratio (0.58, below piton threshold of 0.70) confirms that substantive choice remains meaningful. The classification holds: Scaffold, not degraded Piton, because the system still solves its intended coordination problem and has a defined renewal point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    campaign_finance_distortion_magnitude,
    'Does campaign finance advantage for incumbents and established parties rise to the level of functional extraction (snare) or remain within normal coordination overhead (scaffold)?',
    'Comparative analysis of spending ratios: incumbent vs challenger vs party vs independent candidates; correlation between spending and electoral success; exit analysis for candidates without financial backing',
    'If advantage > 3:1 funding ratio AND >> 80% of elections won by better-funded candidates: escalates to tangled_rope or snare from voter perspective. If advantage < 2:1 and << 60% of outcomes determined by funding: remains within scaffold coordination overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(campaign_finance_distortion_magnitude, empirical, 'Whether campaign finance advantage constitutes functional extraction or normal overhead').

omega_variable(
    marginalization_intentionality,
    'Are suppressive barriers (language complexity, document requirements, transportation) intentional exclusion mechanisms or unintended consequences of administrative procedure?',
    'Historical policy analysis: explicit intent documentation (legislative debates, policy memos); comparative international standards for voting access; longitudinal trend analysis of barrier removal vs persistence',
    'If intentional: constraint escalates to deliberate snare from marginalized perspective. If unintended: constraint remains snare but mandates accessibility reform as part of scaffold sunset mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalization_intentionality, conceptual, 'Whether marginalization barriers are intentional exclusion or unintended consequence').

omega_variable(
    alternative_coordination_readiness,
    'Are institutional alternatives to electoral cycles (participatory budgeting, continuous representation, digital platforms) mature enough to function as a true sunset successor, or do they lack necessary infrastructure?',
    'Empirical evaluation of alternative mechanisms in operating French municipalities and regional bodies; assessment of participation rates, decision quality, cost structure, conflict resolution capacity',
    'If mature: scaffold sunset is credible — next generation can replace electoral cycle with distributed coordination. If immature: sunset becomes aspirational (piton threat) — electoral machinery persists through lack of genuine alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_readiness, empirical, 'Whether alternatives to electoral cycles are institutionally mature').

omega_variable(
    rn_insurgent_trajectory,
    'Will National Rally integration into formal electoral coalitions transform them from challengers (tangled_rope) to establishment beneficiaries (rope), or will they maintain structural exclusion despite electoral gains?',
    'Outcome tracking post-election: RN participation in municipal coalitions, committee assignments, policy influence; mainstream party coalition behavior toward RN candidates; voter treatment analysis',
    'If integrated: tangled_rope resolves toward rope — suppression mechanism dissolves, active enforcement decays. If excluded: tangled_rope persists or escalates — suppression hardens, creating two-tier electoral system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rn_insurgent_trajectory, preference, 'Whether National Rally will achieve insider status or remain structurally excluded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_local_elections_march_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frloc_tr_t0, french_local_elections_march_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(frloc_tr_t3, french_local_elections_march_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(frloc_tr_t6, french_local_elections_march_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(frloc_be_t0, french_local_elections_march_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(frloc_be_t3, french_local_elections_march_2026, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(frloc_be_t6, french_local_elections_march_2026, base_extractiveness, 6, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_local_elections_march_2026, resource_allocation).
narrative_ontology:affects_constraint(french_local_elections_march_2026, french_national_policy_cycle).
narrative_ontology:affects_constraint(french_local_elections_march_2026, national_rally_political_trajectory).
narrative_ontology:affects_constraint(french_local_elections_march_2026, european_regulatory_harmonization).

% DUAL FORMULATION NOTE:
% The March 2026 municipal elections are nested within the broader French electoral cycle (European Parliament 2024, presidential 2027, national assembly 2027). This story focuses on the local institutional constraint; the national policy cycle story captures how local electoral outcomes cascade to national governance. The National Rally trajectory is upstream (their 2022 gains created structural conditions for the 2026 local insurgency); municipal results will feed forward into 2027 national positioning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_local_elections_march_2026, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
