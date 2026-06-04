% ============================================================================
% CONSTRAINT STORY: pre_constitutional_frameworks__northwest_ordinance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pre_constitutional_frameworks__northwest_ordinance, []).

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
 *   constraint_id: pre_constitutional_frameworks__northwest_ordinance
 *   human_readable: The Northwest Ordinance: Legal Territorial Extraction Under Statehood Formula
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The Northwest Ordinance (1787) is presented in conventional historical
 *   narrative as a constitutional achievement: it organizes the territory
 *   northwest of the Ohio River into future equal states, bans slavery north
 *   of the Ohio, and establishes a procedure for statehood progression that
 *   prevents permanent colonial dependency. From the perspective of eastern
 *   established states and confederal Congress, it is an elegant solution to
 *   the acute problem of territorial governance — it generates revenue,
 *   preserves union cohesion, and creates a framework for expansion without
 *   fracturing the confederation. But this reading obscures the constraint's
 *   core structural function: it institutionalizes settler-colonial
 *   extraction of indigenous lands while delegating suppression to legal and
 *   market mechanisms rather than direct military rule. The statehood formula
 *   organizes territory for settler benefit. The slavery ban, while
 *   establishing a racial boundary, simultaneously locks enslaved people into
 *   southern territories by prohibiting their migration north. Indigenous
 *   nations are entirely excluded from the governance structure and
 *   experience the Ordinance as military-backed land dispossession. The
 *   constraint exhibits maximal extraction toward the displaced, veiled by
 *   procedural legality.
 *
 * KEY AGENTS:
 *   - Indigenous nations of the Northwest Territory: Primary victims (powerless/trapped) — experience extraction of land sovereignty through displacement enforced by military occupation; zero participation in governance; treaty violations as standard practice
 *   - Enslaved African Americans: Secondary victims (powerless/trapped) — prohibited from using the free territory as exit route from slavery; slavery ban functions as boundary enforcement mechanism locking them into southern slavery
 *   - Settler colonists and land speculators: Primary beneficiaries (moderate to institutional/constrained to arbitrage) — gain access to indigenous lands at suppressed value through legal claim procedures; benefit from statehood formula providing governance framework and future self-determination; also constrained by speculative intermediaries who may capture most land value appreciation
 *   - Eastern established states and Confederal Congress: Secondary beneficiaries (institutional/arbitrage) — solve territorial governance problem, generate land-sale revenue, preserve confederation cohesion, avoid future client-state liability by establishing statehood formula
 *   - Analytical observer: Examines the constraint's form — legal procedure delegating extraction to impersonal market and legal mechanisms rather than direct rule
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pre_constitutional_frameworks__northwest_ordinance, 0.68).
domain_priors:suppression_score(pre_constitutional_frameworks__northwest_ordinance, 0.72).
domain_priors:theater_ratio(pre_constitutional_frameworks__northwest_ordinance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pre_constitutional_frameworks__northwest_ordinance, extractiveness, 0.68).
narrative_ontology:constraint_metric(pre_constitutional_frameworks__northwest_ordinance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pre_constitutional_frameworks__northwest_ordinance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pre_constitutional_frameworks__northwest_ordinance, snare).
narrative_ontology:human_readable(pre_constitutional_frameworks__northwest_ordinance, "The Northwest Ordinance: Legal Territorial Extraction Under Statehood Formula").
narrative_ontology:topic_domain(pre_constitutional_frameworks__northwest_ordinance, "political/historical/constitutional").

domain_priors:requires_active_enforcement(pre_constitutional_frameworks__northwest_ordinance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pre_constitutional_frameworks__northwest_ordinance, '95b8d0d9-805f-4a4e-acd6-f560d8d07a2d').
narrative_ontology:cs_kernel_codification('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', formalized).
narrative_ontology:cs_authority_grounding('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', extraction).
narrative_ontology:cs_interpretation_layer_present('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d').
narrative_ontology:cs_reading_relation('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', pre_constitutional_frameworks__articles_of_confederation, coexists_with).
narrative_ontology:cs_axiom('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', foundational, statehood_formula_prevents_permanent_colonies).
narrative_ontology:cs_axiom_status(statehood_formula_prevents_permanent_colonies, holdable).
narrative_ontology:cs_axiom_grounding('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', statehood_formula_prevents_permanent_colonies, instrumental).
narrative_ontology:cs_axiom('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', foundational, slavery_ban_establishes_free_territory_principle).
narrative_ontology:cs_axiom_status(slavery_ban_establishes_free_territory_principle, holdable).
narrative_ontology:cs_axiom_grounding('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', slavery_ban_establishes_free_territory_principle, deontological).
narrative_ontology:cs_reference_frame('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', territorial_expansion_through_statehood_formula).
narrative_ontology:cs_drift_state('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', contemporary_post_civil_war, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('95b8d0d9-805f-4a4e-acd6-f560d8d07a2d', '').
narrative_ontology:cs_kernel_id(pre_constitutional_frameworks__northwest_ordinance, pre_constitutional_frameworks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pre_constitutional_frameworks__northwest_ordinance, settler_colonists).
narrative_ontology:constraint_beneficiary(pre_constitutional_frameworks__northwest_ordinance, speculative_land_companies).
narrative_ontology:constraint_beneficiary(pre_constitutional_frameworks__northwest_ordinance, eastern_established_states).
narrative_ontology:constraint_victim(pre_constitutional_frameworks__northwest_ordinance, indigenous_nations_of_northwest_territory).
narrative_ontology:constraint_victim(pre_constitutional_frameworks__northwest_ordinance, enslaved_african_americans_excluded_by_ordinance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The indigenous nations experience the Ordinance as pure extraction disguised as legal procedure. The statehood formula organizes territory for settler benefit while suppressing indigenous land sovereignty through military enforcement, treaty violation, and denial of participation in the territorial governance structure. No exit option exists — the ordinance is enforced through military occupation and displacement. Maximum extraction.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__northwest_ordinance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% The slavery ban in Article VI is extractive toward the enslaved: it excludes them from territory promised as free, preventing their use of the ordinance as an exit route from slavery. Enslavers in southern states retain full extraction of enslaved labor and property rights in areas south of the Ohio. The ban creates a legal boundary that locks enslaved people into chattel slavery in southern territories while offering theoretical freedom only in northern territories they are not permitted to enter. Suppression is maximal — the boundary enforces geographic and legal trapping.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__northwest_ordinance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Settler colonists experience the Ordinance as mixed: it provides genuine coordination benefits (clear statehood formula, property rights framework, eventual self-governance pathway) and also enables extraction (access to indigenous lands at suppressed value, labor advantages from slavery ban creating wage differentiation, property ownership subordinate to the surveying and speculative institutions that precede settlement). The constraint benefits them relative to remaining in established eastern states but constrains them through the statehood progression formula (requiring population thresholds and stages). Moderate power; constrained exit (cannot easily exit once settlement is made; sunk investments in land).
constraint_indexing:constraint_classification(pre_constitutional_frameworks__northwest_ordinance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The Confederal Congress and eastern states experience the Ordinance as pure coordination with significant benefit. It solves the acute problem of how to manage territorial expansion without fracturing the confederation (the statehood formula prevents permanent territorial colonies and future client states). It generates revenue through land sales. It preserves union cohesion by creating a clear procedure for admitting new states as equals rather than as subordinate territories or chartered corporations. This perspective sees the Ordinance as coordination achieving consensus and resolving resource contention. Arbitrage exit — they can always revise the ordinance or manage territories differently.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__northwest_ordinance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational analytical perspective, the Ordinance is a snare whose extraction is veiled by legal form. The statehood-formula procedure creates the appearance of non-colonial governance (future equal states, democratic self-determination) while institutionalizing settler-colonial extraction (land dispossession, indigenous suppression, slavery boundary enforcement). The procedure delegates extraction to legal structures and market mechanisms rather than direct military rule, making the extraction harder to see and easier to defend as inevitable or procedurally fair. High extractiveness, high suppression, moderate theater — the legal form is not fully theatrical (it does establish functional governance), but it obscures the structural extraction underneath.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__northwest_ordinance, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pre_constitutional_frameworks__northwest_ordinance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pre_constitutional_frameworks__northwest_ordinance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pre_constitutional_frameworks__northwest_ordinance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pre_constitutional_frameworks__northwest_ordinance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pre_constitutional_frameworks__northwest_ordinance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts indigenous land sovereignty through legal procedures (surveying, land sales, property law) that appear neutral but function to dispossess. It also extracts by creating a geographic boundary that confines slavery to southern territories. The extractiveness is not maximal (0.85+) because the constraint provides genuine coordination benefits to eastern states and settlers — the statehood formula solves real problems. But the primary function is extraction of indigenous territory and suppression of indigenous governance, veiled in procedural legality. Suppression (0.72): High. Military enforcement of territorial boundaries, treaty violations, denial of indigenous participation in governance structures, and legal mechanisms that prevent indigenous land claims all constitute suppression. The slavery ban adds suppression by creating a legal boundary that prevents enslaved people from crossing into free territory (prohibits migration as an exit route). Theater ratio (0.55): Moderate. The statehood formula is partially functional (it does establish governance procedures, does prevent permanent territorial colonies, does generate actual state structures) but is also partially theatrical — the legal procedure obscures the underlying extraction, makes it appear inevitable or procedurally fair, and delegates suppression to impersonal mechanisms (market forces, survey laws, property law) rather than visible force. The measurement shows theater increasing slightly over the interval as the procedure becomes institutionalized and the legal form increasingly naturalizes the extraction.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the institutional beneficiary (eastern states/Congress) who sees the Ordinance as elegant coordination solving a genuine governance problem, and the indigenous nations who experience it as institutionalized extraction delegated to legal and market mechanisms. The settler colonist perspective reveals a secondary gap: they experience tangled coordination (genuine statehood promise and self-governance pathway) combined with extraction (access to indigenous lands and labor advantages). The slavery ban reveals another gap: from the analytical perspective, the ban appears as anti-slavery principle, but from the enslaved person's perspective, it functions as boundary enforcement that prevents escape to free territory. The eastern states see Rope; indigenous nations see Snare; settlers see Tangled Rope; analytical observer sees Snare with veiled extraction. The perspectival distribution is not random — it aligns with structural power. Those with institutional power and arbitrage options see coordination; those with no power see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to the extraction flow. Indigenous nations as trapped powerless agents: d ≈ 0.95 (full targets of extraction). Eastern states as arbitrage-option institutional actors: d ≈ 0.05 (beneficiaries with exit flexibility). Settler colonists as moderate/constrained agents: d ≈ 0.60 (mixed — they benefit from the ordinance relative to their alternatives but are constrained by the speculative system). The enslaved as trapped powerless agents prevented from accessing free territory: d ≈ 0.92 (near-maximal target of the boundary mechanism). Analytical observer: d ≈ 0.73 (observer position; derives from structural relationship to the extraction pattern being analyzed).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that the Ordinance is a snare from the perspective of those experiencing extraction (indigenous nations, enslaved people prevented from entering free territory) and rope/tangled rope from the perspective of beneficiaries (eastern states, settler colonists). The analytical perspective sees the constraint's form — it is a snare whose extraction is delegated to legal and market procedures, making it harder to see as extraction and easier to defend as inevitable or fair. The mandatrophy is resolved by indexing: the classification is not 'is it snare or rope?' but 'snare for whom and rope for whom?' The eastern states experience it as rope because they solve a genuine coordination problem. Indigenous nations experience it as snare because they are the primary targets of extraction. Both are structurally correct from their respective positions. The constraint's extractiveness remains high (0.68) across perspectives because the base extraction (land dispossession) is real regardless of perspective — what changes is perception and experienced exit capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinance_vs_colonial_form,
    'Is the statehood formula a genuine break from colonial dependency, or does it institutionalize extraction under a different form (legal procedure instead of direct military rule)?',
    'Comparative analysis: examine whether indigenous nations experience the Ordinance as liberating from colonial rule or as entrenchment of settler colonialism. Measure land dispossession rates, treaty violations, and military enforcement intensity pre- and post-ordinance.',
    'If genuine break: Ordinance is Rope from indigenous perspective (coordination without extraction). If institutionalized extraction under new form: Ordinance is Snare (suppression delegated to legal and market mechanisms). Terminal classification shifts from Rope to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinance_vs_colonial_form, empirical, 'Whether statehood formula breaks colonial dependency or institutionalizes it').

omega_variable(
    slavery_ban_structural_function,
    'Does the Article VI slavery ban function as a genuine anti-slavery commitment, or does it function as a mechanism that locks enslaved people into southern territories while providing a racial boundary enforcement mechanism?',
    'Historical analysis: track whether the ban is enforced (preventing southern slaveholders from relocating enslaved people north) or becomes symbolic. Examine whether the ban generates political movements toward abolition or merely creates geographic separation of the slavery system.',
    'If enforced and anti-slavery: the ban represents genuine principle constraint on extraction in this territory. If symbolic or boundary-enforcement only: the ban functions as extraction mechanism (locks enslaved people in southern slavery through prohibition of migration). Classification of the constraint from the enslaved perspective shifts from Snare-with-principle to pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slavery_ban_structural_function, empirical, 'Whether slavery ban functions as principle or as boundary-enforcement mechanism').

omega_variable(
    settler_vs_speculative_capital_extraction,
    'Do settler colonists experience net benefit from the statehood formula, or does the land speculation system extract most value before actual settlement occurs?',
    'Economic analysis: compare land prices at ordinance establishment, prices during speculation phase, and prices at settlement; measure settler wealth accumulation vs speculative land company wealth accumulation.',
    'If settlers net benefit: constraint is Tangled Rope from settler perspective (genuine coordination with asymmetric distribution). If speculators extract most value: constraint is Snare from settler perspective (legal form distributes value away from actual settlers to financial intermediaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_vs_speculative_capital_extraction, empirical, 'Whether settlers or speculators capture most value from ordinance framework').

omega_variable(
    confederal_vs_constitutional_ordinance_reading,
    'Is this constraint a property of the Confederal framework (Articles of Confederation) or does it anticipate the Constitutional framework that will supersede the Confederation?',
    'Kernel framing analysis: examine whether the Ordinance is written as a Confederal institution (temporary, subject to congressional amendment) or as a quasi-constitutional founding document that survives the 1787 constitutional transition.',
    'If Confederal: constraint should be analyzed as property of Articles framework; Confederation''s weakness (lack of enforcement power) shapes how extraction is implemented. If quasi-constitutional: constraint transcends the Confederation and becomes a foundation for the new constitutional order; different reading emerges from relationship to 1787 Constitution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confederal_vs_constitutional_ordinance_reading, conceptual, 'Whether Ordinance is Confederal or proto-Constitutional institution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pre_constitutional_frameworks__northwest_ordinance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nwo_tr_t0, pre_constitutional_frameworks__northwest_ordinance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nwo_tr_t3, pre_constitutional_frameworks__northwest_ordinance, theater_ratio, 3, 0.52).
narrative_ontology:measurement(nwo_tr_t6, pre_constitutional_frameworks__northwest_ordinance, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(nwo_be_t0, pre_constitutional_frameworks__northwest_ordinance, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(nwo_be_t3, pre_constitutional_frameworks__northwest_ordinance, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(nwo_be_t6, pre_constitutional_frameworks__northwest_ordinance, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nwo_su_t0, pre_constitutional_frameworks__northwest_ordinance, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(nwo_su_t3, pre_constitutional_frameworks__northwest_ordinance, suppression_requirement, 3, 0.7).
narrative_ontology:measurement(nwo_su_t6, pre_constitutional_frameworks__northwest_ordinance, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pre_constitutional_frameworks__northwest_ordinance, resource_allocation).
narrative_ontology:affects_constraint(pre_constitutional_frameworks__northwest_ordinance, articles_of_confederation).

% DUAL FORMULATION NOTE:
% The Northwest Ordinance is one reading of the kernel 'pre_constitutional_frameworks'. The Articles of Confederation constraint is the sibling reading describing the governance structure of the confederation itself. The two constraints coexist: the Articles describe the central body's weakness; the Ordinance describes how territorial expansion proceeds despite that weakness. The Ordinance's statehood procedure is enabled BY the Articles' weakness (states retain sovereignty and must be incentivized rather than compelled). They influence each other within the same confederation framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
