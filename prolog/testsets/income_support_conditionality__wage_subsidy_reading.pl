% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy
 *   domain: political_economy/labor_economics/social_policy
 *
 * SUMMARY:
 *   Unconditional income support (UBI, basic income guarantees, expanded cash
 *   transfers) is contested across multiple structural readings. This story
 *   instantiates ONE reading: the wage-subsidy mechanism. Under this reading,
 *   unconditional income support functions as a subsidy to employers,
 *   allowing them to suppress wages below subsistence while the
 *   state-provided floor prevents complete destitution. The constraint
 *   exhibits genuine coordination function (stabilizing labor markets,
 *   preventing worker exit spirals) alongside asymmetric extraction (wage
 *   suppression benefits employers while workers remain income-dependent).
 *   This is precisely the tangled_rope profile: coordination + extraction
 *   cannot be cleanly separated. The core mechanism is that employers can
 *   reduce wages by approximately the support level without losing workers,
 *   since total income (wages + support) remains near subsistence. Workers
 *   cannot use the support as an exit option from low-wage labor because the
 *   support alone is insufficient for autonomous living. The constraint's
 *   extractiveness has increased over time (0.28 → 0.52) as employers have
 *   learned to calibrate wage suppression to the support level and as labor
 *   market concentration has increased, reducing worker bargaining power.
 *   This reading shares the same policy surface with two sibling readings
 *   (freedom-floor and dependency-trap) but differs fundamentally in
 *   structural outcome: where freedom-floor emphasizes decommodification and
 *   dependency-trap emphasizes skill atrophy, wage-subsidy emphasizes
 *   institutional capture of the support mechanism by capital.
 *
 * KEY AGENTS:
 *   - Low-wage workers: Primary victims (powerless/trapped) — provide labor at suppressed wages; cannot exit to alternative sectors without losing both wage and support
 *   - Low-wage employers: Primary beneficiaries (institutional/arbitrage) — suppress wages knowing state support prevents worker exit; capture most of the support value through wage reduction
 *   - Working-age population coalition: Secondary agents (moderate/constrained) — experience mixed coordination and extraction; benefit from income floor but harmed by wage suppression equilibrium
 *   - Wage-floor advocacy coalition: Organized opposition (organized/mobile) — propose structural alternatives (binding minimum wages, conditional support) that would eliminate wage-suppression mechanism
 *   - Social policy bureaucracy: Institutional maintainer (institutional/constrained) — administers support with stated goal of poverty reduction but actual effect is wage suppression; maintains mechanism through inertia
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing the wage-suppression effect as inevitable capitalist logic rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.52).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.58).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/labor_economics/social_policy").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '198e13af-205b-428d-ac4c-f36e3434520c').
narrative_ontology:cs_kernel_codification('198e13af-205b-428d-ac4c-f36e3434520c', formalized).
narrative_ontology:cs_authority_grounding('198e13af-205b-428d-ac4c-f36e3434520c', extraction).
narrative_ontology:cs_interpretation_layer_present('198e13af-205b-428d-ac4c-f36e3434520c').
narrative_ontology:cs_reading_relation('198e13af-205b-428d-ac4c-f36e3434520c', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('198e13af-205b-428d-ac4c-f36e3434520c', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('198e13af-205b-428d-ac4c-f36e3434520c', foundational, unconditional_support_wages_equilibrate_downward).
narrative_ontology:cs_axiom_status(unconditional_support_wages_equilibrate_downward, holdable).
narrative_ontology:cs_axiom_grounding('198e13af-205b-428d-ac4c-f36e3434520c', unconditional_support_wages_equilibrate_downward, empirically_contingent).
narrative_ontology:cs_axiom('198e13af-205b-428d-ac4c-f36e3434520c', foundational, employer_monopsony_enables_wage_suppression_capture).
narrative_ontology:cs_axiom_status(employer_monopsony_enables_wage_suppression_capture, holdable).
narrative_ontology:cs_axiom_grounding('198e13af-205b-428d-ac4c-f36e3434520c', employer_monopsony_enables_wage_suppression_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('198e13af-205b-428d-ac4c-f36e3434520c', labor_market_without_price_controls).
narrative_ontology:cs_drift_state('198e13af-205b-428d-ac4c-f36e3434520c', contemporary_concentrated_labor_markets, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('198e13af-205b-428d-ac4c-f36e3434520c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, capital_accumulation_via_wage_suppression).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, wage_floor_equilibrium).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKER (SNARE) — Structurally trapped: unconditional income support is insufficient for subsistence alone, forcing continued dependence on employment. The employer can reduce wages knowing the state-provided floor prevents complete destitution. Exit from low-wage labor is impossible (no savings, no alternative skill markets, geographic immobility). The constraint appears as pure extraction: the support nominally benefits the worker but its effect is wage suppression they cannot escape.
constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-WAGE EMPLOYER (ROPE) — Experiences the constraint as coordination with significant asymmetric benefit. Unconditional income support solves the coordination problem of maintaining a low-wage labor force: without the subsidy, workers would exit to higher-wage sectors or geographic areas. With the subsidy, employers can suppress wages below subsistence while retaining workers. The employer has arbitrage options (relocate, substitute capital, offshore) but finds the wage-suppression mechanism more profitable than alternatives. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: WORKING-AGE POPULATION (TANGLED ROPE) — A constrained moderate-power coalition that experiences genuine benefits from income support (consumption smoothing, negotiation position improvement, reduced destitution) alongside significant extraction. The constraint provides both coordination (labor market stabilization) and asymmetric extraction (wage suppression). Cost to exit: organizing alternative labor structures, reducing consumption, collective bargaining infrastructure. Benefit from participation: income floor, reduced precarity. Mixed experience — neither rope nor snare, but both simultaneously.
constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WAGE-FLOOR ADVOCACY COALITION (SCAFFOLD) — Organized actors (labor unions, progressive policy coalitions, development economists) see the wage-subsidy mechanism and propose to replace it with binding minimum-wage enforcement, mandatory cost-of-living adjustments, or conditional income support (tied to wage floors). From this perspective, the constraint is temporary — a coordination failure solvable by restructuring the institutional relationship between income support and labor markets. The sunset clause is structural: as wage-floor enforcement infrastructure matures and labor power decommodification accelerates, the wage-suppression mechanism loses force. Effective extraction is low (0.25) because the coalition perceives agency and a concrete exit path.
constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SOCIAL POLICY BUREAUCRACY (PITON) — The administrative apparatus for delivering unconditional income support experiences the constraint as largely performative. The stated purpose is poverty reduction and labor market stabilization; the actual effect (wage suppression via subsidy capitalization) is neither officially acknowledged nor actively enforced by policy designers. The bureaucracy maintains the mechanism through institutional inertia and career incentives (administrators benefit from program expansion, not outcome measurement). Theater ratio (0.65): the policy ritual of income support delivery masks the structural effect of wage suppression. Functional degradation is evident but masked by official narratives.
constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CAPITALIST INEVITABILITY FRAME (MOUNTAIN) — From a civilizational perspective that treats capitalist labor markets as an immutable structure, wage suppression appears as a natural consequence of unrestricted capital mobility and profit-maximization logic. If workers can be sustained by unconditional transfers, wage rates will compress toward the transfer floor — this is presented as a law of economics, not a contingent institutional arrangement. However, structural data (beneficiary declaration, enforcement requirement, suppression measurement) contradicts the mountain classification. The engine's false summit detector will identify this as naturalization of what is actually a designed institutional mechanism that benefits specific classes while harming others.
constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_support_conditionality__wage_subsidy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, TR),
    TR >= 0.70.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine extraction: employers receive wage-suppression benefits (lower labor costs, higher profit margins, reduced worker bargaining power) while workers receive insufficient support for autonomous living. The extractiveness is not at snare-level (0.66+) because workers do receive some benefit (income floor prevents destitution) and the constraint has coordination function (labor market stabilization). The trajectory from 0.28 to 0.52 reflects increasing employer sophistication in wage calibration and declining worker countervailing power. Suppression (0.58): Moderate-high. Workers face significant barriers to exiting low-wage employment: the support level is below subsistence, alternative employment requires skills/credentials, geographic mobility requires capital, care duties limit availability. However, suppression is not total (0.90+) because some workers can exit through organization, skill accumulation, or migration. The rising trajectory (0.40 → 0.58) reflects increasing labor market concentration and erosion of alternative pathways. Theater ratio (0.38): Low-moderate. The wage-subsidy mechanism is relatively transparent — it is straightforward to measure wage suppression effects through labor economics methods. The mechanism requires active enforcement (employers must know the support level and adjust wages accordingly), so it is not primarily performative. Unlike peer review or bureaucratic ritual, wage suppression is a direct economic calculation. Theater rises over time (0.22 → 0.38) as the policy narrative ('poverty reduction') diverges increasingly from actual effect (wage suppression).
 *
 * PERSPECTIVAL GAP:
 *   The wage-subsidy reading produces stark perspectival gaps. Employers see rope (coordination, labor market stability, mutual benefit); workers see snare (extraction, wage suppression, inescapable dependence). The working-age coalition sees tangled_rope (both benefits and costs). The bureaucracy sees piton (performative policy ritual with degraded function). The analytical observer at civilizational scope risks seeing mountain (inevitable capitalism) — but structural data (active enforcement, identified beneficiaries, measurable suppression) reveals this as false summit. The core gap: unconditional support can function as either decommodification (freedom-floor reading) OR wage subsidy (this reading) depending on support level relative to wage floors and worker exit capacity. At low support levels with high labor concentration, the wage-subsidy mechanism dominates. At high support levels with strong wage enforcement, decommodification dominates. The same policy mechanism produces opposite effects at different structural configurations.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares low-wage employers as beneficiaries and low-wage workers as victims. The beneficiary designation (institutional/arbitrage exit) produces low directionality (d ≈ 0.15), yielding negative effective extraction — employers experience the constraint as beneficial coordination. The victim designation (powerless/trapped exit) produces high directionality (d ≈ 0.95), yielding high effective extraction — workers experience maximum burden. The moderate-power coalition (constrained exit) produces intermediate directionality (d ≈ 0.65), yielding moderate extraction — their experience is mixed. No directionality overrides are needed; the structural data (who captures the support value, who cannot exit) directly produces the perspectival gaps observed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by declaring that unconditional income support CAN function as both coordination AND extraction simultaneously. The tangled_rope classification rejects the false choice between 'pure coordination mechanism' (rope) and 'pure extraction mechanism' (snare). The structure is genuinely hybrid: income support stabilizes labor markets (coordination) while enabling wage suppression (extraction). The resolution is not to deny one function, but to measure both. The extractiveness value (0.52) reflects that the mechanism's actual effect — wage suppression — is substantial enough to override its stated purpose (poverty reduction), but not complete enough to eliminate all worker benefit. The constraint is tangled_rope precisely because both functions are real and irreducible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_capitalization_empirical,
    'What proportion of unconditional income support is captured by employers via wage suppression versus retained as genuine worker income gain?',
    'Comparative labor market analysis: wage trajectories in jurisdictions with identical income support levels but different labor market regulations (minimum wages, sectoral bargaining, union density); econometric decomposition of wage suppression vs. support level; worker budget-share analysis across time and regions',
    'If capitalization > 70%: constraint is predominantly snare (extraction). If capitalization < 30%: constraint is predominantly rope (coordination). Current evidence suggests 40-60% range, supporting tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_capitalization_empirical, empirical, 'Empirical measurement of employer wage suppression vs. worker income gain from support').

omega_variable(
    labor_exit_mechanism_sufficiency,
    'Does unconditional income support alone provide sufficient decommodification (economic power to refuse wages below worker reservation price) or is exit blocked by non-wage factors?',
    'Analysis of worker behavior at different support levels: surveys of job refusal/acceptance at varying gap sizes (support level vs. offered wage); measurement of reservation wage shifts; analysis of worker mobility patterns when support levels increase; comparison across sectors with different capital requirements and skill mobility',
    'If exit IS blocked by non-wage factors (care duties, credential requirements, geographic isolation, health constraints): workers remain trapped even with income support, and constraint is snare. If exit IS enabled by sufficient support: constraint shifts toward rope or scaffold. If threshold exists: support must exceed threshold to enable exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_exit_mechanism_sufficiency, empirical, 'Whether unconditional income support sufficient to enable labor exit').

omega_variable(
    reading_kernel_contest,
    'Is this reading (wage-subsidy mechanism) coherent alongside the freedom-floor reading (decommodification), or do they foreclose each other?',
    'Structural analysis: Can both readings coexist within a single policy framework? The freedom-floor reading claims unconditional support decommodifies labor and creates exit power; the wage-subsidy reading claims it enables wage suppression. Can both effects occur simultaneously? If so: coexist_with relation. If not: forecloses relation.',
    'If forecloses: only one reading can be correct within a single framework — the other is empirically falsified. If coexists_with: both readings describe real effects of the same mechanism, experienced differently by different actors (workers with exit power vs. workers without exit power). Current evidence suggests coexistence at different thresholds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether wage-subsidy reading and freedom-floor reading can coexist or logically foreclose each other').

omega_variable(
    policy_mechanism_intention,
    'Is the wage-suppression effect an unintended consequence of unconditional support design, or a deliberately embedded feature?',
    'Historical analysis of policy design documents, economist testimony, legislative debates at time of enactment; comparison with alternatives (conditional support, wage floors, job guarantees) that were explicitly rejected; analysis of program monitoring metrics (do designers measure wage suppression as success or failure indicator?)',
    'If unintended: the constraint exhibits institutional capture or design oversight rather than deliberate extraction structure. If deliberate: the constraint is a consciously engineered wage-suppression device. Distinction affects whether mandatrophy resolution points toward policy redesign (unintended) or structural conflict (deliberate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_mechanism_intention, conceptual, 'Whether wage suppression is intended policy design or unintended consequence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wagesub_theater_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(wagesub_theater_t5, income_support_conditionality__wage_subsidy_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(wagesub_theater_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(wagesub_extract_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wagesub_extract_t5, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(wagesub_extract_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wagesub_supp_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(wagesub_supp_t5, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(wagesub_supp_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, low_wage_labor_market_equilibrium).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, employer_monopsony_power).

% DUAL FORMULATION NOTE:
% The income_support_conditionality kernel decomposes into three constraint stories, each with its own ε and classification. The wage_subsidy_reading (ε=0.52, tangled_rope) is distinct from the freedom_floor_reading (ε estimates toward 0.15, rope) and dependency_trap_reading (ε estimates toward 0.45, snare). These are not measurement variants of one constraint — they are genuinely different claims about what unconditional income support does. The epsilon values differ because the observables differ: wage-subsidy measures employer wage reduction; freedom-floor measures worker bargaining power; dependency-trap measures labor supply/skill accumulation effects. All three readings share the same policy kernel but produce different classifications. Network links preserve the kernel relationship while acknowledging that this reading affects downstream constraints (low-wage equilibrium, monopsony dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
