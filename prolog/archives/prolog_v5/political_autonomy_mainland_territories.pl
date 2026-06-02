% ============================================================================
% CONSTRAINT STORY: political_autonomy_mainland_territories
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_autonomy_mainland_territories, []).

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
 *   constraint_id: political_autonomy_mainland_territories
 *   human_readable: Political Autonomy Constraints in Mainland Territories
 *   domain: political_economy/territorial_governance
 *
 * SUMMARY:
 *   Political autonomy constraints in mainland territories structure the
 *   relationship between central state authority and territorial populations
 *   through formal hierarchy backed by administrative and coercive power.
 *   These constraints operate across multiple dimensions: budgetary control,
 *   legislative authority, executive appointment, military/security command,
 *   and cultural/educational policy. The constraint's structural tension is
 *   between genuine coordination needs (unified defense, currency, interstate
 *   relations, large-scale infrastructure) and extractive mechanisms
 *   (resource concentration, authority concentration, political
 *   subordination). The measurement trajectory shows declining extractiveness
 *   (0.72 → 0.58) and rising theater ratio (0.42 → 0.55), indicating both
 *   growing real autonomy in some domains and increasing performative
 *   governance rituals as central extraction becomes less structurally
 *   necessary. This pattern is characteristic of Tangled Rope under
 *   devolution pressure: the extraction persists through institutional
 *   inertia even as the coordination rationale weakens.
 *
 * KEY AGENTS:
 *   - Territorial Population: Primary victim (powerless/trapped) — structurally unable to exit; bears costs of centralized decision-making and resource extraction; experiences no meaningful participation in governance
 *   - Central State Authority: Primary beneficiary (institutional/arbitrage) — consolidates political power, controls resource flows, maintains unified governance structure; low perceived extraction cost
 *   - Local Governance Apparatus: Secondary victim (moderate/constrained) — bears administrative subordination and budget dependency; also coordinates genuine local services and maintains bureaucratic capacity
 *   - Devolution Coalition: Organized agent (organized/constrained) — political movements, regional governments, international development bodies advocating for autonomy transfer; sees constraint as negotiable and sunset-able
 *   - Metropolitan Capital Interests: Secondary beneficiary (institutional/arbitrage) — historical center that extracts cultural, financial, and political advantage from territorial periphery; maintains extraction through institutional inertia
 *   - Analytical Observer: Neutral observer (analytical/analytical) — recognizes coordination-extraction hybrid; assesses whether autonomy constraints are efficient governance or extractive mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_autonomy_mainland_territories, 0.58).
domain_priors:suppression_score(political_autonomy_mainland_territories, 0.68).
domain_priors:theater_ratio(political_autonomy_mainland_territories, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_autonomy_mainland_territories, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_autonomy_mainland_territories, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(political_autonomy_mainland_territories, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_autonomy_mainland_territories, tangled_rope).
narrative_ontology:human_readable(political_autonomy_mainland_territories, "Political Autonomy Constraints in Mainland Territories").
narrative_ontology:topic_domain(political_autonomy_mainland_territories, "political_economy/territorial_governance").

domain_priors:requires_active_enforcement(political_autonomy_mainland_territories).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_autonomy_mainland_territories, central_state_authority).
narrative_ontology:constraint_beneficiary(political_autonomy_mainland_territories, metropolitan_capital_interests).
narrative_ontology:constraint_victim(political_autonomy_mainland_territories, territorial_population).
narrative_ontology:constraint_victim(political_autonomy_mainland_territories, local_governance_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TERRITORIAL POPULATION (SNARE) — Structurally trapped within the territorial boundary with no realistic exit option. Bears the full cost of centralized extraction (resource flows to capital, decision-making authority removed, local governance subordinated to central mandate). No coordination benefit perceived — extraction is asymmetric and enforced through administrative hierarchy.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL GOVERNANCE APPARATUS (TANGLED ROPE) — Constrained by resource dependency on central state and legal subordination, but also functions as the primary provider of local services and maintains some administrative autonomy. Bears extraction (budgetary control, authority limits) but genuinely coordinates local collective action. Mixed experience reflects both real governance function and embedded extraction.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL STATE AUTHORITY (ROPE) — Benefits from political integration and resource concentration. Experiences autonomy constraints as coordination mechanisms that enable unified governance and resource extraction without decentralized fragmentation. Maximum benefit with low perceived extraction cost — the constraint serves central interests directly.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVOLUTION COALITION (SCAFFOLD) — Organized political movements and international norm-setters promoting subsidiarity and local governance see autonomy constraints as temporary institutional arrangements with sunset logic. Regional governments, NGOs, and international development bodies advocate for graduated transfer of authority with declining central extraction. Constraint is experienced as negotiable rather than immutable.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: METROPOLITAN CAPITAL INTERESTS (PITON) — Historical colonial or post-imperial administrative structures persist through institutional inertia despite reduced functional value. The constraint maintains extractive flows (resource concentration, political consolidation) that are increasingly performed through ceremonial center-periphery relationships rather than essential integration. Theater ratio reflects the performative maintenance of hierarchical control that no longer generates coordination benefits.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global vantage, political autonomy constraints operate as hybrid systems: genuine coordination problems (unified currency, defense, interstate relations) coexist with extractive mechanisms (resource centralization, authority concentration, political capture). The constraint's classification depends critically on the decomposition question: Are autonomy constraints best analyzed as pure extraction, as coordination solved through hierarchy, or as governance hybrid with decomposable functions?
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_autonomy_mainland_territories_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_autonomy_mainland_territories, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_autonomy_mainland_territories, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_autonomy_mainland_territories, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_autonomy_mainland_territories, TR),
    TR >= 0.70.

:- end_tests(political_autonomy_mainland_territories_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The central state extracts significant resources from mainland territories (taxation without proportional service provision, legislative authority concentration, budgetary subordination) while providing some genuine coordination benefits (unified defense, currency stability, large-scale infrastructure). The extraction is not total (territories retain some administrative capacity) but is substantial and asymmetric. Suppression (0.68): High. Territorial populations face significant barriers to exit (geographic isolation, citizenship ties, cultural embeddedness, legal constraints) and substantial barriers to voice (centralized appointment of key officials, limited legislative authority, cultural marginalization). Suppression through both structural barriers and institutional design. Theater ratio (0.55): Moderate. Governance performativity exists (ceremonial center-periphery relationships, symbolic autonomy, consultation without decision-making power) but is not dominant — significant real extraction and real administration persist. The increasing theater ratio over the interval reflects growing autonomy that is initially performed (consultation rituals, advisory bodies) before becoming real (budget transfers, authority devolution).
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gap is maximal between trapped victim (Snare) and institutional beneficiary (Rope). The territorial population perceives the constraint as pure extraction with no coordination benefit — they have no choice in the relationship and receive no benefit from centralized authority. The central state perceives coordination benefits (unified governance, resource concentration, political stability) that justify the asymmetric relationship. The local governance apparatus perceives a genuine mixed relationship: they coordinate essential services and face real efficiency gains from some centralization, but also bear the costs of subordination and resource extraction. The devolution coalition perceives the constraint as temporary — recent decades show gradual autonomy transfer in comparable cases (Scotland, Catalonia, Indonesia), suggesting that the constraint has sunset logic if political will aligns. The analytical observer recognizes that the perspectival gap itself reveals the constraint's hybrid nature: if all perspectives agreed the constraint was pure coordination or pure extraction, it would be Rope or Snare respectively. The disagreement is diagnostic.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation flows from beneficiary/victim declarations and exit options. Central state authority benefits from autonomy constraints and has high exit options (can redesign the constraint unilaterally); derives low d ≈ 0.10. Territorial population bears costs and has trapped exit; derives high d ≈ 0.90. Local governance apparatus has constrained exit (can negotiate but not unilaterally exit) and mixed beneficiary/victim status (benefits from service coordination, bears subordination costs); derives moderate d ≈ 0.50. The sigmoid f(d) maps these d values to experienced extractiveness multipliers. Territorial population experiences maximum extraction flow. Central state experiences extraction flowing away from them (beneficiary effect). Local apparatus experiences intermediate extraction. The constraint's overall χ = 0.58 × f(d) × σ(S) where S varies by perspective from local (0.8) to national (1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that political autonomy constraints are structurally Tangled Rope: they include genuine coordination functions (unified defense, large-scale infrastructure, currency stability) AND significant extractive mechanisms (resource concentration, authority consolidation, political subordination). The trap is dissolving pure extraction (Snare) into either false coordination (Rope) or false immutability (Mountain). The coordination IS real — territorial populations benefit from some centralized functions even under extraction. But the extraction IS also real — the benefits are asymmetric and enforced through administrative hierarchy. The Tangled Rope classification prevents both libertarian dissolution (treating all hierarchy as extraction) and institutional naturalization (treating all hierarchy as necessary coordination). The constraint's theater ratio (0.55) indicates moderate performativity — some governance functions are ritualized without real content, suggesting that the functional coordination threshold could be maintained at lower extractiveness if political design aligned efficiency with equity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_decomposition,
    'Which governance functions genuinely require centralized authority, and which are constrained through extractive mechanism rather than coordination necessity?',
    'Comparative institutional analysis across territories with different autonomy levels; measurement of service delivery quality and resource allocation efficiency under varying centralization regimes',
    'If coordination-dominant: constraint should classify as Tangled Rope or Rope at lower extractiveness (0.30-0.45). If extraction-dominant: constraint should classify as Snare or Tangled Rope with higher extractiveness (0.55-0.75).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decomposition, empirical, 'Decomposition of genuine coordination functions from extractive mechanisms').

omega_variable(
    territorial_population_exit_capacity,
    'Are territorial populations genuinely trapped (inability to exit territory) or constrained (high but surmountable exit costs)?',
    'Migration rate analysis; measurement of exit barriers (legal, economic, social cost); comparison of emigration rates from autonomous vs. non-autonomous territories',
    'If trapped: powerless agent classification correct, suppression ≥ 0.65. If constrained: should be moderate/constrained classification with lower suppression (0.45-0.60).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(territorial_population_exit_capacity, empirical, 'Whether territorial populations face trap or constraint exit barriers').

omega_variable(
    central_state_genuine_dependency,
    'How dependent is the central state on extraction from mainland territories versus benefiting from distributed governance efficiency?',
    'Fiscal analysis of net resource flows; cost-benefit analysis of centralized governance vs. devolved systems; productivity metrics before/after autonomy reforms',
    'If state highly dependent on extraction: extractiveness ≥ 0.60. If state benefits from efficiency gains: extractiveness could be lower (0.35-0.50) despite apparent hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_state_genuine_dependency, empirical, 'Central state dependency on territorial extraction versus distributed efficiency').

omega_variable(
    identity_lock_vs_institutional_capture,
    'Is the territorial population''s political subordination maintained through internalized identity frames (colonial identity inheritance, cultural inferiority beliefs) or through material barriers and institutional design?',
    'Analysis of post-autonomy political consciousness; comparison of self-governance effectiveness under autonomy vs. continued subordination; measurement of post-liberation institutional development',
    'If identity-locked: exit_options should include identity_locked classification at biographical time horizon; classification at biographical shifts from Mountain (if trapped) to Rope (if identity_locked). If institutional capture: trapped/constrained classification appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_institutional_capture, conceptual, 'Identity lock vs. institutional capture mechanisms in political autonomy constraint').

omega_variable(
    devolution_sunset_realism,
    'Is the devolution coalition''s sunset clause scenario (graduated autonomy transfer) structurally achievable or aspirational theater?',
    'Historical analysis of devolution processes in comparable cases (Scottish devolution, Spanish regional autonomy, Canadian federalism); measurement of actual vs. promised authority transfer; analysis of center resistance to permanent decentralization',
    'If achievable: Scaffold classification valid with real sunset horizon. If theatrical: Scaffold is false, constraint persists as Snare or Tangled Rope without genuine sunset mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devolution_sunset_realism, empirical, 'Realism of devolution as genuine sunset mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_autonomy_mainland_territories, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pamt_tr_t0, political_autonomy_mainland_territories, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pamt_tr_t10, political_autonomy_mainland_territories, theater_ratio, 10, 0.5).
narrative_ontology:measurement(pamt_tr_t20, political_autonomy_mainland_territories, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(pamt_be_t0, political_autonomy_mainland_territories, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(pamt_be_t10, political_autonomy_mainland_territories, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(pamt_be_t20, political_autonomy_mainland_territories, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_autonomy_mainland_territories, enforcement_mechanism).
narrative_ontology:affects_constraint(political_autonomy_mainland_territories, fiscal_centralization_resource_flows).
narrative_ontology:affects_constraint(political_autonomy_mainland_territories, legislative_authority_hierarchy).
narrative_ontology:affects_constraint(political_autonomy_mainland_territories, cultural_policy_subordination).

% DUAL FORMULATION NOTE:
% Political autonomy constraints decompose into distinct governance functions with different ε values. Unified defense coordination (ε ≈ 0.10, Rope) operates differently from budgetary extraction (ε ≈ 0.65, Snare). The aggregate constraint has ε = 0.58, reflecting the weighted balance. Separate stories track each decomposed function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_autonomy_mainland_territories, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
