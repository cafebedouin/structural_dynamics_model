% ============================================================================
% CONSTRAINT STORY: project_vault_extraction_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_project_vault_extraction_2026, []).

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
 *   constraint_id: project_vault_extraction_2026
 *   human_readable: Project Vault: Debt-Financed Strategic Extraction
 *   domain: economic/political
 *
 * SUMMARY:
 *   Project Vault exemplifies modern debt-financed extraction: a $12 billion
 *   critical minerals reserve funded by a $10 billion EXIM Bank loan with an
 *   explicit profit mandate. The structure combines legitimate strategic
 *   objectives (securing critical mineral supply chains) with extraction
 *   mechanisms that operate through financial engineering rather than
 *   explicit coercion. The host nation retains nominal sovereignty but
 *   operates under debt covenants that lock in export quotas, pricing
 *   formulas, and operational control to the mining operator and creditor.
 *   The resource-rich but capital-scarce host nation has no exit: debt
 *   servicing requires export revenues; alternative financing is unavailable
 *   or more expensive; renegotiation triggers default risk and secondary
 *   sanctions. This is a Snare from the host nation's perspective — pure
 *   extraction with no coordination benefit to the trapped actor — but
 *   appears as Rope (coordination) from the creditor and operator
 *   perspectives. Artisanal miners and local communities are displaced
 *   without compensation and trapped in the host nation's negotiating
 *   constraints. Importing nations benefit from secured supply but are
 *   themselves locked into single-source dependency. Development finance
 *   institutions provide performative oversight (Piton) without material
 *   constraint on extraction.
 *
 * KEY AGENTS:
 *   - Host Nation Government: Primary victim (powerless/trapped) — structurally trapped by debt covenants; cannot exit without default; nominally sovereign but operationally constrained
 *   - EXIM Bank and Creditor Syndicate: Primary beneficiary (institutional/arbitrage) — secured revenue stream from debt servicing; profit mandate aligns incentives toward maximum extraction
 *   - Integrated Mining Operators: Primary beneficiary (institutional/arbitrage) — management contracts provide de facto control; arbitrage between covenant-locked supply prices and global spot prices
 *   - Artisanal Miners and Local Communities: Secondary victim (powerless/trapped) — displaced from extraction zones; no revenue share; bear full environmental externalities; trapped by land laws and security arrangements
 *   - Importing Nations and Strategic Manufacturers: Mixed actor (moderate/constrained) — benefit from supply security but locked into single-source dependency by host nation's debt structure
 *   - Development Finance Institutions: Institutional observer (institutional/arbitrage) — provide performative sustainability assessment; functionally degraded (piton); presence legitimizes without constraining
 *   - Analytical Observer: Systemic view (analytical/analytical) — reveals constraint as modern colonialism by financial contract, not natural law or inevitable trade necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(project_vault_extraction_2026, 0.68).
domain_priors:suppression_score(project_vault_extraction_2026, 0.72).
domain_priors:theater_ratio(project_vault_extraction_2026, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(project_vault_extraction_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(project_vault_extraction_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(project_vault_extraction_2026, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(project_vault_extraction_2026, snare).
narrative_ontology:human_readable(project_vault_extraction_2026, "Project Vault: Debt-Financed Strategic Extraction").
narrative_ontology:topic_domain(project_vault_extraction_2026, "economic/political").

domain_priors:requires_active_enforcement(project_vault_extraction_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, exim_bank_creditors).
narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, integrated_mining_corporations).
narrative_ontology:constraint_victim(project_vault_extraction_2026, resource_host_nation).
narrative_ontology:constraint_victim(project_vault_extraction_2026, environmental_commons).
narrative_ontology:constraint_victim(project_vault_extraction_2026, artisanal_miners).
narrative_ontology:constraint_victim(project_vault_extraction_2026, domestic_processing_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE HOST NATION (SNARE) — The host country faces structural extraction. Debt servicing obligations are denominated in foreign currency; export revenues are the only means to service debt; Project Vault monopolizes critical mineral extraction; legal sovereignty is constrained by loan covenants (export quotas, pricing formulas, operational control); re-negotiation is politically costly and triggers default risk. Trapped exit with powerless negotiating position. Maximum extractiveness experienced.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ARTISANAL MINERS AND LOCAL COMMUNITIES (SNARE) — Displaced from traditional mining territories; no legal standing in Project Vault licensing; no revenue share in operational profits; bear full environmental externalities (water depletion, soil contamination); trapped by land laws and security arrangements that exclude alternative livelihoods. Extraction flows entirely away from these agents — they are forced to absorb costs without compensation or exit.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: EXIM BANK AND CREDITOR SYNDICATE (ROPE) — Experiences the constraint as coordination: the bank mobilizes capital for strategic mineral security; debt covenants ensure operational compliance and revenue flows; profit mandate aligns bank incentives with mine output. Net beneficiary — extraction runs toward this agent. The coordination function is genuine (securing critical supply chains); the extraction is the profitable margin extracted from the host nation's trapped position.
constraint_indexing:constraint_classification(project_vault_extraction_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATED MINING CORPORATIONS (ROPE) — Operate Project Vault under management contracts; secure long-term supply chains at below-market prices (enforced by debt covenants on the host nation); arbitrage between covenant-locked supply prices and global spot prices; de facto control over operational decisions despite legal host-country sovereignty. Net beneficiary — extraction mechanism sustained through asymmetric contract power.
constraint_indexing:constraint_classification(project_vault_extraction_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IMPORTING NATIONS AND STRATEGIC MANUFACTURERS (TANGLED ROPE) — Benefit from secured critical mineral supply (coordination function: strategic security). However, the covenants lock them into single-source dependency via Project Vault's monopoly structure; diversification is constrained by debt servicing logic of the host nation (extraction mechanism: they cannot exit without supply disruption). Mixed — coordination benefit (security) entangled with extraction risk (supply lock-in). Suppression operates via their inability to develop alternative sources.
constraint_indexing:constraint_classification(project_vault_extraction_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: DEVELOPMENT FINANCE INSTITUTIONS (PITON) — Nominally present (World Bank, regional development banks) but functionally degraded. They endorse sustainability assessments and debt sustainability analyses that fail to flag extraction mechanisms; their presence is performative — legitimizing Project Vault without constraining its operation. Theater ratio high because the DFI oversight is largely ceremonial; real power resides with EXIM Bank and mining operators.
constraint_indexing:constraint_classification(project_vault_extraction_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a systems perspective, Project Vault exemplifies debt-financed extraction mechanisms that replicate colonial commodity dependency via modern financial engineering. The structure: high-value resource + foreign debt + export-revenue servicing + monopoly control + tied purchasing = systematic extraction of economic surplus from resource-rich but capital-poor nations. This is not natural law — it is a contingent institutional arrangement, but one with deep entrenchment through legal contracts, security arrangements, and geopolitical stakes.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_extraction_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(project_vault_extraction_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_extraction_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(project_vault_extraction_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(project_vault_extraction_2026, TR),
    TR >= 0.70.

:- end_tests(project_vault_extraction_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically extracts economic surplus from the host nation through multiple channels: (1) debt servicing obligations denominated in foreign currency drain 15-25% of project revenues; (2) covenant-enforced export quotas prevent supply management to secure higher prices; (3) pricing formulas lock the host nation into below-market rates; (4) operator management contracts transfer operational decision-making to creditor-aligned entity; (5) environmental externalities are not bonded or reserved, creating unfunded liabilities for the host nation. The extractiveness is lower than pure colonial appropriation (0.85+) because the host nation formally retains nominal ownership and sovereignty, but higher than normal commercial lending (0.35) because the structure is explicitly designed to extract maximum surplus while maintaining legal legitimacy. Suppression (0.72): High. Suppression operates through multiple mechanisms: (1) capital constraints prevent the host nation from financing alternative projects; (2) legal covenants restrict policy options (export management, pricing, processing); (3) debt default triggers secondary sanctions and credit market exclusion; (4) political economy of loan conditionality makes renegotiation costly (IMF, World Bank conditionality often attached); (5) security arrangements (foreign operators, international security firms) enforce compliance; (6) information asymmetry — creditor and operators have superior data on mineral reserves and market conditions; (7) artisanal miners and local communities have zero legal standing. Theater ratio (0.48): Moderate. The constraint has low theater at the structural level — debt covenants and profit mandates are explicit, not performatively framed. However, the sustainability assessments and development finance institution involvement add moderate theater (appearing more concessional and development-oriented than the underlying structure warrants). The theater is declining over time as local scrutiny and international attention expose the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications across perspectives. The host nation and displaced communities see a Snare — maximum extraction with no exit, no coordination benefit, and full cost absorption. The creditor and mining operator see Rope — a coordination mechanism solving the legitimate problem of securing critical supply chains, with aligned incentives and clear contractual frameworks. Importing nations see Tangled Rope — coordination benefit (supply security) entangled with extraction risk (supply lock-in). Development finance institutions see themselves as providing Rope (coordination oversight) but operate at the Piton level (performative legitimation). The analytical observer sees a Snare at the systems level — the constraint's structure replicates colonial extraction patterns through financial contracts. The gap reflects genuinely different structural positions: the creditor and operator control the constraint's design and benefit from its enforcement; the host nation is forced into it by capital scarcity; artisanal miners have no seat at the table.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from power, exit options, and beneficiary/victim status. Host nation government: powerless power atom, trapped exit → high d (0.90+) → experiences full extraction coefficient. EXIM Bank: institutional power atom, arbitrage exit → low d (0.05-0.15) → experiences negative/minimal extraction (net beneficiary). Mining operator: institutional power atom, arbitrage exit → low d (0.05-0.15) → net beneficiary. Artisanal miners: powerless power atom, trapped exit → d(0.95+) → maximum extraction experienced but from even weaker structural position than host nation (no formal negotiating capacity). Importing nations: moderate power atom, constrained exit (cannot develop alternative sources without supply disruption) → d(0.55-0.65) → moderate extraction experienced despite nominal purchasing power. Development finance institutions: institutional power atom, arbitrage exit → low d but constrained by reputational risk → d(0.25-0.35) → their arbitrage is reputational rather than financial. The pipeline's f(d) function then maps these directionality values to experienced extraction coefficients via the sigmoid.
 *
 * MANDATROPHY ANALYSIS:
 *   Project Vault resolves the mandatrophy by exposing the structural distinction between coordination and extraction that financial euphemism obscures. EXIM Bank's framing emphasizes coordination (securing critical supply) and treats extraction (high margins, debt servicing terms) as legitimate compensation. Host nation's framing experiences the constraint as pure extraction (Snare) with no coordination benefit — the 'supply security' language refers to the creditor and operator's security, not the host nation's economic security. The analytical observer's task is to measure which framing is structural and which is rhetorical. The measurements show that extractiveness (0.68) and suppression (0.72) are high and increasing over time, while theater (0.48) is moderate and declining. This pattern indicates a genuine extraction mechanism becoming less performatively hidden — confirming the Snare classification. If the constraint were genuinely Rope, we would expect symmetrically declining extractiveness over time as coordination mechanisms matured. Instead, extractiveness is increasing, indicating path-dependent lock-in rather than coordination stabilization. Debt servicing obligations deepen with time; covenant restrictions accumulate; alternative financing options close as Project Vault dominates critical minerals markets. The mandatrophy resolves in favor of Snare at the structural level, with beneficiary-perspective Rope being a function of power asymmetry, not genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_renegotiation_threshold,
    'At what mineral price threshold does the host nation''s debt burden become unsustainable, triggering default or forced renegotiation?',
    'Debt sustainability analysis under varying commodity price scenarios; historical comparison with similar resource-backed debt arrangements (Zambia copper, Mozambique natural gas)',
    'If threshold < current prices: host nation is locked in for decades (high extraction). If threshold > current prices by margin < 15%: extraction is temporary, pressure-release possible. If threshold unknown/contingent: modeling uncertainty itself is part of the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenant_renegotiation_threshold, empirical, 'Mineral price threshold for debt unsustainability and forced renegotiation').

omega_variable(
    alternative_financing_feasibility,
    'Could the host nation finance comparable mineral extraction without EXIM debt by accessing concessional development finance or regional banking?',
    'Cost-benefit analysis comparing EXIM terms to World Bank IDA/IFC terms, China Development Bank terms, and African Development Bank terms for equivalent projects',
    'If concessional alternatives exist but were rejected: extraction is by choice (not snare). If concessional alternatives unavailable: host nation is genuinely trapped (confirms snare). If alternatives exist but require IMF conditionality: extraction mechanism transfers to IMF, not eliminated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_feasibility, empirical, 'Feasibility of alternative concessional financing for comparable projects').

omega_variable(
    environmental_cost_externalization,
    'What is the true environmental remediation cost of Project Vault operations, and is it factored into debt covenants or externalized to the host nation?',
    'Independent environmental audit comparing Project Vault''s environmental liability estimates to actual remediation costs in comparable mining projects; longitudinal tracking of water depletion and soil recovery timelines',
    'If externalized costs exceed 20% of project revenue: extraction mechanism is substantially financial (confirmed snare). If costs are bonded/reserved: extraction is reduced but not eliminated. If costs are unknown: this uncertainty itself enables extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_cost_externalization, empirical, 'True environmental remediation costs and their allocation between operator and host nation').

omega_variable(
    domestic_processing_displacement,
    'Does Project Vault''s structure actively prevent the host nation from developing domestic mineral processing capacity, or is processing capacity development contingent on host nation investment?',
    'Analysis of loan covenants regarding value-added processing; comparison of raw ore export vs processed mineral exports; assessment of whether covenants contain clauses restricting domestic refining/processing to low-margin activities',
    'If covenants actively prevent processing: extraction mechanism is structural (colonialism by contract). If neutral on processing: host nation can develop capacity (reduces extraction over time). If covenants incentivize export of raw materials: extraction is baked into financial structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_processing_displacement, empirical, 'Whether project covenants prevent or enable domestic mineral processing capacity development').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_extraction_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pv_tr_t0, project_vault_extraction_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(pv_tr_t5, project_vault_extraction_2026, theater_ratio, 5, 0.5).
narrative_ontology:measurement(pv_tr_t10, project_vault_extraction_2026, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(pv_be_t0, project_vault_extraction_2026, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(pv_be_t5, project_vault_extraction_2026, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(pv_be_t10, project_vault_extraction_2026, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(project_vault_extraction_2026, resource_allocation).
narrative_ontology:affects_constraint(project_vault_extraction_2026, critical_mineral_supply_security).
narrative_ontology:affects_constraint(project_vault_extraction_2026, developing_nation_debt_sustainability).
narrative_ontology:affects_constraint(project_vault_extraction_2026, artisanal_mining_displacement).

% DUAL FORMULATION NOTE:
% Project Vault decomposes into three constraint stories: (1) Critical Mineral Supply Security (ε=0.15, Mountain/Rope) — the physical necessity of securing rare earth and cobalt supply is genuine. (2) Project Vault Extraction (ε=0.68, Snare) — the specific financial structure of the $12B EXIM loan with profit mandate. (3) Artisanal Mining Displacement (ε=0.62, Snare) — the secondary extraction from displaced miners. The first story represents the legitimate coordination problem; the second and third represent how that coordination problem is resolved through extraction mechanisms. Project Vault (2) is downstream of Critical Minerals (1) and upstream of Displacement (3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(project_vault_extraction_2026, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
