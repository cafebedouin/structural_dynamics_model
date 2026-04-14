% ============================================================================
% CONSTRAINT STORY: defense_acquisition_cost_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_defense_acquisition_cost_escalation, []).

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
 *   constraint_id: defense_acquisition_cost_escalation
 *   human_readable: Defense Acquisition Cost Escalation
 *   domain: defense_economics/procurement
 *
 * SUMMARY:
 *   Defense acquisition cost escalation is a structural constraint that has
 *   intensified over 50+ years, affecting taxpayers, military operational
 *   readiness, and fiscal sustainability. The constraint exhibits asymmetric
 *   extraction (taxpayers and military personnel bear costs; contractors and
 *   political beneficiaries capture benefits) combined with genuine
 *   coordination requirements (complex weapons systems require sustained
 *   technical capability, manufacturing infrastructure, and supply chain
 *   resilience). Cost-plus contracting models, performance incentive fees,
 *   and lack of firm fixed-price competition create extraction mechanisms
 *   layered onto coordination. Classification as Tangled Rope reflects the
 *   presence of both genuine coordination (defense capability problems are
 *   real) and systematic extraction (cost escalation enables contractor
 *   margin expansion and political coalition-building through geographic
 *   distribution). Theater ratio increase (0.35 to 0.68) indicates growing
 *   dominance of performative oversight rituals (independent cost estimates,
 *   variance reviews, oversight committees) that document cost growth without
 *   preventing it. The constraint is not inevitable — alternative procurement
 *   models (fixed-price incentive contracts, commercial off-the-shelf
 *   sourcing) have achieved lower escalation rates in comparable domains,
 *   indicating that the current regime is a contingent institutional choice
 *   rather than a law of nature.
 *
 * KEY AGENTS:
 *   - Taxpayers: Primary victims (powerless/trapped) — bear escalating costs with no exit option or visibility into cost drivers
 *   - Military Personnel: Secondary victims (powerless/trapped) — experience capability delays and aging platforms due to cost-driven budget constraints; no influence on acquisition decisions
 *   - Prime Defense Contractors: Primary beneficiaries (institutional/arbitrage) — capture margin expansion and predictable revenue through cost-plus models; can exit to commercial work
 *   - Congressional Leadership: Secondary beneficiary (organized/constrained) — benefit from re-election support through geographic distribution of programs; constrained by political incentives and classification restrictions
 *   - Mid-Tier Contractor Employees: Mixed (moderate/constrained) — benefit from stable employment; constrained by clearance lock-in and skill specificity to defense sector
 *   - Acquisition Oversight Institutions: Performative actors (institutional/arbitrage) — maintain legitimate oversight rituals while extraction proceeds unabated
 *   - Analytical Observer: Structural view (analytical/analytical) — identifies both genuine coordination needs and extractive institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(defense_acquisition_cost_escalation, 0.58).
domain_priors:suppression_score(defense_acquisition_cost_escalation, 0.62).
domain_priors:theater_ratio(defense_acquisition_cost_escalation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(defense_acquisition_cost_escalation, extractiveness, 0.58).
narrative_ontology:constraint_metric(defense_acquisition_cost_escalation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(defense_acquisition_cost_escalation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(defense_acquisition_cost_escalation, tangled_rope).
narrative_ontology:human_readable(defense_acquisition_cost_escalation, "Defense Acquisition Cost Escalation").
narrative_ontology:topic_domain(defense_acquisition_cost_escalation, "defense_economics/procurement").

domain_priors:requires_active_enforcement(defense_acquisition_cost_escalation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(defense_acquisition_cost_escalation, defense_contractors).
narrative_ontology:constraint_beneficiary(defense_acquisition_cost_escalation, military_leadership).
narrative_ontology:constraint_beneficiary(defense_acquisition_cost_escalation, political_constituencies).
narrative_ontology:constraint_victim(defense_acquisition_cost_escalation, taxpayers).
narrative_ontology:constraint_victim(defense_acquisition_cost_escalation, operational_readiness).
narrative_ontology:constraint_victim(defense_acquisition_cost_escalation, national_fiscal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAXPAYER (SNARE) — Trapped within the tax system with no exit option. Bears escalating costs through appropriations. No alternative to funding acquisition programs; no visibility into cost drivers; no mechanism to exit. Experiences maximum extraction with suppression enforced through classification, budgetary opacity, and political rhetorics of national security.
constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPERATIONAL MILITARY PERSONNEL (SNARE) — Trapped within military hierarchy with no exit from acquisition decisions. Receive fewer new systems due to cost escalation; forced to extend service life of aging platforms. Bear full cost of program delay without compensation. No appeal mechanism; no alternative procurement source.
constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-TIER CONTRACTOR EMPLOYEES (TANGLED ROPE) — Constrained by career dependence on defense contracting but benefit from stable employment. Coordination function: defense employment stabilizes regional economies and provides technical skill development. Extraction function: wages suppressed below market rates for equivalent private-sector skills; constrained to single employer due to security clearance lock-in and lack of transferable credentials. Mixed experience.
constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIME DEFENSE CONTRACTORS (ROPE) — Institutional beneficiaries with arbitrage options (can exit to commercial work, international sales, or alternative programs). Experience the cost escalation as a coordination mechanism for cost-plus contracts and predictable revenue streams. Genuine coordination function: programs require sustained technical capability and manufacturing infrastructure that only contractors can provide. Net beneficiary but not extractive from their perspective — they solve the defense capability problem.
constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL LEADERSHIP (TANGLED ROPE) — Organized agents with constrained exits (re-election dependence on defense spending in their districts; pressure from constituent contractors; inability to defect without political cost). Genuine coordination function: defense committees oversee national security capability and technical risk management. Extraction function: cost escalation enables pork-barrel distribution (program spread across maximum congressional districts to build coalition). High suppression: budget opacity, classification, and ITAR restrictions prevent public visibility into cost drivers.
constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEFENSE ACQUISITION OVERSIGHT (PITON) — Institutions (GAO, DoD Cost Analysis Improvement Group, Congress's oversight committees) that exist to prevent cost escalation but have become performative. Metrics: program reviews occur regularly; independent cost estimates are produced; variance analysis is documented. Function: these review rituals have atrophied — contractors routinely exceed estimates, yet programs continue; the independent cost estimate process is gamed through optimism bias and classification of true costs; oversight committees produce reports that are read but not acted upon. Theater ratio high because the oversight system persists through institutional inertia despite documented failure to prevent escalation.
constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, defense acquisition cost escalation is a genuinely hybrid phenomenon. Coordination function: complex weapons systems require sustained technical capability, manufacturing infrastructure, and supply chain resilience — these are real coordination problems that only contractors can solve. Extraction function: the cost-plus contracting model, performance incentive fees, and lack of firm fixed-price competition create asymmetric extraction. The suppression is structural: classification restrictions and budgetary opacity prevent visibility into cost drivers. The constraint is not a natural law — it is a contingent institutional arrangement with both genuine coordination and extractive components that are analytically separable.
constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(defense_acquisition_cost_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(defense_acquisition_cost_escalation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(defense_acquisition_cost_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(defense_acquisition_cost_escalation, TR),
    TR >= 0.70.

:- end_tests(defense_acquisition_cost_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting sustained asymmetric cost transfer to taxpayers and capability degradation for military operators. Not maximum (0.66+) because genuine technical complexity accounts for portion of escalation (~40%), and some escalation reflects legitimate capability upgrades. Measurement trajectory (0.28→0.58 over 1970–2024) shows monotonic increase, indicating institutional drift toward extraction. Suppression (0.62): High. Taxpayers and military personnel face multiple barriers to exit: (1) obligatory tax system with no alternatives; (2) military hierarchy with no opt-out; (3) classification restrictions preventing public visibility into cost drivers; (4) complexity opacity preventing non-specialist understanding of feasibility; (5) political rhetoric framing cost escalation as necessary security investment. Theater ratio (0.68): High and increasing. Acquisition oversight processes are documented as largely performative: independent cost estimates are routinely exceeded; variance analysis documents overruns but programs continue; review gates are scheduled but do not halt programs; oversight committees produce reports that do not trigger corrective action. The performance of oversight rituals has increased (theater growing faster than extractiveness), indicating that legitimacy maintenance through procedural compliance is increasingly the primary function of oversight, not prevention of escalation. Claimed type (Tangled Rope) rather than Snare reflects: (1) genuine coordination function (complex systems require sustained contractor capability); (2) active enforcement (DoD contracting offices, oversight bodies); (3) beneficiaries (contractors) and victims (taxpayers) both identifiable; (4) analytical gap between beneficiary experience (rope) and victim experience (snare), with analytical observer seeing hybrid structure.
 *
 * PERSPECTIVAL GAP:
 *   The gap between contractor and taxpayer perspectives is maximal. Contractors see a coordination mechanism solving the genuine problem of maintaining defense capability — they experience Rope with predictable revenue and technical problem-solving. Taxpayers see extraction — escalating burden with decreasing visibility and no exit. Congressional leaders see political opportunity (coalition-building through geographic distribution) overlaid on security necessity — they see Tangled Rope. Military operators see Snare: they are trapped in the system, experience capability delays, and have no mechanism to influence acquisition decisions. The oversight system sees itself as performing (piton): it conducts the rituals (reviews, estimates, committees) that legitimate the process, even though those rituals have become decoupled from prevention of escalation. The analytical observer sees the full hybrid structure: genuine coordination needs + extractive institutional arrangements that are not inevitable but contingent on choices (cost-plus contracting, lack of competition, geographic distribution rules, classification restrictions).
 *
 * DIRECTIONALITY LOGIC:
 *   Prime contractors derive d ≈ 0.15 (institutional + arbitrage exit): beneficiary status with exit options → low d → negative f(d) → negative χ. They see coordination, not extraction. Taxpayers derive d ≈ 0.95 (powerless + trapped): victim status with no exit → maximum d → high f(d) → high χ. They see extraction. Military personnel derive d ≈ 0.90 (powerless + trapped): victim status, trapped in hierarchy → high d → high f(d) → experienced extraction. Congressional leaders derive d ≈ 0.50 (organized + constrained): mixed status — organized enough to influence but constrained by re-election incentives and classification limits → moderate d → moderate f(d). Acquisition oversight institutions derive d ≈ 0.25 (institutional + arbitrage): beneficiary status (funding and authority from program existence) with exit option (could move to other domains) → low d. Scope modifier σ(national) = 1.0 (no amplification or dampening from scope alone). Network effects amplify: cost escalation in one program (e.g., F-35) increases baseline expectations for other programs, creating contagion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (complex weapons systems require sustained technical capability, manufacturing infrastructure, supply chain resilience) from contingent institutional extraction (cost-plus contracting, performance incentive fees, lack of firm fixed-price competition, geographic coalition-building). The mandate (national defense) is real; the trophe (extraction mechanism) is a choice. Alternative procurement models (fixed-price incentive, COTS sourcing, public manufacturing options) have demonstrated lower escalation rates in comparable domains, proving that the current regime is not mandated by technical necessity. The increasing theater ratio (from 0.35 to 0.68) indicates that the institutional arrangement is increasingly maintained through legitimacy rituals rather than functional necessity — this is the signature of mandatrophy emergence. The resolution pathway is institutional: adoption of fixed-price contract structures, reduction of geographic distribution rules, decreased classification opacity, and introduction of competition at prime contractor level. These are politically difficult (they reduce contractor margin and disrupt congressional benefit distribution) but structurally feasible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_complexity_genuineness,
    'How much of cost escalation is due to genuine technical complexity increases vs. organizational slack and contractor padding?',
    'Comparative historical analysis: cost escalation patterns for fixed-price vs cost-plus contracts; correlation between stated technical requirements growth and actual cost growth; independent technical audits of specification creep',
    'If genuinely technical (>70%): classification shifts toward Rope (coordination problem). If organizational/padding (>60%): classification shifts toward Snare (pure extraction). Current split estimate: 40% technical, 60% organizational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_complexity_genuineness, empirical, 'Apportionment of cost escalation to technical complexity vs. organizational slack').

omega_variable(
    contractor_exit_option_reality,
    'Can prime contractors genuinely exit to commercial markets and international sales, or are they locked into defense contracts by sunk manufacturing infrastructure?',
    'Historical analysis of contractor diversification attempts; measurement of exit costs (retooling, regulatory compliance); analysis of commercial viability of defense technologies (aerospace, communications, computing)',
    'If high exit cost: contractors have constrained exits (not arbitrage), analysis shifts to identity_locked or constrained — changes chi calculation. If low exit cost: arbitrage exit is real, classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractor_exit_option_reality, empirical, 'Whether contractor exit options are genuinely available or illusory').

omega_variable(
    congressional_coalition_lock,
    'Is congressional cost-escalation tolerance driven by re-election incentives (pork-barrel politics) or by genuine national security concerns?',
    'Analysis of program distribution across congressional districts (spatial spreading enables coalition building); voting pattern analysis during budget constraints; comparison to security-independent economic stimulus programs',
    'If pork-barrel driven: extraction from taxpayers is primary function, snare classification strengthened. If security-driven: classification remains tangled_rope (genuine coordination + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_coalition_lock, empirical, 'Whether congressional support is driven by pork-barrel politics or security rationale').

omega_variable(
    oversight_ritual_effectiveness,
    'Do acquisition oversight processes (independent cost estimates, variance analysis, review gates) actually prevent or merely document cost escalation?',
    'Longitudinal tracking of cost variance from initial estimate through program completion; analysis of whether variance is discovered early (preventable) or late (documented but unstoppable); measurement of corrective action adoption rate',
    'If preventive: theater ratio should be low, classification shifts toward Rope or Scaffold. If purely documentary: piton classification confirmed — oversight is performative ritual maintaining legitimacy while extraction proceeds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oversight_ritual_effectiveness, empirical, 'Whether oversight processes prevent escalation or merely document it').

omega_variable(
    alternative_procurement_models,
    'Could fixed-price incentive contracts, commercial off-the-shelf sourcing, or public manufacturing reduce cost escalation to below threshold for snare classification?',
    'Historical analysis of fixed-price contract performance vs cost-plus in same domain; comparative study of commercial vs defense procurement cost escalation rates; simulation of alternative contracting approaches applied to historical programs',
    'If alternative models reduce escalation by >50%: current constraint is contingent institutional choice, not inherent to defense acquisitions. Mandatrophy resolution: this is extraction enabled by choice of contracting model, not coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_procurement_models, empirical, 'Whether alternative procurement models could significantly reduce escalation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(defense_acquisition_cost_escalation, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dace_tr_t1970, defense_acquisition_cost_escalation, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(dace_tr_t1990, defense_acquisition_cost_escalation, theater_ratio, 1990, 0.48).
narrative_ontology:measurement(dace_tr_t2010, defense_acquisition_cost_escalation, theater_ratio, 2010, 0.62).
narrative_ontology:measurement(dace_tr_t2024, defense_acquisition_cost_escalation, theater_ratio, 2024, 0.68).

% Extraction over time
narrative_ontology:measurement(dace_be_t1970, defense_acquisition_cost_escalation, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(dace_be_t1990, defense_acquisition_cost_escalation, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(dace_be_t2010, defense_acquisition_cost_escalation, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(dace_be_t2024, defense_acquisition_cost_escalation, base_extractiveness, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(defense_acquisition_cost_escalation, resource_allocation).
narrative_ontology:affects_constraint(defense_acquisition_cost_escalation, military_readiness_degradation).
narrative_ontology:affects_constraint(defense_acquisition_cost_escalation, fiscal_sustainability_limits).
narrative_ontology:affects_constraint(defense_acquisition_cost_escalation, contractor_oligopoly_formation).

% DUAL FORMULATION NOTE:
% Defense acquisition cost escalation is decomposable into technical complexity requirements (genuine coordination, lower ε) and institutional extraction mechanisms (contingent design choices, higher ε). This story treats them as unified tangled_rope because they are empirically inseparable in practice — separating them would require counterfactual analysis of alternative institutional structures. Related constraints: military_readiness_degradation (downstream victim constraint) and fiscal_sustainability_limits (macroeconomic consequence constraint) should be linked for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(defense_acquisition_cost_escalation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
