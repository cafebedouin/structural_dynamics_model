% ============================================================================
% CONSTRAINT STORY: defense_contractor_vendor_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_defense_contractor_vendor_capture, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: defense_contractor_vendor_capture
 *   human_readable: Defense Contractor Vendor Capture and Supply Chain Lock-in
 *   domain: defense/procurement/industrial_organization
 *
 * SUMMARY:
 *   Defense contractor vendor capture represents a structural constraint
 *   where military procurement processes create an oligopolistic supply chain
 *   lock-in mechanism. The system coordinates legitimate military supply
 *   chain requirements — standardization, security, technical
 *   interoperability, reliability — while simultaneously extracting rents
 *   from excluded competitors and taxpayers. The constraint exhibits both
 *   genuine coordination function (military requirements for stable, cleared
 *   suppliers) and systematic extraction (qualification barriers that exceed
 *   technical necessity, incumbent preference embedded in procurement
 *   culture, congressional protection of incumbent locations). The increasing
 *   extractiveness trajectory (0.42 → 0.58 over 20 years) suggests
 *   accumulating rent-seeking layers without corresponding increases in
 *   functional requirements, indicating Goodhart degradation (procurement
 *   metrics being gamed by incumbents rather than tracking real capability
 *   needs). The low theater ratio (0.45) reflects that procurement
 *   documentation and qualification processes are substantive, not purely
 *   performative, but rising theater trajectory suggests ritual creep.
 *
 * KEY AGENTS:
 *   - New Vendor Competitors: Primary victims (powerless/trapped) — face qualification barriers with sunk costs and no credible exit pathway; trapped in demand to prove capability without access to revenue that would enable capability investment
 *   - Competing Vendor Coalition: Secondary victim (organized/constrained) — can coordinate through industry associations and legislative channels but face systematic preference for incumbents in contracting decisions
 *   - Incumbent Defense Contractors: Primary beneficiary (institutional/arbitrage) — capture rents through qualification lock-in and switching costs; have multiple exit options (subsidiaries, civilian work, relationship arbitrage across military branches)
 *   - Military Procurement Authority: Mixed actor (powerful/mobile) — benefits from stable supplier relationships but bears costs of reduced competition and constrained technical innovation; nominally mobile but institutionally constrained by sunk relationships and political pressure
 *   - Taxpayers and Procurement Efficiency: Diffuse victim (powerless/trapped) — pay inflated defense budgets driven by reduced competition; cannot organize or exit; benefits only if capability improvements justify costs (contestable)
 *   - Congressional Representatives: Institutional beneficiaries (institutional/arbitrage) — protect incumbent vendors located in their districts through appropriations language and oversight; have arbitrage options through committee assignments and district economic politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(defense_contractor_vendor_capture, 0.58).
domain_priors:suppression_score(defense_contractor_vendor_capture, 0.68).
domain_priors:theater_ratio(defense_contractor_vendor_capture, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(defense_contractor_vendor_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(defense_contractor_vendor_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(defense_contractor_vendor_capture, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(defense_contractor_vendor_capture, tangled_rope).
narrative_ontology:human_readable(defense_contractor_vendor_capture, "Defense Contractor Vendor Capture and Supply Chain Lock-in").
narrative_ontology:topic_domain(defense_contractor_vendor_capture, "defense/procurement/industrial_organization").

domain_priors:requires_active_enforcement(defense_contractor_vendor_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(defense_contractor_vendor_capture, incumbent_defense_contractors).
narrative_ontology:constraint_beneficiary(defense_contractor_vendor_capture, qualified_vendor_oligopoly).
narrative_ontology:constraint_victim(defense_contractor_vendor_capture, military_procurement_efficiency).
narrative_ontology:constraint_victim(defense_contractor_vendor_capture, competing_vendors).
narrative_ontology:constraint_victim(defense_contractor_vendor_capture, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW VENDOR COMPETITOR (SNARE) — Cannot exit the qualification barrier; faces sunk costs in compliance infrastructure with no guarantee of access. Qualification processes, security clearances, technical certifications, and bonding requirements create a captured market where entrants cannot credibly exit. Experiences maximum extraction through artificially high barrier rents captured by incumbents.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING VENDOR COALITION (TANGLED ROPE) — Organized groups of non-incumbents benefit from the procurement system's coordination function (standardized specifications, transparent requirements, known timelines) while bearing extraction through qualification gatekeeping and preference for incumbents. Can coordinate within submarkets and sometimes challenge incumbent dominance through legislative/regulatory channels, but faces significant barriers.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT DEFENSE CONTRACTOR (ROPE) — Primary beneficiary with strong exit options through subsidiary networks and work-around contracts. Experiences the constraint as pure coordination: supplier relationships, technical standards, and procurement schedules enable efficient supply chain management. Benefits from qualification lock-in without bearing its costs.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY PROCUREMENT AUTHORITY (TANGLED ROPE) — Powerful actor with nominally mobile exit options (can change qualification standards, open new vendor categories, consolidate supply chains). Benefits from the coordination function (stable supplier relationships, known technical baselines, predictable availability). Bears extraction through locked supply chain costs, constrained competition driving up unit prices, and reduced ability to innovate through supplier diversity. Technically mobile but institutionally constrained by sunk relationships and congressional pressure.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TAXPAYERS AND PROCUREMENT EFFICIENCY (SNARE) — Diffuse, powerless agent bearing the cost of vendor lock-in through inflated defense budgets. Cannot exit the extraction mechanism; benefits only indirectly if military capability is genuinely improved (contestable). Suppressed from organizing through information asymmetries and political disconnection from procurement details.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: QUALIFICATION RITUAL SYSTEM (PITON) — The formal procurement architecture (FAR regulations, security clearance protocols, technical certifications) is partially performative. Many requirements persist not because they genuinely screen for capability but because changing them requires interagency coordination, congressional notification, and defense-industry consensus. The system is maintained through institutional inertia despite recognized opportunities for streamlining.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ACQUISITION REFORM COALITION (SCAFFOLD) — Reform initiatives (modular procurement, category management, supplier diversity mandates) aim to reduce lock-in through structural sunset: as alternative supply chains mature and technical specifications become modular, the coordination function can be decoupled from incumbent dependency. Organized actors (small business advocates, Congress) see the constraint as temporary, solvable through policy reform with a clear exit pathway.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(defense_contractor_vendor_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(defense_contractor_vendor_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(defense_contractor_vendor_capture, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(defense_contractor_vendor_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(defense_contractor_vendor_capture, TR),
    TR >= 0.70.

:- end_tests(defense_contractor_vendor_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The qualification barrier creates genuine capture: new vendors face 2-5 year qualification timelines, security clearances requiring 6-12 months, technical certifications requiring product-specific testing, and bonding/insurance requirements. These accumulate into sunk costs. The barrier is partially justified (military genuinely requires cleared suppliers), but analysis of incident data suggests requirement scope has expanded beyond necessity. Rising extractiveness over time (0.42 → 0.58) indicates that qualification costs are increasing faster than functional requirements, supporting rent accumulation interpretation. Suppression (0.68): High. Barriers to exit include: (a) sunk qualification costs that cannot be recovered if contract is lost; (b) information asymmetry about true qualification requirements vs. actual gatekeeping preferences; (c) difficulty organizing competing vendors (geographically dispersed, competitive with each other, lack transparency on incumbent advantage mechanisms); (d) political opacity (congressional protection of incumbents is informal, not documented in procurement rules). Theater ratio (0.45): Moderate-low. Procurement documentation is substantive (technical specs, security requirements, performance metrics exist and are measurable), but rising trajectory suggests creeping performativity: specs are increasingly tailored to incumbent capabilities rather than military requirements, requiring re-competition processes that incumbents dominate by default (current source preference).
 *
 * PERSPECTIVAL GAP:
 *   Why do different actors see fundamentally different constraints? Because they have different structural relationships to the extraction mechanism. The incumbent sees a coordination problem solved: 'We need stable suppliers with security clearances and technical capability — the qualification system provides exactly that.' The competing vendor sees a gatekeeping mechanism: 'We could perform as well or better, but the qualification barrier is too expensive and the incumbent preference is too strong.' The military authority sees both: 'We genuinely need cleared suppliers, AND we're paying too much because we've locked ourselves into limited competition.' The taxpayer sees pure extraction: 'Defense costs are higher than they should be due to vendor lock-in, and I have no way to exit or influence the outcome.' The scaffold coalition sees all of these as solvable through structural reform: 'The lock-in persists only because qualification is monolithic; modular procurement, open-source technical standards, and rotating-source contracts could maintain security/stability while enabling competition.' The piton perspective recognizes that much of the qualification infrastructure persists not because it's optimal but because changing it requires interagency consensus and congressional alignment — the ritual is maintained through inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation for this constraint deviates from canonical forms because of institutional heterogeneity. Incumbents derive d ≈ 0.10 (beneficiary + arbitrage = low d, negative χ) — they experience the constraint as enabling, not costly. Competing vendors derive d ≈ 0.92 (victim + trapped = high d, high χ). The military procurement authority derives d ≈ 0.55–0.65 depending on whether their institutional commitment to incumbent relationships is treated as institutional identity lock (identity_locked exit, higher d) or as constrained mobility (constrained exit, lower d). The analytical observer would estimate d ≈ 0.72 (analytical observer canonical). Congressional representatives have no direct d calculation but function as distributed institutional beneficiaries, deriving d ≈ 0.08 (beneficiary + arbitrage through district politics). The tangled rope classification reflects that the constraint simultaneously coordinates (stable supplier relationships, technical standards, qualified sourcing) and extracts (lock-in rents, reduced competition, inflated costs for excluded vendors and taxpayers).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by correctly decomposing coordination function (real: security, technical standards, supplier stability) from extraction mechanism (real: lock-in rents, qualification gatekeeping, incumbent preference). The tangled rope classification affirms both: the constraint provides genuine coordination value for military procurement while simultaneously extracting rents from competing vendors and taxpayers. The measurement trajectory (extractiveness rising faster than theater ratio) supports the tangled rope over rope classification: if pure coordination were occurring, extractiveness should remain stable as theater declines (coordinate value increases, extraction stays constant). Instead, extractiveness is rising, indicating that extraction is accumulating independently of coordination value. This separates vendor capture from pure coordination mechanisms like technical standards or security requirements. The omega variables address where the boundary sits: if genuine security requirements account for 80%+ of qualification costs, the constraint is closer to rope (high-cost coordination). If <50%, it's closer to snare (extraction with coordination cover story). The acquisition reform perspectives (scaffold, piton) address the temporal dimension: is lock-in a stable institutional feature (piton, civilizational maintenance) or a temporary coordination problem with visible sunset pathways (scaffold, biographical exit)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_clearance_necessity,
    'How much of the vendor qualification barrier reflects genuine security requirements vs. captured regulatory overhead?',
    'Comparative analysis of security incident rates among cleared vs. unclearedcontractors; assessment of whether cleared-status requirements correlate with actual risk factors or serve as pure entry gatekeeping',
    'If 80%+ genuine: qualification lock-in is justified coordination cost. If <50% genuine: lock-in is primarily extractive theater, supporting snare classification for entrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_clearance_necessity, empirical, 'Legitimacy of security clearance barriers in qualification').

omega_variable(
    technical_modularity_trajectory,
    'Are defense procurement specifications becoming more modular and interoperable, enabling supplier switching, or consolidating around incumbent designs?',
    'Longitudinal analysis of specification documents (5-10 year horizon); inventory of modular vs. bespoke component requirements; tracking of supplier switching costs over time',
    'If modularity increasing: scaffold perspective is correct; vendor capture is temporary with visible sunset. If consolidating around incumbents: lock-in is structural and worsening; tangled rope is more stable than scaffold suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_modularity_trajectory, empirical, 'Trajectory of technical specification modularity in defense procurement').

omega_variable(
    incumbent_efficiency_premium,
    'Do incumbent suppliers deliver measurable efficiency or capability gains that justify lock-in costs, or is the premium purely extractive rent?',
    'Cost-benefit analysis comparing incumbent-only supply chains to counterfactual competitive scenarios; measurement of actual vs. theoretical cost trajectories; analysis of performance metrics (on-time delivery, quality, innovation)',
    'If genuine efficiency premium exists: vendor capture provides real coordination value, supporting rope classification from military perspective. If no premium: extraction is pure, supporting snare classification for military authority and taxpayers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_efficiency_premium, empirical, 'Whether incumbent vendors deliver efficiency gains or pure extraction').

omega_variable(
    congressional_rent_capturing,
    'To what extent are congressional district protections of incumbent vendors a structural feature of the constraint vs. an external political overlay?',
    'Analysis of appropriations language, earmarks, and congressional district distribution of defense contracts; tracking of voting patterns on acquisition reform; interviews with congressional staff',
    'If structural (built into FAR/DFARS): vendor capture is institutionally stabilized at civilizational scale. If overlay (political preference, not regulatory requirement): constraint could dissolve with congressional reorientation, supporting scaffold sunset logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_rent_capturing, empirical, 'Congressional role in stabilizing vendor capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(defense_contractor_vendor_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dvc_tr_t0, defense_contractor_vendor_capture, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dvc_tr_t10, defense_contractor_vendor_capture, theater_ratio, 10, 0.41).
narrative_ontology:measurement(dvc_tr_t20, defense_contractor_vendor_capture, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(dvc_be_t0, defense_contractor_vendor_capture, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dvc_be_t10, defense_contractor_vendor_capture, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(dvc_be_t20, defense_contractor_vendor_capture, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(defense_contractor_vendor_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(defense_contractor_vendor_capture, 0.12).
narrative_ontology:affects_constraint(defense_contractor_vendor_capture, military_procurement_complexity).
narrative_ontology:affects_constraint(defense_contractor_vendor_capture, defense_industrial_base_concentration).

% DUAL FORMULATION NOTE:
% Vendor capture is downstream of broader procurement complexity and upstream of specific weapons system supply chain constraints. The distinction from pure oligopoly (economic) is that vendor capture is structurally maintained through regulatory requirements (security clearances, technical certifications, FAR compliance), not just market concentration. This is why it resists standard antitrust remedies — breaking up incumbents does not reduce qualification barriers or congressional protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(defense_contractor_vendor_capture, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
