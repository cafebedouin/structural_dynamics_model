% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Scope: Extraction Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   The extraction reading of HOA covenant scope identifies covenants as
 *   mechanisms for revenue generation and board power consolidation rather
 *   than legitimate collective property maintenance. The core structural
 *   claim: board members, property management firms, and legal counsel
 *   benefit directly and substantially from fine proliferation and selective
 *   enforcement, while financially vulnerable homeowners bear extraction
 *   through accelerated lien processes, disproportionate fines, and selective
 *   targeting. The constraint exhibits a 20-year trajectory of increasing
 *   extractiveness (0.35 → 0.62) and suppression (0.50 → 0.68) as enforcement
 *   machinery professionalized and property management firms extracted
 *   greater fees. Theater ratio increased (0.40 → 0.58) as enforcement became
 *   increasingly selective and performance-based rather than transparent.
 *   This reading coexists with two sibling readings: coordination_reading
 *   frames covenants as solving the legitimate collective action problem of
 *   neighborhood maintenance; behavioral_control_reading frames them as
 *   normalizing surveillance and conformity. This constraint story
 *   instantiates the extraction_reading exclusively, modeling the structural
 *   mechanisms through which rent-seeking operates in the HOA context.
 *
 * KEY AGENTS:
 *   - Board Members: Primary beneficiary (institutional/arbitrage) — capture prestige, control, direct payment/contracts; can exit or reposition at will; experience constraint as legitimate coordination opportunity with embedded revenue stream
 *   - Property Management Firms: Primary beneficiary (institutional/arbitrage) — collect management fees scaled to enforcement activity; incentivized to expand covenant scope and fine proliferation; arbitrage exit (can serve other HOAs or corporate clients)
 *   - Legal Counsel: Primary beneficiary (institutional/arbitrage) — direct beneficiary of enforcement machinery (legal fees, lien processing fees, distressed-property acquisition opportunities); arbitrage exit
 *   - Financially Vulnerable Homeowners: Primary victim (powerless/trapped) — targeted by selective enforcement; face accelerated lien processes; cannot exit (illiquid asset, no alternative housing); experience maximum extraction
 *   - Renters (Pass-Through Victims): Primary victim (powerless/trapped) — bear enforcement costs as pass-through rent increases; no legal standing; trapped by rental agreement; zero coordination benefit
 *   - Compliant Middle-Class Homeowners: Secondary victim (moderate/constrained) — experience mixed extraction and coordination benefit; compliance is rewarded with selective nonenforcement; constrained by exit costs but experience some coordination function
 *   - Homeowner Advocacy Organizations: Organized agents (organized/constrained) — recognize system as degraded; advocate for transparency and enforcement reform; constrained by legal precedent and industry entrenchment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.62).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.68).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Scope: Extraction Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '610a10eb-b291-4a68-953b-78f6aeb4eda5').
narrative_ontology:cs_kernel_codification('610a10eb-b291-4a68-953b-78f6aeb4eda5', formalized).
narrative_ontology:cs_authority_grounding('610a10eb-b291-4a68-953b-78f6aeb4eda5', extraction).
narrative_ontology:cs_interpretation_layer_present('610a10eb-b291-4a68-953b-78f6aeb4eda5').
narrative_ontology:cs_reading_relation('610a10eb-b291-4a68-953b-78f6aeb4eda5', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('610a10eb-b291-4a68-953b-78f6aeb4eda5', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('610a10eb-b291-4a68-953b-78f6aeb4eda5', foundational, finedriven_power_consolidation).
narrative_ontology:cs_axiom_status(finedriven_power_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('610a10eb-b291-4a68-953b-78f6aeb4eda5', finedriven_power_consolidation, empirically_contingent).
narrative_ontology:cs_axiom('610a10eb-b291-4a68-953b-78f6aeb4eda5', foundational, enforcement_selectivity_reveals_extraction).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_extraction, holdable).
narrative_ontology:cs_axiom_grounding('610a10eb-b291-4a68-953b-78f6aeb4eda5', enforcement_selectivity_reveals_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('610a10eb-b291-4a68-953b-78f6aeb4eda5', transparent_proportional_enforcement).
narrative_ontology:cs_drift_state('610a10eb-b291-4a68-953b-78f6aeb4eda5', contemporary_professionalized_hoa_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('610a10eb-b291-4a68-953b-78f6aeb4eda5', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE HOMEOWNER (SNARE) — Trapped by illiquidity (home equity is principal asset), cannot exit neighborhood without catastrophic loss. Faces selective enforcement of covenant violations: minor infractions ignored for compliant neighbors, aggressively pursued for those showing signs of financial stress. Lien processes accelerate toward those least able to pay. Zero exit capacity, maximum extraction experienced. The constraint functions purely as rent extraction — no genuine coordination benefit perceived.
constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RENTER (SNARE) — Bears covenant enforcement costs as pass-through charges in rent or lease terms, with no legal standing to contest HOA decisions or covenant scope. Trapped by rental agreement; extraction is fully opaque. No coordination benefit; pure subordination to extraction mechanism.
constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: COMPLIANT HOMEOWNER (TANGLED ROPE) — Constrained by exit costs (selling/relocating) but experiences mixed extraction and coordination benefit. The covenant system genuinely maintains property values and neighborhood aesthetic for compliant agents. Enforcement is selective: violations by compliant agents are overlooked or resolved quickly; violations trigger revenue extraction only from already-marginal agents. This agent experiences coordination (property maintenance) layered over asymmetric extraction (directed at vulnerable agents). High suppression due to threat of selective enforcement hanging over even compliant agents.
constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BOARD MEMBERS / MANAGEMENT FIRMS (ROPE) — Primary beneficiaries. Experience the covenant system as coordination (solving the legitimate problem of collective property maintenance) with embedded revenue generation. Can exit or reposition at will — arbitrage exit options. Net beneficiary: fines, legal fees, property management contracts, and board positions provide direct pecuniary or status benefit. Extraction runs toward this agent.
constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGAL COUNSEL / LIEN PROCESSORS (ROPE) — Direct beneficiaries of enforcement machinery. Each fine generates legal fees; each lien generates processing fees and potential distressed-property acquisition opportunities. Experience the system as pure coordination: managing the legitimate function of enforcing collective agreements. Arbitrage exit (can service other HOAs or practice areas). Net beneficiary.
constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: HOMEOWNER ADVOCATES (PITON) — Organized agents see the covenant enforcement system as degraded: it began as legitimate coordination for property maintenance but has atrophied into revenue generation theater. Advocates have documented that selective enforcement tracks financial vulnerability, not violation severity. The system persists through inertia (legal precedent, board continuity, property-management industry entrenchment) rather than legitimate function. Theater ratio (0.58) captures that enforcement is increasingly performative — visibility and financial targeting matter more than actual covenant violation.
constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some covenant enforcement mechanism is inherent to collective property governance: shared interests require some mechanism to prevent free-riding or neighborhood deterioration. This perspective risks naturalizing the specific extraction-focused implementation as an inevitable structural feature. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that selective enforcement targeting financial vulnerability is contingent institutional design, not law of nature.
constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hoa_covenant_scope__extraction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, TR),
    TR >= 0.70.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The covenant system generates direct pecuniary benefit to board members (contracts, management fees), legal counsel (fines, lien fees, distressed-property opportunities), and property management firms (enforcement-scaled fees). The extraction is not maximal (0.72+) because compliant agents experience genuine coordination benefit alongside selective nonenforcement, and the system's legitimacy still partly rests on genuine property-value maintenance. The 20-year trajectory from 0.35 → 0.62 reflects professionalization of enforcement machinery and increasing fee extraction as the system matured. Suppression (0.68): High. Vulnerable agents face material barriers (illiquidity of home equity, rental agreement constraints, legal standing limitations) and informational suppression (enforcement criteria are opaque, fine amounts are discretionary, selective enforcement is undocumented). The suppression trajectory (0.50 → 0.68) reflects increasing sophistication of enforcement machinery and board power consolidation. Theater ratio (0.58): Moderate-high, rising trend (0.40 → 0.58). Early covenants functioned primarily as maintenance coordination (low theater). Modern enforcement has become increasingly selective and performance-based — enforcement visibility and financial targeting now matter more than consistency or proportionality. However, theater is not maximal (0.70+) because enforcement still delivers material results (property maintenance does occur, compliant agents see value). The rise in theater reflects accumulation of enforcement discretion and shift from transparent rules to performance-based extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism (covenant enforcement) appears as pure extraction (snare) to trapped agents, mixed coordination and extraction (tangled_rope) to compliant agents who benefit from enforcement externalities, coordination pure (rope) to beneficiaries, degraded theater (piton) to organized advocates, and natural law (mountain, false summit) to analytical observers who risk naturalizing contingent institutional design. The perspectival gap is largest between vulnerable homeowners (snare, zero coordination benefit) and board members (rope, pure coordination benefit). The vulnerable homeowner sees selective targeting and accelerated lien processes; the board member sees property-value maintenance. Both observations are accurate from their structural positions — the gap reveals the asymmetric distribution of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from base_properties.extractiveness (0.62) scaled by directionality (d) and scope modifier. Vulnerable homeowners with trapped exit (d ≈ 0.95, high d) experience maximum chi; board members with arbitrage exit (d ≈ 0.05, low d) experience negative chi (net benefit). Compliant homeowners with constrained exit and mixed beneficiary/victim status (d ≈ 0.50) experience mid-range chi. The structural data is explicit: beneficiaries are board_members, property_management_firms, legal_counsel (low d); victims are financially_vulnerable_homeowners, renters_via_pass_through (high d). The analytical observer (d ≈ 0.72) sees the full extraction mechanism from an external vantage. The directionality derivation is automatic from these declarations; no overrides required.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The extraction reading resolves the mandatrophy through structural clarity about who benefits and who bears costs. The official narrative (coordination_reading) claims covenants maintain property values via transparent collective agreements — a legitimate rope function. The extraction reading reveals that implementation has layered onto this coordination a selective enforcement mechanism that targets financially vulnerable agents (d ≈ 0.95) while protecting compliant agents and benefiting board/legal/management actors (d ≈ 0.05). The tangled_rope classification is justified: genuine coordination function (property maintenance, value stabilization) exists alongside asymmetric extraction (selective enforcement, accelerated liens, fee extraction). The mandatrophy is resolved by acknowledging that the constraint is not 'pure coordination misclassified as extraction' or vice versa, but genuinely hybrid — coordination that has been instrumentalized for extraction. Ε = 0.62 reflects this hybrid: not high enough to be pure extraction (snare), not low enough to be pure coordination (rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_causation,
    'Does selective enforcement target financial vulnerability causally, or is it merely correlated with other violation patterns?',
    'Longitudinal audit of enforcement records cross-referenced with property valuations, tax payment status, and debt indicators. Statistical analysis of fine distribution controlling for violation severity.',
    'If causal: extraction reading confirmed — the constraint is designed to target vulnerable agents. If correlational: selective enforcement may reflect legitimate priority-setting (high-value properties get more enforcement attention), and the tangled_rope classification shifts toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_causation, empirical, 'Whether enforcement selectivity causally targets financial vulnerability').

omega_variable(
    covenant_scope_original_intent,
    'Is the covenant scope and enforcement mechanism the product of deliberate design by beneficiaries, or did it evolve through path-dependent institutional drift?',
    'Historical analysis of covenant documents, board minutes, and management contracts. Interviews with original developers and early board members documenting intent.',
    'If deliberate design: extraction reading has stronger axiom_intentionality support (foundational axiom is holdable). If drift: alternative readings (coordination_reading, behavioral_control_reading) gain explanatory force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_scope_original_intent, empirical, 'Whether extraction mechanism was deliberately designed or evolved through drift').

omega_variable(
    alternative_enforcement_framework_efficacy,
    'Could equally effective property maintenance coordination be achieved with substantially lower extractiveness via transparent, published enforcement thresholds and proportionate fines?',
    'Comparison with HOA communities that implement transparent fine schedules and published enforcement criteria. Measurement of property value maintenance and covenant compliance rates with and without extraction-focused mechanisms.',
    'If equally effective: the high extractiveness is revealed as unnecessary — the constraint shifts toward rope classification. If extraction mechanism enables enforcement efficacy: the tangled_rope classification is confirmed and justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_enforcement_framework_efficacy, empirical, 'Whether transparent enforcement frameworks could maintain coordination equally effectively').

omega_variable(
    reading_kernel_contestation,
    'Which reading of the covenant scope kernel is dominant in actual HOA governance practice and legal doctrine?',
    'Survey of state HOA statutes and court precedents. Analysis of enforcement practices across representative sample of HOAs. Discourse analysis of board communications and property management industry standards.',
    'If extraction_reading is dominant: the axiom_finedriven_power is foundational and holdable (the system is explicitly designed as extraction). If coordination_reading is dominant: the extraction reading is a contested alternative that challenges the apparent legitimacy of the official framing. If behavioral_control_reading is dominant: extraction is a secondary consequence rather than primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Which reading of covenant scope is institutionally dominant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoaext_tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hoaext_tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(hoaext_tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(hoaext_be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hoaext_be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(hoaext_be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hoaext_su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hoaext_su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(hoaext_su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, predatory_lien_acceleration).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, financial_vulnerability_as_enforcement_target).

% DUAL FORMULATION NOTE:
% The HOA covenant scope kernel has been decomposed into three constraint stories, each instantiating a distinct reading with different epsilon values and beneficiary/victim structures. The extraction_reading (this story) has ε ≈ 0.62 and models covenants as hybrid coordination-extraction. The coordination_reading has lower ε (estimated 0.25-0.35) and models covenants as legitimate collective action mechanisms. The behavioral_control_reading has different victim/beneficiary structure (victim: autonomy/conformity; beneficiary: status quo preservation) and different primary mechanism. All three readings share the same kernel (HOA covenant authority structure) but decompose its structural mechanisms differently. They are linked via network.affects_constraints because the empirical findings about enforcement selectivity that support the extraction_reading logically constrain the coherence of the coordination_reading and provide evidence for the behavioral_control_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
