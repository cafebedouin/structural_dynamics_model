% ============================================================================
% CONSTRAINT STORY: iud_market_capture_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iud_market_capture_mechanism, []).

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
 *   constraint_id: iud_market_capture_mechanism
 *   human_readable: IUD Market Capture Mechanism
 *   domain: healthcare/contraception/reproductive_medicine
 *
 * SUMMARY:
 *   The IUD market capture mechanism operates through a multi-layered
 *   constraint structure that combines pharmaceutical market consolidation,
 *   professional gate-keeping through specialist certification requirements,
 *   insurance reimbursement design that imposes upfront cost barriers, and
 *   residual cultural resistance based on outdated medical mythology. The
 *   constraint systematically extracts value from low-income and uninsured
 *   women while providing genuine coordination benefits (efficacy guarantees,
 *   safety standardization, professional liability protection) that justify
 *   the coordination function to beneficiaries. The extractiveness has
 *   increased over the measurement interval (0.38→0.58) as market
 *   consolidation deepened and insurance-based cost-shifting became more
 *   prevalent. Theater ratio remains moderate (0.55) because the professional
 *   resistance (menstrual mythology, specialist certification requirements)
 *   is partially performative, but the market capture and cost barriers are
 *   structurally real. From different structural positions, the same
 *   constraint appears as pure extraction (snare for trapped uninsured
 *   women), mixed coordination-extraction (tangled rope for insured women
 *   with deductibles and for professional organizations), pure coordination
 *   (rope for manufacturers), and a sunset problem (scaffold for community
 *   health worker insertion movements). The analytical observer risks
 *   naturalizing market capture as inherent to reproductive medicine (false
 *   summit mountain), when the constraint is actually a contingent
 *   institutional arrangement built from reimbursement design, regulatory
 *   gate-keeping, and professional scope-of-practice restrictions.
 *
 * KEY AGENTS:
 *   - Low-income uninsured women: Primary victim (powerless/trapped) — face maximum extraction through upfront cost barriers and specialist access requirements with no exit options
 *   - Insured women with high deductibles: Secondary victim (moderate/constrained) — experience cost barriers despite insurance coverage; constrained by deductible design and professional monopoly
 *   - Reproductive autonomy (collective good): Tertiary victim (powerless/trapped) — abstract good that cannot organize; systemic underutilization of superior contraceptive method reduces population reproductive autonomy
 *   - Pharmaceutical manufacturers (Bayer, Cooper): Primary beneficiary (institutional/arbitrage) — capture market consolidation benefits and reimbursement premiums; can exit or adjust pricing strategies
 *   - OB-GYN professional organizations: Beneficiary-victim hybrid (organized/constrained) — benefit from gate-keeping and scope-of-practice restrictions but are also constrained by litigation risk and evidence pressure
 *   - Community health worker movement: Organized resistance (organized/constrained) — see sunset pathway through task-shifting and evidence accumulation
 *   - Insurance companies: Indirect beneficiary (institutional/arbitrage) — benefit from cost-shifting via high deductibles despite long-term savings from IUD efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iud_market_capture_mechanism, 0.58).
domain_priors:suppression_score(iud_market_capture_mechanism, 0.68).
domain_priors:theater_ratio(iud_market_capture_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iud_market_capture_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(iud_market_capture_mechanism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(iud_market_capture_mechanism, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iud_market_capture_mechanism, tangled_rope).
narrative_ontology:human_readable(iud_market_capture_mechanism, "IUD Market Capture Mechanism").
narrative_ontology:topic_domain(iud_market_capture_mechanism, "healthcare/contraception/reproductive_medicine").

domain_priors:requires_active_enforcement(iud_market_capture_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iud_market_capture_mechanism, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(iud_market_capture_mechanism, medical_device_companies).
narrative_ontology:constraint_beneficiary(iud_market_capture_mechanism, obstetricians_gynecologists).
narrative_ontology:constraint_victim(iud_market_capture_mechanism, low_income_women).
narrative_ontology:constraint_victim(iud_market_capture_mechanism, uninsured_women).
narrative_ontology:constraint_victim(iud_market_capture_mechanism, women_of_color).
narrative_ontology:constraint_victim(iud_market_capture_mechanism, reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME UNINSURED WOMEN (SNARE) — Face maximum extraction with no exit. IUD insertion requires upfront capital cost ($500-1200) and specialist access despite superior long-term efficacy and lower total cost of ownership. Cannot exit contraception entirely (biological reality), cannot afford upfront cost, cannot access credit. No alternatives available at comparable efficacy levels they can afford. Pure extraction mechanism: market capture prevents price competition and alternative access pathways.
constraint_indexing:constraint_classification(iud_market_capture_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED WOMEN WITH HIGH DEDUCTIBLES (TANGLED ROPE) — Experience mixed coordination and extraction. IUDs do coordinate genuine family planning benefits (efficacy, low maintenance, hormonal control options), but upfront deductible costs create artificial barriers even for insured women. Extraction operates through cost-sharing design, not through unavailability. Exit options exist (can use other contraceptives, can plan differently) but are constrained by cost structure and medical preference asymmetry.
constraint_indexing:constraint_classification(iud_market_capture_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURERS (ROPE) — Benefit from market consolidation (now 2 major manufacturers globally: Bayer Mirena/Kyleena/Skyla, Cooper Paragard) and insurance reimbursement standardization. Experience the constraint as a coordination mechanism: regulatory compliance, insurance formulary management, physician education are coordination problems they solve. Net beneficiary with arbitrage capacity — can adjust pricing, shift product lines, exit specific market segments. No extraction runs toward them; extraction runs away from them.
constraint_indexing:constraint_classification(iud_market_capture_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OB-GYN PROFESSIONAL ORGANIZATIONS (TANGLED ROPE) — Professional societies (ACOG) provide genuine coordination: evidence-based guidelines, training standards, liability protection through consensus. But the constraint also includes extraction: gate-keeping power over IUD insertion (requiring specialist certification), resistance to non-physician insertion pathways, maintenance of high skill-premium for insertion despite evidence that nurse-midwives and trained nurses can insert safely. Extraction operates through credential capture and scope-of-practice regulation.
constraint_indexing:constraint_classification(iud_market_capture_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMUNITY HEALTH WORKER MOVEMENT (SCAFFOLD) — Organized non-physician insertion programs (increasingly common in reproductive justice initiatives) represent a sunset pathway for the specialist gate-keeping constraint. As training and safety data accumulate for community health worker insertion, the physician monopoly loses functional justification. The constraint experiences organized resistance with a structural exit path: task-shifting to trained non-specialists reduces insertion costs and increases access. Theater is moderate — professional resistance is real but declining as evidence accumulates.
constraint_indexing:constraint_classification(iud_market_capture_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MENSTRUAL MYTHOLOGY DEFENSE (PITON) — Residual cultural resistance to IUDs (historically associated with higher infection risk; current evidence contradicts this) persists through institutional inertia and cultural narratives rather than medical evidence. Medical institutions maintain cautious positioning toward IUDs despite superior efficacy profiles, partly because the risk mythology is embedded in institutional training and patient education materials. Theater_ratio is high for this component — much of the institutional resistance is performative risk aversion disconnected from current evidence.
constraint_indexing:constraint_classification(iud_market_capture_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some contraceptive constraint is inherent to human biology: pregnancy requires access to prevention mechanisms, reproductive autonomy requires reliable methods. This perspective risks naturalizing the contingent market capture as an immutable feature of reproductive medicine. However, structural data reveals this as a false summit: the capital cost barrier, specialist requirement, and insurance design choices are all contingent institutional arrangements, not biological necessities. The engine's false summit detector should flag this naturalization.
constraint_indexing:constraint_classification(iud_market_capture_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iud_market_capture_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iud_market_capture_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iud_market_capture_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iud_market_capture_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iud_market_capture_mechanism, TR),
    TR >= 0.70.

:- end_tests(iud_market_capture_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint operates through multiple overlapping extraction mechanisms: pharmaceutical market duopoly (limits price competition), specialist certification requirement (creates artificial access barrier despite evidence that trained nurses can insert safely), upfront capital cost imposed through insurance design (creates poverty trap despite superior cost-effectiveness over 5+ years), and cultural mythology (maintains resistance despite evidence). The extractiveness is not as severe as pure monopolistic extraction (0.72+) because IUDs remain available and efficacy genuinely delivers value. The extraction is systematic and intentional (particularly market consolidation and insurance design) rather than incidental. Suppression (0.68): High. Multiple reinforcing suppression mechanisms operate simultaneously: structural barriers (upfront cost, specialist access, limited availability in low-income areas), informational barriers (medical training perpetuates outdated risk mythology despite current evidence), institutional barriers (scope-of-practice restrictions, insurance gatekeeping), and social barriers (cultural resistance, insufficient patient education). Suppression would be lower if only one mechanism operated; the stack creates multiple redundant barriers that persist even when individual barriers are addressed. Theater ratio (0.55): Moderate. Professional resistance is partially performative — menstrual mythology persists in medical education despite contradictory current evidence, making it a piton-like component of the overall constraint. However, the market capture and cost barriers are substantively real (not merely theatrical), distinguishing this from pure piton degradation. The theater component has increased slightly over the interval as clinical evidence has mounted against historical cautions, yet institutional positioning remains cautious.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism produces radically different classifications depending on the observer's structural position and exit options. Low-income uninsured women (trapped, powerless) experience pure extraction (snare) — they face a capital cost barrier for the most effective option, with no alternatives available at comparable efficacy they can afford. Insured women with deductibles (constrained, moderate) experience mixed coordination-extraction (tangled rope) — the coordination function is real (IUDs do solve family planning problems), but cost-sharing design creates artificial extraction layers. Pharmaceutical manufacturers (arbitrage, institutional) experience pure coordination (rope) — the constraint solves their distribution and reimbursement problems with minimal burden. Professional organizations experience both coordination benefits (liability protection, evidence standardization) and extraction benefits (scope restriction, credential capture). Community health workers see a sunset pathway (scaffold) — evidence of safety in non-physician hands is accumulating, making the specialist requirement increasingly unjustifiable. The analytical observer risks seeing immutable biological necessity (mountain) when the actual constraints are policy and institutional choices. The perspectival gap reveals how market capture can appear as coordination from the beneficiary position and as pure extraction from the victim position, even though both are measuring the same structural fact: the barrier exists, someone benefits, someone pays, and alternatives have been systematically suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position relative to the constraint's extraction flow. Uninsured low-income women have d ≈ 0.92 (very close to 1.0, full target): they are victims in the base properties declaration, they have trapped exit options, and the constraint's primary function is extracting reproductive autonomy and healthcare resources from them. The sigmoid f(d) at d≈0.92 produces maximum f(d) ≈1.40, amplifying their experienced extractiveness. Insured women with deductibles have d ≈0.60 (moderate-high target position): they are declared victims in reproductive autonomy, but have constrained (not trapped) exit options and some benefit from the coordination function, moving d downward from the trapped baseline. OB-GYN organizations have d ≈0.25 (beneficiary position): they are declared beneficiaries through professional gate-keeping, have arbitrage options (can modify training/scope), moving them toward the beneficiary end of the d scale. Pharmaceutical manufacturers have d ≈0.05 (strong beneficiary): declared primary beneficiary, high arbitrage capacity, capture monopoly rents. The gap between trapped victims (d≈0.92) and institutional beneficiaries (d≈0.05) is approximately 1.87 orders of magnitude in experienced directionality, creating the dramatic perspectival gap where the same constraint is snare for one group and rope for another.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the 'pure extraction vs coordination' distinction is structural rather than normative. The constraint has genuine coordination function (standardization of safety, evidence-based practice, professional liability protection, supply chain coordination) AND genuine extraction function (market consolidation, cost barriers, specialist gate-keeping, cultural mythology maintenance). The question is not 'which one is real?' but 'how much of each, and distributed to whom?' The beneficiaries (manufacturers, professionals, insurance companies) perceive primarily coordination. The victims (uninsured women, women of color) perceive primarily extraction. Both perceptions are accurate from their structural positions. The mandatrophy resolves by showing that a single structure can simultaneously coordinate for one group and extract from another — tangled rope is the accurate classification because both functions are genuinely present and actively enforced (manufacturers must maintain quality; women must pay upfront costs). The false-summit mountain perspective (natural biological necessity) is revealed as false because every component (cost barrier, specialist requirement, market duopoly, mythology maintenance) is a policy choice, not a biological fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insertion_skill_requirement_necessity,
    'Is specialist-level training actually necessary for safe IUD insertion, or does the skill requirement reflect professional gate-keeping?',
    'Comparative effectiveness study: infection/perforation/expulsion rates for insertion by physicians vs trained nurses vs community health workers; meta-analysis of international evidence from systems with non-physician insertion',
    'If genuine necessity: specialist requirement is coordination. If gate-keeping: constraint shifts from Tangled Rope to higher extraction (snare characteristics intensify). Affects whether scope-of-practice restrictions are functional or extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insertion_skill_requirement_necessity, empirical, 'Whether specialist training requirement reflects genuine safety needs or professional capture').

omega_variable(
    upfront_cost_necessity,
    'Does the upfront capital requirement for IUD insertion reflect genuine production costs or insurance/reimbursement design choices?',
    'Cost accounting analysis: device manufacture cost, insertion facility cost, labor cost, comparison with countries with subsidized insertion; evidence on whether price reductions increase access',
    'If production necessity: high upfront cost is unavoidable coordination cost. If reimbursement choice: cost barrier is pure extraction mechanism (snare from victim perspective becomes even more severe). Affects whether market capture is structural or policy-contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(upfront_cost_necessity, empirical, 'Whether upfront costs reflect production necessity or reimbursement design').

omega_variable(
    menstrual_risk_mythology_persistence,
    'What portion of IUD resistance in medical institutions reflects actual medical evidence vs residual cultural/historical mythology about infection risk?',
    'Textual analysis of medical training materials and patient education across time; survey of physician knowledge accuracy on IUD safety profile; comparison with objective safety data',
    'If predominantly evidence-based caution: constraint is medical coordination. If predominantly mythology: constraint is piton with high theater. Affects whether professional resistance is functional or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(menstrual_risk_mythology_persistence, empirical, 'Extent to which medical resistance reflects evidence vs historical mythology').

omega_variable(
    market_consolidation_necessity,
    'Does the dominance of two manufacturers (Bayer and Cooper) reflect legitimate economies of scale or regulatory capture that restricts new market entrants?',
    'Regulatory barrier analysis: FDA approval timelines and costs; comparison with generic contraceptive availability; investigation of whether new manufacturers can enter market',
    'If economies of scale: duopoly is coordination outcome. If regulatory barriers: market structure is extractive constraint (snare characteristics for low-income users intensify). Affects whether market capture is structural or policy-contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_consolidation_necessity, empirical, 'Whether market duopoly reflects scale economies or regulatory capture').

omega_variable(
    insurance_deductible_design_intent,
    'Do high deductible structures for IUD insertion reflect actuarial necessity or deliberate cost-shifting to reduce insurer liability?',
    'Actuarial analysis of IUD cost-effectiveness vs claims reduction; comparison of deductible structures across insurance types; analysis of whether deductibles were changed as IUD popularity increased',
    'If actuarial necessity: deductible structure is coordination mechanism. If cost-shifting: deductible design is extraction mechanism targeting insured women. Affects whether constraint manifests as Tangled Rope or higher-extraction snare for insured victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_deductible_design_intent, empirical, 'Whether insurance deductible design reflects actuarial necessity or cost-shifting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iud_market_capture_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iud_tr_t0, iud_market_capture_mechanism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iud_tr_t5, iud_market_capture_mechanism, theater_ratio, 5, 0.48).
narrative_ontology:measurement(iud_tr_t10, iud_market_capture_mechanism, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(iud_be_t0, iud_market_capture_mechanism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(iud_be_t5, iud_market_capture_mechanism, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(iud_be_t10, iud_market_capture_mechanism, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iud_market_capture_mechanism, resource_allocation).
narrative_ontology:affects_constraint(iud_market_capture_mechanism, maternal_mortality_access).
narrative_ontology:affects_constraint(iud_market_capture_mechanism, reproductive_autonomy_policy).
narrative_ontology:affects_constraint(iud_market_capture_mechanism, healthcare_financing_structural_inequality).

% DUAL FORMULATION NOTE:
% The IUD market capture mechanism decomposes into three structurally distinct constraints: (1) pharmaceutical_market_consolidation (ε≈0.45, snare dynamics for low-income access), (2) professional_scope_gatekeeping (ε≈0.35, tangled rope with genuine training coordination + extraction through credential capture), (3) insurance_cost_shifting (ε≈0.50, snare for uninsured, tangled rope for insured). The unified story presents the integrated mechanism; decomposition reveals distinct ε values and failure modes. Each component has different resolution pathways: market consolidation requires antitrust intervention, scope gatekeeping requires evidence-based regulation change, cost-shifting requires insurance policy reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iud_market_capture_mechanism, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
