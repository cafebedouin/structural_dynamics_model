% ============================================================================
% CONSTRAINT STORY: preventive_medicine_implementation_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preventive_medicine_implementation_gap, []).

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
 *   constraint_id: preventive_medicine_implementation_gap
 *   human_readable: Preventive Medicine Implementation Gap
 *   domain: healthcare/public_health/behavioral_economics
 *
 * SUMMARY:
 *   The prevention-implementation gap is the structural mismatch between the
 *   known effectiveness of preventive medicine and the systematic
 *   underfunding and underutilization of prevention in healthcare systems.
 *   Despite overwhelming evidence that prevention (vaccination, screening,
 *   lifestyle intervention, chronic disease management) is cost-effective and
 *   health-effective compared to acute care, healthcare spending patterns
 *   heavily favor acute and episodic intervention. This constraint exhibits
 *   tangled_rope characteristics: a genuine coordination problem (prevention
 *   and acute care require integrated system design) coupled with extractive
 *   institutional incentives (acute care generates higher margins per
 *   episode, creating hidden disincentives for prevention). The constraint's
 *   extractiveness (0.52) reflects moderate but sustained extraction flowing
 *   from disease burden and underinsured populations toward acute care
 *   providers and pharmaceutical manufacturers. Suppression (0.58) reflects
 *   multiple barriers: cost (insurance gaps, out-of-pocket spending), access
 *   (geographic, informational, transportation), behavioral inertia (people
 *   don't pursue prevention even when available), and system structure
 *   (reimbursement incentives against prevention). Theater ratio (0.64)
 *   reflects that prevention has become increasingly performative: guidelines
 *   are issued, programs are funded, but implementation barriers remain
 *   systematically unaddressed. The system produces the appearance of
 *   prevention commitment while maintaining acute-care-centric financial
 *   flows.
 *
 * KEY AGENTS:
 *   - Uninsured/Underinsured Patients: Primary victim (powerless/trapped) — cannot afford preventive care, forced into acute care consumption
 *   - Disease Burden and Population Health: Primary victim (moderate/constrained) — public health commons bears cost of prevention failure
 *   - Federally Qualified Health Centers: Mixed (powerful/mobile) — coordinate prevention delivery but face extractive reimbursement constraints
 *   - Acute Care Hospital Systems: Primary beneficiary (institutional/arbitrage) — extract revenue from preventable disease
 *   - Pharmaceutical Industry: Secondary beneficiary (institutional/arbitrage) — benefit from higher disease incidence through sustained medication demand
 *   - Insurance Industry and Payers: Ambiguous (organized/constrained) — theory says prevention reduces costs; practice shows acute care margins are higher
 *   - Public Health Infrastructure: Degraded institutional actor (institutional/arbitrage) — performative disease surveillance, minimal functional prevention
 *   - Community-Based Prevention Programs: Emerging organized actor (organized/mobile) — building alternative pathways with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination failure and extractive institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preventive_medicine_implementation_gap, 0.52).
domain_priors:suppression_score(preventive_medicine_implementation_gap, 0.58).
domain_priors:theater_ratio(preventive_medicine_implementation_gap, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preventive_medicine_implementation_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(preventive_medicine_implementation_gap, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preventive_medicine_implementation_gap, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preventive_medicine_implementation_gap, tangled_rope).
narrative_ontology:human_readable(preventive_medicine_implementation_gap, "Preventive Medicine Implementation Gap").
narrative_ontology:topic_domain(preventive_medicine_implementation_gap, "healthcare/public_health/behavioral_economics").

domain_priors:requires_active_enforcement(preventive_medicine_implementation_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preventive_medicine_implementation_gap, acute_care_industry).
narrative_ontology:constraint_beneficiary(preventive_medicine_implementation_gap, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(preventive_medicine_implementation_gap, insurance_underwriters).
narrative_ontology:constraint_victim(preventive_medicine_implementation_gap, disease_burden_reduction).
narrative_ontology:constraint_victim(preventive_medicine_implementation_gap, low_income_populations).
narrative_ontology:constraint_victim(preventive_medicine_implementation_gap, preventive_care_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED/UNDERINSURED PATIENT (SNARE) — Structurally trapped by inability to afford preventive care; faces maximum extraction through acute episodes, emergency room visits, and compounding health costs. No exit: preventive medicine becomes unaffordable luxury, acute care becomes forced consumption. Suppression is high (cost barriers) and unilateral (flows toward acute care consumption).
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISEASE BURDEN / POPULATION HEALTH (SNARE) — The public health commons bears the full cost of prevention failure through accumulating disease burden, lost productivity, and emergency care utilization. Cannot organize or exit the constraint. Systematic underfunding of primary prevention while acute care captures costs. Experiences pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERALLY QUALIFIED HEALTH CENTERS (TANGLED ROPE) — Genuinely coordinate preventive care delivery to underserved populations AND extract through grant dependency, patient volume requirements, and restricted reimbursement models. Powerful enough to refuse extraction but face real incentive constraints. Experience is mixed coordination (health delivery) and constrained extraction (funding/reimbursement asymmetry).
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ACUTE CARE HOSPITAL SYSTEM (ROPE) — Primary beneficiary. Captures revenue from preventable disease through emergency department utilization and crisis interventions. For this actor, the constraint functions as pure coordination: the weak prevention system 'solves' their utilization problem by reliably generating acute-care demand. Zero suppression perceived — the actor experiences this as a beneficial market condition, not constraint.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHARMACEUTICAL & MEDICAL DEVICE INDUSTRY (ROPE) — Secondary beneficiary. Prevention-gap extracts directly through higher disease incidence and sustained medication/intervention demand. Experiences constraint as pure coordination mechanism: weak prevention reliably drives therapeutic markets. Theater is low from this perspective (the mechanism is transparent: weak prevention = sustained drug demand). Suppression is minimal — no barriers to capturing this benefit.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSURANCE INDUSTRY / PAYERS (TANGLED ROPE) — Ambiguous position. Insurance theory says prevention reduces long-term costs (coordination function). Actual practice: acute care extracts higher margins per visit; prevention incentives are weak due to adverse selection, moral hazard, and customer churn (constrained by actuarial realities). Genuine coordination function (incentivizing prevention) conflicts with extractive incentive structure (profit maximization through service volume). Requires active enforcement to shift toward prevention; current enforcement is minimal.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: PUBLIC HEALTH INFRASTRUCTURE (PITON) — Formal prevention apparatus (CDC, state health departments, community health programs) has degraded from functional prevention coordination to performative disease surveillance and guideline production. Theater ratio is high: guidelines are issued, programs are funded, but implementation barriers remain unaddressed. The infrastructure persists through institutional inertia and political legitimacy signaling, not through functional disease reduction. Piton classification: ε ≤ 0.25, theater ≥ 0.70.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: COMMUNITY-BASED PREVENTION PROGRAMS (SCAFFOLD) — Emergent programs (vaccination campaigns, workplace wellness, digital health monitoring, lifestyle interventions) show sunsetable extraction: as prevention becomes embedded in primary care norms and employer infrastructure, the temporary 'implementation gap' constraint decays. Sees current gap as solvable through normalization (sunset: 10-15 years as digital, employer-based, and community models scale). Low theater (direct health impact), explicit sunset clause.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the prevention-gap is a hybrid of genuine coordination failure (prevention and acute care require system-level integration) AND extractive institutional design (acute-care-heavy reimbursement incentivizes disease incidence, not prevention). The constraint exists because both functions (coordination + extraction) are simultaneously true: the system genuinely needs acute care capacity, AND it over-invests in acute care relative to prevention-optimal allocation. Resolution requires separating the genuine coordination need from the extractive surplus.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preventive_medicine_implementation_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(preventive_medicine_implementation_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(preventive_medicine_implementation_gap, TR),
    TR >= 0.70.

:- end_tests(preventive_medicine_implementation_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint channels resources away from prevention toward acute care through reimbursement incentives, insurance design, and patient cost-sharing structures. Extractiveness has risen from 0.35 to 0.52 over the interval as healthcare consolidation has concentrated acute-care capacity and margin-maximization has become more explicit. The value reflects that extraction is not total (prevention funding exists, public health infrastructure persists) but systematic and sustained. Suppression (0.58): High. Multiple overlapping barriers: cost barriers (uninsured/underinsured), access barriers (geographic, informational, transportation), behavioral inertia (people don't pursue prevention even when cost-free in universal-access systems), and system-structural barriers (reimbursement models penalize prevention). Theater ratio (0.64): Moderate-high. Prevention is increasingly discussed, recommended, and funded, but implementation remains minimal relative to the scale of the problem. Guidelines are issued (CDC, AHA, ACS), programs are funded (community health centers, workplace wellness), but actual prevention uptake lags far behind evidence-based recommendations. The gap between prevention discourse and prevention practice has widened (theater ratio rising from 0.48 to 0.64) as prevention has become more politically salient while implementation barriers remain.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence across power positions. For the powerless and trapped (uninsured patients), the gap appears as a snare: they cannot afford prevention and are forced into acute care consumption at high cost. For the disease burden (population health), it is also a snare: systematic prevention failure accumulates disease burden with no self-correction. For FQHCs and organized prevention programs, it is tangled_rope or scaffold: genuine coordination challenges exist (integration, access), but they can organize around solutions. For acute care providers and pharma, it is pure rope: prevention failure is experienced as a beneficial market condition that reliably generates demand. For the insurance industry, it is tangled_rope: the theory says prevention is cost-reducing, but the practice of acute-care-heavy reimbursement conflicts with prevention incentives. For public health infrastructure, it is piton: the formal apparatus persists through political legitimacy signaling while functional prevention capacity has degraded. For the analytical observer, it is tangled_rope at the civilizational level: a genuine system-integration problem (prevention and acute care need coordinated design) is exploited by extractive incentive structures (acute care over-investment relative to prevention-optimal allocation).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the prevention-gap constraint. Uninsured patients are full targets (d ≈ 0.95): they bear costs of prevention failure through acute-care consumption and have zero exit options (trapped). Disease burden is a full target (d ≈ 1.0): it is an abstract commons with no agency or exit. FQHCs are mixed (d ≈ 0.55): they are structurally mobile and have genuinely benefited from prevention mission alignment, but face reimbursement constraints that extract from their model. Acute care and pharma are beneficiaries (d ≈ 0.10): they extract from prevention failure through higher disease incidence and have high exit capacity (can switch models). Insurance industry is complex (d ≈ 0.50): they are nominally affected by prevention outcomes but their reimbursement incentives are structurally misaligned with prevention. The derived d values produce a perspectival gap: powerless/trapped agents see snare (high f(d)), institutional beneficiaries see rope (low f(d)), and mixed actors show tangled_rope depending on their specific structural relationship to prevention mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the prevention gap is genuinely hybrid: it contains BOTH a coordination problem (integrating prevention and acute care at the system level requires coordinated design) AND an extraction problem (reimbursement incentives systematically favor acute care over prevention, creating a financial extraction flow from prevention toward acute care). Neither snare nor rope alone captures the structure. The tangled_rope classification is correct: beneficiaries (acute care, pharma) experience pure coordination (the weak prevention system solves their utilization problem), while victims (disease burden, underinsured populations) experience extraction, and mixed actors (insurance, FQHCs) experience both simultaneously. The theater ratio (0.64) indicates that the constraint is not purely natural or purely institutional — prevention has become increasingly performative as policy discourse has separated from implementation, suggesting institutional inertia and policy theater rather than genuine natural law. Resolving the mandatrophy requires separating the genuine coordination need (system integration) from the extractive surplus (acute-care-biased reimbursement) and addressing them through distinct mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prevention_attribution_causality,
    'Does prevention cause observed health improvements or does selection bias (healthier populations self-select into prevention) account for measured outcomes?',
    'Randomized controlled trials of prevention programs; instrumental variable analysis of natural policy discontinuities; longitudinal tracking of prevention adopters vs matched controls with baseline health adjustment',
    'If prevention is highly causal: the gap is pure extraction (prevention works but is systematically underfunded). If selection is dominant: the gap partly reflects efficient sorting (prevention targets populations with lower baseline risk; acute care captures higher-need populations). Changes incentive attribution across beneficiary/victim classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prevention_attribution_causality, empirical, 'Whether prevention causally improves health or reflects selection effects').

omega_variable(
    time_horizon_discount_rate,
    'What discount rate is appropriate for comparing prevention costs (now) to acute care savings (future)? Should prevention be optimized for societal benefit or individual patient financial benefit?',
    'Policy analysis of implicit discount rates in reimbursement structures; comparison of prevention ROI at 3%, 5%, and 10% discount rates; survey of patient time preferences for prevention vs acute care spending',
    'At high discount rates (>8%): prevention appears uneconomical, justifying acute-care-heavy investment (snare perspective validated). At low rates (<3%): prevention is highly cost-effective, revealing the gap as pure extraction. Determines whether the constraint should optimize for individual financial incentives or population health.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(time_horizon_discount_rate, preference, 'Appropriate discount rate for prevention ROI evaluation').

omega_variable(
    reimbursement_incentive_specificity,
    'Are current acute-care-biased reimbursement incentives a deliberate extraction mechanism or an artifact of Fee-for-Service model agnosticism to outcome?',
    'Historical policy analysis of reimbursement design decisions; comparison of prevention-optimized vs acute-care-optimized payer models; structured interviews with payer strategy teams on prevention ROI calculations',
    'If deliberate extraction: snare classification is correct. If artifact: tangled_rope classification is correct — the system needs redesign but is not malicious. Affects policy remediation (regulatory enforcement vs structural redesign).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reimbursement_incentive_specificity, empirical, 'Whether prevention underfunding reflects deliberate extraction or design artifact').

omega_variable(
    behavioral_inertia_vs_structural_barrier,
    'How much of the prevention-gap reflects patient behavioral inertia (people don''t pursue prevention even when cost-free) vs structural barriers (lack of access, transportation, information)?',
    'Comparison of prevention uptake in universal-access systems (Canada, UK) vs systems with cost barriers (US); RCTs of behavioral nudges (appointment reminders, default enrollment) vs structural interventions (co-located services, transportation assistance); measurement of prevention uptake when all barriers removed',
    'If behavioral: targets enforcement at patient incentives (behavioral economics, choice architecture). If structural: targets enforcement at system redesign (access, convenience, integration). Affects which agent bears responsibility for the gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_inertia_vs_structural_barrier, empirical, 'Relative contribution of behavioral inertia vs structural barriers to prevention gap').

omega_variable(
    prevention_substitution_vs_addition,
    'Does prevention reduce acute care utilization (substitution) or add to total health spending through early detection of previously undiagnosed conditions (addition)?',
    'Longitudinal tracking of total health care spending (prevention + acute) before/after prevention program adoption; cost-effectiveness analysis comparing integrated vs separate prevention/acute systems',
    'If substitution: prevention is cost-saving and the gap is pure extraction. If addition: prevention increases total spending (earlier detection → more treatment), changing the economic case for prevention and the beneficiary/victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_substitution_vs_addition, empirical, 'Whether prevention substitutes for or adds to acute care spending').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preventive_medicine_implementation_gap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prev_med_tr_t0, preventive_medicine_implementation_gap, theater_ratio, 0, 0.48).
narrative_ontology:measurement(prev_med_tr_t10, preventive_medicine_implementation_gap, theater_ratio, 10, 0.58).
narrative_ontology:measurement(prev_med_tr_t20, preventive_medicine_implementation_gap, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(prev_med_be_t0, preventive_medicine_implementation_gap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prev_med_be_t10, preventive_medicine_implementation_gap, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(prev_med_be_t20, preventive_medicine_implementation_gap, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preventive_medicine_implementation_gap, resource_allocation).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, health_insurance_risk_selection).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, chronic_disease_burden_accumulation).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, primary_care_access_gap).

% DUAL FORMULATION NOTE:
% The prevention gap decomposes into three structurally distinct constraints: (1) Health Insurance Risk Selection — insurers optimize profit through enrollment selection rather than prevention incentives (ε ≈ 0.65, Snare); (2) Prevention Coordination Failure — system-level integration of prevention and acute care is underfunded and uncoordinated (ε ≈ 0.35, Tangled Rope); (3) Primary Care Access Gap — access barriers (geographic, financial, informational) prevent prevention utilization even when incentives align (ε ≈ 0.48, Tangled Rope). The prevention implementation gap is downstream of all three and represents their combined effect. Each story has its own ε value and perspectives; the implementation gap is their aggregate manifestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preventive_medicine_implementation_gap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
