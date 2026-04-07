% ============================================================================
% CONSTRAINT STORY: 1948_truman_national_health_insurance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1948_truman_national_health_insurance, []).

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
 *   constraint_id: 1948_truman_national_health_insurance
 *   human_readable: Truman's Proposed National Health Insurance System (1945-1950)
 *   domain: healthcare/political_economy
 *
 * SUMMARY:
 *   President Truman's proposed national health insurance system (1945-1950)
 *   represents a transformative attempt to shift healthcare financing from
 *   individual catastrophic vulnerability and private insurance extraction to
 *   collective insurance pooling with federal administration. The constraint
 *   embodies a fundamental political-economic reorganization: converting
 *   healthcare from a market commodity to a social right, funded through
 *   progressive taxation and distributed via federal authority. The
 *   constraint exhibits both genuine coordination function (pooling risk,
 *   guaranteeing payment, reducing administrative fragmentation) and
 *   significant asymmetric extraction (redistribution from wealthy to poor,
 *   from healthy to sick; federal control over medical practice; suppression
 *   of private insurance industry). The proposal failed politically due to
 *   organized opposition from the American Medical Association, private
 *   insurers, and conservative politicians, but established the intellectual
 *   and political template that would eventually produce Medicare (1965) and
 *   the ongoing tension between market-based and universal-access healthcare
 *   systems.
 *
 * KEY AGENTS:
 *   - Uninsured Patients: Primary victims (powerless/trapped) — vulnerable to medical bankruptcy; depend on charity care or emergency departments; have no exit from healthcare dependency
 *   - Employed Workers with Private Coverage: Mixed victims/beneficiaries (moderate/constrained) — benefit from pooling but face wage suppression and employment lock-in for coverage continuity
 *   - Large Medical Providers: Primary beneficiaries (institutional/arbitrage) — gain guaranteed payment, reduced billing administration, predictable revenue; have negotiating power
 *   - Private Insurance Industry: Victims (powerful/arbitrage) — face displacement by unified federal system; suppress threat through political lobbying
 *   - State Medical Societies: Secondary victims (institutional/arbitrage) — lose control over fee-setting and practice norms; maintain gatekeeping functions but as theater
 *   - Labor Union Movement: Mixed beneficiaries/victims (organized/mobile) — gain universal coverage reducing wage-slavery but lose health-benefits negotiating leverage
 *   - Wealthy Individuals: Victims (powerful/arbitrage) — face net fiscal redistribution through progressive healthcare financing; can exit through political suppression or private supplemental insurance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1948_truman_national_health_insurance, 0.38).
domain_priors:suppression_score(1948_truman_national_health_insurance, 0.65).
domain_priors:theater_ratio(1948_truman_national_health_insurance, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1948_truman_national_health_insurance, extractiveness, 0.38).
narrative_ontology:constraint_metric(1948_truman_national_health_insurance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(1948_truman_national_health_insurance, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1948_truman_national_health_insurance, tangled_rope).
narrative_ontology:human_readable(1948_truman_national_health_insurance, "Truman's Proposed National Health Insurance System (1945-1950)").
narrative_ontology:topic_domain(1948_truman_national_health_insurance, "healthcare/political_economy").

domain_priors:requires_active_enforcement(1948_truman_national_health_insurance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1948_truman_national_health_insurance, low_income_citizens).
narrative_ontology:constraint_beneficiary(1948_truman_national_health_insurance, medical_providers).
narrative_ontology:constraint_beneficiary(1948_truman_national_health_insurance, federal_government).
narrative_ontology:constraint_victim(1948_truman_national_health_insurance, private_insurance_industry).
narrative_ontology:constraint_victim(1948_truman_national_health_insurance, wealthy_individuals).
narrative_ontology:constraint_victim(1948_truman_national_health_insurance, state_medical_societies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT (SNARE) — Faces medical bankruptcy from catastrophic illness; cannot exit the trap of healthcare dependency without insurance. Employment-based coverage is the only non-governmental pathway, but millions lack stable employment. The constraint extracts maximum cost burden during health crises while offering no exit.
constraint_indexing:constraint_classification(1948_truman_national_health_insurance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYED WORKER WITH PRIVATE COVERAGE (TANGLED ROPE) — Benefits from employer-provided insurance coordination (pooling, negotiation) but faces extraction through: (a) wages suppressed by insurance premium costs, (b) lock-in to employment for coverage continuity, (c) gaps in coverage for catastrophic illness or unemployment. Genuine coordination function mixed with asymmetric extraction.
constraint_indexing:constraint_classification(1948_truman_national_health_insurance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE MEDICAL PROVIDER (ROPE) — Experiences national insurance as pure coordination: guaranteed payment mechanisms, reduced billing administration, predictable revenue enable capital investment and scale. Large providers have arbitrage options (can negotiate terms, exclude payers, provide concierge services). Net beneficiary.
constraint_indexing:constraint_classification(1948_truman_national_health_insurance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIVATE INSURANCE INDUSTRY (SNARE) — Would face existential extraction: transition from private profit center to regulated utility or complete displacement. Despite powerful position and arbitrage options (lobbying, blocking legislation, creating alternatives), the constraint's implementation would eliminate the fundamental extraction mechanism (risk selection, premium pricing, claims denial). Powerful victim with exit options through political suppression.
constraint_indexing:constraint_classification(1948_truman_national_health_insurance, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE MEDICAL SOCIETY (PITON) — Maintains professional gatekeeping and licensing authority under national insurance, but loses control over fee-setting and practice norms. Functions degrade from regulatory power to performative credentialing. Institutional inertia: professional licensing rituals persist (board exams, continuing education) but lose real economic leverage. Theater ratio reflects that much of the society's function becomes ceremonial under federal payment control.
constraint_indexing:constraint_classification(1948_truman_national_health_insurance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR UNION MOVEMENT (TANGLED ROPE) — Benefits from universal coverage reducing wage-slavery dynamics (workers no longer tethered to employers for health security), but loses negotiating leverage over health benefits — a primary union organizing issue. Genuine coordination (universal access) mixed with asymmetric extraction (loss of health-benefit bargaining power). Mobile exit: unions can shift organizing focus to wages and conditions.
constraint_indexing:constraint_classification(1948_truman_national_health_insurance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SCAFFOLD) — From a civilizational view, universal health insurance is a temporary coordination solution addressing a specific post-war structural problem: soldiers returning from WWII created political demand for universal healthcare; industrialization had decoupled healthcare from family/community provision; market failure in health insurance (adverse selection, moral hazard) was widely recognized. The analytical observer sees this as a time-bound policy response, not a permanent institutional form. Future exit: technological change (diagnostic automation, direct-to-consumer care) and political shifts may render centralized insurance obsolete.
constraint_indexing:constraint_classification(1948_truman_national_health_insurance, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1948_truman_national_health_insurance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1948_truman_national_health_insurance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1948_truman_national_health_insurance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1948_truman_national_health_insurance, TR),
    TR >= 0.70.

:- end_tests(1948_truman_national_health_insurance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The system exhibits genuine coordination function (pooling, guaranteed payment, universal access) alongside significant redistribution (from wealthy to poor, from healthy to sick). The extractiveness value reflects that the redistribution is intentional policy design, not hidden extraction — the system is transparent about cross-subsidization. The initial value (0.52) reflects the transition period where pre-existing extraction mechanisms are disrupted; as the system stabilizes (0.38), extractiveness declines because the coordination function becomes primary. Suppression (0.65): High. Multiple mechanisms suppress alternative arrangements: (a) mandatory participation eliminates individual exit, (b) federal administration consolidates control, (c) physician licensing and fee controls constrain provider alternatives, (d) political suppression of private insurance lobby. However, suppression is not total — wealthy individuals retain arbitrage (supplemental insurance, private practice). Theater ratio (0.35): Low-moderate. The system's functions are substantive rather than performative — actual risk pooling occurs, actual payments are made, actual medical care is delivered. Unlike degraded institutions where form divorces from function, national insurance maintains alignment. The theater component emerges in professional credentialing rituals and regulatory administration that exceed their functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across power levels. The uninsured patient sees existential Snare — medical vulnerability is inescapable. The employed worker sees Tangled Rope — genuine benefits mixed with extraction. The large provider sees Rope — pure coordination benefit. The private insurer sees Snare — the constraint eliminates their profit mechanism. The medical society sees Piton — regulatory authority degrades to ceremonial gatekeeping. The labor union sees Tangled Rope — universal access gained at cost of bargaining leverage. The analytical observer sees Scaffold — the constraint is a time-bound response to post-war structural conditions. The perspectival gap is structurally grounded: each agent has different exit options and different relationships to the redistribution mechanism. No single type captures all perspectives because the constraint's core function is redistribution, which is inherently asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the constraint's redistribution and payment mechanisms. Uninsured patients (powerless/trapped) have maximum victim directionality — they depend on the system for access and face medical bankruptcy without it (d ≈ 0.95). Employed workers (moderate/constrained) have high victim directionality offset by beneficiary status from pooling (d ≈ 0.65). Large providers (institutional/arbitrage) have low victim directionality — they are net beneficiaries with exit options through negotiating leverage (d ≈ 0.20). Private insurers (powerful/arbitrage) have paradoxical positioning: nominally powerful with exit options, but facing existential extraction from the constraint's implementation (d ≈ 0.75). Wealthy individuals have beneficiary directionality from health security but victim directionality from progressive financing (d ≈ 0.55). The labor movement has beneficiary directionality from universal coverage but victim directionality from lost bargaining leverage (d ≈ 0.50). The analytical observer (analytical/analytical) has neutral directionality reflecting observation stance (d ≈ 0.72 canonical).
 *
 * MANDATROPHY ANALYSIS:
 *   POLITICAL FAILURE CASE: Truman's national health insurance failed politically despite resolving the mandatrophy through clear identification of beneficiaries (uninsured patients, medical providers), victims (private insurers, wealthy individuals), and coordination function (risk pooling, guaranteed payment). The constraint's classification as Tangled Rope was analytically correct but politically unstable: the victims (private insurance, medical societies) had sufficient concentrated power to suppress implementation through lobbying, professional opposition, and framing the constraint as 'socialism.' The false summit detector would have flagged the 'national health is a natural right' framing as potentially naturalizing a contingent institutional choice — this framing was indeed deployed politically but failed to overcome organized opposition. The mandatrophy is resolved by noting that all six types are legitimate perspectival readings: the constraint genuinely coordinates (Rope for providers), genuinely redistributes (Tangled Rope for workers), genuinely threatens (Snare for insurers), and genuinely addresses market failure (Scaffold for civilizational observers). The constraint's failure was political (organized extraction industry suppressed coordination mechanism), not analytical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_enforcement_capacity,
    'Does the federal government have the institutional capacity to administer nationwide health insurance without creating bottlenecks that recreate suppression through bureaucracy?',
    'Comparative analysis of federal program implementation (Medicare, VA, Social Security); measurement of claims processing timelines and denial rates under different administrative structures',
    'If capacity is adequate: Tangled Rope classification holds, extraction is manageable. If capacity fails: Snare classification dominates — federal monopoly replaces market extraction with bureaucratic suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_enforcement_capacity, empirical, 'Federal administrative capacity for nationwide health insurance').

omega_variable(
    physician_supply_elasticity,
    'Will physicians respond to flat-fee payment by reducing hours, limiting patient access, or exiting medicine — thereby recreating scarcity-based extraction through supply constraints?',
    'Historical comparison with countries implementing physician fee controls (UK NHS, Canada); measurement of physician labor supply changes; tracking of wait times for specialist care',
    'If physicians maintain supply: coordination mechanism stabilizes. If physicians reduce supply: Snare from patient perspective — scarcity replaces price as extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physician_supply_elasticity, empirical, 'Whether physicians reduce supply under centralized payment').

omega_variable(
    political_sustainability,
    'Can universal health insurance survive as a permanent institutional form, or is it inherently unstable against rent-seeking and regulatory capture by residual private interests?',
    'Historical tracking of post-implementation policy stability; measurement of lobbying expenditure and legislative amendments; comparison with other countries'' durability of universal systems',
    'If sustainable: Tangled Rope remains stable. If unstable: Constraint degrades to Piton (performative universal system maintained alongside residual private extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_sustainability, conceptual, 'Political sustainability of universal health insurance against rent-seeking').

omega_variable(
    technology_disruption,
    'Do future diagnostic and treatment technologies (telemedicine, AI diagnosis, decentralized medicine) render centralized insurance coordination obsolete, enabling the sunset of this constraint?',
    'Technological roadmap analysis; measurement of direct-to-consumer healthcare adoption; assessment of whether centralized payment remains necessary for future medical practice',
    'If technology enables decentralization: Scaffold perspective confirmed — natural sunset path exists. If technology requires centralization: Constraint becomes permanent institutional form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_disruption, empirical, 'Whether future technology enables constraint sunset').

omega_variable(
    redistribution_acceptability,
    'Is the constraint''s redistribution mechanism (from healthy to sick, from wealthy to poor, from young to old) politically sustainable, or will wealthy-healthy populations exit through political channels?',
    'Polling data on support for universal insurance by income and health status; measurement of political mobilization against constraint; tracking of exit mechanisms (private supplemental insurance, geographic relocation)',
    'If acceptability remains high: coordination mechanism persists. If acceptability declines: constraint fragments into parallel tiered system (Piton: public system becomes theater while private extraction re-emerges).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_acceptability, preference, 'Political sustainability of cross-subsidization mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1948_truman_national_health_insurance, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(truman_tr_t0, 1948_truman_national_health_insurance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(truman_tr_t2, 1948_truman_national_health_insurance, theater_ratio, 2, 0.37).
narrative_ontology:measurement(truman_tr_t5, 1948_truman_national_health_insurance, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(truman_be_t0, 1948_truman_national_health_insurance, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(truman_be_t2, 1948_truman_national_health_insurance, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(truman_be_t5, 1948_truman_national_health_insurance, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1948_truman_national_health_insurance, resource_allocation).
narrative_ontology:affects_constraint(1948_truman_national_health_insurance, medicare_implementation_1965).
narrative_ontology:affects_constraint(1948_truman_national_health_insurance, medicaid_state_variation).
narrative_ontology:affects_constraint(1948_truman_national_health_insurance, aca_hybrid_mandate).

% DUAL FORMULATION NOTE:
% Truman's proposal is upstream of Medicare (1965) and the ongoing ACA debate. It establishes the template for universal-access healthcare systems; downstream constraints (Medicare, Medicaid, ACA) are partial implementations or compromises reflecting the failed Truman constraint. The proposal's extractiveness (0.38) reflects the coordination function itself; actual implementations show higher extractiveness due to political compromises introducing new extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1948_truman_national_health_insurance, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
