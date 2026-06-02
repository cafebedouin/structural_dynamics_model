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
 *   The prevention-implementation gap represents a fundamental structural
 *   mismatch in healthcare systems between known evidence (prevention is
 *   cost-effective and health-effective) and institutional practice (acute
 *   and episodic care dominates funding and prestige). This constraint
 *   exhibits Tangled Rope structure: genuine coordination function exists
 *   (preventing disease reduces overall system burden, saves lives, improves
 *   population health) alongside systematic extraction (acute care providers,
 *   hospitals, and pharmaceutical manufacturers benefit from keeping
 *   prevention underfunded, maintaining a steady stream of treatable disease
 *   cases). The constraint has intensified over 30 years as healthcare costs
 *   have risen, acute care specialization has deepened, and fee-for-service
 *   reimbursement has become dominant. The theater ratio (0.65) reflects
 *   performative prevention activities: public health agencies conduct
 *   screening and reporting, insurance companies advertise preventive
 *   benefits, healthcare systems sponsor wellness programs, yet actual
 *   prevention funding and implementation capacity remain marginal. The
 *   suppression mechanisms are both structural (reimbursement incentives
 *   favor acute care, insurance barriers to preventive access) and
 *   informational (patient awareness of preventive options remains low,
 *   medical education concentrates on disease treatment). The constraint's
 *   extractiveness has risen from 0.35 to 0.52 over the interval as
 *   institutional capture has intensified — hospitals and pharmaceutical
 *   companies have consolidated power over payers and public health agencies.
 *   An emerging organized coalition (value-based care reformers, employers,
 *   progressive insurance companies, preventive health startups) is building
 *   alternative pathways (capitation, bundled payments, outcomes-based
 *   models) that could shift funding toward prevention at timescales of 10-15
 *   years, suggesting a genuine sunset mechanism.
 *
 * KEY AGENTS:
 *   - Preventable-Disease Population: Primary victim (powerless/trapped) — systematically denied access to known effective prevention; bears full cost of preventable illness
 *   - Primary Care Providers and Public Health Workers: Secondary victim (moderate/constrained) — underfunded, deprioritized, career paths favor acute-care specialization; also coordinate some prevention function
 *   - Acute Care Providers and Hospital Systems: Primary beneficiary (institutional/arbitrage) — sustained by steady stream of acute interventions; high reimbursement rates; incentivized to suppress prevention
 *   - Pharmaceutical Manufacturers: Secondary beneficiary (institutional/arbitrage) — profit from chronic disease treatment markets; would lose revenue if prevention reduced disease incidence; participate in suppression
 *   - Insurance Companies and Employers: Mixed position (organized/mobile) — fund healthcare, bear costs of preventable disease, but face short-term cash flow pressures and adverse selection; organized actors with exit paths
 *   - Public Health Agencies: Degraded institutional actor (institutional/constrained) — performative role, historical coordination function atrophied, persist through institutional obligation
 *   - Value-Based Care Reform Coalition: Organized actors (organized/constrained) — arXiv of healthcare: alternative payment models building exit paths with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional misalignment as inevitable consequence of human risk perception
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preventive_medicine_implementation_gap, 0.52).
domain_priors:suppression_score(preventive_medicine_implementation_gap, 0.68).
domain_priors:theater_ratio(preventive_medicine_implementation_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preventive_medicine_implementation_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(preventive_medicine_implementation_gap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(preventive_medicine_implementation_gap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preventive_medicine_implementation_gap, tangled_rope).
narrative_ontology:human_readable(preventive_medicine_implementation_gap, "Preventive Medicine Implementation Gap").
narrative_ontology:topic_domain(preventive_medicine_implementation_gap, "healthcare/public_health/behavioral_economics").

domain_priors:requires_active_enforcement(preventive_medicine_implementation_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preventive_medicine_implementation_gap, acute_care_providers).
narrative_ontology:constraint_beneficiary(preventive_medicine_implementation_gap, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(preventive_medicine_implementation_gap, hospital_systems).
narrative_ontology:constraint_victim(preventive_medicine_implementation_gap, preventable_disease_population).
narrative_ontology:constraint_victim(preventive_medicine_implementation_gap, public_health_infrastructure).
narrative_ontology:constraint_victim(preventive_medicine_implementation_gap, long_term_population_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREVENTABLE-DISEASE POPULATION (SNARE) — Structurally trapped by healthcare system incentive structures that do not reach them until disease is established. Cannot exit the system; bears full cost of preventable illness. No voice in resource allocation decisions. Maximum experienced extraction — knows preventive care exists but cannot access or afford it.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIMARY CARE PROVIDERS AND PUBLIC HEALTH WORKERS (TANGLED ROPE) — Constrained by resource limitations, insurance reimbursement structures, and time pressures. Also benefit from prevention-focused care models that reduce acute crises and emergency burden. Mixed position: some coordination function (disease prevention reduces system strain) alongside genuine extraction (underfunded relative to acute care, career prestige concentration in specialist practice).
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ACUTE CARE PROVIDERS AND HOSPITAL SYSTEMS (ROPE) — Primary institutional beneficiary. Experiences the constraint as coordination: the systematic underfunding of prevention maintains a steady stream of acute interventions that sustain acute care revenue. High fees and reimbursement rates for procedures, surgeries, and emergency care. Effective arbitrage position — can shift between acute and preventive focus but economically incentivized toward acute.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURERS (ROPE) — Secondary institutional beneficiary. Benefit from chronic disease treatment markets (statins, antihypertensives, diabetes medications, cancer treatments). Prevention that reduces disease incidence would reduce long-term treatment revenue. Coordination function exists: pharmaceutical companies provide treatments that manage preventable diseases, enabling life extension and quality of life maintenance. But underlying incentive structure opposes prevention at scale.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH AGENCIES (PITON) — Institutional actors with degraded function and inertial persistence. Public health infrastructure (CDC, local health departments) performed critical prevention coordination during mid-20th century (vaccination campaigns, sanitation, infectious disease control). Now largely performative: underfunded, understaffed, with theater-heavy mandates (reporting, surveillance) while prevention programs lack sustained funding. Persist through institutional obligation rather than functional necessity in eyes of healthcare system. Theater ratio high due to emphasis on compliance reporting over actual prevention delivery.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: VALUE-BASED CARE REFORM COALITION (SCAFFOLD) — Organized agents (value-based care advocates, insurance companies experimenting with outcomes-based reimbursement, preventive health startups, employee wellness programs) are building alternative payment mechanisms with sunset logic. Capitation models, outcomes-based bundling, and direct primary care are creating pathways where prevention funding increases because providers capture long-term savings. Low effective extraction because these organized actors have agency and exit paths. Sunset clause: as value-based care matures (estimated 10-15 years), the acute-care extraction mechanism's force diminishes.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INSURANCE COMPANIES AND EMPLOYERS (TANGLED ROPE) — Organized agents bearing costs but also capturing some benefits. Insurance companies and employers both fund healthcare and bear costs of preventable disease (claims, lost productivity). Incentivized toward prevention at long-term timescales but face short-term cash flow pressures and adverse selection risks (prevention investments benefit competitors if enrollees switch). Mobile exit option: can shift between prevention-focused and acute-focused plans. Mixed position reflects both coordination function (disease prevention is economically rational) and extraction (structural misalignment means prevention underfunded relative to actuarial benefit).
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, the prevention-implementation gap appears as an immutable property of how healthcare systems coordinate under uncertainty. Prevention benefits are distributed across populations and delayed in time; acute care benefits are concentrated and immediate. This asymmetry is inherent to how humans process risk and allocate resources. However, the structural data contradicts the mountain classification — identifiable beneficiaries (acute care providers, hospitals, pharmaceutical companies) profit from prevention underfunding. The engine will compute this as a false summit, revealing that the 'inherent to human risk perception' framing naturalizes what is actually extractive institutional design.
constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preventive_medicine_implementation_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preventive_medicine_implementation_gap, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. The constraint systematically extracts from preventable-disease populations and public health capacity to sustain acute-care provider revenue. The level reflects that the extraction is substantial but not total — some prevention does occur, some coordination function persists, and value-based care alternatives are emergent. The rising trajectory (0.35 → 0.52) indicates that institutional capture has intensified as healthcare consolidation has concentrated acute-care provider power and pharmaceutical company influence over payment systems. Suppression (0.68): High. Multiple suppression mechanisms operate simultaneously: reimbursement structures (prevention receives 1-3% of healthcare spending despite evidence supporting 10-20% allocation); informational barriers (patients and primary care providers lack access to evidence of prevention effectiveness); authorization requirements (insurance companies require prior approval for preventive services); and time poverty (providers lack time to deliver prevention in fee-for-service settings). The rising trajectory (0.55 → 0.68) reflects that suppression mechanisms have strengthened as healthcare complexity has increased and institutional coordination has weakened. Theater ratio (0.65): Moderately high. Public health agencies conduct performative surveillance and reporting; healthcare systems sponsor wellness programs with low utilization; insurance companies advertise preventive benefits without removing access barriers; medical education teaches about prevention while career paths reward acute-care specialization. Theater has increased over time as public expectations of prevention have risen without corresponding institutional restructuring to deliver it.
 *
 * PERSPECTIVAL GAP:
 *   The powerless perspective (Snare) and the institutional beneficiary perspective (Rope) present contradictory phenomenologies of the same structural phenomenon. From the beneficiary's frame, prevention-focused messaging and occasional screening programs are coordination — communication about health, enabling better outcomes. From the trapped perspective, this same activity is performative theater masking systematic exclusion from actual prevention access and resources. The gap reveals that the 'coordination' beneficiaries experience depends entirely on suppression mechanisms preventing transparent comparison of prevention vs acute-care effectiveness. When prevention access barriers are visible, the Rope classification collapses into Snare for those with power to act. The Scaffold classification (value-based reformers) coexists with Tangled Rope and Snare at current time — reformers see a sunset path while victims see ongoing extraction. The resolution of this gap depends on whether value-based care reforms can scale to make prevention the economically dominant strategy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions: beneficiaries (acute care, pharmaceuticals) receive positive institutional support and arbitrage options, producing low d (around 0.15-0.20), experienced as Rope from their perspective. Victims (preventable-disease population) are trapped with no exit options, producing high d (around 0.90-0.95), experienced as Snare from their perspective. Mixed actors (insurance, employers) occupy moderate positions (d around 0.55-0.65) reflecting both beneficiary and victim roles. Organized reformers (value-based care coalition) have mobile exit options and partial agency, producing moderate d (around 0.50-0.60). Public health agencies, despite institutional power, face constrained exits due to budget dependence and legal mandates, producing d around 0.65-0.70. The Tangled Rope classification at base level reflects that the constraint simultaneously coordinates disease prevention (genuine shared function) and extracts from vulnerable populations, with power asymmetrically concentrated in acute-care beneficiaries. The classification holds because both properties are structurally present — it is not a pure coordination problem (victims are actively harmed) but neither is it pure extraction (disease prevention remains a real collective good that the constraint nominally serves).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through a clear structural mechanism: beneficiaries of acute care have institutional power to suppress prevention coordination at scale while maintaining nominal commitment to prevention as a public good. The 'we support prevention' rhetoric accompanied by 1-3% funding allocation is the mandatrophy — the institutional commitment to preventing disease explicitly coexists with structural choices that ensure disease remains treatable and profitable. The engine's false summit detector will flag the analytical observer's 'natural law' classification as naturalization of extractive design: the supposed inevitability of prevention underfunding masks institutional choice. The Scaffold classification provides the mandatrophy's temporal resolution: as value-based care models mature, the acute-care extraction mechanism loses its economic rationale. Capitation, bundling, and outcomes-based models align provider incentives with prevention, enabling institutions to credibly commit to prevention without sacrificing revenue. The constraint's current Tangled Rope classification is stable only under fee-for-service reimbursement; the Scaffold sunset mechanism, if successful, removes the fundamental extraction driver by changing what 'coordination' actually means (coordination around population health rather than acute-care volume).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discounting_mechanism_ambiguity,
    'Is prevention underfunding driven by rational time-discounting of future health benefits (individuals and systems legitimately discount distant outcomes), or by institutional extraction (beneficiaries deliberately suppress prevention to maintain acute-care revenue)?',
    'Comparative analysis of prevention funding in systems with different institutional structures: single-payer (UK NHS, Canada) vs fee-for-service (US) vs capitation-based (managed care). If UK/Canada still underfund prevention at similar rates despite different incentives, suggests discounting. If substantial difference, suggests extraction.',
    'If discounting dominates: reclassify as Rope (coordination problem around temporal misalignment) rather than Tangled Rope/Snare. If extraction dominates: current classifications confirmed. Mixed mechanism: omega resolution required for policy design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discounting_mechanism_ambiguity, empirical, 'Whether prevention underfunding reflects rational discounting or institutional extraction').

omega_variable(
    prevention_effectiveness_measurement_ambiguity,
    'Do cost-effectiveness estimates for prevention reflect genuine health economic benefit, or do they systematically undercount implementation costs (behavioral change barriers, systems redesign, coordination overhead) and overestimate compliance rates?',
    'Prospective randomized trials comparing cost-effectiveness estimates (pre-implementation) with realized costs and outcomes (post-implementation); analysis of compliance attrition in prevention programs; subgroup analysis of prevention effectiveness by socioeconomic status and health literacy.',
    'If estimates are accurate and implementation barriers are surmountable: prevention gap is purely extractive (Snare/Tangled Rope). If estimates are optimistic: some underfunding reflects rational skepticism about delivery feasibility; constraint may be Piton (degraded coordination) rather than active extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_effectiveness_measurement_ambiguity, empirical, 'Whether prevention cost-effectiveness estimates reflect true implementable benefit').

omega_variable(
    moral_hazard_suppression_mechanisms,
    'Do insurance companies and healthcare systems suppress prevention information or access to avoid moral hazard (insured populations becoming complacent about risk if prevention is available), or is information suppression driven by cost-containment extraction?',
    'Comparative messaging analysis across insurance products (high-deductible vs comprehensive plans); tracking of patient communication about preventive benefits; analysis of prior-authorization barriers for preventive services; patient survey data on awareness of preventive coverage.',
    'If moral hazard suppression is primary: constraint is Rope (coordination failure due to adverse incentives). If cost-containment dominates: constraint is Tangled Rope/Snare (active extraction via suppression). Pattern reveals whether suppression is defensive or extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_suppression_mechanisms, empirical, 'Whether prevention suppression is driven by moral hazard concerns or cost-extraction motives').

omega_variable(
    organizational_inertia_vs_rational_equilibrium,
    'Is the acute-care bias a stable rational equilibrium (acute care does maximize short-term system revenue and institutional prestige under current conditions), or is it organizational inertia (systems persist in acute-care focus despite rational alternatives being available)?',
    'Historical analysis of prevention funding by age of healthcare institution (newer systems, de novo built institutions, reform pilots); simulation models of healthcare system payoffs under different prevention/acute ratios; case studies of system transitions to prevention-focused models and barriers encountered.',
    'If equilibrium: constraint is Rope or Tangled Rope reflecting genuine trade-offs. If inertia: constraint is Piton (degraded, theater-dependent). Distinction affects policy intervention: inertia requires active disruption; equilibrium requires incentive restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_inertia_vs_rational_equilibrium, empirical, 'Whether acute-care bias is stable equilibrium or organizational inertia').

omega_variable(
    value_based_care_sustainability,
    'Can value-based care models (capitation, bundled payments, outcomes-based reimbursement) actually scale to address the prevention-implementation gap, or do they encounter fundamental adverse selection and sustainability limits?',
    'Longitudinal tracking of value-based care adoption rates; analysis of populations served by value-based vs fee-for-service providers; profitability and sustainability data for value-based models over 10+ year timescale; assessment of whether value-based providers continue prevention investment or revert to acute care during financial stress.',
    'If value-based models scale: Scaffold classification is confirmed — organized exit path with sunset trajectory. If models hit sustainability limits: Scaffold reclassifies to Tangled Rope/Snare (temporary relief masked as structural reform). Affects confidence in prevention-gap resolution timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_based_care_sustainability, empirical, 'Whether value-based care models can sustainably fund prevention at scale').

omega_variable(
    prevention_population_heterogeneity,
    'Do prevention program benefits distribute uniformly across population, or do they concentrate in relatively healthy, motivated populations while missing high-risk, hard-to-reach populations that need prevention most?',
    'Subgroup analysis of prevention program participation and outcomes by socioeconomic status, health literacy, chronic disease prevalence, geographic isolation; cost-effectiveness analysis stratified by baseline risk; tracking of equity metrics in prevention initiatives.',
    'If benefits concentrate in already-healthy populations: prevention gap widens health inequities (constraint becomes worse for powerless perspective). If benefits distribute broadly: prevention is genuine coordination good. Affects assessment of whether prevention is Rope (true coordination) vs Snare (benefits for healthy, extraction from vulnerable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prevention_population_heterogeneity, empirical, 'Whether prevention benefits reach or exclude high-risk populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preventive_medicine_implementation_gap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prev_med_theater_t0, preventive_medicine_implementation_gap, theater_ratio, 0, 0.5).
narrative_ontology:measurement(prev_med_theater_t15, preventive_medicine_implementation_gap, theater_ratio, 15, 0.62).
narrative_ontology:measurement(prev_med_theater_t30, preventive_medicine_implementation_gap, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(prev_med_extractiveness_t0, preventive_medicine_implementation_gap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prev_med_extractiveness_t15, preventive_medicine_implementation_gap, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(prev_med_extractiveness_t30, preventive_medicine_implementation_gap, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prev_med_suppression_t0, preventive_medicine_implementation_gap, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prev_med_suppression_t15, preventive_medicine_implementation_gap, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(prev_med_suppression_t30, preventive_medicine_implementation_gap, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preventive_medicine_implementation_gap, resource_allocation).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, chronic_disease_burden).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, health_equity_concentration).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, primary_care_workforce_degradation).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, pharmaceutical_pricing_extraction).
narrative_ontology:affects_constraint(preventive_medicine_implementation_gap, insurance_adverse_selection).

% DUAL FORMULATION NOTE:
% The prevention-implementation gap decomposes into multiple structurally distinct constraints sharing a common driver (reimbursement misalignment): prevention underfunding (this story, ε=0.52, Tangled Rope), acute-care specialization cascade (ε=0.48, Tangled Rope with different victim set), pharmaceutical chronic-disease profiteering (ε=0.68, Snare), and insurance adverse selection dynamics (ε=0.42, Rope). Each has distinct beneficiary/victim structures but all propagate from the underlying fee-for-service payment model. They are linked through network.affects_constraints rather than merged because their ε values differ and their local classification dynamics (which agents perceive extraction, which coordination) differ by perspective domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preventive_medicine_implementation_gap, institutional, 0.18).
constraint_indexing:directionality_override(preventive_medicine_implementation_gap, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
