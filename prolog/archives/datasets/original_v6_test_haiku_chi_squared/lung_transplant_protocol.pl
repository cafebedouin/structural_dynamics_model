% ============================================================================
% CONSTRAINT STORY: lung_transplant_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lung_transplant_protocol, []).

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
 *   constraint_id: lung_transplant_protocol
 *   human_readable: Lung Transplant Allocation Protocol
 *   domain: social/healthcare/organ_allocation
 *
 * SUMMARY:
 *   The lung transplant allocation protocol creates a structural constraint
 *   on access to scarce organs through a system designed to be medically
 *   rational and procedurally fair, but which systematically advantages
 *   patients with institutional resources, medical advocacy, and
 *   documentation capacity. The constraint is not primarily about the
 *   shortage of organs — that is a natural scarcity problem — but about the
 *   allocation mechanism's design, which embeds extraction mechanisms in the
 *   exception process, advantage accumulation for high-volume centers, and
 *   systematic disadvantage for under-resourced populations. The constraint
 *   exhibits snare characteristics: victims are trapped (disease progression
 *   cannot wait), suppression is high (institutional barriers to exception
 *   requests, geographic disadvantages, documentation requirements), and
 *   beneficiaries are institutions with resources to navigate the system.
 *   Extractiveness has increased over time as the protocol has matured and as
 *   high-volume centers have learned to optimize exception requests and
 *   patient referral patterns. Theater ratio remains moderate because the
 *   protocol maintains genuine functionality (it does allocate organs
 *   according to stated criteria) but with increasing performative elements
 *   (exception requests that succeed based on institutional capacity rather
 *   than medical urgency). The constraint is not natural law — other
 *   countries allocate organs via different mechanisms with different equity
 *   outcomes — and thus does not qualify as mountain despite occasional
 *   naturalization.
 *
 * KEY AGENTS:
 *   - Waitlisted patients without medical exception: Primary victims (powerless/trapped) — disease progression cannot wait; no exit except death or continued deterioration
 *   - Under-resourced regional programs: Secondary victims (moderate/constrained) — systematic disadvantage in exception processes and algorithmic allocation; limited capacity for institutional advocacy
 *   - High-volume transplant centers: Primary extractors and beneficiaries (institutional/constrained) — capture proportionally more organs through exception requests, selective referrals, and procedural expertise; benefit from protocol's coordination function
 *   - Organ Procurement Network (OPO) administration: Institutional beneficiary (institutional/arbitrage) — benefits from coordination function; sees protocol as solving collective action problem
 *   - Exception review process: Extraction conduit (institutional/constrained) — ostensibly provides equity mechanism but functions as institutional advantage mechanism; theater ratio reflects performative element
 *   - Equity reform coalition: Organized agents (organized/mobile) — advocacy groups pursuing algorithmic revision and geographic distribution reforms; sees sunset pathway
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional constraint as inherent to organ scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lung_transplant_protocol, 0.58).
domain_priors:suppression_score(lung_transplant_protocol, 0.72).
domain_priors:theater_ratio(lung_transplant_protocol, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lung_transplant_protocol, extractiveness, 0.58).
narrative_ontology:constraint_metric(lung_transplant_protocol, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lung_transplant_protocol, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lung_transplant_protocol, snare).
narrative_ontology:human_readable(lung_transplant_protocol, "Lung Transplant Allocation Protocol").
narrative_ontology:topic_domain(lung_transplant_protocol, "social/healthcare/organ_allocation").

% --- Structural relationships ---
narrative_ontology:constraint_victim(lung_transplant_protocol, waitlisted_patients_without_medical_exception).
narrative_ontology:constraint_victim(lung_transplant_protocol, patients_from_under_resourced_regions).
narrative_ontology:constraint_victim(lung_transplant_protocol, patients_without_institutional_advocacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAITLISTED PATIENT WITHOUT EXCEPTION (SNARE) — Trapped by disease progression and organ scarcity; no exit option except death or continued deterioration. Protocol prioritizes exception requests that require institutional resources or medical documentation most patients lack. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(lung_transplant_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDER-RESOURCED REGIONAL PROGRAM (SNARE) — Constrained by limited institutional capacity to generate favorable exception documentation or participate in national exception review processes. Systematic disadvantage embedded in protocol that rewards institutional knowledge and resources. d≈0.85, f(d)≈1.25, σ=0.9 → χ≈0.64.
constraint_indexing:constraint_classification(lung_transplant_protocol, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-VOLUME TRANSPLANT CENTER (TANGLED ROPE) — Benefits from protocol's coordination function (standardized allocation reduces chaos; enables outcome tracking) while also extracting through exception requests, institutional advocacy, and selective patient referrals. Constrained by regulatory requirements but experienced enough to navigate them. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(lung_transplant_protocol, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OPO ADMINISTRATION (ROPE) — Primary beneficiary of the coordination function: allocation protocol standardizes process, reduces chaos, enables outcome data collection and quality assurance. Sees itself as solving collective action problem. d≈0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(lung_transplant_protocol, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXCEPTION REVIEW PROCESS (PITON) — Ostensibly provides equity mechanism for medical urgency not captured by standard protocol. In practice, serves as extraction conduit: success depends on institutional resources for documentation, specialist advocacy, and appeals expertise. Theater ratio=0.48 reflects that exception reviews occur but with low functional separation from standard allocation. Many exceptions denied or delayed by bureaucratic bottlenecks despite documented medical need. d≈0.30, f(d)≈0.25, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(lung_transplant_protocol, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EQUITY REFORM COALITION (SCAFFOLD) — Organized advocacy groups, regional networks, and policy researchers see the allocation protocol as a temporary constraint now being addressed through algorithmic revision (LAS 2.0, geographic distribution reforms). Reform pathways visible and actively pursued; sunset clause implicit in documented policy revision cycles. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.11.
constraint_indexing:constraint_classification(lung_transplant_protocol, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL SCARCITY VIEW (MOUNTAIN) — Some observers naturalize the constraint as inherent to organ scarcity: insufficient donors will always require allocation mechanisms that necessarily burden someone. This perspective treats the protocol as unavoidable natural law. However, ε=0.58 and suppression=0.72 contradict mountain requirements — allocation mechanisms could be designed to minimize extraction while maintaining coordination. Engine detects false summit.
constraint_indexing:constraint_classification(lung_transplant_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lung_transplant_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lung_transplant_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lung_transplant_protocol, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lung_transplant_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lung_transplant_protocol, TR),
    TR >= 0.70.

:- end_tests(lung_transplant_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The protocol extracts disproportionately from patients without institutional resources. Exception requests, which nominally provide equity mechanism, function as extraction conduit because success depends on resources for documentation, specialist advocacy, and appeals expertise. Geographic distribution algorithms may disadvantage lower-density regions. Extractiveness has increased from 0.42 to 0.58 over the interval as centers have learned to optimize the system and as documentation requirements have increased. Suppression (0.72): High. Multiple barriers prevent trapped patients from obtaining organs: disease progression cannot wait; information asymmetries about exception pathways; institutional resource requirements; geographic disadvantages; complexity of documentation and appeals processes; knowledge that some centers are more successful at exceptions creates incentive to travel (further barrier for under-resourced patients). Theater ratio (0.48): Moderate. The allocation protocol maintains genuine functionality (it does allocate organs according to stated criteria and reduces chaos), but performative elements have emerged: exception reviews often succeed based on institutional capacity for documentation rather than medical urgency; algorithmic allocation may be gamed through patient selection and referral patterns; the exception process itself has become partially theatrical (appears to provide equity but actually rewards institutional resources). Theater has increased from 0.35 to 0.48 as the system has matured.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival divergence. High-volume centers see coordination (Tangled Rope) — the protocol solves the collective action problem of organ allocation and enables outcome tracking, while also providing extraction opportunities through exception requests. OPO administration sees coordination (Rope) — the protocol standardizes process and reduces chaos. Under-resourced programs and waitlisted patients without resources see extraction (Snare) — trapped with no meaningful exit, facing systematic disadvantage embedded in protocol design. The exception process appears to provide equity (Piton perspective: degraded ritual that should work but doesn't) but actually functions as extraction mechanism. Reform advocates see a solvable problem with sunset pathways (Scaffold) through algorithmic revision and geographic distribution reform. The analytical observer risks naturalizing the constraint as inherent to organ scarcity (Mountain), but the structural data reveals this as false: allocation mechanisms are contingent institutional choices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Waitlisted patients: Victims + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Under-resourced programs: Victims + constrained → d≈0.85, f(d)≈1.25. High extraction, though slightly less than fully trapped because some workarounds exist (traveling to higher-success centers, though this increases cost/burden). High-volume centers: Mixed beneficiary/victim status + constrained → d≈0.50, f(d)≈0.65. They benefit from coordination function and exception process optimization but face regulatory constraints. OPO administration: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary; experiences protocol as solving collective action problem. Exception review: Institutional + constrained → d≈0.30, f(d)≈0.25. Low effective extraction because process is partially performative; most cases follow standard protocol despite exception pathway existing. Reform coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction; organized agents see exit pathway through policy revision. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit classification reveals naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION IS APPROPRIATE: The protocol satisfies all snare gates (ε=0.58 ≥ 0.46, suppression=0.72 ≥ 0.60, χ≈0.68 ≥ 0.66), and the structural data supports snare interpretation from the primary victim perspective. The mandatrophy question is: does the protocol serve a coordination function that partially redeems the extraction, pushing it toward Tangled Rope? Analysis: The protocol does provide genuine coordination benefits (it reduces chaos, enables outcome tracking, creates standardized process). However, these benefits accrue to institutional actors (OPO, high-volume centers) rather than to the trapped victims. From the victim perspective, the coordination benefits are instrumental to the extraction system, not alternative purposes. From the institutional beneficiary perspective, the constraint may be Tangled Rope (coordination + asymmetric extraction). From the victim perspective, it is pure Snare (extraction with no coordination benefit for them). The perspectival gap is justified: different agents experience structurally different phenomena. The classification resolves as Snare for victims, Tangled Rope for beneficiaries — the presheaf over the agent set captures the full structure. No false coordination can hide behind the snare label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exception_documentation_bias,
    'What proportion of eligible exceptions under the protocol go unrequested due to institutional resource barriers vs documentation knowledge gaps vs patient/physician lack of awareness?',
    'Longitudinal audit of exception eligibility vs requests; stratified by transplant center resources and regional access to exception navigators; patient surveys on awareness of exception pathways',
    'If barrier is primarily awareness: targeted information campaigns could reduce extraction. If barrier is primarily resources: exception process itself is extractive mechanism. If mixed: tiered extraction model where institutional capacity determines exception success independent of medical criteria.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exception_documentation_bias, empirical, 'Proportion of eligible exceptions unrequested due to institutional barriers').

omega_variable(
    algorithmic_fairness_degradation,
    'Do post-revision allocation algorithms (LAS 2.0+) achieve the stated equity gains or do new extraction mechanisms emerge around algorithmic gaming and selective referral patterns?',
    'Comparison of allocation equity metrics pre- and post-revision; analysis of waitlist time distributions across institutional types and regional resources; examination of patient outcomes by socioeconomic status over time',
    'If genuine equity improvement observed: scaffold perspective validated, sunset is real. If new mechanisms emerge: constraint type remains snare but with obfuscated extraction mechanism. Classification unchanged but mechanism complexity increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_fairness_degradation, empirical, 'Whether algorithmic revisions achieve stated equity improvements or spawn new extraction mechanisms').

omega_variable(
    scarcity_elasticity_threshold,
    'At what organ donation rate would allocation protocol suppression decrease materially, and are there policy interventions that could push toward that threshold?',
    'Comparative analysis of countries with different donation rates (opt-in vs opt-out, living donor expansion) and their allocation protocol suppression scores; modeling of extraction reduction as function of supply',
    'If threshold achievable through policy: constraint is contingent (Tangled Rope → Rope over time). If threshold requires biological/cultural shifts unlikely on generational timescale: constraint is quasi-natural (Snare with limited remediation pathways). Affects mandatrophy resolution strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_elasticity_threshold, empirical, 'Supply elasticity threshold at which allocation protocol suppression materially decreases').

omega_variable(
    hidden_allocation_channels,
    'Do formal allocation mechanisms account for all significant organ distribution (e.g., living donation, paired exchange, bridge therapies, waitlist management via patient selection), or do untracked channels constitute material portion of real allocation?',
    'Comprehensive audit of organ utilization pathways; tracking of living vs deceased donor distribution; analysis of waitlist management practices across centers; network analysis of patient flow between centers',
    'If formal protocol captures >85% of allocation: snare classification is accurate. If <70%: measured extractiveness is understated; true snare is more severe and hidden behind formal opacity. Theater increases; suppression harder to measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_allocation_channels, empirical, 'Proportion of organ allocation occurring through formal protocol vs untracked channels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lung_transplant_protocol, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lung_tr_t0, lung_transplant_protocol, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lung_tr_t10, lung_transplant_protocol, theater_ratio, 10, 0.42).
narrative_ontology:measurement(lung_tr_t20, lung_transplant_protocol, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(lung_be_t0, lung_transplant_protocol, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lung_be_t10, lung_transplant_protocol, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(lung_be_t20, lung_transplant_protocol, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lung_transplant_protocol, resource_allocation).
narrative_ontology:affects_constraint(lung_transplant_protocol, organ_procurement_shortage).
narrative_ontology:affects_constraint(lung_transplant_protocol, transplant_center_outcome_accountability).
narrative_ontology:affects_constraint(lung_transplant_protocol, living_donor_solicitation_pressure).

% DUAL FORMULATION NOTE:
% The lung transplant protocol is downstream of fundamental organ scarcity (organ_procurement_shortage, ε≈0.15 Mountain) but represents a distinct extractive constraint layered onto the scarcity problem. The allocation mechanism itself is contingent institutional design, not natural law. Upstream: whether donation rates can expand (affects whether allocation suppression is essential or contingent). Downstream: how allocation decisions drive transplant center patient selection and outcome reporting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lung_transplant_protocol, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
