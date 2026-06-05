% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Simulation-as-Proxy-Catastrophe: Drills as Functionally Equivalent Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   The constraint 'High-fidelity simulation constitutes genuine practice;
 *   drills are functionally equivalent to real catastrophic events for
 *   competence maintenance' is one reading of a contested kernel about how
 *   organizations maintain competence in catastrophe response (nuclear
 *   operators, airline pilots, maritime emergency crews, surgical teams). The
 *   reading instantiates the premise that simulation-based training with high
 *   fidelity can maintain the competence required to handle real catastrophic
 *   events without requiring actual catastrophes. This is structurally
 *   opposed to the sibling reading (catastrophe-as-necessary-selector), which
 *   holds that only exposure to real catastrophic events produces the
 *   competence needed. The third sibling (hybrid near-miss learning) occupies
 *   intermediate ground: competence is maintained via continuous feedback
 *   from quasi-catastrophic operational events that are analyzed, shared, and
 *   learned from without requiring full catastrophes. This story generates
 *   the simulation-as-proxy reading with its own ε-invariant classification,
 *   perspectives, and temporal trajectory. The constraint exhibits measurable
 *   theater_ratio drift (0.52 → 0.68) reflecting that simulation regimes have
 *   increasingly become compliance rituals rather than active competence
 *   verification. Extractiveness and suppression both rise over the interval,
 *   indicating that the regulatory apparatus has become more entrenched and
 *   resource-intensive, despite uncertain evidence that high-fidelity
 *   simulation actually predicts real catastrophic competence.
 *
 * KEY AGENTS:
 *   - Field Practitioners: Primary victim (powerless/trapped) — subject to mandatory drill schedules; career advancement tied to simulation performance; cannot exit the regime without abandoning professional advancement
 *   - Safety Officers / Risk Managers: Secondary actor (moderate/constrained) — experience mixed coordination (enabling competence verification) and extraction (career dependency on simulation infrastructure maintenance)
 *   - Regulatory Agencies: Primary beneficiary (institutional/arbitrage) — can verify compliance without waiting for real catastrophes; maintain audit authority and certification power; can arbitrage to alternative verification methods if simulation loses credibility
 *   - Simulation Infrastructure Vendors: Secondary beneficiary (institutional/arbitrage) — capture economic rents from mandated high-fidelity simulation; have arbitrage options across multiple regulatory domains and adjacent markets
 *   - Near-Miss Learning Coalition: Organized actor (organized/constrained) — building alternative competence maintenance pathways with sunset logic; currently constrained by incomplete data infrastructure but advancing toward replacement capability
 *   - Formal Safety Management System: Institutional apparatus (institutional/arbitrage) — maintains simulation compliance regime through performative audit and certification structures; inertial rather than functionally justified (piton classification)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement (simulation as law of organizational physics) while the structural incentives that justify simulation remain underdetermined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.38).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.52).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation-as-Proxy-Catastrophe: Drills as Functionally Equivalent Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e827dc2d-a87d-47ad-912b-a2d7a8fab162').
narrative_ontology:cs_kernel_codification('e827dc2d-a87d-47ad-912b-a2d7a8fab162', formalized).
narrative_ontology:cs_authority_grounding('e827dc2d-a87d-47ad-912b-a2d7a8fab162', extraction).
narrative_ontology:cs_interpretation_layer_present('e827dc2d-a87d-47ad-912b-a2d7a8fab162').
narrative_ontology:cs_reading_relation('e827dc2d-a87d-47ad-912b-a2d7a8fab162', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('e827dc2d-a87d-47ad-912b-a2d7a8fab162', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('e827dc2d-a87d-47ad-912b-a2d7a8fab162', foundational, simulation_transfer_validity).
narrative_ontology:cs_axiom_status(simulation_transfer_validity, holdable).
narrative_ontology:cs_axiom_grounding('e827dc2d-a87d-47ad-912b-a2d7a8fab162', simulation_transfer_validity, empirically_contingent).
narrative_ontology:cs_axiom('e827dc2d-a87d-47ad-912b-a2d7a8fab162', foundational, competence_decay_without_practice_significant).
narrative_ontology:cs_axiom_status(competence_decay_without_practice_significant, holdable).
narrative_ontology:cs_axiom_grounding('e827dc2d-a87d-47ad-912b-a2d7a8fab162', competence_decay_without_practice_significant, empirically_contingent).
narrative_ontology:cs_reference_frame('e827dc2d-a87d-47ad-912b-a2d7a8fab162', scheduled_drill_competence_maintenance).
narrative_ontology:cs_drift_state('e827dc2d-a87d-47ad-912b-a2d7a8fab162', contemporary_high_reliability_operations, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e827dc2d-a87d-47ad-912b-a2d7a8fab162', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_agencies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_vendors).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, field_practitioners).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operational_resource_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD PRACTITIONER (SNARE) — Mandatory simulation drills remove time and resources from operational work. The constraint traps practitioners in a regime where they must prove competence via proxy (simulation performance) regardless of actual operational mastery. Career advancement depends on drill performance in artificially controlled scenarios; practitioners cannot exit the regime without abandoning career advancement. High experienced extraction with minimal coordination benefit — the practitioner bears the cost of simulation infrastructure they may perceive as artificial or misaligned with real catastrophic decision-making.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAFETY OFFICER (TANGLED ROPE) — Experiences genuine coordination benefit: drills enable distributed competence verification without waiting for actual catastrophes. But also experiences extraction: constrained to maintain simulation regimes and compliance documentation; career and budget dependent on the simulation infrastructure's perceived necessity; cannot simply declare practitioners competent through observation of operational skill. Mixed: real coordination function (preventing competence decay) alongside asymmetric extraction (time, budget, career dependency).
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AGENCY (ROPE) — Benefits structurally from simulation-as-proxy: can verify compliance without waiting for real catastrophes; can set and enforce clear drill metrics; generates institutional justification (audit trails, certification logs). Sees the constraint as pure coordination: 'We solved the competence verification problem by institutionalizing drills.' Minimal suppression experienced — regulatory agencies have authority to set the rules. Arbitrage exit: if simulation loses credibility, regulators can shift to alternative verification (observation, certification, near-miss analysis) without fundamental disruption.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SIMULATION VENDOR (ROPE) — Primary economic beneficiary. The constraint institutionalizes demand for high-fidelity simulation hardware, software, and maintenance services. Vendor has arbitrage options: if one regulatory domain shifts away from simulation-based compliance, other domains remain; vendor can pivot to adjacent markets (training, research, entertainment simulation). Sees the constraint as coordination: 'We enabled safe competence verification by building realistic training platforms.' Suppression minimal — vendors have no regulatory obligation; they operate in a market where demand is generated by regulatory mandate.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NEAR-MISS LEARNING COALITION (SCAFFOLD) — Organized agents (safety scientists, high-reliability practitioners, some regulatory bodies) see simulation-as-proxy as a temporary measure with a sunset: structured collection and analysis of near-miss events provides superior competence feedback compared to drills on artificial schedules. This reading sees the constraint as a bridge mechanism: simulation maintains competence baseline while near-miss infrastructure matures and institutionalizes. Has sunset logic: as near-miss reporting becomes normative and data-rich, the simulation mandate becomes less critical — practitioners maintain competence through continuous feedback from quasi-catastrophic operational events rather than scheduled drills. Suppression is real but temporary: the constraint is being deliberately built with an exit pathway.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL SAFETY MANAGEMENT (PITON) — The institutional apparatus treating simulation-as-proxy has substantial theater_ratio: audit schedules, certification logs, competency reports, and compliance documentation often dominate over actual verification that practitioners' drill performance predicts operational competence. The formal system persists through institutional inertia — simulation drills have become part of the compliance landscape because they are measurable and auditable, not because causally validated evidence shows they prevent actual catastrophes. Theater ratio 0.68 reflects the performative content: many organizations conduct drills because they are mandated, calibrate difficulty to pass rates rather than real decision load, and maintain compliance records that are themselves rarely reviewed for predictive validity. The apparatus remains because alternatives haven't fully replaced it, not because it demonstrably works.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, competence decay under catastrophic conditions is a universal property of human skill: any competence requiring active deployment degrades without practice; complex emergency response is no exception. This reading naturalizes simulation-as-proxy as an immutable law: 'You cannot maintain catastrophic-response competence without practicing catastrophic response under controlled conditions.' However, the structural data reveals this as likely false summit: the constraint's beneficiaries (regulatory agencies, simulation vendors) have institutional incentives to naturalize simulation as necessary, and the empirical evidence base (whether drill performance actually predicts real catastrophic decision quality) is substantially underdetermined. This perspective instantiates the oracle gap: the analytical observer's native instruments (formal training science) cannot detect the structural incentives that make simulation infrastructure appear necessary.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, TR),
    TR >= 0.70.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits moderate base extraction because simulation infrastructure produces genuine coordination benefits (preventing competence decay without waiting for real catastrophes) alongside asymmetric resource flows (vendors capture rents, practitioners bear time costs). The value reflects that beneficiaries (regulators, vendors) experience net coordination benefit with minimal cost, while victims (practitioners) experience extraction partly justified by real competence needs. If the competence need is genuine and transfer validated, extractiveness may be accurate reflection of a tangled rope with real coordination content. If competence transfer is low and the apparatus is primarily regulatory theater with vendor rent-seeking, the true extractiveness is underestimated (should be >0.50). Suppression (0.52): Moderate-high. Practitioners face mandatory participation in drills; career advancement depends on drill performance; alternative competence verification methods (apprenticeship, near-miss learning, operational mastery observation) are not accepted as substitutes under current regulatory regimes. Suppression rises over the interval as the formal safety management system becomes more entrenched (0.40 → 0.52), indicating that exit alternatives are shrinking rather than expanding. Theater ratio (0.68): High and rising. The constraint shows substantial performative content: drill schedules often follow regulatory compliance calendars rather than operational competence needs; drill difficulty is calibrated to pass rates; certification logs and audit trails document compliance rather than validating competence transfer; post-drill debriefing and competence assessment are often abbreviated or formulaic. Theater ratio rises over the interval (0.52 → 0.68) as the apparatus becomes more institutionalized and divorced from evidence of functional competence maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across observer positions. The regulatory agency and simulation vendor see rope (coordination mechanism solving the verification problem); the safety officer sees tangled rope (mixed coordination and extraction); the field practitioner sees snare (extraction with minimal coordination benefit); the organized near-miss coalition sees scaffold (temporary mechanism with sunset); the formal safety apparatus sees piton (degraded performative ritual); the analytical observer risks seeing mountain (naturalized necessity) when the structural data suggests the constraint's justification is empirically underdetermined. The gap reveals that the constraint's classification depends critically on unstable empirical assumptions: whether drill performance predicts real catastrophic competence (omega 1), whether competence decay is a real organizational problem (omega 5), and whether the beneficiaries' incentives drive the constraint's persistence despite weak functional evidence (omega 5 again).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural relationships to the extraction flow. Regulatory agencies and vendors occupy beneficiary positions with arbitrage-level exit (institutional power + arbitrage → low d → negative χ from their perspective); they experience the constraint as coordination with minimal cost. Field practitioners occupy victim positions with trapped exit (powerless power + trapped → high d → high χ from their perspective); they experience the constraint as pure extraction despite its coordination content. Safety officers occupy hybrid positions (moderate power + constrained exit): they benefit from the verification capability but are constrained to maintain the infrastructure, producing intermediate d values and experienced extractiveness. The near-miss coalition occupies organized positions with constrained exit: they are building alternative pathways but remain constrained by current regulatory requirements, producing intermediate d with strong directional pressure toward exit as near-miss infrastructure matures. The formal safety apparatus occupies institutional positions with arbitrage options: it can shift compliance metrics if simulation loses credibility, producing low d despite the performative content. The analytical observer occupies a position at d ≈ 0.72 (observer-relative), which maps to χ ≈ 1.15 via f(d), making the universal scope observation moderately extractive in the observer's experience — the observer must navigate the full complexity of competing readings despite not being a beneficiary of any single reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by embedding the core ambiguity into omega variables rather than into the classification. The tangled-rope classification holds if competence decay is real, transfer is validated, and the coordination benefits exceed the extraction costs. But the classification is contingent on three empirical facts that remain substantially underdetermined: (1) whether drill performance predicts real catastrophic competence; (2) whether competence decay without drills is significant enough to justify the resource cost; (3) whether near-miss infrastructure could achieve equivalent competence maintenance at lower cost. If (1) and (2) are both affirmative, tangled rope is correct. If (1) is negative or (2) is negative, the constraint reclassifies toward snare (pure extraction masquerading as coordination). If (3) is affirmative, the constraint may still be tangled rope but with explicit sunset (scaffold classification from the near-miss coalition perspective is not wrong — it is a different reading of the same kernel with different empirical assumptions). The mandatrophy resolves by recognizing that the constraint is a reading of a contested kernel, not a universal structural fact. The reading is causally stable (well-reasoned from its empirical premises), empirically fragile (depends on validating omega variables), and politically contingent (supported by beneficiary interests aligned with vendors and regulators).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drill_to_reality_transfer_validity,
    'Does performance on high-fidelity simulation drills causally predict competence in actual catastrophic conditions?',
    'Longitudinal correlation analysis: track practitioners through multiple drills and subsequent actual operational events; measure whether high drill performers have lower error rates, better decision speed, and higher success outcomes in real catastrophic events. Compare to practitioners trained via alternative methods (near-miss analysis, apprenticeship with experienced operators, narrative case studies).',
    'If transfer is high (>0.70 correlation): simulation-as-proxy classification holds; competence decay management via drills is structurally sound. If transfer is low (<0.40 correlation): the classification degrades toward piton (performative) or snare (extraction); near-miss alternatives become dominant. If transfer is domain-specific (high for some catastrophe types, low for others): constraint family requires decomposition into per-domain stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drill_to_reality_transfer_validity, empirical, 'Causal relationship between simulation performance and real catastrophic competence').

omega_variable(
    catastrophe_frequency_assumption,
    'What is the true baseline catastrophe frequency that justifies the scheduling and resource cost of simulation drills?',
    'Historical analysis of catastrophe rates in the domain (aviation, nuclear, maritime, etc.); Bayesian calculation of risk reduction from simulation-based competence maintenance vs. baseline; cost-benefit analysis comparing drill resource cost to prevented catastrophe cost.',
    'If catastrophes are sufficiently rare (< 1 per 1000 operational years): simulation may be net-negative in cost-benefit (better to allocate resources to design redundancy). If catastrophes are frequent enough (> 5 per 1000 operational years): simulation competence maintenance becomes clearly justified. Frequency threshold directly affects classification: low-frequency domains may show snare characteristics; high-frequency domains show rope. The same constraint type may collapse into piton if the catastrophe frequency assumption is overestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_frequency_assumption, empirical, 'Baseline catastrophe frequency justifying drill scheduling').

omega_variable(
    near_miss_infrastructure_maturity,
    'Is near-miss learning infrastructure mature enough to replace simulation-as-proxy as the primary competence maintenance mechanism?',
    'Assessment of near-miss reporting systems, data analysis capacity, and feedback loop speed in the domain; comparison of competence decay rates in organizations using primarily near-miss learning vs. simulation-based learning; measurement of time-to-feedback for near-miss analysis vs. drill-based feedback.',
    'If mature (data-rich, fast feedback, causally analyzed): scaffold perspective is correct; simulation mandate has an achievable sunset. If immature (sparse reporting, slow analysis, narrative-only): near-miss learning cannot yet replace simulation; tangled-rope classification is stable. Timeline estimates shift: mature near-miss infrastructure suggests 5-10 year sunset; immature suggests 20+ year persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(near_miss_infrastructure_maturity, empirical, 'Maturity and effectiveness of near-miss learning infrastructure').

omega_variable(
    simulation_fidelity_threshold,
    'What level of simulation fidelity is actually necessary to achieve competence transfer? Is the entire high-fidelity apparatus required, or do low-fidelity paper/tabletop scenarios achieve comparable transfer at lower cost?',
    'Experimental comparison: randomized cohorts trained on high-fidelity simulation vs. low-fidelity scenarios vs. narrative-based learning; measure subsequent real-world competence outcomes and cost per unit of competence gain.',
    'If high-fidelity is necessary: vendor beneficiary classification and extraction values hold. If low-fidelity or narrative achieve comparable transfer: extractiveness drops significantly (because the simulation infrastructure becomes over-built); vendor captures rents from regulatory compliance with unnecessary complexity; constraint reclassifies toward snare (the apparatus is pure rent-seeking on top of a simpler coordination mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Minimum simulation fidelity required for competence transfer').

omega_variable(
    reading_kernel_contestation,
    'Is competence maintenance a genuine functional need that drills satisfy, or does the regulatory mandate create artificial demand for simulation infrastructure?',
    'Compare organizational competence decay and catastrophe rates in domains with mandatory simulation vs. domains relying on apprenticeship, near-miss analysis, and operational experience. Assess whether organizations in mandate-free domains show higher competence decay or catastrophe rates attributable to inadequate competence vs. other factors (design failures, resource constraints, environmental volatility).',
    'If competence decay is real and significant: this reading (simulation-as-proxy) is functionally grounded; sibling reading (catastrophe-as-necessary-selector) loses ground. If competence decay is minimal or indistinguishable from zero: the reading kernel is primarily a construct of regulatory/vendor incentives; catastrophe-as-necessary-selector gains plausibility; this reading forecloses rather than coexists. Omega resolves the fundamental structural ambiguity: is simulation-as-proxy solving a real problem, or is it solving a problem created by removing natural catastrophic feedback?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Whether competence decay is a genuine functional problem or regulatory construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carp_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.52).
narrative_ontology:measurement(carp_tr_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 10, 0.62).
narrative_ontology:measurement(carp_tr_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(carp_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(carp_be_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(carp_be_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(carp_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(carp_su_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(carp_su_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__hybrid_near_miss_learning).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_compliance_theater).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, vendor_mandated_infrastructure).

% DUAL FORMULATION NOTE:
% The catastrophe-avoidance-retention kernel decomposes into three constraint stories with different ε values and reading assumptions. This story (simulation-as-proxy) assumes competence decay is real and simulation transfer is validated. The catastrophe-as-necessary-selector reading assumes neither; it classifies as snare (extraction masquerading as necessity) under the premise that real catastrophic feedback is irreplaceable. The hybrid-near-miss reading assumes competence comes from distributed continuous feedback rather than scheduled drills, producing different beneficiary/victim structures and extraction flows. All three are stories about the same kernel (maintaining catastrophe response competence) with ε values that vary by empirical assumptions. They are linked here as a constraint family to enable contamination propagation analysis: if near-miss infrastructure matures (validating hybrid-near-miss), both this story and catastrophe-as-necessary-selector will experience pressure toward reclassification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
