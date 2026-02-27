% ============================================================================
% CONSTRAINT STORY: hollow_state_syndrome
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hollow_state_syndrome, []).

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
 *   constraint_id: hollow_state_syndrome
 *   human_readable: The Shell Governance Mirage
 *   domain: political/organizational
 *
 * SUMMARY:
 *   The hollow state syndrome describes the structural condition where a
 *   state or major institution maintains formal legal authority and
 *   democratic legitimacy while outsourcing its core operational functions to
 *   private contractors, non-state actors, or quasi-governmental entities.
 *   This constraint exhibits the signature of a tangled rope: it provides
 *   genuine coordination benefits (specialized expertise, operational
 *   capacity, efficiency gains) while simultaneously extracting through
 *   accountability fragmentation, lock-in effects, and the capture of service
 *   margins by private actors. The constraint's theater ratio (0.81, rising
 *   from 0.52 over 20 years) reflects the increasing performative nature of
 *   oversight institutions: parliamentary committees hold hearings, watchdog
 *   organizations publish reports, executives claim efficiency gains, yet
 *   actual accountability mechanisms degrade as responsibility diffuses
 *   across public-private boundaries that no single actor controls. The
 *   extractiveness (0.58, rising from 0.35) captures the growing asymmetry:
 *   citizens depend on services they cannot compel, contractors accumulate
 *   proprietary lock-in, executives avoid political costs through outsourcing
 *   while maintaining formal authority. The suppression (0.68) reflects both
 *   structural barriers to exit (citizens trapped in jurisdiction, frontline
 *   workers fragmented across contractors, oversight bodies blocked by
 *   confidentiality claims) and asymmetric information (contractors possess
 *   proprietary knowledge of service delivery; executives use information
 *   gaps to avoid responsibility).
 *
 * KEY AGENTS:
 *   - Citizens Dependent on Services (powerless/trapped): Primary victims. Nominally entitled to public services but actual delivery depends on contractor profit incentives and state supervision capacity, both weakened by hollow state structure. No legal recourse against contractors (shielded by immunity) and limited political leverage against states.
 *   - Public Accountability as Institutional Capacity (abstract victim): The democratic function of holding government responsible degrades as outsourcing fragments the chain of command. No single entity can be held accountable for systemic failures.
 *   - Private Contractors (institutional/arbitrage): Primary beneficiaries. Gain exclusive contracts, profit margins, proprietary lock-in, and liability shields. Can exit unprofitable contracts or rebid, giving them genuine arbitrage exit.
 *   - State Executive/Political Leadership (powerful/arbitrage): Beneficiary. Outsourcing transfers direct political risk (service failures blamed on contractors, not leaders), reduces apparent budget size, provides plausible deniability. Maintains formal authority while avoiding operational costs.
 *   - Frontline State Workers (moderate/constrained): Mixed position. Benefit from contractor expertise and technical resources; constrained by employment fragmentation, loss of career continuity, training lock-in to proprietary systems.
 *   - Democratic Oversight Coalition (organized/constrained): Organized victims. Parliamentary committees, watchdog NGOs, journalists attempt accountability but are blocked by contractor confidentiality claims, legal protections, and fragmented responsibility chains.
 *   - Transparency and Reconstitution Movement (organized/mobile): Emerging agents attempting to establish sunset mechanisms and rebuild direct state capacity through mandatory performance benchmarking and periodic reconstitution of service chains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hollow_state_syndrome, 0.58).
domain_priors:suppression_score(hollow_state_syndrome, 0.68).
domain_priors:theater_ratio(hollow_state_syndrome, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hollow_state_syndrome, extractiveness, 0.58).
narrative_ontology:constraint_metric(hollow_state_syndrome, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hollow_state_syndrome, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hollow_state_syndrome, tangled_rope).
narrative_ontology:human_readable(hollow_state_syndrome, "The Shell Governance Mirage").
narrative_ontology:topic_domain(hollow_state_syndrome, "political/organizational").

domain_priors:requires_active_enforcement(hollow_state_syndrome).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hollow_state_syndrome, private_contractors).
narrative_ontology:constraint_beneficiary(hollow_state_syndrome, state_executive).
narrative_ontology:constraint_victim(hollow_state_syndrome, public_accountability).
narrative_ontology:constraint_victim(hollow_state_syndrome, citizens_dependent_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CITIZEN (SNARE) — Cannot exit the jurisdiction or compel service quality. Nominally entitled to public services but actual delivery depends on contractor profit incentives. Bears full cost of service degradation with no legal recourse (private contractors have liability shields). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(hollow_state_syndrome, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC ACCOUNTABILITY COMMONS (SNARE) — The institutional capacity for democratic oversight is the primary victim. Outsourcing fragments accountability chains: the state claims authority while contractors claim confidentiality (proprietary methods, trade secrets). No single entity bears responsibility. The commons has no agent to defend it. d≈0.98, f(d)≈1.50, σ=1.2 → χ≈0.87.
constraint_indexing:constraint_classification(hollow_state_syndrome, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FRONTLINE STATE WORKER (TANGLED ROPE) — Constrained by employment security (contractors shed staff during downturns) and training fragmentation (private systems don't cross-train across sectors). But benefits from specialized contractor resources, technical expertise, and performance bonus structures that coordinated hiring alone wouldn't provide. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.59.
constraint_indexing:constraint_classification(hollow_state_syndrome, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRIVATE CONTRACTOR (ROPE) — Benefits from exclusive contracts, reduces competition through lock-in via proprietary systems, captures efficiency gains through margin extraction. But also provides genuine coordination function: standardized methods, technical capacity beyond state budgets, operational redundancy. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary through arbitrage exit (can leave contracts, rebid, sell to competitors).
constraint_indexing:constraint_classification(hollow_state_syndrome, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE EXECUTIVE (ROPE) — Benefits from outsourcing: avoids direct political cost of service failures (contractors absorb blame), reduces apparent budget and headcount (shifted to private sector), maintains plausible deniability. Coordination benefit: contractors provide expert capacity that civil service hiring cannot quickly scale. d≈0.05, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Negative effective extraction = net beneficiary through arbitrage (can renegotiate or replace contractors).
constraint_indexing:constraint_classification(hollow_state_syndrome, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEMOCRATIC OVERSIGHT COALITION (PITON) — Organized agents (parliamentary committees, watchdog NGOs, journalists) attempt accountability but are blocked by contractor confidentiality claims, legal protections, and the fragmentation of responsibility across multiple entities. Their oversight ritual persists (hearings, reports, media investigations) but has degraded function: findings rarely translate to enforcement because the formal chain of command has been broken. theater_ratio=0.81 indicates high performative content. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.48.
constraint_indexing:constraint_classification(hollow_state_syndrome, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TRANSPARENCY MOVEMENT (SCAFFOLD) — Emerging organized response: open data mandates, mandatory performance benchmarking, public reporting requirements, sunset clauses on contractor agreements. Sees hollow state as a temporary institutional failure with a clearing mechanism: transparency and periodic reconstitution of service delivery chains can rebuild direct state capacity. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.16. Low effective extraction because the movement has agency and sees a path to dismantling the extraction mechanism.
constraint_indexing:constraint_classification(hollow_state_syndrome, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW CLAIM (MOUNTAIN) — Some scholars argue that outsourcing is an inevitable response to complexity and specialization: modern states cannot deliver services efficiently without private expertise, making contractor dependency a structural law of contemporary governance. However, the base properties (ε=0.58, suppression=0.68, theater=0.81) contradict the mountain criteria. This perspective represents a false summit: what appears as natural law is actually a contingent institutional arrangement that naturalizes extraction.
constraint_indexing:constraint_classification(hollow_state_syndrome, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hollow_state_syndrome_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hollow_state_syndrome, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hollow_state_syndrome, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hollow_state_syndrome, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hollow_state_syndrome, TR),
    TR >= 0.70.

:- end_tests(hollow_state_syndrome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts through multiple mechanisms: (1) contractor profit margins on outsourced services, (2) lock-in to proprietary systems that increase switching costs, (3) information asymmetries that allow executives to avoid oversight, (4) service quality degradation due to profit-driven cost-cutting with weak accountability. The rise from 0.35 to 0.58 over 20 years reflects the accumulation of lock-in effects and the progressive weakening of oversight capacity. Suppression (0.68): High. Citizens have limited options to compel service quality (no market exit, limited political leverage). Frontline workers face fragmentation across multiple contractor systems with no unified career path. Oversight institutions encounter legal barriers (confidentiality claims, contractor immunity), information barriers (proprietary methods), and structural barriers (no single entity to hold accountable). Theater ratio (0.81): Very high and rising. The constraint exhibits profound performative character: parliamentary oversight hearings occur regularly but lack enforcement power; executives claim efficiency gains while actual service quality metrics are hidden behind proprietary confidentiality; contractors claim specialized expertise while using confidentiality to avoid scrutiny; oversight NGOs publish reports that influence policy rhetoric but not behavior. The rise from 0.52 to 0.81 reflects the progressive substitution of oversight performance (holding hearings, publishing reports, issuing recommendations) for actual functional accountability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp divergence across structural positions. The dependent citizen sees pure extraction (Snare) — they are trapped with no recourse against service degradation and no ability to compel contractor accountability. The public accountability commons sees equally severe extraction (Snare) — the institutional capacity for oversight has fragmented beyond repair. The private contractor sees coordination benefit (Rope) — they genuinely provide expertise and operational capacity that civil service hiring cannot quickly scale, and they have exit options (can rebid, exit unprofitable sectors). The state executive sees coordination and net benefit (Rope) — outsourcing solves the real problem of operational capacity while shifting political risk to contractors. The frontline worker sees mixed coordination and extraction (Tangled Rope) — constrained by fragmentation but benefits from contractor expertise. The oversight coalition sees degradation of their own function (Piton) — they perform oversight rituals (hearings, reports) but these have become theatrical, maintained through institutional inertia rather than actual function. The transparency movement sees a temporary problem with a clearing mechanism (Scaffold) — open data mandates, mandatory benchmarking, and sunset clauses on contractor agreements can rebuild direct state capacity. The analytical observer risks naturalizing the constraint as inevitable (Mountain) — 'modern states require contractor expertise' — but the base properties reveal this as false: the extractiveness (0.58) and theater (0.81) indicate a contingent institutional arrangement that benefits some actors at the cost of collective oversight capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Dependent Citizens: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit from jurisdiction, no leverage over contractors. Private Contractors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can rebid contracts, exit unprofitable sectors, leverage proprietary lock-in. State Executive: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.11. Net beneficiary. Outsourcing transfers political risk while maintaining formal authority. Can renegotiate contracts, rotate contractors, use confidentiality claims. Public Accountability: Victim + trapped → d≈0.98, f(d)≈1.50. Absolute victim. No agent represents the commons; accountability is fragmented; no exit mechanism. Frontline Workers: Victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction. Employment fragmented, benefits from contractor resources are conditional on contractor profitability, training locked into proprietary systems. Oversight Coalition: Victim + constrained → d≈0.55, f(d)≈0.75. Constrained by legal barriers (confidentiality), information barriers (proprietary methods), structural barriers (fragmented responsibility). Transparency Movement: Organized + mobile → d≈0.35, f(d)≈0.35. Lower directionality because they have agency (organizing for transparency mandates, sunset clauses) and see a path forward (reconstitution of direct state capacity).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves a critical mandatrophy: is outsourcing a coordination solution or an extraction mechanism? The answer is YES to both. Outsourcing provides genuine coordination benefits — contractors can scale expertise faster than civil service hiring, can specialize in services that states cannot efficiently operate directly, can assume operational risk and performance accountability (in principle). Simultaneously, outsourcing enables extraction through lock-in, accountability fragmentation, and information asymmetries. The tangled rope correctly models this: χ ≈ 0.58 × 0.45 × 1.0 ≈ 0.26 (average across perspectives), reflecting that the effective extractiveness is substantial but not maximal — the coordination function partially legitimates the arrangement, preventing it from being classified as pure snare. However, the rising theater ratio (0.81) and the fragmentation of accountability (both victims powerless/trapped) indicate that the extraction mechanism is strengthening relative to the coordination function. If theater exceeds 0.85 and both primary beneficiaries gain arbitrage exit while all victims remain trapped, the constraint would degrade toward piton (performative, inertial). If transparency mandates fail to rebuild accountability, the constraint approaches snare (pure extraction masked by institutional theater). The scaffold perspective identifies the path to resolve the mandatrophy: mandatory performance benchmarking + transparency requirements + periodic reconstitution of contractor agreements can maintain the coordination benefits (specialized expertise, operational capacity) while removing the extraction mechanism (lock-in, accountability fragmentation). The constraint's future classification depends on whether transparency and sunset mechanisms succeed or fail — empirically, omegas_transparency_mandate_effectiveness and omegas_accountability_fragmentation_threshold are critical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_private_boundary_stability,
    'Is the public-private boundary in service delivery structurally unstable, or is the extraction permanent?',
    'Longitudinal study of contractor relationships across 20+ years: do contracts expire, get rebid, rotate providers, or become entrenched? Can states rebuild direct capacity or are they locked in?',
    'If boundary is unstable: scaffold perspective is accurate and constraint is temporary. If entrenched: extraction is permanent and snare classification is canonical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_private_boundary_stability, empirical, 'Whether outsourcing contracts become permanent lock-in or cyclically reconstituted').

omega_variable(
    proprietary_methods_counterfactual,
    'Do contractor proprietary methods genuinely provide services that civil service could not deliver, or are they artificial scarcity created through IP barriers?',
    'Comparative study of service delivery quality (speed, error rates, user satisfaction) between public agencies that retained direct capacity vs those that fully outsourced. Analysis of whether proprietary methods are replicable by public sector with training investment.',
    'If genuine expertise: rope classification is valid (real coordination function). If artificial scarcity: tangled rope collapses toward pure snare, and extraction is unmasked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_methods_counterfactual, empirical, 'Whether contractor proprietary methods are genuine expertise or artificial scarcity').

omega_variable(
    accountability_fragmentation_threshold,
    'At what degree of outsourcing does accountability formally collapse? Is there a threshold where no single actor can be held responsible?',
    'Case law analysis: litigation patterns when services fail. Do courts hold contractors liable, the state liable, or neither? Are there systemic cases where responsibility diffusion is legally formalized?',
    'If threshold exists and is reached: the constraint shifts from tangled_rope toward piton (accountability becomes purely theatrical). If accountability chains remain intact: the constraint is more rope than snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_fragmentation_threshold, empirical, 'Legal and functional collapse point for accountability under outsourcing').

omega_variable(
    contractor_exit_capacity,
    'Do private contractors have genuine exit capacity (can abandon contracts without catastrophic loss), or are they also structurally trapped once they become indispensable?',
    'Analysis of contractor divestment patterns: have major contractors exited service sectors? What are the costs and barriers to exit? Do contracts include penalties for termination that lock contractors in?',
    'If contractors have true arbitrage exit: their perspective is correctly classified as rope (net beneficiary). If locked in by contracts: they become secondary victims and the constraint is more symmetrical (both beneficiary and contractor are trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractor_exit_capacity, empirical, 'Whether private contractors have genuine exit or are contractually trapped').

omega_variable(
    transparency_mandate_effectiveness,
    'Can transparency and open data requirements actually rebuild public oversight, or do contractors and executives use legal complexity to evade disclosure?',
    'Evaluation of transparency laws (FOIA exemptions, contractor confidentiality claims): what percentage of contractor operations remains hidden? Do transparency mandates change behavior or generate performative compliance?',
    'If effective: scaffold perspective is valid, sunset is achievable. If ineffective: transparency becomes another piton (theater without function) and the constraint persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_mandate_effectiveness, empirical, 'Whether transparency mandates can rebuild effective public oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hollow_state_syndrome, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hollow_tr_t0, hollow_state_syndrome, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hollow_tr_t10, hollow_state_syndrome, theater_ratio, 10, 0.68).
narrative_ontology:measurement(hollow_tr_t20, hollow_state_syndrome, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(hollow_be_t0, hollow_state_syndrome, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hollow_be_t10, hollow_state_syndrome, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hollow_be_t20, hollow_state_syndrome, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hollow_state_syndrome, enforcement_mechanism).
narrative_ontology:affects_constraint(hollow_state_syndrome, regulatory_capture).
narrative_ontology:affects_constraint(hollow_state_syndrome, governance_legitimacy_degradation).
narrative_ontology:affects_constraint(hollow_state_syndrome, accountability_commons_tragedy).

% DUAL FORMULATION NOTE:
% The hollow state syndrome is downstream of regulatory capture (contractors capture the outsourcing decision) and upstream of accountability commons degradation (fragmented responsibility erodes oversight). These three constraints form a causal chain: regulatory capture → outsourcing expansion → accountability fragmentation. Each has distinct ε: regulatory capture (ε≈0.45, institutional level) represents the mechanism that creates the hollow state; hollow state syndrome (ε=0.58, operational level) represents the structural condition; accountability degradation (ε≈0.70, systemic level) represents the consequence if transparency mandates fail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hollow_state_syndrome, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
