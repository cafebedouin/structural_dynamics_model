% ============================================================================
% CONSTRAINT STORY: mandatrophic_margin_collapse_diagnostic
% ============================================================================
% Version: 2.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandatrophic_margin_collapse_diagnostic, []).

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
 *   constraint_id: mandatrophic_margin_collapse_diagnostic
 *   human_readable: Mandatrophy (Systemic Resilience Wasting)
 *   domain: institutional/technological/systems_administration
 *
 * SUMMARY:
 *   Mandatrophy is the structural extraction of a system's resilience margin
 *   (its ability to absorb shocks, recover from failure, and adapt to
 *   disruption) in service of a high-priority performance mandate. The
 *   extraction is invisible because resilience is defined as capacity *not*
 *   used under normal operation. When everything functions smoothly, the
 *   margin appears as slack or waste—and mandate-driven optimization
 *   eliminates it. The extraction becomes visible only in catastrophic
 *   failure, when the system's reduced resilience manifests as cascading
 *   collapse. This temporal asymmetry (invisibility until catastrophe)
 *   combined with the institutional incentive to attribute failures to
 *   external causes (bad luck, external shocks, employee error) rather than
 *   to prior margin extraction creates a snare: the powerless victims (the
 *   system's resilience, the downstream population) have no mechanism to
 *   resist or negotiate, and the mandate authority bears no cost for the
 *   extraction until collapse occurs. At that point, institutional narratives
 *   reframe the failure as unforeseeable rather than as the predictable
 *   result of invisible extraction. Mandatrophy exhibits all six DR types
 *   from different perspectives, making it a diagnostic exemplar for how
 *   institutional asymmetries hide extraction in plain sight.
 *
 * KEY AGENTS:
 *   - System Resilience Margin: Primary victim (powerless/trapped) — cannot organize, exit, or negotiate; extracted invisibly through mandate-driven optimization
 *   - Downstream Population: Primary victim (powerless/trapped) — bears cost of margin collapse through service failure, safety risk, or death; has no control over extraction
 *   - Operations and Maintenance Coalition: Secondary actor (organized/constrained) — forced to implement margin extraction while absorbing operational risk and blame; organized enough to perceive the hybrid structure (tangled rope perspective)
 *   - Mandate Authority (Executive/Political): Primary beneficiary (institutional/arbitrage) — benefits from measurable performance improvement without bearing cost of margin collapse; experiences constraint as coordination (rope)
 *   - Structural Inevitability Narrative: Meta-institutional force — maintains extraction through theater (piton); 'all organizations sacrifice resilience for performance' displaces accountability
 *   - Analytical Observer: Civilizational perspective — risks naturalizing mandatrophy as an immutable law of systems design, obscuring the institutional choice that enables extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandatrophic_margin_collapse_diagnostic, 0.68).
domain_priors:suppression_score(mandatrophic_margin_collapse_diagnostic, 0.75).
domain_priors:theater_ratio(mandatrophic_margin_collapse_diagnostic, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandatrophic_margin_collapse_diagnostic, extractiveness, 0.68).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse_diagnostic, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse_diagnostic, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandatrophic_margin_collapse_diagnostic, snare).
narrative_ontology:human_readable(mandatrophic_margin_collapse_diagnostic, "Mandatrophy (Systemic Resilience Wasting)").
narrative_ontology:topic_domain(mandatrophic_margin_collapse_diagnostic, "institutional/technological/systems_administration").

domain_priors:requires_active_enforcement(mandatrophic_margin_collapse_diagnostic).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, system_resilience_margin).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, fault_tolerance_capacity).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, operational_redundancy).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, maintenance_infrastructure).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, emergency_response_buffer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM RESILIENCE (SNARE) — The margin cannot negotiate, exit, or organize. It is extracted invisibly through mandate-driven operational pressure. Optimization for immediate performance (mandate) suppresses the redundancy, slack, and maintenance that constitute resilience. d≈0.98, f(d)≈1.44, σ=1.2 → χ≈0.78. This is pure extraction: the system's ability to absorb shocks, recover from failures, or adapt to disruption is sacrificed for measurable short-term performance.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM POPULATION (SNARE) — Citizens, patients, employees, or other users who depend on system reliability but have no control over mandate-driven extraction of resilience. When the margin collapses, they bear the cost. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.75. The snare is hidden until catastrophic failure: collapse appears as sudden and unpredictable, not as the predictable result of invisible extraction.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: OPERATIONS AND MAINTENANCE (TANGLED ROPE) — Operators, engineers, and maintenance staff see BOTH the coordination benefit (mandate-driven optimization creates shared goals and resource allocation) AND the extraction (they must enforce the margin reduction, absorbing the operational risk and blame when failures occur). They are organized enough to perceive the hybrid structure. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.64. This is the perspective from which mandatrophy becomes visible: ops teams can see that they are being forced to trade resilience for performance metrics.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MANDATE AUTHORITY (ROPE) — The executive or political leadership that sets the mandate experiences it as pure coordination: the mandate solves the collective action problem of 'how do we ensure the system is optimized for this goal?' They see the system's compliance as coordination, not extraction. They may not even perceive the margin as a resource being extracted. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Negative effective extraction = net beneficiary. The mandate authority benefits from measurable performance without bearing the cost of margin collapse.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STRUCTURAL INEVITABILITY (PITON) — The meta-institutional view that mandatrophy is an inevitable feature of hierarchical systems. 'All organizations sacrifice resilience for performance targets. This is just how bureaucracy works.' Theater ratio = 0.82: the system persists in extracting resilience not because the extraction is necessary (it is not), but because the inevitability narrative displaces accountability. The institutional inertia maintains the constraint despite low functional justification. d≈0.10, f(d)≈-0.06, σ=1.0 → χ≈-0.01. The piton classification reflects that the constraint is now maintained by narrative (theater) rather than by any real functional requirement.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN attempt) — The temptation to naturalize mandatrophy: 'Every system has finite resources. You cannot maximize both performance and resilience. It is mathematically impossible.' This framing treats the margin as a scarcity problem rather than an institutional extraction problem. However, the structural data (ε=0.68, suppression=0.75, theater=0.82) reveals this as a FALSE SUMMIT. The constraint is not a law of nature but a contingent institutional choice. The engine will flag this perspective as exhibiting false natural law characteristics: attempting to disguise extractive institutional structure as immutable physics.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandatrophic_margin_collapse_diagnostic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandatrophic_margin_collapse_diagnostic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mandatrophic_margin_collapse_diagnostic, TR),
    TR >= 0.70.

:- end_tests(mandatrophic_margin_collapse_diagnostic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts the system's entire resilience margin—its ability to absorb shocks and recover from failures. The extraction is not partial; it is aggressive optimization of every available capacity for immediate performance. The extractiveness has grown over the interval (from 0.35 to 0.68) as mandate-driven optimization matured and became institutionalized. Suppression (0.75): High. Multiple mechanisms suppress alternatives: (1) Performance metrics that measure only success under normal operation, not resilience under disruption. (2) Institutional narrative that attributes failures to external causes, not to prior margin extraction. (3) Organizational incentives that reward mandate achievement, not margin preservation. (4) Asymmetric accountability: mandate authority bears zero cost for collapse; operations staff and downstream users bear all cost. Theater ratio (0.82): High and increasing. The constraint persists not because it achieves any functional benefit (once the margin is extracted, further optimization yields diminishing returns), but because the institutional narrative—'resilience is slack, slack is waste, waste is inefficiency'—maintains the extraction through pure narrative force. The theater has increased as the institutional narrative has solidified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The mandate authority sees pure coordination (Rope): setting and enforcing performance targets solves the collective action problem of system optimization. They experience no extraction, only achievement. The operations coalition sees mixed coordination and extraction (Tangled Rope): the mandate creates shared goals and resource allocation (coordination) but forces them to implement margin reduction while absorbing operational risk (extraction). The downstream population sees pure extraction (Snare): they have no participation in mandate-setting, no control over margin reduction, and they bear full cost of collapse. The system's resilience margin sees pure extraction (Snare): it cannot negotiate, exit, or even be measured in a way that captures its value until it is too late. The structural inevitability frame sees an immutable institutional condition (Piton): mandatrophy is just how hierarchical systems work; the inevitability narrative maintains the extraction through theater. The analytical observer risks seeing a natural law (Mountain): 'you cannot optimize both performance and resilience simultaneously'—but this false naturalization masks the contingent institutional choice to distribute costs asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   System resilience margin: Victim + trapped → d≈0.98, f(d)≈1.44. Maximum extraction. The margin cannot organize, negotiate, or exit. Downstream population: Victim + trapped → d≈0.95, f(d)≈1.42. Near-maximum extraction. Users depend on the system but have no control over margin reduction. Operations coalition: Victim + constrained (but organized) → d≈0.65, f(d)≈0.95. Moderate-high extraction. Constrained by institutional hierarchy, but organized enough to perceive the mixed structure. Mandate authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Zero cost for extraction; full benefit of performance improvement. Structural inevitability: Institutional + arbitrage → d≈0.10, f(d)≈-0.06. Piton classification from theater gate (0.82 ≥ 0.70), not from high extraction. The narrative maintains the constraint through inertia. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain attempt is FALSE SUMMIT: the constraint is not immutable, and naturalizing it obscures institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY EXEMPLAR: This constraint RESOLVES the mandatrophy by showing that mandatrophy itself is the problem being diagnosed. The mandate (optimize system performance) appears to be a coordination mechanism (Rope from the authority's perspective). The reality is that the mandate drives extraction of resilience (Snare from the system's perspective). The resolution is not to choose between the two classifications, but to recognize that INSTITUTIONAL ASYMMETRY ENABLES THE EXTRACTION. The mandate authority can declare performance targets that the operations coalition must implement, but the authority bears no cost if those targets cause margin collapse. This asymmetry is the structural mechanism that converts what could be a negotiated tradeoff (Tangled Rope with mutual acknowledgment of costs) into a pure extraction (Snare where victims have no agency). The mandatrophy resolution distinguishes between: (1) LEGITIMATE TRADEOFF: If all parties acknowledge that margin is being reduced and explicitly accept the resilience cost in exchange for performance gain, this is Tangled Rope with mutual understanding. (2) HIDDEN EXTRACTION: If the authority claims the margin reduction is 'efficiency' or 'optimization' without acknowledging the resilience cost, while operations staff and downstream users bear the consequences, this is Snare. Mandatrophy occurs when institutional structure enables the authority to declare (1) while implementing (2). The engine detects this by computing directionality asymmetry: the authority's d ≈ 0.08 (beneficiary), while the system's d ≈ 0.98 (victim). This di-polar directionality is the signature of resolved mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resilience_measurement_circularity,
    'Can resilience be measured in a way that is not itself corrupted by the mandate''s performance optimization?',
    'Independent post-hoc analysis of system response to unplanned disruptions; comparison of recovery metrics in systems with vs. without margin extraction; empirical failure-mode analysis',
    'If resilience CAN be measured independently: the snare classification is confirmed, and mandatrophy becomes visible and actionable. If measurement is circular: resilience becomes formally invisible, and the snare classification can only be diagnosed through catastrophic failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resilience_measurement_circularity, empirical, 'Whether resilience can be measured independently of mandate-driven metrics').

omega_variable(
    margin_extraction_necessity,
    'Is the margin extraction actually necessary to achieve the mandate''s performance target, or is it a failure of institutional design?',
    'Comparative analysis of systems that achieved mandate targets WITH margin preservation vs. those that achieved targets through margin extraction; identification of structural alternatives to the extraction',
    'If extraction is necessary: mandatrophy is a tragic tradeoff (difficult reframing but not a pure snare). If extraction is unnecessary: mandatrophy is pure extraction with no coordination justification, confirming snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(margin_extraction_necessity, empirical, 'Whether margin extraction is structurally necessary for mandate achievement').

omega_variable(
    collapse_attribution_asymmetry,
    'When the margin collapses and the system fails, is the failure attributed to ''bad luck,'' ''external shock,'' or ''human error''—or to the prior extraction of resilience?',
    'Causal analysis of post-catastrophe institutional narratives; tracing of blame assignment patterns; comparison of failure attribution in systems with transparent margin tracking vs. hidden extraction',
    'If collapse is attributed to external factors: mandatrophy remains invisible and the snare persists. If collapse is attributed to prior margin extraction: mandatrophy becomes visible and actionable; snare classification becomes institutional liability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collapse_attribution_asymmetry, empirical, 'Whether margin collapse is attributed to extraction or to external causes').

omega_variable(
    institutional_incentive_misalignment,
    'Does the mandate authority actually bear any cost for margin collapse, or is the cost entirely externalized to operations staff and downstream users?',
    'Accountability mapping: who is held responsible when the margin collapses? Career outcomes, performance evaluations, budgets, and institutional consequences for mandate authority vs. operations staff vs. downstream populations',
    'If mandate authority bears zero cost: mandatrophy is pure snare with full institutional asymmetry. If mandate authority bears meaningful cost: mandatrophy becomes a snare with partial self-regulation through accountability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_misalignment, empirical, 'Cost externalization from mandate authority to operations and users').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandatrophic_margin_collapse_diagnostic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandatrophic_margin_collapse_diagnostic, theater_ratio, 0, 0.45).
narrative_ontology:measurement(mand_tr_t5, mandatrophic_margin_collapse_diagnostic, theater_ratio, 5, 0.63).
narrative_ontology:measurement(mand_tr_t10, mandatrophic_margin_collapse_diagnostic, theater_ratio, 10, 0.82).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandatrophic_margin_collapse_diagnostic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mand_be_t5, mandatrophic_margin_collapse_diagnostic, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mand_be_t10, mandatrophic_margin_collapse_diagnostic, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandatrophic_margin_collapse_diagnostic, enforcement_mechanism).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse_diagnostic, systemic_safety_margin_degradation).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse_diagnostic, organizational_antifragility_loss).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse_diagnostic, infrastructure_brittleness_accumulation).

% DUAL FORMULATION NOTE:
% Mandatrophy is a meta-constraint that describes how institutional structures extract resilience from ANY system that operates under performance mandates. The three downstream constraints specify mandatrophy's effects in particular domains (safety, antifragility, infrastructure). All three are linked upstream to this diagnostic constraint because mandatrophy is the mechanism that generates their high extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandatrophic_margin_collapse_diagnostic, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
