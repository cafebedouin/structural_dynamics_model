% ============================================================================
% CONSTRAINT STORY: institutional_compliance_theater
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_compliance_theater, []).

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
 *   constraint_id: institutional_compliance_theater
 *   human_readable: Institutional Compliance Theater
 *   domain: organizational/governance
 *
 * SUMMARY:
 *   Institutional compliance theater describes the pervasive phenomenon where
 *   organizations invest in elaborate compliance procedures, documentation
 *   systems, and audit processes that provide defensibility and institutional
 *   legitimacy but contribute minimally to actual risk reduction. The
 *   constraint exhibits piton characteristics: the primary function
 *   (demonstrating compliance to regulators) has largely decoupled from the
 *   original purpose (reducing actual risk), yet the constraint persists
 *   through institutional inertia, fear of audit failure, and the regulatory
 *   obligation to maintain compliance infrastructure. The theater ratio
 *   (0.78) reflects that the majority of compliance activity — training
 *   sessions, policy documentation, audit trails, attestations, committee
 *   meetings — serves performative purposes: creating evidence of compliance
 *   effort rather than identifying and mitigating actual risks. The
 *   constraint simultaneously extracts resources from operational units
 *   (suppression = 0.65: organizational mandate, regulatory penalty risk,
 *   audit expectations) while enriching the compliance industry through
 *   consulting fees, software licensing, and training contracts. This creates
 *   a mixed extraction-coordination structure: genuine coordination problem
 *   (how to standardize and communicate risk governance across complex
 *   organizations) layered with asymmetric extraction (compliance costs borne
 *   by operational units, compliance benefits captured by leadership and
 *   compliance industry).
 *
 * KEY AGENTS:
 *   - Compliance Officer: Primary victim (powerless/trapped) — mandatory participant in degraded system; cannot exit without career damage
 *   - Operational Unit: Secondary victim (moderate/constrained) — bears resource costs of compliance theater; constrained by regulatory mandate and audit expectations
 *   - Regulatory Agency: Beneficiary (institutional/arbitrage) — experiences compliance framework as coordination mechanism; can pivot focus as risks change
 *   - Compliance Industry: Beneficiary (institutional/constrained) — captures revenue stream from compliance mandates; trapped by organizational dependence on continued regulatory complexity
 *   - Risk-Based Reform Advocates: Organized actors (organized/mobile) — building alternative pathways; see sunset opportunity in risk-based compliance frameworks
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable bureaucratic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_compliance_theater, 0.58).
domain_priors:suppression_score(institutional_compliance_theater, 0.65).
domain_priors:theater_ratio(institutional_compliance_theater, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_compliance_theater, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_compliance_theater, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_compliance_theater, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_compliance_theater, piton).
narrative_ontology:human_readable(institutional_compliance_theater, "Institutional Compliance Theater").
narrative_ontology:topic_domain(institutional_compliance_theater, "organizational/governance").

domain_priors:requires_active_enforcement(institutional_compliance_theater).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_compliance_theater, compliance_industry).
narrative_ontology:constraint_beneficiary(institutional_compliance_theater, institutional_leadership).
narrative_ontology:constraint_victim(institutional_compliance_theater, operational_efficiency).
narrative_ontology:constraint_victim(institutional_compliance_theater, actual_risk_management).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPLIANCE OFFICER (PITON) — Trapped within the constraint by employment dependence and regulatory mandate. Sees the compliance system as largely performative: box-checking rituals, documentation theater, and procedural compliance that may not meaningfully reduce actual risk. Cannot exit without career damage. The constraint persists through institutional inertia and the need to defend against audit failure, not because it effectively manages risk. Experiences extraction through mandatory participation in a degraded system.
constraint_indexing:constraint_classification(institutional_compliance_theater, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPERATIONAL UNIT (SNARE) — Constrained by compliance requirements that divert resources from core mission. Faces high suppression: regulatory mandates, audit expectations, and penalty risk limit practical alternatives. Compliance theater creates asymmetric burden — operational units bear compliance costs while leadership benefits from defensibility. Theater ratio masks low actual risk reduction behind extensive documentation and procedural investment.
constraint_indexing:constraint_classification(institutional_compliance_theater, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AGENCY (ROPE) — Coordinates institutional behavior through compliance frameworks. From the agency's perspective, the constraint functions as pure coordination: standardizing compliance procedures enables oversight and reduces information asymmetry. The agency experiences the framework as working (organizations comply, audit trails exist) and does not perceive extraction because enforcement cost is built into the institutional structure. Arbitrage exit enables the agency to pivot compliance focus as risk landscapes change.
constraint_indexing:constraint_classification(institutional_compliance_theater, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPLIANCE INDUSTRY (TANGLED ROPE) — Consulting firms, software vendors, training providers. Genuine coordination function: provides expertise on complex regulatory requirements, standardizes practice across organizations. Asymmetric extraction: compliance industry captures revenue stream that grows with regulatory complexity regardless of actual risk reduction. Requires active enforcement: compliance mandates create demand; tightening regulations increase consulting revenue. Mixed function and extraction in one institutional actor.
constraint_indexing:constraint_classification(institutional_compliance_theater, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RISK-BASED COMPLIANCE REFORM (SCAFFOLD) — Organized advocates (auditors, academics, some regulators) see institutional compliance theater as a temporary coordination failure being solved by risk-based approaches: focusing compliance investment on high-risk areas rather than uniform procedural compliance. This perspective has mobility and agency — can point to examples of successful risk-based frameworks. Has sunset logic: as risk-based methods mature and regulatory appetite for them grows, the theater-driven compliance regime becomes optional. Estimated sunset: 10-15 years for risk-based frameworks to displace procedure-driven compliance in most sectors.
constraint_indexing:constraint_classification(institutional_compliance_theater, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational perspective, one might argue that institutional compliance theater is an immutable natural law: all large organizations require formal risk governance; the gap between formal procedure and actual risk reduction is inherent to bureaucracy; coordination cost always exceeds the apparent extraction. This perspective risks naturalizing what is actually a contingent institutional arrangement. The false summit emerges because the observer conflates 'institutional complexity is real' (true) with 'current compliance theater is inevitable' (false). The engine's false summit detector will identify this as misclassification.
constraint_indexing:constraint_classification(institutional_compliance_theater, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_compliance_theater_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_compliance_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_compliance_theater, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_compliance_theater, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_compliance_theater, TR),
    TR >= 0.70.

:- end_tests(institutional_compliance_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The compliance system extracts substantial resources from operational units (personnel time, system costs, process friction) that do not proportionally reduce actual risk. However, extraction is not total (0.72+) because: (a) some compliance procedures do address real risks, (b) organizations have some agency in how they implement compliance (they can focus on high-risk areas even within procedural frameworks), and (c) regulatory agencies are not wholly captured by the compliance industry. The trajectory from 0.48 to 0.62 over 15 years reflects that compliance theater is increasing: regulatory complexity grows faster than organizations can integrate it meaningfully; compliance procedures accumulate without removing obsolete procedures; the compliance industry grows, increasing pressure for more complex frameworks. Suppression (0.65): Moderate-high. Organizations face significant barriers to exiting compliance theater: regulatory mandate creates legal obligation; audit failure carries financial and reputational penalty; competitors engage in equal-or-greater compliance investment (competitive suppression through expectation matching). However, suppression is not total (0.85+) because: (a) organizations retain discretion in implementation approaches, (b) some regulatory agencies accept risk-based alternatives, (c) external pressure for compliance framework reform is building. Theater ratio (0.78): Very high. The majority of compliance work is documenting compliance rather than managing risk. Audit trails, policy libraries, training records, and committee attestations demonstrate compliance effort but do not directly reduce risk. The theater has increased over the interval as organizations have invested in compliance technology and processes that create sophisticated appearance of control while actual risk reduction per unit of compliance investment has declined. This is classic piton degradation: the original coordination function (establishing risk governance norms) has decoupled from the mechanism maintaining it (ever-more-elaborate theater).
 *
 * PERSPECTIVAL GAP:
 *   The compliance officer sees piton degradation: the system asks for meaningful risk assessment but then requires box-checked procedures regardless of risk level. The operational unit sees snare: trapped by mandate, burdened with disproportionate cost, offered no exit. The regulatory agency sees rope: coordination is working, organizations are reporting risks, oversight is enabled. The compliance industry sees tangled_rope: genuine value provided (expertise, standards, tools) but with asymmetric extraction (revenue grows independent of risk reduction effectiveness). The reform coalition sees scaffold: temporary problem being solved by risk-based methods; sunset is visible if adoption accelerates. The civilizational observer risks mountain: bureaucratic necessity, inherent to all large organizations, natural law of institutional life. The perspectival gap reveals that the analytical mountain is a false summit — the constraint is contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural relationship to the constraint. Compliance officers and operational units are victims with limited exit: trapped/constrained exit produces high d values, yielding high experienced extraction (χ). They bear compliance costs with minimal direct benefit. The regulatory agency benefits from the coordination function (standardized risk governance frameworks enable oversight) and has arbitrage exit (can shift compliance focus): low d yields low or negative χ. The compliance industry benefits from growing regulatory complexity: positive d but with institutional power and constrained (not mobile) exit, producing moderate extraction experience. The reform coalition has mobile exit and organized power, so they can choose to build alternatives: their d is driven by power (organized) and exit options (mobile), producing low experienced extraction despite high nominal extractiveness. The piton classification emerges from high theater_ratio, not from high χ: the engine's piton gate fires when theater_ratio ≥ 0.70 regardless of experienced extraction. This correctly identifies that the constraint's binding mechanism is institutional inertia (the procedure persists because it exists and feels necessary) rather than active extraction pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint exhibits mandatrophy in the gap between the compliance industry's genuine coordination function (standardizing risk governance practice) and its asymmetric extraction (revenue that grows with regulatory complexity independent of risk reduction). The tangled_rope classification for the compliance industry resolves the apparent contradiction by acknowledging both functions simultaneously. The compliance officer's piton perspective reveals that the original coordination function (establishing risk governance) has degraded into theater, yet the institutional obligation persists. The constraint does not falsely label coordination as extraction or vice versa — it clarifies that institutional compliance systems can simultaneously coordinate (from the regulatory agency's perspective), extract (from the operational unit's perspective), and degrade (from the compliance officer's perspective). The reform coalition's scaffold perspective provides the temporal resolution: risk-based compliance frameworks represent a genuine alternative pathway that could reduce theater_ratio and lower suppression. The mandatrophy is fully resolved by the perspectival array: all six readings are structurally valid; none is 'the' correct classification. The constraint is a presheaf over the observation site.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theater_vs_real_compliance_distinction,
    'What fraction of institutional compliance activity represents actual risk reduction versus performative compliance theater?',
    'Comparative analysis: institutions with high compliance theater vs. low theater; correlation between compliance investment and actual incident reduction; audit outcomes vs. real-world risk materialization rates',
    'If actual risk reduction < 20%: extraction component is significantly higher, piton classification becomes snare. If actual risk reduction > 50%: coordination function is genuine, classification shifts toward tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_real_compliance_distinction, empirical, 'Ratio of actual risk reduction to performative compliance activity').

omega_variable(
    regulatory_capture_in_compliance_framework,
    'Is the compliance industry captured by the regulatory agencies that create compliance mandates, or do independent interests shape framework evolution?',
    'Analysis of regulatory rulemaking: whose input shapes compliance requirements; correlation between industry lobbying and regulatory complexity; exit options for non-captured regulatory agencies',
    'If captured: compliance industry perspective shifts from tangled_rope to snare (pure extraction). If independent: tangled_rope classification holds. Regulatory agency perspective may shift from rope to constrained institutional if agency autonomy is compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_compliance_framework, empirical, 'Degree of compliance industry capture in regulatory framework evolution').

omega_variable(
    risk_based_compliance_scalability,
    'Can risk-based compliance frameworks scale to complex multi-sector organizations, or does complexity force reversion to procedural theater?',
    'Implementation outcomes: organizations that adopted risk-based frameworks; measurement of compliance effectiveness under risk-based vs. procedure-based regimes; failure rate analysis during framework transitions',
    'If scalable: scaffold perspective is structural (sunset is real), theater_ratio expected to decline. If not scalable: risk-based approaches remain aspirational, scaffold is theater itself, and institutional_compliance_theater persists indefinitely at piton or snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(risk_based_compliance_scalability, empirical, 'Whether risk-based compliance frameworks can scale across complex institutional landscapes').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of alternative compliance approaches structural (regulatory prohibition) or internalized (organizations believe procedural compliance is necessary)?',
    'Comparative institutional analysis: organizations operating under weak/no compliance mandates vs. strong mandates; measurement of voluntary procedural compliance; exit behavior when regulatory pressure relaxes',
    'If structural: suppression = 0.65 is accurate (external barriers are the binding constraint). If internalized: effective suppression is higher (organizations carry compliance theater internalization even when external mandate weakens), and identity_locked exit options may be present in compliance officer perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative compliance approaches is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_compliance_theater, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compli_tr_t0, institutional_compliance_theater, theater_ratio, 0, 0.62).
narrative_ontology:measurement(compli_tr_t5, institutional_compliance_theater, theater_ratio, 5, 0.7).
narrative_ontology:measurement(compli_tr_t10, institutional_compliance_theater, theater_ratio, 10, 0.78).
narrative_ontology:measurement(compli_tr_t15, institutional_compliance_theater, theater_ratio, 15, 0.81).

% Extraction over time
narrative_ontology:measurement(compli_be_t0, institutional_compliance_theater, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(compli_be_t5, institutional_compliance_theater, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(compli_be_t10, institutional_compliance_theater, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(compli_be_t15, institutional_compliance_theater, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_compliance_theater, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(institutional_compliance_theater, 0.18).
narrative_ontology:affects_constraint(institutional_compliance_theater, regulatory_capture_dynamics).
narrative_ontology:affects_constraint(institutional_compliance_theater, organizational_process_complexity_accumulation).

% DUAL FORMULATION NOTE:
% Institutional compliance theater is distinct from the underlying regulatory mandate it implements. The regulatory framework itself is a separate constraint (regulatory_capture_dynamics) with different ε; compliance theater emerges as a secondary constraint when organizations respond to regulatory obligation by layering procedural infrastructure. The two constraints are linked: escalating regulatory complexity drives increased theater ratio in institutional compliance response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_compliance_theater, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
