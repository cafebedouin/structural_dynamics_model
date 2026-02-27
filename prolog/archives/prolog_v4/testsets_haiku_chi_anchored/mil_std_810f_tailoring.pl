% ============================================================================
% CONSTRAINT STORY: mil_std_810f_tailoring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mil_std_810f_tailoring, []).

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
 *   constraint_id: mil_std_810f_tailoring
 *   human_readable: MIL-STD-810F Environmental Tailoring Standard
 *   domain: technological/defense_systems
 *
 * SUMMARY:
 *   MIL-STD-810F Environmental Tailoring Standard creates a structural
 *   tension between legitimate engineering efficiency and asymmetric
 *   extraction through cost-driven test reduction. The standard gives prime
 *   contractors and program managers authority to 'tailor' environmental test
 *   limits to match predicted service conditions, eliminating redundant
 *   testing. This coordination function is real: systematic testing of all
 *   environmental permutations is economically infeasible and technically
 *   wasteful. However, the authority to define 'predicted service conditions'
 *   becomes an extraction mechanism when held by actors whose incentive
 *   structure favors cost reduction over risk mitigation. Subcontractors and
 *   field reliability communities lack transparent visibility into tailoring
 *   decisions and cannot challenge them without contract termination risk.
 *   The constraint exhibits Snare dynamics (trapped victims), Rope dynamics
 *   (legitimate coordination), and institutional theater (performative
 *   standards committee). The theater ratio has increased over the 30-year
 *   interval as test complexity has grown and contractor pressure to reduce
 *   test scope has intensified, while the substantive oversight mechanisms
 *   have remained stable, creating drift toward performative compliance.
 *
 * KEY AGENTS:
 *   - Prime Contractors: Primary beneficiary (institutional/arbitrage) — captures cost savings through reduced test scope; controls tailoring authority
 *   - Subcontractors: Primary victim (powerless/trapped) — must comply with tailoring decisions they cannot review or challenge; bear cost of compliance uncertainty
 *   - Field Users / Deployed Equipment Operators: Secondary victim (powerless/trapped) — cannot exit deployed systems; bear catastrophic costs if tailoring is inadequate
 *   - Engineering Test Groups: Moderate/constrained — benefit from standardized methods but harmed by prime contractor manipulation of tailoring parameters
 *   - Program Managers: Institutional beneficiary (institutional/arbitrage) — control tailoring authority; benefit from cost savings when properly applied
 *   - DoD Standards Committee: Institutional actor (institutional/arbitrage) — maintains standard through routine; exercises limited oversight of actual tailoring decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mil_std_810f_tailoring, 0.52).
domain_priors:suppression_score(mil_std_810f_tailoring, 0.65).
domain_priors:theater_ratio(mil_std_810f_tailoring, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mil_std_810f_tailoring, extractiveness, 0.52).
narrative_ontology:constraint_metric(mil_std_810f_tailoring, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mil_std_810f_tailoring, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mil_std_810f_tailoring, tangled_rope).
narrative_ontology:human_readable(mil_std_810f_tailoring, "MIL-STD-810F Environmental Tailoring Standard").
narrative_ontology:topic_domain(mil_std_810f_tailoring, "technological/defense_systems").

domain_priors:requires_active_enforcement(mil_std_810f_tailoring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mil_std_810f_tailoring, prime_contractors).
narrative_ontology:constraint_beneficiary(mil_std_810f_tailoring, program_managers).
narrative_ontology:constraint_victim(mil_std_810f_tailoring, subcontractors).
narrative_ontology:constraint_victim(mil_std_810f_tailoring, field_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBCONTRACTOR (SNARE) — Trapped in a supply chain where prime contractors dictate tailoring parameters without transparent justification. Subcontractors bear cost of compliance uncertainty and cannot exit the contract. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD RELIABILITY COLLECTIVE (SNARE) — Abstract actors (soldiers, pilots, equipment users) cannot exit deployed systems. If tailoring is inadequate, they bear catastrophic costs with no voice in the tailoring process. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ENGINEERING TEST GROUPS (TANGLED ROPE) — Constrained by budget caps and schedule pressure. Benefit from standardized methods that reduce design uncertainty. Also harmed by prime contractor manipulation of tailoring parameters to avoid full testing. d≈0.60, f(d)≈0.78, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIME CONTRACTOR (ROPE) — Benefits from coordinating test strategy with government customer. The tailoring framework provides legitimate efficiency gains by avoiding redundant testing. Captures cost savings through reduced test scope. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Negative effective extraction = net beneficiary through coordination.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRAM MANAGER (ROPE) — Controls tailoring authority. Experiences the constraint as a coordination tool: tailoring resolves the legitimate tension between cost-efficiency and adequacy. Benefits from cost savings when tailoring is properly applied. d≈0.12, f(d)≈-0.02, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DoD STANDARDS COMMITTEE (PITON) — Maintains the tailoring standard through institutional routine. MIL-STD-810F is largely performative: the actual test limits are determined by program managers with minimal oversight from standards body. The standard persists through regulatory inertia despite limited enforcement mechanism. theater_ratio=0.58 reflects moderate performativity — some genuine engineering guidance, but much ritualistic documentation. d≈0.10, f(d)≈-0.06, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal perspective, some test tailoring is inherent to engineering: you cannot test infinite environmental permutations. This perspective sees the constraint as an immutable property of how complex systems are verified. However, base properties (ε=0.52, suppression=0.65) contradict the mountain classification. The engine will flag this as a false summit, revealing that the 'inherent to engineering' framing naturalizes what is actually a contestable institutional arrangement with asymmetric decision power.
constraint_indexing:constraint_classification(mil_std_810f_tailoring, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mil_std_810f_tailoring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mil_std_810f_tailoring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mil_std_810f_tailoring, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mil_std_810f_tailoring, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mil_std_810f_tailoring, TR),
    TR >= 0.70.

:- end_tests(mil_std_810f_tailoring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Prime contractors systematically use tailoring authority to reduce test scope beyond what risk-based analysis would justify. The extraction is constrained by some oversight and technical legitimacy of tailoring concept, but the asymmetric cost incentives create systematic bias toward under-testing. The increase from 0.28 to 0.52 over the interval reflects intensifying cost pressure in defense contracting. Suppression (0.65): High. Subcontractors cannot realistically challenge tailoring decisions without contract termination risk. Field users have zero formal input into parameters. Tailoring rationale is documented but not transparently justified. Suppression is structural to the contracting hierarchy. Theater ratio (0.58): Moderate-high. The standard prescribes processes for tailoring (documentation, justification, etc.) but actual enforcement of the justification quality is minimal. Program managers with cost incentives determine the 'sufficiency' of tailoring arguments with limited technical oversight. The theater ratio's increase reflects that the ritualistic documentation has become more elaborate while substantive analysis remains superficial.
 *
 * PERSPECTIVAL GAP:
 *   Prime contractors and program managers experience the constraint as legitimate coordination (Rope) — tailoring solves the real problem of economically infeasible comprehensive testing. Subcontractors experience systematic extraction (Snare) — they bear cost of compliance with decisions they cannot review. Field users experience maximum extraction (Snare) — they cannot exit deployed systems and bear catastrophic costs if tailoring was inadequate. Engineering test groups experience mixed coordination-extraction (Tangled Rope) — the standard provides useful guidance but is weaponized by contractors to avoid adequate testing. The DoD Standards Committee experiences its own authority as degraded (Piton) — the standard exists but is not substantively enforced. The analytical observer risks naturalizing the constraint as inherent to engineering (Mountain) — the perspectival gap reveals this as a false summit masking institutional asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Subcontractors: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction; cannot exit or challenge. Field users: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction; zero voice in process. Engineering test groups: Victim + constrained → d≈0.60, f(d)≈0.78. Significant extraction but some capacity to push back through technical expertise. Prime contractors: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary through cost capture and decision authority. Program managers: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.02. Net beneficiary through budget control. DoD Standards Committee: Institutional + arbitrage → d≈0.10, f(d)≈-0.06. Neutral to slightly positive; maintains standard through routine without major cost/benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Tangled Rope classification captures the true structure: the constraint combines a genuine coordination function (economically efficient test planning) with systematic asymmetric extraction (cost-driven under-testing that transfers risk from contractors to subcontractors and field users). The Rope perspective (prime contractor) represents the legitimate coordination benefit. The Snare perspectives (subcontractors, field users) represent the extracted victims. The Piton perspective (standards committee) shows institutional theater masking the asymmetry. The analytical observer's Mountain is a false summit — the 'inherent to engineering' framing naturalizes what is actually a contingent institutional arrangement with biased decision authority. The Tangled Rope prevents mislabeling this as either 'pure efficiency coordination' (which would ignore the systematic extraction) or 'pure predatory extraction' (which would ignore the genuine cost-efficiency gains of tailoring). The mandatrophy is resolved by acknowledging both functions operate simultaneously and asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tailoring_transparency_threshold,
    'What degree of transparency in tailoring rationale would distinguish legitimate cost-benefit optimization from strategic underfunding of critical tests?',
    'Comparative analysis of field failure modes vs tailored test envelopes; correlation between contractor cost savings and actual field reliability; post-mortem examination of systems that failed outside tailored limits',
    'If threshold shows systematic bias toward cost savings over risk: Snare classification strengthened, extraction mechanism confirmed. If failures track well with tailoring: Rope classification strengthened, coordination mechanism confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tailoring_transparency_threshold, empirical, 'Transparency threshold for distinguishing cost optimization from strategic underfunding').

omega_variable(
    subcontractor_exit_viability,
    'Can subcontractors realistically challenge prime contractor tailoring decisions without contract termination risk?',
    'Survey of subcontractor experience with raising test adequacy concerns; analysis of contract termination patterns following technical objections; examination of alternative procurement pathways available to subcontractors',
    'If exit is viable: Snare perspective weakens (constrained instead of trapped), chi drops. If exit is blocked: Snare perspective confirmed, extraction ceiling removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subcontractor_exit_viability, empirical, 'Whether subcontractors can challenge tailoring decisions without termination risk').

omega_variable(
    performance_correlation_environmental_scope,
    'Does the correlation between environmental test scope and field reliability differ significantly when controlled for contractor cost pressure vs when contractor cost pressure is excluded?',
    'Regression analysis of field failure modes vs test scope across systems with comparable operational environments but different contracting cost structures; analysis of voluntary over-testing vs contractually-mandated scope',
    'If correlation weakens when cost pressure removed: extraction mechanism confirmed. If correlation remains strong: environmental variation is the primary driver, and tailoring represents legitimate efficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_correlation_environmental_scope, empirical, 'Whether cost pressure affects correlation between test scope and field reliability').

omega_variable(
    field_user_representation_in_tailoring,
    'Do field users (soldiers, pilots, equipment operators) have any formal input into tailoring parameters for systems they will deploy and operate?',
    'Audit of stakeholder engagement procedures in tailoring process; comparison of DoD systems with field user input vs those without; analysis of downstream failure modes and user complaints pre/post-deployment',
    'If no formal field user input: Snare classification confirmed (victims trapped, voiceless). If field users have formal channel: Tangled Rope or Rope classification more appropriate (asymmetry reduced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(field_user_representation_in_tailoring, empirical, 'Field user representation in tailoring decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mil_std_810f_tailoring, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mil810f_tr_t0, mil_std_810f_tailoring, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mil810f_tr_t15, mil_std_810f_tailoring, theater_ratio, 15, 0.5).
narrative_ontology:measurement(mil810f_tr_t30, mil_std_810f_tailoring, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(mil810f_be_t0, mil_std_810f_tailoring, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mil810f_be_t15, mil_std_810f_tailoring, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(mil810f_be_t30, mil_std_810f_tailoring, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mil_std_810f_tailoring, enforcement_mechanism).
narrative_ontology:affects_constraint(mil_std_810f_tailoring, defense_supplier_power_asymmetry).
narrative_ontology:affects_constraint(mil_std_810f_tailoring, field_reliability_knowledge_gap).

% DUAL FORMULATION NOTE:
% MIL-STD-810F tailoring is downstream of both the supplier power asymmetry in defense contracting and the field reliability knowledge gap between planners and users. The standard operationalizes cost pressure from the asymmetry into test scope decisions, and it blocks field user input that could reduce the knowledge gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mil_std_810f_tailoring, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
