% ============================================================================
% CONSTRAINT STORY: arbitration_as_claim_suppression_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arbitration_as_claim_suppression_infrastructure, []).

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
 *   constraint_id: arbitration_as_claim_suppression_infrastructure
 *   human_readable: Mandatory Arbitration as Claim Suppression Infrastructure
 *   domain: labor_law/dispute_resolution/corporate_governance
 *
 * SUMMARY:
 *   Mandatory arbitration in employment contracts has evolved from a niche
 *   dispute resolution mechanism (1990s) into comprehensive claim suppression
 *   infrastructure (2020s). The constraint operates through multiple
 *   suppression layers: upfront filing costs ($200-$400) that exceed weekly
 *   take-home pay for low-wage workers; information asymmetry (workers
 *   unaware of rights or arbitration implications); attorney unavailability
 *   (contingency fee model unworkable due to reduced recovery rates and
 *   discovery limitations); repeat-player advantage (arbitrators dependent on
 *   employer business); and procedural barriers (shortened statutes of
 *   limitation, limited discovery, confidentiality provisions preventing
 *   pattern detection). The observable delta is stark: 98% of workers subject
 *   to mandatory arbitration never file claims, compared to estimated 25-40%
 *   violation rates in wage theft audits. The $9.27 billion wage theft
 *   recovery gap for workers earning <$13/hour subject to arbitration
 *   represents direct measurement of extraction. The theater_ratio (0.81)
 *   reflects that the 'alternative dispute resolution' framing is
 *   performative: the system's function is not to resolve disputes
 *   alternatively but to prevent disputes from being raised at all. Legal
 *   rights (minimum wage, overtime, anti-discrimination, safety standards)
 *   remain formally valid but become unenforceable abstractions for the 60+
 *   million workers subject to mandatory arbitration.
 *
 * KEY AGENTS:
 *   - Low-Wage Workers with Valid Claims: Primary victim (powerless/trapped) — bear full cost of wage theft and claim suppression; cannot exit without finding arbitration-free employment (increasingly rare)
 *   - Employers with Systemic Violations: Primary beneficiary (institutional/arbitrage) — capture wage theft proceeds; experience 98% claim suppression as efficiency; can exit arbitration at will but choose not to
 *   - Arbitration Provider Firms: Secondary beneficiary (institutional/arbitrage) — revenue model depends on employer satisfaction; repeat-player advantage creates structural bias
 *   - Employment Attorneys: Secondary victim (moderate/constrained) — professional capacity to enforce labor law systematically suppressed; contingency model unworkable in arbitration
 *   - Labor Rights Advocacy Coalition: Organized agents (organized/constrained) — see mixed coordination and extraction; have agency to challenge system but face resource and political constraints
 *   - State Regulatory Agencies: Institutional actors (institutional/mobile) — see scaffold with sunset as public enforcement grows and state bans spread
 *   - Federal Arbitration Act Framework: Institutional artifact (institutional/analytical) — piton perspective: original commercial dispute function atrophied, application to employment is institutional drift
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees pure extraction infrastructure with minimal coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arbitration_as_claim_suppression_infrastructure, 0.78).
domain_priors:suppression_score(arbitration_as_claim_suppression_infrastructure, 0.92).
domain_priors:theater_ratio(arbitration_as_claim_suppression_infrastructure, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arbitration_as_claim_suppression_infrastructure, extractiveness, 0.78).
narrative_ontology:constraint_metric(arbitration_as_claim_suppression_infrastructure, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(arbitration_as_claim_suppression_infrastructure, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arbitration_as_claim_suppression_infrastructure, snare).
narrative_ontology:human_readable(arbitration_as_claim_suppression_infrastructure, "Mandatory Arbitration as Claim Suppression Infrastructure").
narrative_ontology:topic_domain(arbitration_as_claim_suppression_infrastructure, "labor_law/dispute_resolution/corporate_governance").

domain_priors:requires_active_enforcement(arbitration_as_claim_suppression_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arbitration_as_claim_suppression_infrastructure, employers_with_systemic_violations).
narrative_ontology:constraint_beneficiary(arbitration_as_claim_suppression_infrastructure, arbitration_provider_firms).
narrative_ontology:constraint_victim(arbitration_as_claim_suppression_infrastructure, low_wage_workers_with_valid_claims).
narrative_ontology:constraint_victim(arbitration_as_claim_suppression_infrastructure, legal_rights_as_enforceable_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKER (SNARE) — Trapped by economic dependency and information asymmetry. The arbitration clause was signed as condition of employment; exit requires finding new employment without arbitration (increasingly rare). Filing costs ($200-$400 upfront) exceed weekly take-home pay. No attorney will accept case on contingency due to arbitration's structural disadvantages. Legal rights exist in abstract but are unenforceable in practice. Maximum extraction: wage theft persists unchallenged, and the worker bears full cost of the suppression mechanism.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYMENT ATTORNEY (SNARE) — Constrained by economic rationality of case selection. Can exit individual cases but cannot exit the arbitration system's structural effects on practice viability. Arbitration reduces expected recovery by 60-80% compared to litigation, eliminates discovery rights, and caps damages. Contingency fee model becomes unworkable: cases that would be viable in court are rejected in arbitration. The attorney experiences this as extraction: professional capacity to enforce labor law is systematically suppressed. Not trapped (can practice other law) but constrained within employment law specialty.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYER WITH SYSTEMIC VIOLATIONS (ROPE) — Primary beneficiary. Arbitration functions as coordination: standardizes dispute resolution, reduces litigation costs, provides predictable outcomes. The 98% claim suppression rate is experienced as efficiency, not extraction. Can exit arbitration system at will (remove clause from contracts) but chooses not to because benefits are substantial. Experiences constraint as pure coordination mechanism solving the 'problem' of employee litigation.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ARBITRATION PROVIDER (ROPE) — Secondary beneficiary. Repeat-player advantage: employers provide ongoing business; workers are one-time participants. Revenue model depends on employer satisfaction. Experiences arbitration as coordination: providing dispute resolution services, maintaining arbitrator panels, administering cases. The structural bias toward repeat players is invisible from this position. Can exit relationships with specific employers but benefits from the overall system.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR ADVOCACY COALITION (TANGLED ROPE) — Organized agents (worker centers, legal aid, policy advocates) see both coordination and extraction. Arbitration does solve some disputes (genuine coordination function exists for workers with resources and knowledge). But asymmetric extraction dominates: claim suppression infrastructure prevents enforcement of labor law at scale. Coalition has agency to challenge system through legislation, litigation, and organizing, but faces resource constraints and political opposition. Mixed experience: some workers helped through arbitration support, but systemic extraction persists.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE REGULATORY AGENCY (SCAFFOLD) — Sees arbitration as temporary coordination mechanism with sunset logic. State enforcement of labor law (wage-hour divisions, OSHA, EEOC) continues regardless of arbitration clauses. As public enforcement capacity grows and state-level arbitration bans spread (California AB 51, New York S2844A), the private arbitration system's claim suppression function will decline. Sunset timeline: 10-20 years as state enforcement budgets increase and legislative bans proliferate. Agency has exit options (can deprioritize arbitration cases or focus on pattern enforcement) and sees structural path to reduced extraction.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: FAA FRAMEWORK (PITON) — The statutory framework (FAA 1925) was designed for commercial disputes between merchants of equal bargaining power. Its application to adhesion employment contracts is institutional drift: the original coordination function (efficient resolution of commercial disputes) has atrophied, but the framework persists through judicial inertia and Supreme Court expansion (Concepcion 2011, Epic Systems 2018). Theater ratio high: the 'consent' and 'alternative dispute resolution' framing is performative. The framework sees its own degradation: application to employment far exceeds original purpose, maintained not because it works for workers but because it works for employers.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From civilizational/global perspective, mandatory arbitration in adhesion employment contracts is pure extraction infrastructure. The claim filing rate (2% vs 100% baseline if rights were enforceable) reveals suppression, not coordination. The $9.27 billion wage theft recovery gap for low-wage workers is direct measurement of extraction. Coordination function exists but is minimal: genuine dispute resolution occurs in <5% of cases where workers have resources to pursue claims. Asymmetric extraction dominates: employers capture wage theft proceeds, workers bear enforcement costs, legal rights become unenforceable abstractions. This is the constraint's structural reality, not a perspective-dependent reading.
constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arbitration_as_claim_suppression_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arbitration_as_claim_suppression_infrastructure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arbitration_as_claim_suppression_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arbitration_as_claim_suppression_infrastructure, TR),
    TR >= 0.70.

:- end_tests(arbitration_as_claim_suppression_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The $9.27 billion wage theft recovery gap for low-wage workers subject to arbitration is direct measurement. Employers capture wage theft proceeds that would be recovered in litigation. Workers bear enforcement costs (filing fees, lost wages for hearings, attorney unavailability) that exceed claim value. The 98% claim suppression rate reveals that extraction operates primarily through deterrence rather than through biased adjudication of filed claims. Suppression (0.92): Very high. Multiple suppression layers operate simultaneously: economic (filing costs exceed weekly pay), informational (workers unaware of rights or arbitration implications), structural (attorney unavailability due to contingency model failure), procedural (limited discovery, shortened statutes of limitation), and reputational (confidentiality provisions prevent pattern detection and worker coordination). Exit options are minimal: finding arbitration-free employment requires switching sectors or employers, increasingly difficult as arbitration clauses spread to 60+ million workers. Theater ratio (0.81): High. The 'alternative dispute resolution' framing is performative. The system's function is dispute prevention, not dispute resolution. The 'consent' narrative (workers 'agree' to arbitration) is theatrical: clauses are buried in onboarding paperwork, presented as non-negotiable conditions of employment, and workers lack information to evaluate implications. The procedural fairness theater (neutral arbitrator, hearing process, written decision) obscures that 98% of claims are suppressed before filing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Employers see rope (coordination mechanism solving litigation 'problem'). Workers see snare (legal rights rendered unenforceable). Attorneys see snare (professional capacity systematically suppressed). Advocacy coalitions see tangled rope (mixed coordination and extraction). State agencies see scaffold (temporary problem with sunset as enforcement grows). The FAA framework sees piton (degraded institutional artifact maintained through inertia). The analytical observer sees snare (pure extraction infrastructure). The gap is not about different interpretations of the same facts — it reflects different structural positions relative to the extraction flow. Beneficiaries experience coordination; victims experience extraction. The 98% claim suppression rate is efficiency from the employer perspective, rights nullification from the worker perspective. The mandatrophy resolution: all perspectives are structurally valid readings from their positions. The analytical classification (snare) reflects the constraint's dominant structural feature (asymmetric extraction vastly exceeds coordination function), but the beneficiary's rope experience is also real.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers are primary victims with trapped exit options, yielding high d → high f(d) → high chi. They experience maximum extraction: wage theft persists, legal rights become unenforceable, and they bear costs of the suppression mechanism. Employment attorneys are secondary victims with constrained exit options, yielding moderate-high d. They experience extraction as systematic suppression of professional capacity to enforce labor law, but can exit to other practice areas. Employers with systemic violations are primary beneficiaries with arbitrage exit options, yielding low d → negative f(d) → negative chi. They experience the constraint as pure coordination: efficient dispute resolution with predictable outcomes. Arbitration providers are secondary beneficiaries, also with arbitrage exit. Labor advocacy coalitions are organized agents with constrained exit, yielding moderate d — they see both coordination (some disputes resolved) and extraction (systemic claim suppression). State agencies have mobile exit options and see scaffold (sunset as public enforcement grows). The analytical observer sees the structural reality: high extraction with minimal coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as snare at the analytical level because asymmetric extraction dominates the structural relationship. The coordination function is real but minimal: genuine dispute resolution occurs in <5% of cases where workers have resources to pursue claims. The 98% claim suppression rate, $9.27 billion wage theft recovery gap, and attorney case acceptance rate differential (60-80% reduction in arbitration) are direct measurements of extraction magnitude. However, the beneficiary's rope experience is not false consciousness — employers genuinely experience arbitration as coordination mechanism. The perspectival gap is the diagnostic signal: when beneficiaries see rope and victims see snare from the same base properties, the constraint is tangled rope or snare depending on whether coordination function is substantial (tangled rope) or minimal (snare). Here, coordination is minimal — the system's primary function is claim suppression, not dispute resolution. The snare classification at analytical level does not invalidate the rope classification at beneficiary level; it identifies which structural feature dominates. The mandatrophy is resolved by recognizing that classification is indexical: the constraint IS rope from the employer position AND snare from the worker position. The analytical classification aggregates across positions to identify dominant structural feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    claim_suppression_vs_frivolous_filtering,
    'Does the 98% non-filing rate represent suppression of valid claims or filtering of frivolous ones?',
    'Comparison of claim validity rates in arbitration-free sectors vs arbitration-mandatory sectors for similar worker populations; audit studies of wage theft prevalence vs claim filing rates; attorney case evaluation data comparing arbitration vs litigation acceptance criteria',
    'If suppression: snare classification confirmed across all perspectives except beneficiaries. If filtering: some rope characteristics emerge (coordination function is real). Empirical data strongly suggests suppression: wage theft audit studies show 25-40% violation rates in low-wage sectors, but claim filing is 2%.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(claim_suppression_vs_frivolous_filtering, empirical, 'Whether low filing rate reflects claim suppression or frivolous case filtering').

omega_variable(
    arbitrator_bias_mechanism,
    'Is arbitrator bias toward repeat-player employers a structural feature of the arbitration market or a correctable selection problem?',
    'Randomized arbitrator assignment experiments; longitudinal outcome tracking for arbitrators with varying employer repeat-business exposure; comparison of arbitrator selection mechanisms (party-choice vs random assignment vs list-strike)',
    'If structural (repeat-player advantage is inherent to private arbitration market): mountain component emerges (immutable feature of privatized adjudication). If correctable: tangled rope (coordination with extractive implementation that could be reformed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arbitrator_bias_mechanism, empirical, 'Whether arbitrator bias is structural or correctable').

omega_variable(
    information_asymmetry_persistence,
    'Would disclosure requirements and mandatory arbitration transparency reduce claim suppression, or is the suppression mechanism primarily economic (filing costs, attorney unavailability) rather than informational?',
    'Natural experiments in jurisdictions with arbitration transparency mandates; comparison of claim filing rates before/after disclosure requirements; survey data on worker awareness of arbitration clauses and rights',
    'If informational: scaffold perspective strengthened (transparency reforms could sunset the extraction). If economic: snare persists regardless of information (structural barriers remain even with full knowledge).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether claim suppression is informational or economic').

omega_variable(
    class_action_waiver_separability,
    'Is the extraction primarily from individual arbitration (vs litigation) or from class action waiver (preventing aggregate claims)?',
    'Comparison of recovery rates in individual arbitration vs individual litigation vs class litigation; analysis of claim types suppressed by arbitration (individual wage theft vs systemic pattern claims); legislative experiments separating arbitration mandate from class waiver',
    'If class waiver is primary: constraint should decompose into two stories (individual arbitration + class waiver) with different epsilon values. If individual arbitration is primary: current single-story model is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_action_waiver_separability, conceptual, 'Whether to decompose arbitration and class waiver into separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arbitration_as_claim_suppression_infrastructure, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arb_theater_1990, arbitration_as_claim_suppression_infrastructure, theater_ratio, 0, 0.45).
narrative_ontology:measurement(arb_theater_2000, arbitration_as_claim_suppression_infrastructure, theater_ratio, 10, 0.62).
narrative_ontology:measurement(arb_theater_2010, arbitration_as_claim_suppression_infrastructure, theater_ratio, 20, 0.74).
narrative_ontology:measurement(arb_theater_2020, arbitration_as_claim_suppression_infrastructure, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(arb_extract_1990, arbitration_as_claim_suppression_infrastructure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arb_extract_2000, arbitration_as_claim_suppression_infrastructure, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(arb_extract_2010, arbitration_as_claim_suppression_infrastructure, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(arb_extract_2020, arbitration_as_claim_suppression_infrastructure, base_extractiveness, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arbitration_as_claim_suppression_infrastructure, enforcement_mechanism).
narrative_ontology:affects_constraint(arbitration_as_claim_suppression_infrastructure, wage_theft_enforcement_gap).
narrative_ontology:affects_constraint(arbitration_as_claim_suppression_infrastructure, legal_rights_as_unenforceable_abstractions).

% DUAL FORMULATION NOTE:
% This constraint is downstream of repeat_player_structural_advantage (mountain — arbitrator bias toward repeat players is inherent to private adjudication markets) and consent_as_structural_fiction (tangled rope — adhesion contract 'consent' has genuine coordination function for some transactions but is extractive in employment context). The upstream constraints establish the structural conditions; this constraint measures the specific extraction mechanism in employment arbitration. Potential decomposition: class action waiver may be separable constraint with different epsilon (omega variable class_action_waiver_separability addresses this).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arbitration_as_claim_suppression_infrastructure, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
