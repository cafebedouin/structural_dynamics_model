% ============================================================================
% CONSTRAINT STORY: bureaucratic_rent_seeking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_rent_seeking, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bureaucratic_rent_seeking
 *   human_readable: Bureaucratic Rent Seeking: Institutional Self-Preservation Through Regulatory Expansion
 *   domain: political_economy/institutional_capture
 *
 * SUMMARY:
 *   Bureaucratic rent-seeking describes the structural dynamic in which
 *   regulatory agencies systematically expand rules, regulatory complexity,
 *   and enforcement scope in order to maintain and grow their budgets,
 *   authority, and staff, independent of whether additional regulation solves
 *   genuine coordination problems. The constraint exhibits the tangled rope
 *   signature: genuine coordination functions (safety standards, information
 *   standardization, collective action problem solving) coexist with
 *   asymmetric extraction (compliance costs disproportionately borne by
 *   market entrants and smaller firms, benefiting the regulatory agency's
 *   budget and authority). The temporal measurements show a 20-year
 *   trajectory of increasing extractiveness (0.28 → 0.52) and theater ratio
 *   (0.45 → 0.68), indicating that as the agency consolidates its
 *   institutional position, the regulatory apparatus becomes increasingly
 *   performative—compliance costs rising faster than coordination problems
 *   justify. Suppression intensity increases from 0.40 to 0.65, reflecting
 *   both structural barriers (legal requirements for compliance) and the
 *   agency's growing enforcement capacity. This constraint is ubiquitous
 *   across developed regulatory states: environmental protection agencies
 *   expanding pollution reporting requirements; financial regulators
 *   multiplying disclosure rules; occupational licensing boards raising
 *   credential barriers; telecommunications regulators managing spectrum
 *   allocation through ever-finer technical rules. Each exhibits genuine
 *   coordination value alongside systematic extraction serving the agency's
 *   organizational interests.
 *
 * KEY AGENTS:
 *   - Regulatory Agency: Primary beneficiary (institutional/arbitrage) — captures budget growth, authority expansion, and staffing increases as agency size grows
 *   - Small Market Entrant: Primary victim (powerless/trapped) — cannot afford compliance costs scaled to regulatory complexity; excluded from market by regulatory barriers independent of actual risk or capability
 *   - Established Regulated Firm: Mixed victim-beneficiary (powerful/constrained) — receives coordination benefits but bears compliance costs; has some negotiating power through size and political influence
 *   - Industry Trade Association: Organized victim-beneficiary (organized/constrained) — represents collective interests; influences rule-setting (benefits) but members bear compliance costs (victims); often allies with agency against smaller competitors
 *   - Legislative Oversight Body: Nominal constraint (institutional/arbitrage) — theoretically monitors agency budgets and regulatory scope; in practice exercises symbolic oversight without substantive constraint
 *   - Analytical Observer: Sees the full structure (analytical/analytical) — identifies both coordination function and asymmetric extraction; sees how agency frames budget growth as necessary complexity management
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_rent_seeking, 0.52).
domain_priors:suppression_score(bureaucratic_rent_seeking, 0.65).
domain_priors:theater_ratio(bureaucratic_rent_seeking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_rent_seeking, extractiveness, 0.52).
narrative_ontology:constraint_metric(bureaucratic_rent_seeking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bureaucratic_rent_seeking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_rent_seeking, tangled_rope).
narrative_ontology:human_readable(bureaucratic_rent_seeking, "Bureaucratic Rent Seeking: Institutional Self-Preservation Through Regulatory Expansion").
narrative_ontology:topic_domain(bureaucratic_rent_seeking, "political_economy/institutional_capture").

domain_priors:requires_active_enforcement(bureaucratic_rent_seeking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_rent_seeking, regulatory_agency).
narrative_ontology:constraint_victim(bureaucratic_rent_seeking, regulated_industries).
narrative_ontology:constraint_victim(bureaucratic_rent_seeking, market_entrants).
narrative_ontology:constraint_victim(bureaucratic_rent_seeking, general_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MARKET ENTRANT (SNARE) — Trapped by compliance costs that scale with regulatory complexity regardless of actual risk. Cannot exit or arbitrage; must absorb full regulatory burden. Bureaucratic expansion is pure extraction with no coordination benefit perceived by this agent — the entrant cannot afford to enter at all.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ESTABLISHED REGULATED FIRM (TANGLED ROPE) — Benefits from coordination function (predictable rules, information standardization, safety standards) but also bears asymmetric extraction through compliance costs that benefit the regulatory agency's budget expansion. Constrained exit: can move to less regulated jurisdictions but at significant cost. Experiences genuine mixed coordination and extraction.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AGENCY (ROPE) — Primary beneficiary. Experiences the regulatory system as pure coordination: standardizing rules, enabling market operation, solving information asymmetries. The agency perceives itself as solving genuine coordination problems. Its budget growth and staffing expansion are viewed as necessary to manage complexity, not as self-interested extraction. Low d value: full institutional beneficiary with arbitrage options (can recalibrate scope, seek different regulatory mandates).
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRY TRADE ASSOCIATION (TANGLED ROPE) — Represents collective interests of established firms. Coordinating function: works with agency to set coherent standards, negotiate compliance timelines, develop industry best practices. Extraction function: rent-seeking agency often sets rules that advantage large established members over small entrants. Trade association is both beneficiary (influence on rule-setting) and partial victim (members still bear compliance costs). Organized power modulates experienced extraction downward from full snare to tangled rope.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE OVERSIGHT BODY (PITON) — Theoretically responsible for authorizing regulatory budgets and monitoring scope creep. In practice, exercises symbolic rather than substantive oversight. Oversight rituals (budget hearings, sunset review clauses) exist but rarely constrain agency behavior. Theater ratio elevated because the performative oversight apparatus persists despite low functional constraint on agency expansion. High theatrical content reflects that legislatures conduct oversight reviews without changing agency budgets or mandates.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Identifies both genuine coordination functions (safety standards, information standardization, collective action problem solving) AND systematic extraction (regulatory scope expanding faster than coordination problems justify). The constraint exhibits tangled rope signature: coordination function is real and beneficial, but extraction through budget and authority growth is asymmetric and obscured by framing as necessary complexity management.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_rent_seeking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_rent_seeking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_rent_seeking, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_rent_seeking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_rent_seeking, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_rent_seeking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The regulatory agency captures substantial extraction through budget growth, staffing expansion, and authority consolidation. However, the extraction is not maximal (not pure snare territory, ε ≥ 0.66) because genuine coordination functions exist—environmental standards do reduce pollution, financial disclosure does mitigate information asymmetries, occupational licensing does maintain quality signals. The 20-year trajectory shows extractiveness rising from 0.28 to 0.52, indicating that regulatory expansion is outpacing genuine coordination problem growth. Suppression (0.65): Moderate-high. The regulatory apparatus creates substantial barriers to non-compliance through legal requirements, enforcement capacity, and audit mechanisms. But suppression is not absolute (not mountain-level, ≤0.05) because violation is possible at a cost—firms can lobby for rule changes, litigate regulatory overreach, or relocate to lighter-touch jurisdictions. The 20-year increase (0.40 → 0.65) reflects both legal entrenchment and growth in enforcement infrastructure. Theater ratio (0.68): High. Regulatory processes become increasingly performative as agencies develop compliance bureaucracies that operate semi-independently of the underlying coordination problem. Environmental agencies conduct environmental impact assessments for projects with trivial environmental consequences; financial regulators require disclosures that market participants already know; licensing boards maintain exam standards that test credential rather than competence. The high theater reflects the gap between the stated coordination function and actual compliance practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between the regulatory agency's perception of itself (solving genuine coordination problems justifying budget growth) and the structural reality of asymmetric extraction. From the agency's perspective (institutional/arbitrage), the constraint appears as rope—pure coordination with no extraction. The agency genuinely sees itself as solving information asymmetries, enabling market operation, and protecting public goods. From the small entrant's perspective (powerless/trapped), the same constraint appears as snare—pure extraction with no coordination benefit (they cannot enter the market at all). From the established firm's perspective (powerful/constrained), it appears as tangled rope—real coordination benefits (predictable rules, information standardization) but also real extraction costs (compliance disproportionate to actual risk). The analytical observer sees the full structure: the agency's budget and authority grow faster than coordination problems justify, indicating that expansion serves organizational self-interest more than problem-solving. The perspectival gap between beneficiary and victim is the extraction mechanism: what the agency experiences as necessary coordination appears to victims as bureaucratic over-reach.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary across perspectives based on structural position. The regulatory agency (institutional/arbitrage beneficiary) has d ≈ 0.05 — it captures extraction value while experiencing the constraint as beneficial coordination. The small entrant (powerless/trapped victim) has d ≈ 0.95 — bears full cost of regulatory complexity with no exit option. The established firm (powerful/constrained victim-beneficiary) has d ≈ 0.60 — mixed position: benefits from coordination but bears compliance costs; powerful enough to negotiate some relief but constrained by legal requirements. The trade association (organized/constrained mixed) has d ≈ 0.45 — organized power modulates its experienced extraction downward from the snare range. The legislative body (institutional/arbitrage nominal overseer) has d ≈ 0.10 — theoretically positions it to constrain extraction, but in practice the legislative perspective applies weak constraint (low effective power). These d values flow through the sigmoid f(d) to produce constraint-relative extractiveness (χ): beneficiaries experience low chi, victims experience high chi, organized agents experience modulated chi proportional to their power.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA PERSPECTIVAL MULTIPLICITY: Bureaucratic rent-seeking is a genuine tangled rope — not a classification error or mislabeled snare. The coordination function is real: regulatory agencies do solve information asymmetries, enable collective action, establish quality signals, and internalize externalities. But the extraction is also real: budgets grow faster than coordination problems justify, enforcement intensity escalates beyond what problem-solving requires, and compliance costs concentrate on those least able to bear them (small entrants, new market competitors). The mandatrophy dissolves when we recognize that no single perspective captures the full constraint. The agency sees rope (pure coordination). The entrant sees snare (pure extraction). The established firm sees tangled rope (mixed). The analytical observer sees tangled rope (coordination + asymmetric extraction). The engine's computed `constraint_claim` is tangled rope because it represents the full structural picture: genuine coordination function + demonstrable asymmetric extraction + active enforcement required to maintain the extraction. The resolution is not to declare one perspective 'correct' but to recognize that the constraint's true nature is the presheaf of all perspectives—the agency is solving real problems while systematically expanding beyond what those problems require, and this contradiction is the constraint's essential structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_expansion_attribution,
    'How much of observed regulatory expansion reflects genuine increase in coordination problems (new technologies, market scale, complexity) versus self-interested agency budget growth?',
    'Comparative analysis of rule growth rates across agencies with different coordination functions; correlation between market complexity indicators and regulatory scope change; longitudinal tracking of regulatory metrics in response to actual vs claimed coordination failures',
    'If expansion tracks genuine complexity: constraint is legitimate rope or lower tangled rope (ε ≤ 0.35). If expansion outpaces coordination needs: constraint is snare or higher tangled rope (ε ≥ 0.50).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_expansion_attribution, empirical, 'Attribution of regulatory expansion to coordination need vs agency self-interest').

omega_variable(
    compliance_cost_recovery_asymmetry,
    'Are regulatory compliance costs recovered through improved market efficiency / reduced information asymmetries, or do they primarily flow to agency budgets and staffing?',
    'Cost-benefit analysis of major regulations; comparison of industry cost savings from standardization vs increased spending on compliance infrastructure and agency operations; empirical measurement of information asymmetry reduction post-regulation',
    'If costs are recovered: extraction is lower (ε ≈ 0.30-0.40), classification shifts toward legitimate rope. If unrecovered: extraction is higher (ε ≈ 0.55-0.70), classification remains snare/tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_recovery_asymmetry, empirical, 'Whether compliance costs produce offsetting efficiency gains or flow to agency extraction').

omega_variable(
    alternative_coordination_mechanisms_viability,
    'Could industry self-regulation, peer certification, private standard-setting bodies, or market reputation mechanisms achieve the same coordination functions at lower cost?',
    'Case studies of sectors with private standard-setting (e.g., finance, software, pharmaceuticals); comparison of outcomes under private vs regulatory coordination; analysis of which coordination functions require enforcement vs those achieved through reputation',
    'If alternatives are viable: the regulatory extraction is higher (agency is unnecessary for coordination), and suppression is more clearly coercive. If alternatives fail: regulation is more clearly necessary, extraction is more justified, and classification may drop to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms_viability, empirical, 'Viability of non-regulatory coordination mechanisms').

omega_variable(
    agency_capture_vs_mission_drift,
    'Is regulatory expansion driven by the agency''s own structural incentives (budget maximization, scope growth) or by capture by regulated industries seeking to entrench competitive advantages?',
    'Analysis of agency budget requests vs legislative authorization; tracking of staff growth vs workload metrics; comparison of agencies with strong legislative oversight vs weak oversight; examination of which firms benefit most from new regulations',
    'If agency-driven: classification remains tangled rope with snare overtones (pure self-interested extraction). If industry-captured: classification may shift to institutional capture with different beneficiary structure (captured regulator benefits the industry, not the agency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_capture_vs_mission_drift, empirical, 'Primary driver of expansion: agency self-interest vs regulatory capture').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is regulatory suppression structural (legal barriers to non-compliance) versus internalized (regulated entities believe regulation is necessary even if burdensome)?',
    'Analysis of compliance behavior when enforcement is weakened or absent; surveys of industry attitudes toward regulatory necessity; examination of firm behavior in low-enforcement periods; comparison of compliance rates across jurisdictions with different enforcement intensity',
    'If primarily structural: suppression score is justified (0.65). If partially internalized: the constraint''s true suppressive force is higher than the structural measure suggests (internalization persists after regulatory removal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_rent_seeking, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brs_tr_t0, bureaucratic_rent_seeking, theater_ratio, 0, 0.45).
narrative_ontology:measurement(brs_tr_t10, bureaucratic_rent_seeking, theater_ratio, 10, 0.58).
narrative_ontology:measurement(brs_tr_t20, bureaucratic_rent_seeking, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(brs_be_t0, bureaucratic_rent_seeking, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(brs_be_t10, bureaucratic_rent_seeking, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(brs_be_t20, bureaucratic_rent_seeking, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(brs_su_t0, bureaucratic_rent_seeking, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(brs_su_t10, bureaucratic_rent_seeking, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(brs_su_t20, bureaucratic_rent_seeking, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_rent_seeking, enforcement_mechanism).
narrative_ontology:affects_constraint(bureaucratic_rent_seeking, regulatory_capture).
narrative_ontology:affects_constraint(bureaucratic_rent_seeking, occupational_licensing_cartels).
narrative_ontology:affects_constraint(bureaucratic_rent_seeking, financial_disclosure_theater).

% DUAL FORMULATION NOTE:
% Bureaucratic rent-seeking is an upstream structural constraint that affects multiple downstream domain-specific manifestations. Regulatory capture (agency captured by industry) is a sibling constraint with different beneficiary structure. Occupational licensing and financial disclosure theater are downstream applications of the same bureaucratic self-expansion mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
