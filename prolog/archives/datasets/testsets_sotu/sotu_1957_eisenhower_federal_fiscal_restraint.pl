% ============================================================================
% CONSTRAINT STORY: sotu_1957_eisenhower_federal_fiscal_restraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1957_eisenhower_federal_fiscal_restraint, []).

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
 *   constraint_id: sotu_1957_eisenhower_federal_fiscal_restraint
 *   human_readable: Eisenhower's 1957 Federal Fiscal Restraint Mandate
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   Eisenhower's 1957 State of the Union proposes a structural constraint on
 *   federal spending whereby departments must systematically identify cost
 *   savings and operate within executive-determined aggregate fiscal limits.
 *   The constraint operates as a pre-emptive inflation control mechanism: by
 *   limiting spending growth through executive discipline rather than
 *   legislative appropriation controls, the administration attempts to
 *   protect purchasing power without congressional veto battles. The
 *   constraint exhibits the full range of DR classifications from different
 *   perspectives. It appears as pure extraction (snare) to federal agencies
 *   seeking programmatic expansion; as coordination with asymmetric cost
 *   distribution (tangled rope) to congressional coalitions and state
 *   governments; as pure coordination (rope) to the executive branch and to
 *   inflation-conscious savers; and as degraded ritual (piton) to
 *   appropriations committees whose deliberative power has been
 *   pre-constrained. The constraint's theater ratio (0.54) reflects moderate
 *   performative content: the executive maintains public messaging about
 *   'fiscal responsibility' and 'prudent resource allocation,' but the core
 *   enforcement mechanism is genuine — budget authority limits, not ritual.
 *   The extractiveness trajectory (0.22 → 0.40) shows increasing extraction
 *   as the constraint's enforcement hardens and Democratic resistance
 *   escalates. The theater ratio rise (0.38 → 0.58) reflects the
 *   appropriations committees' increasing performativity as their real
 *   decisional space contracts.
 *
 * KEY AGENTS:
 *   - Eisenhower Administration: Primary beneficiary (institutional/arbitrage) — controls fiscal constraint framework and prevents agency mission-creep; benefits from unified executive coordination
 *   - Federal Agencies (Defense, State, Interior, etc.): Primary victim (powerless/trapped) — face binding spending constraints and cannot unilaterally expand programs or redirect savings; suppressed programmatic impulses
 *   - Democratic Congressional Coalition: Secondary victim (organized/constrained) — seeks programmatic expansion but faces executive fiscal constraint; can override through legislative action but at high political cost
 *   - Inflation-Conscious Savers and Fixed-Income Pensioners: Beneficiary (powerful/arbitrage) — purchasing power protected by pre-emptive inflation control; exit available if mechanism fails (commodity hedges, foreign assets)
 *   - State and Local Governments: Mixed (moderate/constrained) — benefit from fiscal stability in federal revenue forecasting, but constrained in accessing federal matching funds for capital projects
 *   - Congressional Appropriations Committees: Ritualized (institutional/arbitrage) — maintain constitutional appropriations power but function degraded by executive constraint; continue detailed budget review performatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1957_eisenhower_federal_fiscal_restraint, 0.38).
domain_priors:suppression_score(sotu_1957_eisenhower_federal_fiscal_restraint, 0.48).
domain_priors:theater_ratio(sotu_1957_eisenhower_federal_fiscal_restraint, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1957_eisenhower_federal_fiscal_restraint, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1957_eisenhower_federal_fiscal_restraint, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1957_eisenhower_federal_fiscal_restraint, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1957_eisenhower_federal_fiscal_restraint, tangled_rope).
narrative_ontology:human_readable(sotu_1957_eisenhower_federal_fiscal_restraint, "Eisenhower's 1957 Federal Fiscal Restraint Mandate").
narrative_ontology:topic_domain(sotu_1957_eisenhower_federal_fiscal_restraint, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1957_eisenhower_federal_fiscal_restraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_federal_fiscal_restraint, inflation_conscious_savers).
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_federal_fiscal_restraint, fixed_income_pensioners).
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_federal_fiscal_restraint, treasury_stewards).
narrative_ontology:constraint_victim(sotu_1957_eisenhower_federal_fiscal_restraint, federal_agencies_seeking_expansion).
narrative_ontology:constraint_victim(sotu_1957_eisenhower_federal_fiscal_restraint, domestically_focused_program_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL AGENCY PROGRAM DESIGNERS (SNARE) — Agencies face binding constraints on discretionary expansion. The mandate forecloses programmatic response to newly identified social needs without legislative override. Exit is blocked: agencies cannot unilaterally expand programs, cannot redirect savings to priority areas without executive approval, and cannot exit the constraint without violating chain of command. Maximum extraction from the agency's perspective: legitimate programmatic impulses are suppressed by fiscal discipline imposed from above. Theater is minimal — the constraint is enforced through budget authority, not ritual.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL DEMOCRATIC COALITION (TANGLED ROPE) — Democratic Congress seeks programmatic expansion (rural electrification, veterans benefits, education) but faces executive fiscal constraint. They experience mixed signals: the constraint genuinely coordinates on preventing runaway inflation (coordination benefit), but it also asymmetrically extracts from their legislative agenda (extraction cost). Exit is constrained but real: override through legislative appropriation is possible but costly (presidential veto, public messaging campaign, political capital). The coalition can mobilize and does — but at significant cost.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH / EISENHOWER ADMINISTRATION (ROPE) — The constraint is a coordination mechanism solving the executive's coordination problem: how to prevent agency mission-creep and inter-agency competition from driving spending. The executive benefits from having a unified fiscal framework; departments understand the constraint is systemic (not personal punishment) and can plan accordingly. Theater is moderate — the constraint requires some performative 'fiscal responsibility' messaging to maintain political support. But the core function is genuinely coordinative: it establishes a common frame for interdepartmental trade-offs.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INFLATION-CONSCIOUS SAVERS AND FIXED-INCOME PENSIONERS (ROPE) — The constraint protects purchasing power by pre-empting inflation-driving spending growth. These beneficiaries experience the constraint as pure coordination: fiscal restraint solves their collective action problem (no individual saver can prevent runaway government spending, but coordinated restraint does). Exit is costless — if inflation control fails, they can exit to foreign assets or commodity hedges. Theater is moderate — the constraint requires public endorsement of 'fiscal responsibility' messaging, but the underlying mechanism is genuinely protective.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE AND LOCAL GOVERNMENTS (TANGLED ROPE) — States experience mixed effects. The constraint coordinates on preventing federal spending inflation (benefit: stable federal revenue forecasting), but it also constrains the federal matching funds available for state programs (cost: highways, schools, hospitals requiring federal cost-share). Exit is constrained: states can fund programs independently but at high fiscal cost; they cannot exit the federal constraint without legislative action. States experience asymmetric extraction relative to the executive's benefit.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL APPROPRIATIONS COMMITTEES (PITON) — The appropriations process becomes increasingly performative. Committees maintain the ritual of detailed line-item budget review, but the constraint has already determined aggregate spending ceilings. The detailed work (hearings, testimony, deliberation) becomes theatrical — the real decisions are made at the aggregate constraint level by the executive. The committee's function persists through institutional inertia (committees are constitutionally empowered), but the constraint has already degraded their real decisional power. Theater ratio is moderately high because the ritual is maintained despite functional constraint.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, fiscal discipline appears as an immutable constraint on governance: 'government spending must be constrained or inflation results.' This naturalized framing presents Eisenhower's fiscal restraint as alignment with economic law rather than contingent political choice. However, structural data reveals a false summit: identifiable beneficiaries exist (savers, pensioners, treasury stewards), active enforcement is required, and alternative framing (counter-cyclical spending, demand management, targeted investment) is suppressed. The 'natural law' framing itself serves beneficiaries by making the constraint appear beyond political contestation.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1957_eisenhower_federal_fiscal_restraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1957_eisenhower_federal_fiscal_restraint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1957_eisenhower_federal_fiscal_restraint, TR),
    TR >= 0.70.

:- end_tests(sotu_1957_eisenhower_federal_fiscal_restraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high, but not maximal. The constraint imposes real costs on agencies seeking expansion and on congressional coalitions seeking programmatic growth, but it is not purely extractive because a genuine coordination function exists — preventing departmental competition and spending cascade is a real executive coordination problem. The baseline (0.22) reflects Eisenhower's initial soft enforcement and rhetorical framing. The peak (0.40 at year 6) reflects hardened enforcement and Democratic resistance. The subsequent slight decline (0.38 at year 8) reflects Congress successfully extracting some programmatic concessions and partial override of the constraint through legislative pressure. Suppression (0.48): Moderate. Agencies face binding constraints on their ability to expand programs, but suppression is not total — Congress retains override power (though costly to exercise), and some agencies find creative reinterpretation of the constraint. Savers face no suppression; they benefit from the mechanism. Theater ratio (0.54): Moderate. The constraint requires sustained executive messaging about 'fiscal responsibility' and 'prudent management' — this is performative. Congressional appropriations hearings become increasingly theatrical (detailed discussion of agency budgets that have already been constrained at aggregate level). But the core mechanism (budget authority limits) is enforced through genuine institutional power, not ritual.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The executive and savers see a coordination mechanism solving a genuine collective action problem: preventing inflation through fiscal discipline. Federal agencies and Democratic Congress see extraction: legitimate programmatic needs are suppressed by fiscal ideology imposed from above. Congressional committees see degradation: their constitutional role persists performatively while their real power has contracted. The analytical observer risks the false summit: naturalizing fiscal restraint as economic law ('government spending must be constrained or inflation results') rather than recognizing it as a contingent institutional choice that benefits specific agents (savers, treasury stewards) while imposing costs on others (agencies, expansion constituencies). The false summit framing is particularly dangerous because it makes fiscal restraint appear beyond political contestation — as if Congress has no real choice, when in fact override and alternative fiscal regimes are available (though costly).
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply across agent types. The executive and savers experience low d (beneficiary + arbitrage = low d → low or negative χ); the constraint coordinates their preferences and imposes no cost on them. Federal agencies and Democratic Congress experience high d (victim + constrained to trapped = high d → high χ); the constraint extracts from their programmatic ambitions. State governments experience moderate d (mixed beneficiary/victim + constrained = moderate d → moderate χ); they benefit from fiscal stability but pay extraction cost in constrained matching funds. Appropriations committees experience low d from an institutional perspective (arbitrage exit is real — the ritual persists through choice, not compulsion), but medium d if we account for the degradation of their actual decisional power (exit from ritual is political suicide for committee members). The perspectival gap is sharp: the executive sees coordination (rope), the agencies see extraction (snare), and Congress sees mixed (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that fiscal restraint can serve both genuine coordination (preventing spending cascade, protecting purchasing power) and genuine extraction (suppressing programmatic expansion, constraining congressional agency). The resolution lies in rejecting the false choice between 'it is purely coordination' and 'it is purely extraction.' The constraint is tangled_rope: it has a real coordination function (executive inter-departmental discipline) AND asymmetric extraction (benefiting savers/pensioners at the expense of agencies and expansion constituencies). The perspectival divergence is not measurement error — it is the correct result of analyzing the constraint from different structural positions. The federal agency sees extraction because they ARE being extracted from. The saver sees coordination because they ARE being coordinated with. The analytical observer's mountain (natural law) is a false summit that naturalizes what is actually a political choice by revealing that identifiable beneficiaries exist and active enforcement is required. The mandate is neither inevitable law nor arbitrary power — it is institutionalized political choice with measurable costs and benefits unevenly distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_causation_mechanism,
    'Does federal spending growth directly cause inflation, or are other monetary and supply-side factors more determinative?',
    'Econometric analysis of federal spending growth vs inflation trajectory in comparable 1950s economies with different fiscal regimes; comparison of Eisenhower''s fiscal restraint period (1953-1961) inflation outcomes vs subsequent administrations'' spending patterns and inflation results',
    'If federal spending is primary inflation driver: constraint is economically justified coordination. If other factors dominate (monetary policy, supply constraints, expectations): constraint is ideologically motivated extraction disguised as economic necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_causation_mechanism, empirical, 'Whether federal spending growth is the primary inflation mechanism').

omega_variable(
    opportunity_cost_of_restraint,
    'What programmatic opportunities (infrastructure, education, health research) were forgone due to the fiscal restraint mandate, and what was their long-term economic impact?',
    'Historical counterfactual analysis: comparison of post-1957 US infrastructure quality, R&D capacity, and human capital development vs alternative fiscal scenarios; longitudinal analysis of deferred programmatic investments that matured into later constraints (infrastructure gaps, research deficits)',
    'If opportunity costs exceed inflation prevention benefits: constraint is net-extractive despite stated coordination purpose. If inflation prevention benefits exceed opportunity costs: constraint is genuinely coordinative despite asymmetric distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_of_restraint, empirical, 'Opportunity cost of foregone programmatic expansion').

omega_variable(
    beneficiary_intentionality,
    'Did Eisenhower intend the fiscal restraint to benefit savers and pensioners specifically, or was inflation prevention a genuinely shared public good?',
    'Historical analysis of Eisenhower''s stated rationale, advisor memos, and public statements; comparison with Democratic alternatives (counter-cyclical spending, targeted investment) that would distribute benefits differently; examination of whether fiscal restraint mechanism could have been designed to protect fixed-income groups while allowing programmatic expansion',
    'If intentional asymmetric benefit: constraint is snare disguised as coordination. If genuinely intended as public good: constraint is tangled_rope with legitimate coordination function alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality, conceptual, 'Whether fiscal restraint was designed to benefit savers or protect shared economic stability').

omega_variable(
    legislative_veto_sufficiency,
    'Were congressional override mechanisms (veto override, legislative appropriation) genuinely available to Democratic congressional majorities, or did they face insurmountable political costs?',
    'Analysis of veto override voting patterns (1953-1961); comparison of successful overrides vs attempted overrides; assessment of political capital cost of override attempts in contemporary polling and media analysis',
    'If override was genuinely available: exit options were constrained but real (mobile-grade exit cost). If override was politically impossible despite legislative supermajority: exit options were trapped-grade (no effective exit available).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_veto_sufficiency, empirical, 'Whether congressional override was a genuine or illusory exit option').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1957_eisenhower_federal_fiscal_restraint, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eisenhower_fiscal_tr_t0, sotu_1957_eisenhower_federal_fiscal_restraint, theater_ratio, 0, 0.38).
narrative_ontology:measurement(eisenhower_fiscal_tr_t2, sotu_1957_eisenhower_federal_fiscal_restraint, theater_ratio, 2, 0.45).
narrative_ontology:measurement(eisenhower_fiscal_tr_t4, sotu_1957_eisenhower_federal_fiscal_restraint, theater_ratio, 4, 0.54).
narrative_ontology:measurement(eisenhower_fiscal_tr_t6, sotu_1957_eisenhower_federal_fiscal_restraint, theater_ratio, 6, 0.58).
narrative_ontology:measurement(eisenhower_fiscal_tr_t8, sotu_1957_eisenhower_federal_fiscal_restraint, theater_ratio, 8, 0.54).

% Extraction over time
narrative_ontology:measurement(eisenhower_fiscal_be_t0, sotu_1957_eisenhower_federal_fiscal_restraint, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(eisenhower_fiscal_be_t2, sotu_1957_eisenhower_federal_fiscal_restraint, base_extractiveness, 2, 0.31).
narrative_ontology:measurement(eisenhower_fiscal_be_t4, sotu_1957_eisenhower_federal_fiscal_restraint, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(eisenhower_fiscal_be_t6, sotu_1957_eisenhower_federal_fiscal_restraint, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(eisenhower_fiscal_be_t8, sotu_1957_eisenhower_federal_fiscal_restraint, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1957_eisenhower_federal_fiscal_restraint, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sotu_1957_eisenhower_federal_fiscal_restraint, 0.12).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_federal_fiscal_restraint, congressional_appropriations_authority).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_federal_fiscal_restraint, federal_agency_spending_discretion).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_federal_fiscal_restraint, inflation_expectations_formation).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific agency program constraints (which receive aggregate budget ceilings as input) and downstream of monetary policy constraints (which co-determine inflation outcomes alongside fiscal policy). The constraint family includes: (1) Eisenhower's executive coordination problem (ε=0.15, Rope) — how to prevent inter-agency competition; (2) Inflation prevention through fiscal discipline (ε=0.38, Tangled Rope, THIS STORY) — coordination with asymmetric cost distribution; (3) Congressional appropriations degradation (ε=0.52, Piton) — committees' ritual persistence despite functional constraint. Decomposition reflects different ε values based on whether the observable is executive coordination, macroeconomic impact, or institutional ritual.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1957_eisenhower_federal_fiscal_restraint, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
