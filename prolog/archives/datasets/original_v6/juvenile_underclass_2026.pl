% ============================================================================
% CONSTRAINT STORY: juvenile_underclass_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_juvenile_underclass_2026, []).

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
 *   constraint_id: juvenile_underclass_2026
 *   human_readable: The Minor Underclass Structural Constraint
 *   domain: social/political
 *
 * SUMMARY:
 *   The juvenile underclass is a structural constraint in which children, as
 *   a biological category, occupy a permanent position of zero political
 *   agency and economic self-determination. Children cannot vote, cannot
 *   enter binding contracts, cannot refuse parental authority or state
 *   custody, cannot claim property ownership, and cannot legally exit the
 *   subordinate role. This constraint operates universally across all human
 *   societies but with variable intensity. The constraint exhibits all six DR
 *   classification types from different perspectives: for the child it is a
 *   pure snare (extraction without alternatives); for institutions it is
 *   coordination (necessary socialization and protection); for
 *   labor-dependent sectors it is a pure extraction mechanism; for child
 *   rights movements it is a scaffold with a sunset clause (expanding
 *   autonomy over time); for traditional authority it appears as piton (a
 *   degraded but persistent ritual); and for the analytical observer it risks
 *   misclassification as a mountain (natural law). The extractiveness has
 *   increased over the measurement interval (0.55 to 0.68) as childhood has
 *   lengthened (extended education, delayed economic participation, delayed
 *   suffrage eligibility), increasing the extraction window. Theater ratio
 *   has risen from 0.35 to 0.55, indicating that child welfare discourse has
 *   become increasingly performative as a substitute for structural autonomy
 *   expansion.
 *
 * KEY AGENTS:
 *   - Children as a class: Primary victim (powerless/trapped) — zero legal agency, no exit from biological dependency, all economic output appropriated
 *   - Adult household authority holders: Primary beneficiary (institutional/arbitrage) — legal authority over children, access to their labor and obedience, ability to extract value without reciprocal obligation
 *   - State custodial apparatus: Institutional beneficiary (powerful/arbitrage) — controls compulsory education, juvenile justice, child welfare systems; extracts legitimacy and budget from 'child protection' mandate
 *   - Labor-dependent sectors: Secondary beneficiary (powerful/arbitrage) — agricultural, manufacturing, domestic work sectors benefit from child labor where enforcement is weak
 *   - Constrained parents: Mixed role (moderate/constrained) — experience both coordination benefits (shared childcare infrastructure) and extraction costs (unlimited liability, time investment, mandated obligations)
 *   - Child rights and emancipation movement: Organized challenger (organized/mobile) — working to expand youth agency through rights, education reform, eventual suffrage
 *   - Analytical observer: Potential false summit detector (analytical/analytical) — risks naturalizing contingent institutions as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(juvenile_underclass_2026, 0.68).
domain_priors:suppression_score(juvenile_underclass_2026, 0.82).
domain_priors:theater_ratio(juvenile_underclass_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(juvenile_underclass_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(juvenile_underclass_2026, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(juvenile_underclass_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(juvenile_underclass_2026, snare).
narrative_ontology:human_readable(juvenile_underclass_2026, "The Minor Underclass Structural Constraint").
narrative_ontology:topic_domain(juvenile_underclass_2026, "social/political").

domain_priors:requires_active_enforcement(juvenile_underclass_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(juvenile_underclass_2026, adult_household_authority).
narrative_ontology:constraint_beneficiary(juvenile_underclass_2026, state_custodial_apparatus).
narrative_ontology:constraint_beneficiary(juvenile_underclass_2026, child_labor_dependent_sectors).
narrative_ontology:constraint_victim(juvenile_underclass_2026, children_as_class).
narrative_ontology:constraint_victim(juvenile_underclass_2026, parental_choice_constrained).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHILD AS CAPTIVE (SNARE) — Children have zero political agency, no economic self-determination, no legal standing to exit parental/state custody, and no enforceable rights to refuse labor, education, or discipline. Trapped in biological dependency (requiring food, shelter, care) and legal dependency (guardianship status). Maximum structural extraction: all time, resources, and obedience flow outward; no reciprocal claims recognized. Suppression is maximal — alternatives (self-provision, exit to peer society, legal autonomy) are developmentally unavailable and legally foreclosed.
constraint_indexing:constraint_classification(juvenile_underclass_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CONSTRAINED PARENT (TANGLED ROPE) — Parents experience the constraint as both coordination mechanism (children require caregiving coordination; education requires institutional infrastructure) and extraction mechanism (parents bear unlimited liability, time cost, and responsibility without proportional legal authority). Legal authority is granted but comes with mandated obligations; inability to delegate or exit creates asymmetric cost. Parents can constrain child behavior but cannot opt out of custodial role. Moderate power — authority over the child but constrained by compulsory schooling laws, mandated reporting, and liability regimes.
constraint_indexing:constraint_classification(juvenile_underclass_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL CARE AND SOCIALIZATION SYSTEM (ROPE) — Schools, youth welfare systems, and family services institutions see the juvenile underclass as a coordination solution: children must be socialized, educated, and protected; concentrating this role in adults and institutions solves genuine collective action problems (socialization, skill transfer, protection from harm). The system experiences itself as providing beneficent coordination. Beneficiary from the institutional perspective — receives funding, legitimacy, and social role through the constraint. Exit via arbitrage: institutional actors can adjust within the system (change pedagogical models, funding structures) without disrupting the core arrangement.
constraint_indexing:constraint_classification(juvenile_underclass_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR-DEPENDENT SECTOR (SNARE) — Agricultural, manufacturing, informal service, and domestic work sectors benefit from access to child labor where enforcement is weak. Children provide extractable labor at minimal cost, minimal organization risk (no unionization, no exit power), and with impunity (no legal recourse). This perspective sees the juvenile underclass as a pure extraction mechanism: maximum output, minimal resistance, zero bargaining power. Powerful institutional beneficiary with arbitrage options (access to adult labor, automation, relocation) but maintains child labor extraction where it remains available.
constraint_indexing:constraint_classification(juvenile_underclass_2026, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CHILD RIGHTS AND EMANCIPATION MOVEMENT (SCAFFOLD) — Organized actors (human rights NGOs, youth advocacy, progressive legislatures) perceive the juvenile underclass constraint as a temporary institutional arrangement with a sunset clause. The movement aims to expand child agency through expanded rights, educational autonomy, labor protections, and eventual suffrage. This perspective sees the constraint as coordination with extractive overhead that can be progressively reduced. Mobile exit options (lobbying, international norm-setting, alternative models like youth courts) suggest this is a scaffold: high enforcement initially, but declining coercive overhead as norms shift. Theater ratio is moderate (child welfare discourse masks power asymmetries, but the movement explicitly challenges those asymmetries).
constraint_indexing:constraint_classification(juvenile_underclass_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: BIOLOGICAL DEPENDENCY AS NATURAL LAW (PITON DEGRADATION OF MOUNTAIN) — One traditional framing holds that childhood dependency is a natural, unchangeable biological fact requiring custodial authority. Adults are physically larger, cognitively mature, and economically productive; children are dependent and require direction. This perspective naturalizes the constraint as emerging from immutable facts of human development. However, the structural data reveals this framing as a piton: it is inert institutional orthodoxy (theater_ratio 0.55) rather than an actual law. Biological dependency is real, but the specific legal, economic, and political structures of the juvenile underclass are contingent historical arrangements. The constraint persists because the 'natural law' framing suppresses recognition that alternatives (graduated autonomy, youth representation, child-centered resource distribution) are structurally possible.
constraint_indexing:constraint_classification(juvenile_underclass_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - FALSE SUMMIT RISK (MOUNTAIN MISCLASSIFICATION) — From the most abstract analytical view, one might argue that all social systems require some form of authority, transmission of knowledge, and coordination of resources for the young; this is inherent to human society and cannot be eliminated. The analytical observer risks concluding that the juvenile underclass is a mountain (natural law of socialization). However, the metrics contradict this: suppression (0.82), theater (0.55), and extractiveness (0.68) reveal contingent institutional arrangements, not natural limits. The false summit detector flags this perspective: the 'inherent to human development' framing naturalizes what are actually modifiable political choices about authority distribution, resource allocation, and youth agency.
constraint_indexing:constraint_classification(juvenile_underclass_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(juvenile_underclass_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(juvenile_underclass_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(juvenile_underclass_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(juvenile_underclass_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(juvenile_underclass_2026, TR),
    TR >= 0.70.

:- end_tests(juvenile_underclass_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Children have zero economic self-determination and all their output (labor, obedience, time) flows to adults or institutional actors. The constraint is not pure extraction at 1.0 because biological dependency creates some genuine care coordination function — not all adult authority is extractive rent-seeking. However, the degree to which actual authority exceeds necessary protection and socialization is substantial. The 0.68 value reflects significant extraction overlaid on genuine coordination. Suppression (0.82): Very high. Children have no legal alternatives to parental/state custody, no option to exit through self-provision, no enforceable rights to refuse labor or discipline, and limited de facto capacity to exit through emigration or legal emancipation (available only in extreme circumstances). Biological dependency creates absolute suppression; legal structures reinforce it. Theater ratio (0.55): Moderate. Child welfare discourse (child protection, best interests of the child, development-appropriate care) serves partly as genuine protective function and partly as ideological cover for authority maintenance. The constraint is not purely theatrical (unlike a piton at 0.70+) because real child protection functions exist, but the ratio has risen over the interval as welfare language has proliferated without proportional agency expansion.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range spans from the child's snare (powerless/trapped maximum extraction) to the institutional system's rope (coordination with minimal cost). The gap reflects profound structural asymmetry: the same constraint is simultaneously a beneficent coordinating mechanism (institutional perspective) and a pure extraction trap (child perspective). The constrained parent occupies the middle ground (tangled rope): they benefit from the coordination infrastructure and authority but bear extraction cost through unlimited responsibility. The labor-dependent sector sees pure extraction opportunity (snare from their beneficiary position) — child labor is maximally profitable because the child has no bargaining power. The child rights movement explicitly rejects the piton/'natural law' framing and aims to shift the constraint toward scaffold status through graduated autonomy. The analytical observer risks the false summit by naturalizing what are actually contingent institutional choices (how much authority is necessary? what is the optimal age for expanded agency?) as laws of human development.
 *
 * DIRECTIONALITY LOGIC:
 *   The child's experienced extractiveness (chi) derives from maximum directionality value (d ≈ 0.95): trapped exit, victim status, powerless agent position. The sigmoid f(d) for d=0.95 is approximately 1.42, amplifying base extractiveness across all scopes. Adult household authority derives from beneficiary status with arbitrage options (d ≈ 0.15), producing f(d) ≈ -0.01, substantially dampening experienced extraction — the authority holder sees the constraint as coordination with minimal personal cost. Institutional care systems derive from institutional power + arbitrage (d ≈ 0.05), producing f(d) ≈ -0.12 (negative effective extraction), supporting the rope perspective. Labor-dependent sectors derive from powerful status + arbitrage (d ≈ 0.40), producing f(d) ≈ 0.40, moderating experienced extraction but maintaining high absolute chi due to their power level. The constrained parent derives from moderate power + constrained exit (d ≈ 0.60), producing f(d) ≈ 0.85, creating the tangled rope experience: both beneficiary from coordination and victim from unlimited obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION AT ε=0.68: The constraint avoids misclassification through perspectival clarity. The child rights movement perspective (scaffold) prevents false snare classification by showing that the constraint is NOT purely extractive and stable — it has organized opposition and an implicit sunset through generational emancipation logic. However, the snare perspective (child as captive) prevents false rope classification by showing that the coordination function does not require the degree of suppression observed (0.82) — much of it is extractive rent-seeking overlaid on genuine care. The tangled rope perspective (constrained parent) prevents both snare and rope misclassifications by making explicit that the constraint serves BOTH coordination AND extraction simultaneously. The piton perspective (natural law framing) serves as a false summit detector: the constraint's theater ratio and measurement trajectory suggest that traditional 'childhood is natural biological dependency' framing is inert ideology, not functional truth. The mountain perspective is explicitly flagged as risk — if the analytical observer naturalizes the constraint as immutable law, the engine detects it as a false summit (accessibility_collapse fails the gate). The multi-perspectival presheaf resolves the mandatrophy by showing that the constraint is genuinely mixed (tangled rope) with extractive overhang (snare) reducible through organized agency expansion (scaffold), not a coordination mechanism incorrectly labeled extraction (pure rope misclassification) nor an immutable law (false mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_threshold_for_autonomy,
    'At what developmental stage does meaningful autonomy become possible? Is agency capacity a continuous spectrum or a developmental threshold?',
    'Comparative study of age-graduated autonomy models (Swiss apprenticeship, Scandinavian youth councils, US state variation); longitudinal tracking of decision-making competence development; cross-cultural variation in attributed agency',
    'If continuous spectrum: current all-or-nothing legal structure becomes clearly extractive; graduated autonomy models gain legitimacy. If threshold exists: its location determines whether current age gates are accurate or arbitrary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_threshold_for_autonomy, empirical, 'Whether autonomy capacity is continuous or threshold-based').

omega_variable(
    extraction_vs_necessary_authority,
    'What portion of parental/institutional authority is necessary coordination (socialization, protection, skill transmission) versus extractive rent-seeking (time, obedience, unpaid labor)?',
    'Cross-cultural comparative analysis of child-rearing models; measurement of authority reduction in contexts with high child welfare outcomes; identification of minimum sufficient authority structures',
    'If substantial portion is extractive: juvenile underclass classification as snare is confirmed. If most authority is necessary: constraint might be primarily rope with extraction overhang rather than pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_necessary_authority, conceptual, 'Decomposition of necessary authority from extractive authority').

omega_variable(
    emancipation_cascade_irreversibility,
    'If child agency expands (suffrage, labor rights, property rights), would institutional actors voluntarily maintain expanded autonomy or revert to maximum suppression?',
    'Historical analysis of prior expansions of youth rights (compulsory schooling, child labor laws, juvenile courts); policy robustness testing; institutional incentive analysis for authority reversion',
    'If irreversible: scaffold sunset logic is sound and constraint is genuinely temporary. If subject to reversion: constraint requires continuous enforcement and may be structurally stable (snare with active containment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emancipation_cascade_irreversibility, empirical, 'Whether youth autonomy expansion is institutionally irreversible').

omega_variable(
    collective_organization_possibility,
    'Could children organize collectively for bargaining power under current institutional constraints? Are powerless agents rendered organizationally impossible by the constraint itself?',
    'Analysis of child organization attempts (school walkouts, youth movements, peer societies); examination of legal barriers to youth assembly and bargaining; comparison with other powerless groups',
    'If organization is possible: powerless agent may upgrade to organized under critical mass conditions (dynamic coalition extension). If legally/developmentally foreclosed: constraint ensures permanent powerlessness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_organization_possibility, empirical, 'Whether collective child organization is structurally possible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(juvenile_underclass_2026, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juv_under_tr_t0, juvenile_underclass_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(juv_under_tr_t50, juvenile_underclass_2026, theater_ratio, 50, 0.48).
narrative_ontology:measurement(juv_under_tr_t100, juvenile_underclass_2026, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(juv_under_be_t0, juvenile_underclass_2026, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(juv_under_be_t50, juvenile_underclass_2026, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(juv_under_be_t100, juvenile_underclass_2026, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(juvenile_underclass_2026, resource_allocation).
narrative_ontology:affects_constraint(juvenile_underclass_2026, compulsory_education_enforcement).
narrative_ontology:affects_constraint(juvenile_underclass_2026, parental_liability_asymmetry).
narrative_ontology:affects_constraint(juvenile_underclass_2026, labor_export_dependency).

% DUAL FORMULATION NOTE:
% The juvenile underclass is a constraint family with three downstream constraints: compulsory education enforcement (ε ≈ 0.42, tangled rope — coordination with extraction overhead), parental liability asymmetry (ε ≈ 0.55, snare — parents trapped in unlimited obligation), and labor export dependency (ε ≈ 0.70, snare — dependent sectors extract child labor). Each downstream constraint has distinct ε and structural features but all derive from the foundational minor underclass constraint. The family link enables propagation analysis: if child agency expands (upstream constraint softens), downstream constraints weaken proportionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(juvenile_underclass_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
