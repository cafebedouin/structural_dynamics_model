% ============================================================================
% CONSTRAINT STORY: union_corruption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_union_corruption, []).

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
 *   constraint_id: union_corruption
 *   human_readable: Union Leadership Corruption and Member Entrenchment
 *   domain: labor/organizational/political
 *
 * SUMMARY:
 *   Union corruption represents a structural constraint where leadership
 *   leverages control of collective resources to extract dues and negotiate
 *   benefits that accrue disproportionately to officeholders while members
 *   receive diminished value. The constraint is sustained by suppression
 *   mechanisms (closed-shop requirements, expulsion threats, ballot access
 *   control) that prevent collective exit or reform. Unlike simple extraction
 *   (snare with no coordination), union corruption retains nominal
 *   coordination benefits (wage bargaining, grievance procedures) that create
 *   plausible deniability—leadership can claim to be serving members while
 *   systematically extracting. The theater ratio (0.65) reflects that formal
 *   governance structures (elections, member meetings, contract votes)
 *   persist despite being substantially performative; real decisions are made
 *   by leadership with member consent manufactured rather than genuine. The
 *   extractiveness trajectory (0.38 → 0.58 over 30-year interval) shows
 *   corruption accumulating as institutional capture deepens and leadership
 *   develops increasingly sophisticated mechanisms to maintain control while
 *   suppressing dissent.
 *
 * KEY AGENTS:
 *   - Rank-and-file Members: Primary victims (powerless/trapped) — mandatory dues, closed-shop employment, suppressed voice in governance, bear costs of corrupt negotiation outcomes
 *   - Non-Union Workers in Organized Sectors: Secondary victims (powerless/trapped) — excluded from union-negotiated benefits, denied employment access through union wage floors and hiring control, no representation in union decisions that affect them
 *   - Corrupt Leadership: Primary beneficiaries (institutional/arbitrage) — capture dues stream, negotiate contracts that provide personal benefits, control enforcement mechanisms, can exit consequences-free
 *   - Union Institutional Legitimacy: Tertiary victim (powerless/trapped) — abstract collective good damaged by demonstrated corruption, erodes public and member support for labor organization generally
 *   - Progressive Reform Coalition: Secondary agents (moderate/constrained) — seek to reform unions internally; face retaliation, expulsion risk, loss of institutional support; constrained but not completely trapped due to external legal/political allies
 *   - Union Institutional Apparatus: Structural maintainer (institutional/arbitrage) — formal governance rituals persist to provide legitimacy theater; apparatus itself is neither beneficiary nor victim but a mechanism of suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(union_corruption, 0.58).
domain_priors:suppression_score(union_corruption, 0.72).
domain_priors:theater_ratio(union_corruption, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(union_corruption, extractiveness, 0.58).
narrative_ontology:constraint_metric(union_corruption, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(union_corruption, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(union_corruption, snare).
narrative_ontology:human_readable(union_corruption, "Union Leadership Corruption and Member Entrenchment").
narrative_ontology:topic_domain(union_corruption, "labor/organizational/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(union_corruption, corrupt_leadership).
narrative_ontology:constraint_victim(union_corruption, rank_and_file_members).
narrative_ontology:constraint_victim(union_corruption, industry_workers_unrepresented).
narrative_ontology:constraint_victim(union_corruption, union_institutional_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DUES-PAYING MEMBER (SNARE) — Trapped by union closed-shop requirements, seniority systems, and absence of alternative representation. Bears costs through mandatory dues, reduced wages negotiated for kickbacks, and lack of voice in union governance. Cannot exit without losing job access in union-controlled industries. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(union_corruption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-UNION WORKER (SNARE) — Trapped by union-negotiated wage floors that exclude non-union workers and by union political opposition to alternative labor arrangements. Bears costs through wage suppression and restricted employment access. No voice in union governance despite being affected by union wage and hiring decisions. Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(union_corruption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORRUPT LEADERSHIP (ROPE) — Experiences the constraint as pure coordination: maintaining member suppression through control of information, ballot access, and internal governance enables stable extraction of dues and negotiated benefits. Low effective extraction from their perspective because they are net beneficiaries. Can exit with impunity (arbitrage) — they control enforcement mechanisms and can redirect union resources to personal benefit or allies.
constraint_indexing:constraint_classification(union_corruption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE REFORM COALITION (TANGLED ROPE) — Sees both genuine coordination function (collective bargaining does improve wages vs unorganized workers) and entrenched extraction (leadership corruption, governance capture, lack of member voice). Constrained by union institutional capture and leadership's control of enforcement — reformers face expulsion, career damage, and loss of union support. Significant extraction but real coordination benefits create the mixed type.
constraint_indexing:constraint_classification(union_corruption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UNION INSTITUTIONAL APPARATUS (PITON) — The formal governance structures (member meetings, officer elections, grievance procedures) are substantially performative. Elections are scripted, member assemblies ratify leadership decisions rather than deliberate them, and grievance procedures protect corrupt officers. Theater ratio (0.65) reflects that governance rituals persist while authentic member voice has atrophied. The apparatus is maintained through institutional inertia and legal obligation rather than functional legitimacy.
constraint_indexing:constraint_classification(union_corruption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, union corruption is not an inevitable feature of collective organization but a specific structural failure: concentration of leadership power combined with suppression of information and exit options. The constraint is analytically identifiable as a snare—not a natural law—but it persists across decades because the members trapped within it lack mechanisms for collective exit or reform. The structure is contingent; its persistence reflects real power asymmetries, not inherent necessity.
constraint_indexing:constraint_classification(union_corruption, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(union_corruption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(union_corruption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(union_corruption, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(union_corruption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(union_corruption, TR),
    TR >= 0.70.

:- end_tests(union_corruption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Union corruption combines sustained dues extraction with negotiated benefits that favor leadership. The extractiveness reflects the gap between dues paid and member value received, plus the opportunity cost of members' inability to exit or voice concerns. The value is high but not extreme (0.72+) because some wage bargaining benefit remains tangible, even if corrupted. Suppression (0.72): High. Multiple independent barriers to exit and voice: (1) closed-shop rules requiring union membership for employment; (2) expulsion threats for dissent; (3) ballot access control preventing leadership challenges; (4) information suppression through controlled communications; (5) grievance procedure capture preventing internal recourse. These are structural, not merely cultural — they have legal/institutional backing. Theater ratio (0.65): Moderate-high. Elections are held but leadership maintains control through balloting rules. Member meetings are conducted but decisions are ratified rather than deliberated. Grievance procedures exist but defend corrupt officers. Contracts are 'voted' on but negotiated outcomes are predetermined. The theater has increased over the interval as leadership sophistication in manufacturing consent has grown, while member capacity for actual governance has atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap between trapped members and institutional beneficiaries. Members see a snare—they cannot exit without losing job access, and the constraint yields them minimal benefit while extracting dues. Leadership sees rope—they are solving the coordination problem of collective bargaining, and from their perspective the constraint is legitimate because they created it and control its enforcement. The analytical observer sees snare, not because of ignorance but because trapped members and institutional beneficiaries are not equally positioned agents—one has power, one does not. The reform coalition sees tangled rope because they experience both genuine coordination functions (wage bargaining does work) and extraction (corruption does exist), and they are constrained but not powerless. The piton perspective (union apparatus) reflects that governance rituals persist through institutional inertia despite loss of member legitimacy. The institutional apparatus does not classify as rope (pure coordination) because the coordination function is degraded; does not classify as snare because the apparatus itself is not a victim (it is a mechanism); classifies as piton because it is a degraded institutional form maintained by theater rather than function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position: trapped members with no exit and victim status derive maximum d (~0.95) and maximum f(d) (~1.42), producing high experienced extraction chi. Institutional beneficiaries with arbitrage options and beneficiary status derive minimum d (~0.05) and negative f(d) (~-0.12), producing negative/low extraction from their perspective. The reform coalition with constrained exit and victim status derives moderate-high d (~0.75), producing moderate-high experienced extraction. The analytical observer derives d from cross-position analysis: recognizing that power asymmetries are real and structural, not consensual, which anchors d toward victim-like values (~0.85) even though the observer is not materially trapped. The institutional apparatus has arbitrage exit (can maintain current function indefinitely) and beneficiary status (serves leadership interests), deriving low d and negative chi—but the piton gate (theater ratio ≥ 0.70) overrides chi classification because the function has degraded regardless of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Union corruption does not present mandatrophy because the snare classification is unambiguous from the trapped member perspective and confirmed by the analytical observer. The constraint reliably extracts from members with high suppression and minimal coordination benefit that members actually receive. The tangled rope perspective (reform coalition) is also coherent—it reflects genuine mixed experiences where some coordination benefit exists alongside extraction. The danger is NOT classification confusion but rather the naturalization of the snare as inevitable ('unions are inherently corrupt') or the romanticization of nominal coordination benefits ('unions are inherently good') both of which erase the structural extraction. The mandatrophy would arise if the constraint were classified as rope (pure coordination) across all perspectives—this would be false natural law detection. Current perspectives correctly identify snare as primary and rope as a false beneficiary perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solidarity_capture,
    'Is suppression maintained primarily through material coercion (job loss, expulsion) or through identity fusion (members identify with union despite extraction)?',
    'Post-exit survey data: do members who leave union-represented industries report relief or identity loss? Do members defend corrupt unions as ''better than nothing'' or recognize extraction?',
    'If identity_locked: suppression is internalized; members carry constraints with them after exit. If trapped: suppression is structural; removal of barriers would enable exit. Classification could shift from snare to rope if identity fusion is dominant (members perceive coordination benefit they don''t experience materially).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solidarity_capture, empirical, 'Whether suppression is material or identity-based').

omega_variable(
    genuine_coordination_residue,
    'What portion of observed wage premium derives from union bargaining effectiveness vs. from other factors (industry consolidation, regulatory barriers, monopsony prevention)?',
    'Comparative wage analysis: union vs non-union workers in same industries with same skills; historical wage series before/after union capture; cross-national comparison with different union governance models.',
    'If bargaining effectiveness is high (>60% of premium): coordination benefits are real and substantial, potentially shifting classification toward tangled_rope across more perspectives. If low (<20%): coordination benefit is nominal, and snare classification is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_coordination_residue, empirical, 'Portion of wage premium attributable to union bargaining vs other factors').

omega_variable(
    exit_option_feasibility,
    'How many union members actually face job loss if they exit the union or challenge leadership (closed-shop enforcement vs industry reality)?',
    'Survey of exit attempts: how many members have challenged leadership and faced consequences? How many have left organized sectors and what barriers did they cite? Are enforcement mechanisms consistently applied?',
    'If exit barriers are selective (leadership enforces against vocal critics but ignores quiet exit): exit_options shift from trapped to constrained, reducing experienced extraction. If uniformly enforced: trapped classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Actual vs nominal exit barriers for union members').

omega_variable(
    reform_mechanism_viability,
    'Can internal democratic processes reform corrupt unions, or are governance structures sufficiently captured that reform requires external intervention?',
    'Historical case studies of union reform: what percentage of reform movements succeeded through internal process vs required DOJ intervention, court orders, or new union formation? Timeline to reform success?',
    'If internal reform is viable: scaffold perspective gains credence—corruption is temporary aberration with internal correction mechanism. If external intervention required: snare classification is reinforced across all agent perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_mechanism_viability, empirical, 'Whether internal democratic reform of corrupted unions is viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(union_corruption, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(union_corrupt_tr_t0, union_corruption, theater_ratio, 0, 0.45).
narrative_ontology:measurement(union_corrupt_tr_t10, union_corruption, theater_ratio, 10, 0.55).
narrative_ontology:measurement(union_corrupt_tr_t20, union_corruption, theater_ratio, 20, 0.65).
narrative_ontology:measurement(union_corrupt_tr_t30, union_corruption, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(union_corrupt_be_t0, union_corruption, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(union_corrupt_be_t10, union_corruption, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(union_corrupt_be_t20, union_corruption, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(union_corrupt_be_t30, union_corruption, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(union_corruption, resource_allocation).
narrative_ontology:affects_constraint(union_corruption, labor_market_monopsony).
narrative_ontology:affects_constraint(union_corruption, political_regulatory_capture_by_unions).
narrative_ontology:affects_constraint(union_corruption, apprenticeship_gatekeeping).

% DUAL FORMULATION NOTE:
% Union corruption is structurally distinct from (1) legitimate union bargaining (rope/scaffold), (2) labor market monopsony power by employers (snare with different beneficiaries), and (3) union political capture of regulators (tangled_rope at institutional level). These three stories share causal ancestry with union corruption but have distinct ε values and structural data. Unions that bargain effectively without corruption classify as rope; the union corruption constraint represents the specific degradation into snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
