% ============================================================================
% CONSTRAINT STORY: power_without_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_without_responsibility, []).

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
 *   constraint_id: power_without_responsibility
 *   human_readable: The Asymmetric Mandate: Power Without Responsibility
 *   domain: political/organizational/legal
 *
 * SUMMARY:
 *   The asymmetric mandate is a structural pattern in which a coordinator or
 *   executive is granted authority to manage high-stakes systems (emergency
 *   powers, automated law enforcement, sovereign immunity, surveillance
 *   systems) with insufficient corresponding accountability mechanisms. The
 *   coordination function is genuine: executives do need decision-making
 *   authority in crises; delegated power enables rapid response without
 *   legislative approval at each step. However, the accountability side is
 *   systematically weaker. Formal oversight mechanisms (legislative
 *   committees, judicial review, inspector general offices) are slow,
 *   cumbersome, or deferred to executive judgment. The result is a Tangled
 *   Rope that exhibits properties of both coordination (the mandate is
 *   necessary) and extraction (the authority holder benefits asymmetrically
 *   and the subject population bears the suppression cost). The constraint's
 *   theater ratio (0.65) reflects that formal accountability mechanisms are
 *   largely performative: courts defer to executive judgment in security
 *   matters; legislative oversight is procedurally blocked during active
 *   emergencies; inspector general reviews happen months or years after
 *   abuses. This manifest demonstrates how a single structural phenomenon
 *   (delegating power without proportional accountability) appears as pure
 *   extraction (Snare) to the subject population, as necessary coordination
 *   (Rope) to the authority holder, as a temporary solvable problem
 *   (Scaffold) to reform movements, and as an immutable feature of politics
 *   (false Mountain) to observers who naturalize the status quo.
 *
 * KEY AGENTS:
 *   - Delegated Authority Holder (institutional/arbitrage): Executive, military commander, or sovereign entity — primary beneficiary. Experiences the mandate as coordination; captures authority gain while bearing minimal accountability cost.
 *   - Subject Population (powerless/trapped): Citizens or stakeholders subject to emergency powers or enforcement actions — primary victim. No exit option; bears suppression and extraction cost.
 *   - Competing Power Center (powerful/mobile): Legislature, courts, or parallel institutional actors — secondary actor. Formally retains checking authority but faces structural friction in exercising it.
 *   - Intermediate Enforcement Agent (moderate/constrained): Police, official, or bureaucrat tasked with implementing the asymmetric mandate — secondary victim. Constrained by 'following orders' while bearing risk.
 *   - Accountability Reform Movement (organized/mobile): Civil society, transparency organizations, legal scholars — tertiary actor. Building alternative pathways (sunset clauses, mandatory audits, civilian boards) with structural sunset logic.
 *   - Formal Legal Architecture (institutional/arbitrage): Courts, legislatures, constitutional frameworks — maintains performative accountability theater. Persists through institutional inertia despite limited functional enforcement.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective risking naturalization of contingent institutional arrangements as immutable features of political order.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_without_responsibility, 0.52).
domain_priors:suppression_score(power_without_responsibility, 0.68).
domain_priors:theater_ratio(power_without_responsibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_without_responsibility, extractiveness, 0.52).
narrative_ontology:constraint_metric(power_without_responsibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(power_without_responsibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_without_responsibility, tangled_rope).
narrative_ontology:human_readable(power_without_responsibility, "The Asymmetric Mandate: Power Without Responsibility").
narrative_ontology:topic_domain(power_without_responsibility, "political/organizational/legal").

domain_priors:requires_active_enforcement(power_without_responsibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(power_without_responsibility, delegated_authority_holder).
narrative_ontology:constraint_victim(power_without_responsibility, accountability_deficit_populations).
narrative_ontology:constraint_victim(power_without_responsibility, rule_of_law_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Citizens or stakeholders subject to executive authority (emergency powers, automated enforcement systems, sovereign immunity rulings) have no exit option and no formal accountability mechanism. They bear full extraction cost. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(power_without_responsibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DELEGATED AUTHORITY HOLDER (ROPE) — Executive, commander, or sovereign entity experiences the mandate as pure coordination: 'I need authority to manage crises efficiently without legislative interference at each decision.' Perceives no extraction cost because accountability is framed as constraint on capability rather than as constraint on abuse. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(power_without_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPETING POWER CENTER (TANGLED ROPE) — Legislature, judicial oversight body, or parallel institutional power has formal authority to constrain the delegated mandate but faces structural friction: emergency declarations are difficult to revoke mid-crisis, judicial review is slow, legislative check requires supermajority. Benefits from the coordination function (shared governance apparatus) but loses authority asymmetrically. d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(power_without_responsibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERMEDIATE ENFORCEMENT AGENT (SNARE) — Police, court clerk, or subordinate official tasked with implementing the asymmetric mandate without clear accountability boundaries. Constrained by 'following orders' doctrine while bearing career and legal risk. Cannot exit without losing employment. d≈0.82, f(d)≈1.25, σ=0.9 → χ≈0.58.
constraint_indexing:constraint_classification(power_without_responsibility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ACCOUNTABILITY REFORM MOVEMENT (SCAFFOLD) — Civil society, transparency organizations, and legal reformers see the asymmetric mandate as a solvable structural problem with a sunset: codified sunset clauses on emergency powers, mandatory impact audits, civilian oversight boards, or constitutional amendment processes. These movements are building alternative accountability pathways. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.24. Low extraction because this perspective sees agency and a path forward.
constraint_indexing:constraint_classification(power_without_responsibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FORMAL LEGAL ARCHITECTURE (PITON) — Constitutional and statutory frameworks that nominally constrain executive power (separation of powers, due process, proportionality review) are largely performative when applied to emergency or sovereign functions. Courts defer to executive judgment in crisis. Legislative checks are procedurally cumbersome. Formal accountability mechanisms persist through institutional inertia (courts exist, legislatures meet) but lack functional enforcement capacity. theater_ratio=0.65 reflects high performative content of formal review processes that rarely overturn executive decisions. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(power_without_responsibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, the asymmetric mandate appears as an immutable feature of governance: 'All sovereigns require some sphere of unaccountable action; accountability mechanisms always impede crisis response; the tension between power and responsibility is inherent to political order itself.' However, the structural data (ε=0.52, suppression=0.68, theater=0.65) contradicts the mountain classification. The engine will compute this as a false summit, revealing that the 'inherent to politics' framing naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(power_without_responsibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_without_responsibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(power_without_responsibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_without_responsibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(power_without_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(power_without_responsibility, TR),
    TR >= 0.70.

:- end_tests(power_without_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The authority holder captures asymmetric benefits (decision-making power without proportional accountability cost), and the subject population bears suppression (constrained exit options from authority decisions). The extractiveness is not as severe as pure Snare (0.80+) because some coordination function is legitimate: executives do need real authority in genuine crises. But it significantly exceeds pure coordination (Rope ε≤0.45) because the accountability asymmetry enables abuse rent-seeking and the subject population's constraints are structural, not temporary. Suppression (0.68): High. Subject populations face multiple barriers to exit from or challenging authority decisions: (1) formal legal status (citizens cannot leave without extraordinary cost), (2) procedural barriers to judicial review (standing, exhaustion, deference doctrines), (3) legislative blocking (supermajority needed to override emergency declaration), (4) information asymmetry (executive controls classified information). Theater ratio (0.65): Moderate-high. Formal oversight mechanisms are substantially performative. Courts issue writs of habeas corpus that take months to adjudicate during active emergencies. Legislative committees hold hearings months after actions occurred. Inspector general reviews are classified or buried. Judicial review defers to executive judgment on national security/public safety grounds. The performativity has increased over the interval (0.42 → 0.65) as oversight mechanisms have faced technological acceleration (surveillance systems, automated enforcement) that outpaces review timelines.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The delegated authority holder (Rope perspective) experiences pure coordination: 'I need authority to act in crises without legislative permission slowing me down; the system works because I exercise it responsibly.' The subject population (Snare perspective) experiences pure extraction: 'I have no recourse if my rights are violated; there is no meaningful accountability.' The competing power center (Tangled Rope perspective) experiences mixed coordination and constraint: 'The mandate is necessary but I should have more real checking power; currently my checks are procedurally blocked.' The accountability reform movement (Scaffold perspective) sees a solvable problem: 'Mandatory sunset clauses, civilian oversight boards, and rapid appellate review can balance power and responsibility within 10-15 years.' The formal legal architecture (Piton perspective) sees a degraded ritual: 'Our review processes exist but rarely overturn executive decisions; we maintain the appearance of accountability while exercising minimal functional power.' The analytical observer (false Mountain perspective) risks naturalizing the asymmetry: 'This is inherent to politics; all sovereigns need unaccountable spheres.' The perspectival gap reveals that the mandate's fundamental structure is contested: is it a necessary coordination mechanism (Rope) that happens to have accountability costs, or is it an extraction mechanism (Snare/Tangled Rope) that is justified as coordination?
 *
 * DIRECTIONALITY LOGIC:
 *   Subject population: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum directionality toward extraction target. No exit option; no formal accountability leverage. Delegated authority holder: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; can exit by resigning or transferring authority but has no incentive to do so. Competing power center: Victim + mobile → d≈0.52, f(d)≈0.65. Moderate directionality; has formal authority but faces structural friction in exercising it; can theoretically override but procedurally blocked. Intermediate enforcement agent: Victim + constrained → d≈0.82, f(d)≈1.25. High directionality; constrained by employment and legal doctrine of following orders; bears career and legal risk. Accountability reform movement: Organized + mobile → d≈0.45, f(d)≈0.45. Low-moderate directionality; has agency and coalition strength; sees path forward through institutional redesign. Formal legal architecture: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification derives from theater_ratio gate (0.65 ≥ 0.70 threshold close), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False mountain; the observer who naturalizes political asymmetry as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The asymmetric mandate resolves mandatrophy by decomposing the constraint into its two structural components: the Coordination Function (executive authority in crises is necessary) and the Extraction Mechanism (unaccountable authority enables abuse). The Rope reading (delegated authority holder's perspective) emphasizes the coordination function: 'Crisis response requires speed; democracy is slow; the mandate solves a real coordination problem.' The Snare reading (subject population's perspective) emphasizes the extraction mechanism: 'The mandate is a vehicle for abuse with no accountability; I have no recourse.' Neither is false. Both are structurally true. The constraint IS a coordination mechanism AND an extraction mechanism simultaneously. Tangled Rope classification captures this hybrid nature. Mandatrophy is resolved by recognizing that (1) the coordination function is not questioned — even the accountability reform movement accepts that executives need real authority in crises; (2) the extraction risk is the contested variable — how can authority be granted without enabling abuse? The reform pathways (sunset clauses, mandatory audits, civilian boards, rapid appellate review) are designed to preserve the coordination function while reducing the extraction surface. If these reforms succeed, the constraint could migrate toward Rope or even Scaffold (temporary coordination with sunset). If they fail (immunity doctrines persist, oversight remains performative), the constraint locks into Snare as authorities learn that the accountability mechanisms are theater and can be safely ignored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accountability_mechanism_sufficiency,
    'What accountability mechanism (if any) would transform the asymmetric mandate from Snare to Tangled Rope without compromising necessary crisis authority?',
    'Comparative analysis of jurisdictions with strong post-hoc accountability (mandatory audits, courts of inquiry, parliamentary review committees) vs jurisdictions with weaker accountability; correlation between accountability strength and abuse prevalence/severity',
    'If strong mechanism exists: constraint can be rebalanced toward true hybrid. If no mechanism works: the asymmetry is structural and cannot be solved by institutional design alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_mechanism_sufficiency, conceptual, 'Whether accountability mechanisms can be designed to balance power and responsibility').

omega_variable(
    emergency_scope_definition,
    'Is the scope of emergency/crisis authority objectively definable (specific threats, time limits, geographic bounds) or inherently elastic?',
    'Historical analysis of emergency declarations: what proportion fall within the stated scope definition? What proportion expand beyond or persist after crisis resolution?',
    'If objectively definable: accountability mechanisms can be precise. If inherently elastic: the delegated authority holder can redefine scope, making accountability targets unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_scope_definition, empirical, 'Whether emergency scope is objectively definable or inherently elastic').

omega_variable(
    collective_action_problem_in_oversight,
    'Do distributed oversight bodies (legislatures, courts, inspector general offices) face a collective action problem that prevents coordinated accountability check?',
    'Game-theoretic analysis of incentive structures for oversight agents; case studies of failed/successful collective accountability actions; measurement of time-to-action vs crisis timeline',
    'If collective action problem exists: formal oversight bodies become Piton (performative). If collective action is solvable: accountability can be effective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_action_problem_in_oversight, empirical, 'Whether distributed oversight bodies face collective action problems').

omega_variable(
    vicarious_liability_escape,
    'Can delegated authority holders insulate themselves from personal/institutional liability through doctrines like qualified immunity, official immunity, or sovereign immunity?',
    'Legal doctrinal analysis across jurisdictions; case law tracking on what actions cross the immunity threshold; measurement of actual liability exposure for delegated actors',
    'If immunity doctrines are robust: accountability is theater (Piton). If immunity is narrow: accountability mechanism can be functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vicarious_liability_escape, empirical, 'Whether immunity doctrines insulate delegated authority from liability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_without_responsibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pwr_tr_t0, power_without_responsibility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pwr_tr_t5, power_without_responsibility, theater_ratio, 5, 0.54).
narrative_ontology:measurement(pwr_tr_t10, power_without_responsibility, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pwr_be_t0, power_without_responsibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pwr_be_t5, power_without_responsibility, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(pwr_be_t10, power_without_responsibility, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_without_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(power_without_responsibility, sovereign_immunity_doctrine).
narrative_ontology:affects_constraint(power_without_responsibility, emergency_powers_indefinite_renewal).
narrative_ontology:affects_constraint(power_without_responsibility, qualified_immunity_police_accountability).

% DUAL FORMULATION NOTE:
% The asymmetric mandate is a structural meta-constraint that affects multiple downstream constraints (sovereign immunity, emergency powers, police qualified immunity) by creating a permissive environment for accountability avoidance. Upstream constraints include the separation of powers doctrine and the crisis-response legitimacy framework; downstream constraints are domain-specific instantiations (emergency declarations in public health, law enforcement authority expansion, national security classifications).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(power_without_responsibility, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
