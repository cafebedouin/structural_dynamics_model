% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exercise_as_competence_maintenance_flat_control
 *   human_readable: Exercise-as-Competence-Maintenance Commitment in Safety Systems
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   The shared institutional commitment that catastrophe-avoidance competence
 *   must be actively exercised to remain operational creates a structural
 *   tension between a genuine coordination function (competence does decay
 *   without practice) and extractive enforcement mechanisms (universal
 *   mandates, bureaucratic audit trails, surveillance of compliance,
 *   perpetual resource claims by safety disciplines and exercise vendors).
 *   This constraint exhibits classic tangled-rope structure: it solves a real
 *   problem (maintaining the specialized knowledge and embodied skill
 *   required for catastrophe prevention) while simultaneously extracting
 *   value through the mechanisms that enforce the solution. The constraint's
 *   theater ratio has risen from 0.45 to 0.72 over thirty years, indicating
 *   that exercises have become increasingly performative — designed to
 *   satisfy audit requirements and documentation standards rather than to
 *   test competence under realistic conditions. This drift suggests the
 *   constraint is shifting from coordination-dominant (rope, early years)
 *   toward extraction-dominant (snare, current trajectory), mediated by
 *   institutional inertia that maintains the exercise infrastructure long
 *   after its functional necessity has degraded (piton dynamics from the
 *   exercise industry perspective). The constraint vindicates two
 *   institutional propositions: the 'competence decay doctrine' (skills
 *   inherently decay without practice) and the 'dynamic knowledge hypothesis'
 *   (knowledge must be actively maintained to remain operational). Both are
 *   partly true and partly contestable — expertise can be partially retained
 *   through documentation and episodic refresher training, and
 *   domain-dependent variation is substantial. The constraint's enforcement
 *   mechanisms (regulatory mandates, certification requirements, audit
 *   trails) suppress alternatives: organizations cannot negotiate lower
 *   exercise intensity even when evidence suggests current frequency is
 *   over-specified, cannot substitute alternative competence-maintenance
 *   mechanisms (mentoring, real-time performance monitoring), and face
 *   regulatory penalties for non-compliance. Field personnel and
 *   resource-constrained organizations bear the primary extraction cost:
 *   diverted labor hours, budget pressure, and compliance burden without
 *   corresponding discretion over what constitutes 'adequate' competence
 *   maintenance. The analytical observer risks seeing the constraint as a
 *   natural law of human skill decay — an immutable property of how expertise
 *   works — but the structural data (beneficiary presence, enforcement
 *   requirements, high suppression, rising theater ratio, institutional
 *   capture by safety disciplines) reveals this as a false summit: a
 *   contingent institutional commitment that naturalizes what could be
 *   otherwise organized.
 *
 * KEY AGENTS:
 *   - Resource-Constrained Organizations: Primary victim (powerless/trapped) — mandatory participation without discretion over exercise frequency or format; full cost of diverted resources
 *   - Field Safety Officers: Secondary victim (moderate/constrained) — responsible for organizing exercises while managing budget pressure; mixed experience with some coordination benefit
 *   - Field Personnel / Workforce: Victim and partial beneficiary (moderate/constrained) — bear time and evaluation burden while also benefiting from genuine safety improvement; some coalition power but constrained by mandatory participation
 *   - Safety Engineering Discipline: Primary beneficiary (institutional/arbitrage) — vindicates its core claim that active maintenance is necessary; generates funding, careers, professional authority
 *   - Certifying Authorities: Beneficiary and coordinator (powerful/constrained) — maintain regulatory legitimacy and authority through perpetual requirement to supervise exercises; also genuinely coordinate competence maintenance
 *   - Exercise Industry Ecosystem: Beneficiary (institutional/arbitrage) — vendors of simulations, training consultants, exercise-design firms extract revenue from perpetual requirement; maintain largely performative infrastructure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable laws of human skill decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance_flat_control, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance_flat_control, 0.62).
domain_priors:theater_ratio(exercise_as_competence_maintenance_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance_flat_control, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance_flat_control, "Exercise-as-Competence-Maintenance Commitment in Safety Systems").
narrative_ontology:topic_domain(exercise_as_competence_maintenance_flat_control, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(exercise_as_competence_maintenance_flat_control, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance_flat_control, safety_engineering_discipline).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance_flat_control, certification_authorities).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance_flat_control, institutional_risk_managers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance_flat_control, operational_efficiency).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance_flat_control, resource_constrained_organizations).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance_flat_control, field_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance_flat_control, field_safety_officer).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance_flat_control, field_personnel_workforce).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance_flat_control, certifying_authority).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance_flat_control, exercise_industry).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance_flat_control, resource_constrained_operator).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance_flat_control, field_safety_officer).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance_flat_control, field_personnel_workforce).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance_flat_control, competence_decay_doctrine).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance_flat_control, dynamic_knowledge_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations with limited budgets (small hospitals, regional emergency services, local utilities) must dedicate personnel hours, equipment, and travel resources to mandatory exercises. They cannot negotiate lower frequency, cannot substitute alternative competence-maintenance mechanisms, cannot refuse participation without losing certification. They experience the constraint as a pure cost imposed by external regulatory requirement, with no discretion over what constitutes 'adequate' competence maintenance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, resource_constrained_operator, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance_flat_control, resource_constrained_operator, excluded).

% Responsible for organizing, conducting, and documenting exercises. Experiences both coordination benefit (exercises do identify genuine competence gaps and improve actual safety) and extraction cost (time spent on exercise design/execution, budget pressure, responsibility for compliance, career consequences if exercises fail to meet standards). Has some agency (can influence exercise design, timing, intensity within organizational bounds) but is constrained by regulatory mandates and organizational pressure to demonstrate compliance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, field_safety_officer, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance_flat_control, field_safety_officer, beneficiary).

% Required to participate in exercises, often on compressed timelines or during operational periods. Time away from operational work, potential for evaluation-based career consequences if performance is deemed insufficient, emotional burden of high-fidelity simulation of catastrophic scenarios. Also benefits from exercises: identifies knowledge gaps, improves actual readiness, reduces their own risk of making critical errors. Organized through unions and professional associations with some collective bargaining power but no ability to refuse the requirement entirely.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, field_personnel_workforce, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance_flat_control, field_personnel_workforce, beneficiary).

% The discipline's core claim — that competence must be actively maintained through practice — is vindicated and institutionalized by the requirement. Generates academic programs, professional certifications, conference economies, consulting opportunities, regulatory authority. Could choose to emphasize alternative competence-maintenance mechanisms (mentoring, apprenticeship, knowledge management systems) but does not. Benefits substantially from the perpetual requirement without bearing its costs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, safety_engineering_discipline, beneficiary,
    institutional, generational, arbitrage, global).

% Regulatory bodies and certification authorities set exercise standards, approve compliance documentation, audit organizations for adherence. Sustain their institutional relevance and authority through the perpetual requirement to supervise, validate, and enforce exercises. Constrained by statutory mandate to ensure safety but also beneficiary of the institutional arrangements that perpetuate the requirement. Sets the boundaries of what counts as 'acceptable' competence maintenance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, certifying_authority, agenda_setter,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance_flat_control, certifying_authority, beneficiary).

% Simulation vendors, training consultants, exercise-design firms, software companies providing compliance-tracking systems. Extract revenue from the perpetual requirement. Design exercises optimized for compliance documentation and pass-rate achievement rather than high-fidelity competence testing. No stake in whether exercises genuinely maintain competence, only in perpetuation of the requirement.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, exercise_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% A vindicated proposition: the claim that human competence decays without active practice. This is a theoretical construct, not an agent, kept in the stakeholder manifest for narrative completeness because the constraint's legitimacy rests substantially on this doctrine. The doctrine is partially empirically grounded and partially contested regarding its scope and severity.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, competence_decay_doctrine, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(exercise_as_competence_maintenance_flat_control, competence_decay_doctrine).

% Approaches to competence maintenance not currently institutionalized as primary mechanisms in most safety-critical domains: continuous mentoring systems, apprenticeship models, real-time performance monitoring with AI-assisted feedback, knowledge management systems enabling rapid reference to critical procedures, distributed practice scheduling optimized to individual cognitive decay curves, peer-based competence assessment rather than institutional evaluation. These are absent from the conversation because regulatory frameworks lock in the exercise-based approach and suppress institutional experiments with alternatives.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance_flat_control, alternative_competence_mechanisms, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(exercise_as_competence_maintenance_flat_control, alternative_competence_mechanisms).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevent competence decay in high-consequence safety domains where errors can cause catastrophic harm. Competence — both specialized knowledge and embodied skill — decays without active use and practice. The constraint solves the real coordination problem of maintaining this decaying competence across organizations and time periods without allowing it to erode below safety-critical thresholds.
% TRANSFER_FUNCTION: The constraint moves time and resources from operational/revenue-generating activity toward exercise participation and compliance documentation. It transfers institutional authority toward safety disciplines and certifying regulators. It transfers revenue toward the exercise industry (simulation vendors, training consultants). It transfers liability protection toward organizations that can demonstrate documented evidence of competence maintenance.
% ABSENT_VOICES: Frontline workers in safety-critical domains whose judgment about 'sufficient' competence maintenance might differ from regulatory mandates; organizations in resource-constrained environments where the cost of exercises is substantial relative to operational budgets; individuals whose competence decays slower than standard exercise frequencies assume; alternative competence-maintenance mechanisms (mentoring systems, knowledge management, real-time performance monitoring) that cannot be substituted for formal exercises within current regulatory frameworks; cost-benefit analysts who might conclude the current exercise frequency is over-specified relative to actual safety improvement.
% DISAPPEARANCE_RATIONALE: If the exercise-as-maintenance commitment disappeared, organizations would need to maintain competence through alternative mechanisms (continuous mentoring, apprenticeship, real-time performance monitoring, episodic refresher training, knowledge documentation). Some competence would likely decay — whether this matters depends on the actual functional requirement (how much decay is tolerable?) and domain specifics. Safety certification regimes would collapse without documented evidence of competence maintenance, forcing new insurance/liability frameworks. The exercise industry would cease to exist. Regulatory authority based on perpetual supervision would be disrupted. The constraint's disappearance would rearrange institutional structures, liability frameworks, and organizational practices substantially.
% FOUNDING_PROBLEM: In the mid-20th century, as industrial catastrophes (aviation accidents, nuclear incidents, chemical plant failures) increased, organizations and regulators discovered that even highly trained personnel made critical errors after periods of non-use. The commitment emerged that catastrophe-avoidance competence is not like knowledge (retainable through documentation) but like skill (requiring continuous practice). The mandate to actively exercise competence was built to solve this genuine coordination problem: ensure that specialized competence remained operational across time and organizational boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by cognitive science research on skill decay (exponential decay models, context-dependent retention, stress-induced performance degradation) and by post-incident investigations that sometimes cite competence loss as a contributing factor. However, the problem's severity and the frequency of competence-caused failure is contested. Some domains (aviation) have documented multiple incidents attributable to competence degradation; others (nuclear operations, with low accident rates) present ambiguous evidence (are accident rates low because exercises work, or despite over-specified exercises? Counterfactual difficult to establish). The founding problem's scope is over-extended: not all 'catastrophe-avoidance competence' shows the same decay rate or requires the same exercise frequency. The institutional actors most dependent on perpetuating the requirement (safety disciplines, regulatory bodies, exercise vendors) are also the primary attestors to the severity of the founding problem, creating potential conflict of interest. Independent corroboration would require: (1) organizations capable of choosing lower exercise intensity without losing certification, showing whether safety outcomes degrade, and (2) longitudinal studies of actual competence decay in representative domains with realistic time intervals.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED OPERATOR (SNARE) — No discretion over exercise frequency or format; must participate in drills and simulations while managing core operational demands with fixed budgets. Cannot negotiate out of the requirement without loss of certification or operational status. Bears full cost: diverted personnel hours, equipment wear, travel expenses, training time. No exit path — regulatory mandate makes participation non-negotiable. Extraction is maximal because the constraint denies the trapped agent the ability to determine what proportion of competence maintenance is 'enough.'
constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD SAFETY OFFICER (TANGLED ROPE) — Experiences both genuine coordination function and asymmetric extraction. The exercise requirement solves a real problem: competence does decay without active practice, and drills genuinely identify knowledge gaps. But the constraint also extracts: field safety officers bear responsibility for organizing and justifying exercises while absorbing budget pressure and operational disruption. They are constrained (high cost to exit via career consequences or organizational sanctions) but also partially beneficiary (exercises improve actual operational safety, which is their core mandate). Mixed experience with moderate exit cost.
constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAFETY ENGINEERING DISCIPLINE (ROPE) — Net beneficiary of the exercise-as-maintenance doctrine. The constraint vindicates the discipline's core claim: competence must be actively maintained through practice. This generates funding for safety programs, careers in exercise design and evaluation, professional societies, and regulatory authority. The discipline experiences the constraint as pure coordination: ensuring competence through active exercise is exactly what safety engineering is for. No perception of extraction because the constraint aligns with disciplinary interest. Arbitrage option available (choosing to emphasize alternative competence-maintenance mechanisms) but not exercised.
constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CERTIFYING AUTHORITY (TANGLED ROPE) — Experiences coordination (the requirement ensures documented competence maintenance, justifying the authority's regulatory legitimacy and liability protection) alongside extraction (the authority extracts authority and relevance from the perpetual requirement to supervise, validate, and enforce exercises). Constrained by the regulatory mandate but also beneficiary (the system sustains the authority's institutional role). Power asymmetry: the authority sets exercise standards and determines compliance; operators must conform. The constraint is genuinely mixed — the coordination function is real (exercises do maintain competence and reduce catastrophic failure risk), but it also extracts by making the authority permanently necessary to validate compliance.
constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WORKFORCE COALITION (TANGLED ROPE) — Organized agents (unions, workforce representatives, professional associations) recognize the genuine coordination function: competence does decay and exercises do work. But they also recognize extraction: the constraint's enforcement mechanisms (audit trails, pass/fail evaluation, career consequences for 'insufficient' exercise participation) create surveillance and control infrastructure that extends beyond the functional need. The coalition is constrained (has some negotiating power but cannot refuse the requirement) and partially benefits (exercises improve actual safety, which is workers' primary interest) but also bears extraction (burden of continuous evaluation, time away from revenue-generating work, risk of reputational damage from exercise failure). Mixed classification with moderate perceived extraction.
constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EXERCISE INDUSTRY ECOSYSTEM (PITON) — Simulation vendors, training consultants, and exercise-design firms maintain the constraint's operational infrastructure. From their seat, the exercise requirement is largely performative: many exercises are designed to satisfy audit requirements and documentation standards rather than to actually test competence under realistic conditions. Theater ratio is high (0.68 base; likely 0.75+ from this perspective) — exercises often involve 'clean' scenarios with advance notice, structured role-play, and evaluation criteria optimized for pass rates rather than error detection. The industry collects revenue from perpetual exercise requirements but the functional verification of competence has degraded. The piton classification reflects that exercises are maintained largely because they have become institutionalized, not because they optimally preserve competence.
constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal vantage, the constraint appears to express an immutable property of human competence: skills decay without practice (exponential decay model from cognitive science), catastrophe-avoidance competence is specialized and high-consequence, therefore active exercise is inherent to safe operation. This perspective sees the requirement not as a contingent institutional arrangement but as a natural law of skill maintenance. However, the structural data (presence of beneficiaries, enforcement requirements, high suppression, significant theater ratio) contradicts the mountain classification — the engine will detect this as a false summit, revealing that 'competence decay is a law of nature' naturalizes what is actually a contestable institutional commitment about what level of exercise is necessary and sufficient.
constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exercise_as_competence_maintenance_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exercise_as_competence_maintenance_flat_control, TR),
    TR >= 0.70.

:- end_tests(exercise_as_competence_maintenance_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint solves a genuine problem (competence decay is real and safety-critical), which would justify extractiveness near zero for pure coordination. But extraction has accumulated over three decades through regulatory expansion, institutional capture, and mission creep — the current extractiveness reflects both the functional requirement (maintaining competence) and the extractive overlay (mandating frequency/format beyond what evidence suggests is necessary, surveillance and control infrastructure, perpetual institutional dependence). The rising trajectory (0.35 → 0.58 over 30 years) indicates accumulating extraction as alternatives are foreclosed and institutional dependence deepens. Suppression (0.62): Moderate-high. Organizations cannot opt out of the requirement (regulatory mandate), cannot freely choose alternative competence-maintenance mechanisms (certification standards lock in the exercise-based approach), cannot reduce frequency (standards are non-negotiable), and face reputational/legal consequences for non-compliance. Field personnel cannot refuse participation without career consequences. The suppression is structural (enforcement by regulatory/legal mechanisms) not internalized, though some institutional actors have internalized the doctrine that exercises are necessary and beneficial. Theater ratio (0.68, rising): High and increasing. Many exercises are designed to satisfy audit requirements and pass-rate targets rather than to test competence under realistic conditions. Scenarios are often pre-announced, simplified, role-play based, and evaluated against standardized rubrics that optimize for compliance rather than error detection. The rising trajectory suggests exercises are becoming more formulaic and less functionally aligned with real catastrophe-avoidance scenarios. This is a hallmark of Goodhart drift (when a measure becomes a target, it ceases to measure what it was meant to measure).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates wide perspectival disagreement on classification from structurally different positions. The beneficiary (safety discipline, certifying authority) sees rope or tangles rope with coordination-dominant experience — the requirement genuinely maintains competence and sustains legitimate regulatory authority. The victim (resource-constrained operator) sees snare — mandatory participation with no exit, no discretion, full cost absorption. The organized workforce sees tangled rope — the coordination function is real (exercises do improve actual safety) but embedded in surveillance and control infrastructure that extends extraction beyond the functional need. The exercise industry sees piton — the constraint is maintained largely through institutional inertia and revenue extraction, not because exercises optimally preserve competence. The analytical observer risks mountain — seeing competence decay as a law of nature, which the structural data contradicts. The perspectival gaps are genuine: different agents truly experience different classifications because the constraint is structurally asymmetric (benefits and costs flow differently depending on position). The gaps are not perceptual disagreements about the same thing but real distributional inequalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent flows from their structural relationship to the extraction flow and their exit options. Resource-constrained organizations are trapped victims with zero discretion (d ≈ 1.0 → high experienced extractiveness). Field safety officers are constrained-exit partial victims who also benefit from the coordination function (d ≈ 0.65 → moderate experienced extractiveness). Safety disciplines are beneficiaries with arbitrage-level exit (they can choose to emphasize alternative competence-maintenance mechanisms but choose not to, so d ≈ 0.0 → negative experienced extractiveness, i.e., subsidy). Certifying authorities are beneficiary-coordinators with constrained exit but also regulatory power that gives them some arbitrage-like position (d ≈ 0.35 → net modest beneficiary experience). The exercise industry is pure arbitrage beneficiary (d ≈ -0.2 → strong subsidy/extraction source). The high suppression value (0.62) modulates these d values upward (trapped agents feel extraction more acutely when suppression is high, because they cannot escape through any exit mechanism), but the underlying directionality structure determines which agents the extraction flows toward.
 *
 * MANDATROPHY ANALYSIS:
 *   CONTESTED MANDATE WITH ACCUMULATED EXTRACTION: The founding mandate is clear — maintain the competence required for catastrophe avoidance — and remains legitimate. But the institutional commitment has accumulated extraction mechanisms that exceed the functional requirement: regulatory standards specify exercise frequency and format beyond what evidence suggests is optimal; enforcement infrastructure (audit trails, compliance tracking, pass/fail evaluation) extracts surplus institutional authority; mission creep from safety engineering disciplines expands the scope of 'catastrophe avoidance competence' to include peripheral domains; industry capture by exercise vendors sustains the requirement partly for revenue reasons. The theater ratio rising from 0.45 to 0.72 signals that exercises increasingly satisfy documentation and compliance requirements rather than genuinely testing competence. Mandatrophy is NOT resolved — the mandate is still being executed but with degraded functional alignment. The constraint could be reformed by: (1) empirical specification of exercise frequency based on actual competence decay rates by domain, (2) flexibility to substitute alternative competence-maintenance mechanisms (mentoring, real-time monitoring), (3) reduction of theater through higher-fidelity scenarios, (4) transparency about institutional interests in perpetuating the requirement. The analytical observer's false-summit risk is high: the constraint presents itself as a natural law ('competence inherently decays') when it is actually a contestable institutional commitment about the level and form of maintenance necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_empirical_rate,
    'What is the actual empirical decay rate of catastrophe-avoidance competence without active exercise, and how does it vary by domain, expertise level, and time since last practice?',
    'Longitudinal studies tracking competence retention in safety-critical domains (aviation, nuclear operations, emergency response, surgical teams) with controlled intervals between exercises and validated competence metrics. Comparison of performance degradation curves across domains.',
    'If decay is rapid (half-life < 6 months): the exercise requirement is a genuine coordination mechanism, not extractive. If decay is slow (half-life > 2 years) or domain-dependent: the universal requirement may be over-specified, and the constraint shifts from rope/tangled_rope toward snare (extraction through unnecessary enforcement). If decay is measurable but recoverable through brief refresher: the frequency and duration of required exercises may be substantially overspecified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_decay_empirical_rate, empirical, 'Actual empirical decay rate of catastrophe-avoidance competence').

omega_variable(
    exercise_functionality_versus_theater,
    'What proportion of required exercises actually test competence under conditions relevant to real catastrophe-avoidance scenarios, versus satisfying audit/documentation requirements with low-stakes, pre-announced, role-play conditions?',
    'Content analysis of exercise objectives, design protocols, and evaluation criteria across representative organizations. Measurement of exercise conditions: how much advance notice, how much role-play versus simulated real-time conditions, how much scenario novelty versus repetition. Comparison of exercises that detect competence failures versus exercises that typically result in pass ratings.',
    'If theater_ratio > 0.75: most exercises are performative, and the constraint is less about competence maintenance and more about compliance documentation. The snare and piton classifications become more justified. If theater_ratio < 0.40: exercises are genuinely high-fidelity assessments, and rope/tangled_rope classifications hold. Theater shift over time (increasing) suggests the constraint is drifting from coordination toward extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exercise_functionality_versus_theater, empirical, 'Ratio of genuine competence testing versus audit theater in required exercises').

omega_variable(
    alternative_competence_maintenance_mechanisms,
    'What alternative competence-maintenance mechanisms exist (apprenticeship, mentoring, simulation, real-time performance monitoring, knowledge management systems) and what is their relative effectiveness at preventing competence-caused catastrophes?',
    'Comparative effectiveness studies across domains: organizations using primarily formal exercises versus those emphasizing continuous mentoring, on-the-job practice, or real-time performance feedback. Measurement of catastrophe rates, competence assessment outcomes, and cost of competence maintenance across mechanisms.',
    'If alternatives are equally or more effective: the exercise requirement is not a natural law of competence maintenance but one institutional choice among others. The constraint becomes less justifiable as a universal mandate and more identifiable as extractive. If exercises are substantially more effective: the rope classification is justified and the constraint is genuinely coordination. If effectiveness is highly domain-dependent: the universal requirement is over-specified and should vary by domain, suggesting extraction through one-size-fits-all enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_competence_maintenance_mechanisms, empirical, 'Relative effectiveness of alternative competence-maintenance mechanisms').

omega_variable(
    regulatory_capture_in_exercise_standards,
    'Do certifying authorities and safety disciplines maintain elevated exercise frequency/intensity because they reflect genuine competence-preservation needs, or because the perpetual requirement sustains institutional funding, authority, and professional employment?',
    'Historical analysis of exercise standard changes: do standards tighten during periods of institutional resource abundance (suggesting institutional expansion) or during periods of documented competence failure (suggesting response to evidence)? Comparison of standards across regulatory regimes: do higher-resource jurisdictions mandate more intensive exercises, and do they have lower catastrophe rates proportional to the increased exercise burden? Measurement of career/funding consequences for regulators or safety professionals if exercise requirements were reduced.',
    'If standards tighten during resource abundance periods: regulatory capture is operating and the constraint extracts partly through institutional self-perpetuation. The snare/tangled_rope classifications are justified. If standards respond primarily to documented failures: the constraint is more genuinely coordinated. If catastrophe rates do not correlate with exercise intensity: the requirement is extractive (imposing costs without proportional safety benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_exercise_standards, empirical, 'Extent of regulatory capture in exercise standard-setting').

omega_variable(
    competence_maintenance_doctrine_contingency,
    'Is the commitment that ''catastrophe-avoidance competence must be actively exercised to remain operational'' a universal principle of human skill maintenance, or a historically contingent institutional arrangement that reflects 20th-century bureaucratic approaches to safety?',
    'Historical sociology of safety practice: how did the exercise requirement emerge? What were the alternatives that were considered and rejected? How have different domains (aviation, nuclear, medical, military) developed different approaches to competence maintenance? What changes in technology (real-time monitoring, AI-assisted competence assessment, simulation fidelity) might enable different approaches? Ethnographic study of how organizations actually maintain competence in practice versus what their formal documentation claims.',
    'If the doctrine is contingent: the constraint is a contestable institutional choice, not a law of nature. The false-summit detection is justified — this is a mountain misclassification. The constraint could be reformed or replaced. If the doctrine is universal: the requirement is closer to a natural law, and rope classifications are more justified. Most likely outcome: the doctrine is partially universal (some active practice is necessary) but substantially contingent (the specific frequency, format, and enforcement mechanisms are institutional choices that extract beyond the functional minimum).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_maintenance_doctrine_contingency, conceptual, 'Whether exercise-as-maintenance is universal principle or contingent institutional arrangement').

omega_variable(
    knowledge_retention_versus_skill_decay,
    'Do exercises primarily test/maintain knowledge (facts, procedures, decision trees) or embodied skill (motor coordination, pattern recognition under stress, judgment under uncertainty)? Do these require different maintenance frequencies?',
    'Cognitive psychology analysis of what competence domains require continuous exercise versus what can be effectively refreshed through knowledge review. Performance studies distinguishing knowledge errors from skill failures in catastrophic incidents. Comparison of exercise effectiveness when targeting knowledge versus skill maintenance.',
    'If knowledge and skill have different decay rates: the universal frequency requirement is misspecified. Knowledge can likely be maintained through documentation and periodic review; skill requires continuous practice. The constraint could be reformed to distinguish them. If the current requirement conflates knowledge and skill: some portion of the exercise burden is extractive (maintaining knowledge at a higher-than-necessary frequency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_retention_versus_skill_decay, empirical, 'Differential decay rates of competence knowledge versus embodied skill').

omega_variable(
    institutional_insurance_versus_genuine_safety,
    'To what extent does the exercise requirement function as genuine safety assurance (prevents actual catastrophes) versus institutional liability insurance (creates documented evidence of competence maintenance, protecting organizations from legal liability regardless of actual safety outcome)?',
    'Forensic analysis of catastrophic failures: do investigation reports cite exercise failures or exercise absence as causal factors, or do they cite factors independent of exercise history? Measurement of correlation between exercise compliance and catastrophe rates across organizations. Analysis of whether exercise records function in post-incident litigation (they do, making them valuable as liability protection). Comparison of organizations with high exercise compliance but accidents versus high safety outcomes with lower exercise intensity.',
    'If exercises prevent actual catastrophes: the constraint is genuinely coordinated (rope/tangled_rope). If exercises primarily function as liability insurance: the constraint extracts by imposing compliance costs without proportional safety benefit — shifting toward snare classification. Most likely outcome: genuine safety benefit exists but is substantially smaller than the institutional overhead of enforcement, making the constraint extractive (the excess exercise beyond the safety-optimal frequency is insurance, not coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_insurance_versus_genuine_safety, empirical, 'Extent to which exercise requirement functions as safety mechanism versus liability insurance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance_flat_control, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excmaint_tr_t0, exercise_as_competence_maintenance_flat_control, theater_ratio, 0, 0.45).
narrative_ontology:measurement(excmaint_tr_t10, exercise_as_competence_maintenance_flat_control, theater_ratio, 10, 0.58).
narrative_ontology:measurement(excmaint_tr_t20, exercise_as_competence_maintenance_flat_control, theater_ratio, 20, 0.68).
narrative_ontology:measurement(excmaint_tr_t30, exercise_as_competence_maintenance_flat_control, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(excmaint_be_t0, exercise_as_competence_maintenance_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(excmaint_be_t10, exercise_as_competence_maintenance_flat_control, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(excmaint_be_t20, exercise_as_competence_maintenance_flat_control, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(excmaint_be_t30, exercise_as_competence_maintenance_flat_control, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(excmaint_su_t0, exercise_as_competence_maintenance_flat_control, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(excmaint_su_t10, exercise_as_competence_maintenance_flat_control, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(excmaint_su_t20, exercise_as_competence_maintenance_flat_control, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(excmaint_su_t30, exercise_as_competence_maintenance_flat_control, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance_flat_control, regulatory_cargo_cult_dynamics).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance_flat_control, safety_certification_regime_capture).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance_flat_control, competence_documentation_theater).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance_flat_control, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
