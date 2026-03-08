% ============================================================================
% CONSTRAINT STORY: institutional_mandate_vs_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_mandate_vs_autonomy, []).

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
 *   constraint_id: institutional_mandate_vs_autonomy
 *   human_readable: School Phone Bans and Student Autonomy Development
 *   domain: technology_governance/behavioral_psychology/social_infrastructure
 *
 * SUMMARY:
 *   School phone bans represent a structural tension between institutional
 *   management needs and student developmental requirements. The constraint
 *   emerged as a response to legitimate classroom distraction problems but
 *   has evolved into a compliance theater that extracts from student autonomy
 *   development while providing diminishing coordination benefits. Initial
 *   implementations (2016-2018) focused on reducing distraction and
 *   cyberbullying; current implementations (2024-present) increasingly serve
 *   liability management and visible policy response to parent anxiety. The
 *   constraint exhibits the tangled rope pattern: genuine coordination
 *   function (classroom attention management) coexists with asymmetric
 *   extraction (students bear developmental cost while institutions capture
 *   management benefits). Student circumvention methods (burner phones, Apple
 *   Watch usage, pouch destruction) reveal the theater: enforcement becomes
 *   cat-and-mouse rather than skill-building. The biographical time horizon
 *   is critical: autonomy skills require practice during adolescence, and the
 *   mandate prevents this practice during a developmental window that may not
 *   reopen. The constraint is downstream of two structural realities:
 *   attention as capturable resource (mountain — phones genuinely compete for
 *   attention) and friction as intervention medium (rope — adding steps can
 *   redirect behavior). But the mandate's specific implementation choice
 *   (blanket ban rather than graduated autonomy) converts these upstream
 *   constraints into extraction from student development.
 *
 * KEY AGENTS:
 *   - Developing Student: Primary victim (powerless/trapped) — bears full developmental cost, cannot exit, cannot practice autonomy during critical window
 *   - Student Autonomy Development: Abstract victim (powerless/trapped) — collective developmental good with no advocate; long-term capability loss
 *   - School Administrator: Primary beneficiary (institutional/arbitrage) — captures management simplification, liability reduction, policy credit
 *   - Compliance Technology Vendor: Secondary beneficiary (institutional/arbitrage) — Yondr, phone locker systems, monitoring software market
 *   - Concerned Parent: Mixed position (moderate/constrained) — benefits from distraction reduction, bears cost of unprepared college student
 *   - Classroom Teacher: Mixed position (moderate/constrained) — benefits from attention management, bears enforcement burden and pedagogical constraint
 *   - Digital Literacy Coalition: Organized agents (organized/mobile) — advocacy groups seeing sunset path through pedagogical alternatives
 *   - School Board (Long View): Institutional observer (institutional/constrained) — recognizes theater but constrained by reversal costs
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination and genuine extraction coexisting structurally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_mandate_vs_autonomy, 0.48).
domain_priors:suppression_score(institutional_mandate_vs_autonomy, 0.62).
domain_priors:theater_ratio(institutional_mandate_vs_autonomy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_mandate_vs_autonomy, extractiveness, 0.48).
narrative_ontology:constraint_metric(institutional_mandate_vs_autonomy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_mandate_vs_autonomy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_mandate_vs_autonomy, tangled_rope).
narrative_ontology:human_readable(institutional_mandate_vs_autonomy, "School Phone Bans and Student Autonomy Development").
narrative_ontology:topic_domain(institutional_mandate_vs_autonomy, "technology_governance/behavioral_psychology/social_infrastructure").

domain_priors:requires_active_enforcement(institutional_mandate_vs_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_mandate_vs_autonomy, institutional_administrators).
narrative_ontology:constraint_beneficiary(institutional_mandate_vs_autonomy, compliance_technology_vendors).
narrative_ontology:constraint_victim(institutional_mandate_vs_autonomy, student_autonomy_development).
narrative_ontology:constraint_victim(institutional_mandate_vs_autonomy, student_agency_practice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING STUDENT (SNARE) — Trapped in institutional environment with no exit during critical developmental window. Bears full cost of autonomy skill atrophy. Cannot opt out of the mandate, cannot practice self-regulation, cannot develop executive function skills that require failure and recovery. The constraint extracts from their developmental trajectory while providing no coordination benefit from their position — they experience only control, not preparation.
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONCERNED PARENT (TANGLED ROPE) — Constrained by school choice limitations and competing priorities. Benefits from reduced distraction concerns during school hours but bears cost of student arriving at college unprepared for autonomy. Experiences both coordination (school handles attention management) and extraction (developmental preparation outsourced to institution that optimizes for compliance rather than capability). Can advocate but cannot exit without relocating or private school costs.
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SCHOOL ADMINISTRATOR (ROPE) — Benefits from simplified classroom management, reduced liability exposure, and visible policy response to parent concerns. Experiences constraint as coordination: solves collective action problem of classroom distraction through uniform rule. Can arbitrage between enforcement mechanisms (Yondr pouches, phone lockers, confiscation policies) and adjust implementation. Extraction runs toward this agent through reduced management burden and policy credit.
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: COMPLIANCE TECHNOLOGY VENDOR (ROPE) — Yondr pouch manufacturers, phone locker systems, monitoring software providers. Pure beneficiary: constraint creates market for enforcement products. Experiences as coordination: providing tools that schools need. Can arbitrage between school districts and product lines. No extraction from their position — they are downstream beneficiaries of the mandate structure.
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CLASSROOM TEACHER (TANGLED ROPE) — Benefits from reduced in-class distraction but bears cost of enforcement burden and loss of pedagogical flexibility. Cannot use phones for legitimate learning activities (research, documentation, calculation, translation). Constrained by administrative mandate but also benefits from simplified attention management. Mixed experience: coordination function (distraction reduction) coexists with extraction (pedagogical autonomy loss, enforcement labor).
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: DIGITAL LITERACY COALITION (SCAFFOLD) — Organizations advocating for scaffolded phone use, digital citizenship curricula, and graduated autonomy models. See the blanket ban as temporary overreach with sunset logic: as schools develop capacity for teaching self-regulation rather than imposing external control, the mandate structure will give way to pedagogical approaches that build autonomy skills. Mobile because they can shift advocacy to receptive districts. Low extraction because they see a path to replacement with better coordination mechanisms.
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: SCHOOL BOARD LONG VIEW (PITON) — From a generational perspective, school boards implementing these mandates increasingly recognize the theater: students circumvent with burner phones, Apple Watches, and pouch destruction; enforcement becomes cat-and-mouse; the policy persists because reversing it signals weakness rather than because it works. Theater ratio high: visible policy action maintained through institutional inertia despite low functional impact on actual phone use or attention development.
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint genuinely coordinates classroom attention management (addressing real collective action problem) while simultaneously extracting from student developmental preparation (autonomy skills require practice with failure and recovery, which the mandate prevents). The coordination function is real but could be achieved through graduated autonomy models; the extraction is also real and compounds over biographical time as students reach college without self-regulation practice. Both functions coexist structurally.
constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mandate_vs_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mandate_vs_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_mandate_vs_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_mandate_vs_autonomy, TR),
    TR >= 0.70.

:- end_tests(institutional_mandate_vs_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Students bear significant developmental cost (autonomy skill atrophy during critical window, college transition shock, executive function practice deficit) while institutions capture management benefits (simplified classroom control, reduced liability exposure, visible policy response). The extraction is not maximal because some coordination benefit exists (genuine distraction reduction in early implementation) and because circumvention provides partial agency practice. But the asymmetry is substantial: the biographical cost to students compounds over time while institutional benefits are immediate and concentrated. Suppression (0.62): High. Students cannot exit the institutional environment during the mandate period (compulsory attendance, limited school choice, high private school costs). Cannot opt into graduated autonomy alternatives. Cannot practice self-regulation because the constraint removes the domain where practice would occur. Enforcement mechanisms (confiscation, detention, parent notification) create compliance pressure. But suppression is not total: circumvention is possible (burner phones, watches, pouch destruction), and some schools offer exemptions for specific use cases. Theater ratio (0.68): High and rising. Initial implementations had lower theater (0.35) when enforcement was novel and circumvention methods were undeveloped. Current implementations show high theater: students circumvent systematically, enforcement becomes performative (checking pouches that students have learned to defeat), policy persists because reversing signals weakness rather than because it achieves stated goals (attention management, skill development). The theater increase over the interval reflects Goodhart drift: the measurable proxy (phones physically separated from students) has replaced the real goal (students developing self-regulation capacity).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the tangled rope pattern through perspectival divergence. School administrators see rope (coordination: solving classroom distraction through uniform rule, capturing management benefits). Students see snare (pure extraction: autonomy practice prevented, developmental cost imposed, no exit during critical window). Parents and teachers see tangled rope (mixed: distraction reduction coexists with developmental harm and enforcement burden). The digital literacy coalition sees scaffold (temporary problem with sunset: pedagogical alternatives are emerging that achieve coordination without extraction). School boards taking the long view see piton (degraded ritual: policy persists through inertia despite high theater and low functional impact). The analytical observer sees tangled rope at the civilizational scale (both coordination and extraction are structurally real and coexist). The gap between the administrator's rope and the student's snare is the core diagnostic: what appears as pure coordination from the beneficiary position appears as pure extraction from the victim position, revealing that both functions are present but asymmetrically distributed. The scaffold perspective is critical: if graduated autonomy models can achieve the coordination function without the extraction cost, the constraint is a false necessity rather than an inherent tradeoff.
 *
 * DIRECTIONALITY LOGIC:
 *   Students are primary victims with trapped exit options, yielding high directionality values and high experienced extraction. Their powerless position and biographical time horizon mean they bear maximum cost with no agency. School administrators are primary beneficiaries with arbitrage exit options (can choose between enforcement mechanisms, can adjust implementation, can reverse policy if costs exceed benefits), yielding low directionality values and low or negative experienced extraction — they capture benefits while bearing minimal costs. Compliance technology vendors are pure beneficiaries (the mandate creates their market) with arbitrage options (multiple school districts, multiple product lines), yielding very low directionality and negative experienced extraction. Parents and teachers occupy mixed positions: they are listed as beneficiaries (distraction reduction, management simplification) but also bear costs (developmental preparation deficit, enforcement burden), and their constrained exit options (limited school choice, employment lock-in) yield moderate directionality values. The digital literacy coalition has organized power and mobile exit options (can shift advocacy to receptive districts), yielding low experienced extraction because they see a path to replacing the constraint with better alternatives. The analytical observer sees both the coordination function (genuine classroom attention management) and the extraction mechanism (developmental cost) as structurally coexisting, confirming the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled rope is the correct classification when BOTH coordination and extraction are structurally present and neither can be removed without changing the constraint's identity. The coordination function is real: blanket bans do reduce classroom distraction in the immediate term, solving a genuine collective action problem. The extraction is also real: students lose autonomy practice during a critical developmental window, bearing biographical costs that compound into college and adulthood. The constraint is not a rope mislabeled as extraction (the developmental harm is structural, not incidental) and not a snare mislabeled as coordination (the distraction reduction is genuine, not theatrical). It is a hybrid where both functions coexist and are inseparable given the current implementation choice. The mandatrophy resolution depends on the omega variable about pedagogical alternatives: if graduated autonomy models can achieve coordination without extraction, the tangled rope is contingent (scaffold perspective confirmed). If no alternative works, the tangled rope is inherent (the tradeoff between immediate attention management and long-term autonomy development is structural). The analytical observer's tangled rope classification at civilizational scope reflects this: both functions are real, both are measurable, and the perspectival gap between beneficiaries (who see only coordination) and victims (who see only extraction) is itself diagnostic evidence that both are present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_skill_transferability,
    'Do students who experience phone bans in high school develop compensatory self-regulation skills in other domains, or does the developmental window close?',
    'Longitudinal study comparing college outcomes (academic performance, mental health, time management) for students from ban vs. non-ban high schools, controlling for prior achievement and socioeconomic factors',
    'If skills transfer: extraction is lower than measured (students adapt). If window closes: extraction is higher (critical period missed, autonomy deficit persists into adulthood).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_skill_transferability, empirical, 'Whether autonomy skill development transfers across domains or requires domain-specific practice').

omega_variable(
    circumvention_as_agency_practice,
    'Does student circumvention of phone bans (burner phones, watch usage, pouch destruction) constitute harmful rule-breaking or valuable agency practice?',
    'Qualitative analysis of student circumvention strategies and motivations; correlation between circumvention sophistication and later autonomy outcomes; ethical framework for distinguishing legitimate resistance from harmful defection',
    'If circumvention is agency practice: the constraint''s suppression is lower than measured (students are practicing autonomy through resistance). If harmful defection: suppression is accurately measured (circumvention undermines legitimate coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(circumvention_as_agency_practice, conceptual, 'Whether circumvention represents agency development or coordination defection').

omega_variable(
    attention_management_pedagogical_alternative,
    'Could graduated autonomy models (scaffolded phone use with explicit self-regulation instruction) achieve the coordination function without the extraction cost?',
    'Comparative study of schools using blanket bans vs. graduated autonomy curricula; measurement of both classroom attention metrics and student self-regulation skill development; cost-benefit analysis including teacher training requirements',
    'If alternatives work: the constraint is a false necessity (scaffold perspective confirmed, sunset is feasible). If alternatives fail: the coordination-extraction tradeoff is inherent (tangled rope is structural, not contingent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_management_pedagogical_alternative, empirical, 'Whether pedagogical alternatives can achieve coordination without extraction').

omega_variable(
    college_transition_shock_magnitude,
    'How severe is the autonomy shock when students from phone-ban high schools reach college environments with no external attention management?',
    'First-year college performance data comparing students from ban vs. non-ban high schools; mental health service utilization rates; time-to-degree completion; qualitative interviews about transition experience',
    'If shock is severe: extraction is higher than measured (biographical cost extends beyond high school). If shock is mild: extraction is lower (students adapt quickly, developmental harm is minimal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(college_transition_shock_magnitude, empirical, 'Magnitude of autonomy deficit when external control is removed').

omega_variable(
    institutional_liability_vs_developmental_mission,
    'Are schools optimizing for liability minimization (preventing phone-related incidents) or developmental mission (preparing students for autonomous adulthood)?',
    'Policy document analysis; administrator interview data about decision rationale; comparison of stated mission vs. revealed preference in policy design; legal analysis of actual liability exposure from phone-related incidents',
    'If liability-driven: the constraint is institutional risk transfer (extraction from students to protect institution). If mission-driven: the constraint is misguided coordination (genuine attempt to help that backfires).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_liability_vs_developmental_mission, conceptual, 'Whether policy optimizes for institutional protection or student development').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_mandate_vs_autonomy, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, institutional_mandate_vs_autonomy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_early, institutional_mandate_vs_autonomy, theater_ratio, 2, 0.48).
narrative_ontology:measurement(theater_mid, institutional_mandate_vs_autonomy, theater_ratio, 4, 0.58).
narrative_ontology:measurement(theater_late, institutional_mandate_vs_autonomy, theater_ratio, 6, 0.65).
narrative_ontology:measurement(theater_current, institutional_mandate_vs_autonomy, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(extract_initial, institutional_mandate_vs_autonomy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(extract_early, institutional_mandate_vs_autonomy, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(extract_mid, institutional_mandate_vs_autonomy, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(extract_late, institutional_mandate_vs_autonomy, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(extract_current, institutional_mandate_vs_autonomy, base_extractiveness, 8, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_mandate_vs_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_mandate_vs_autonomy, attention_as_capturable_resource).
narrative_ontology:affects_constraint(institutional_mandate_vs_autonomy, friction_as_intervention_medium).

% DUAL FORMULATION NOTE:
% This constraint is downstream of two structural realities: attention as capturable resource (mountain — phones genuinely compete for attention, this is not contingent) and friction as intervention medium (rope — adding steps can redirect behavior, this is a coordination tool). The phone ban mandate converts these upstream constraints into a specific institutional implementation that exhibits tangled rope characteristics. The upstream constraints have their own extractiveness values reflecting their structural properties; the mandate has its own extractiveness reflecting the developmental cost of the specific implementation choice (blanket ban rather than graduated autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
