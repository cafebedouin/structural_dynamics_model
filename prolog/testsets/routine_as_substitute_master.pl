% ============================================================================
% CONSTRAINT STORY: routine_as_substitute_master
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_routine_as_substitute_master, []).

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
 *   constraint_id: routine_as_substitute_master
 *   human_readable: Routine as Substitute Master: Habit-Based Servitude Without External Authority
 *   domain: philosophy_of_work/political_economy/ethics
 *
 * SUMMARY:
 *   Routine-as-substitute-master describes a constraint where inherited habit
 *   replaces external authority as the mechanism of servitude. Unlike
 *   traditional employment relationships where a boss directs labor, this
 *   constraint operates through the worker's internalized routines —
 *   procedures, heuristics, and decision trees that were initially learned
 *   under direction but persist after formal supervision ends. The constraint
 *   is particularly insidious because it masquerades as autonomy: the
 *   self-employed contractor or remote worker experiences freedom from a boss
 *   while remaining bound by routines that foreclose deliberative judgment.
 *   The extraction mechanism is the atrophy of agency capacity — the worker's
 *   ability to question, revise, and reject routines degrades over time as
 *   routine execution becomes constitutive of professional identity. This
 *   constraint exhibits strong identity-lock dynamics: the autonomous worker
 *   cannot exit because their self-concept as 'competent professional' is
 *   fused with routine mastery. The theater_ratio (0.45) reflects moderate
 *   performative content: some routine compliance is genuinely functional
 *   (coordination benefit), but a significant portion is ritual maintenance
 *   of professional norms that no longer serve their original purpose. The
 *   constraint has intensified over the 60-year interval as knowledge work
 *   has become more proceduralized and as productivity optimization
 *   frameworks (Lean, Agile, OKRs) have codified routines at increasingly
 *   granular levels.
 *
 * KEY AGENTS:
 *   - Autonomous Worker Who Stopped Judging: Primary victim (powerless/identity_locked) — has internalized routine as competence; cannot exit without abandoning professional identity; experiences agency atrophy as expertise
 *   - Deliberative Agency Capacity: Abstract victim (powerless/trapped) — collective cognitive capacity for judgment that cannot organize or advocate for itself; bears full cost of routine's displacement of deliberation
 *   - Self-Employed Contractor: Secondary victim (moderate/constrained) — retains some judgment through project selection but constrained by market expectations for standardized deliverables; mixed coordination and extraction
 *   - Organizational Efficiency System: Primary beneficiary (institutional/arbitrage) — captures productivity gains from routinized labor; experiences constraint as pure coordination mechanism
 *   - Worker Cooperative Movement: Organized agents (organized/mobile) — building alternative structures that restore judgment to routine through democratic governance and job rotation; sees sunset pathway
 *   - Senior Professional: Powerful agent (powerful/mobile) — can question routines but embedded in professional communities that enforce compliance; mixed experience of coordination and extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing routine-as-master as inherent cognitive efficiency rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(routine_as_substitute_master, 0.58).
domain_priors:suppression_score(routine_as_substitute_master, 0.68).
domain_priors:theater_ratio(routine_as_substitute_master, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(routine_as_substitute_master, extractiveness, 0.58).
narrative_ontology:constraint_metric(routine_as_substitute_master, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(routine_as_substitute_master, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(routine_as_substitute_master, snare).
narrative_ontology:human_readable(routine_as_substitute_master, "Routine as Substitute Master: Habit-Based Servitude Without External Authority").
narrative_ontology:topic_domain(routine_as_substitute_master, "philosophy_of_work/political_economy/ethics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(routine_as_substitute_master, organizational_efficiency_systems).
narrative_ontology:constraint_beneficiary(routine_as_substitute_master, productivity_optimization_frameworks).
narrative_ontology:constraint_victim(routine_as_substitute_master, autonomous_worker_who_stops_judging).
narrative_ontology:constraint_victim(routine_as_substitute_master, deliberative_agency_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AUTONOMOUS WORKER (SNARE) — Identity-locked despite structural mobility. The worker has internalized routine as competence itself. Exit would require abandoning the professional identity constructed through mastery of the routine. The binding is cognitive: the worker cannot see that the routine has replaced judgment because their self-concept as 'skilled professional' is constituted through routine execution. High extraction: agency capacity atrophies while the worker experiences this as expertise.
constraint_indexing:constraint_classification(routine_as_substitute_master, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: SELF-EMPLOYED CONTRACTOR (TANGLED ROPE) — Constrained by market expectations and client demands for standardized deliverables, but retains some deliberative capacity through project selection and method variation. Experiences genuine coordination benefit (routine enables reliable delivery) alongside extraction (routine narrows judgment to optimization within fixed parameters). Can exit specific routines at career cost but cannot exit the routine-expectation structure of professional services markets.
constraint_indexing:constraint_classification(routine_as_substitute_master, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORGANIZATIONAL EFFICIENCY SYSTEM (ROPE) — Benefits from routine as coordination mechanism. Standardized procedures enable prediction, scalability, and quality control. Experiences the constraint as pure coordination: routine solves the legitimate problem of coordinating distributed labor. Can arbitrage between routine frameworks (Taylorism, Lean, Agile) based on optimization metrics. Extraction runs toward this agent — captures productivity gains from workers' routinized labor.
constraint_indexing:constraint_classification(routine_as_substitute_master, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WORKER COOPERATIVE MOVEMENT (SCAFFOLD) — Organized agents building alternative ownership structures that restore judgment to routine. Sees routine-as-master as temporary: democratic workplace governance, job rotation, and participatory decision-making are creating pathways where routine serves deliberation rather than replacing it. Sunset mechanism: as cooperative models mature and scale, the separation of routine execution from strategic judgment becomes optional rather than structural. Estimated timeline: 20-40 years for cooperative norms to achieve significant market share in knowledge work sectors.
constraint_indexing:constraint_classification(routine_as_substitute_master, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / HABIT FORMATION VIEW (MOUNTAIN) — From a civilizational perspective, habit formation is a cognitive efficiency mechanism inherent to human psychology. All learning involves converting deliberative processes into automatic routines to free cognitive resources for novel problems. This perspective sees routine-as-master as an immutable feature of how minds work. However, structural data contradicts this: the constraint's extractiveness derives not from habit formation per se but from institutional arrangements that prevent workers from revisiting and revising routines. The mountain classification is a false summit — naturalizing contingent power relations as cognitive necessity.
constraint_indexing:constraint_classification(routine_as_substitute_master, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SENIOR PROFESSIONAL (TANGLED ROPE) — Has accumulated enough career capital to question and modify routines, but remains embedded in professional communities that enforce routine compliance through peer review, certification, and reputation. Experiences coordination benefit (routine enables collaboration with junior colleagues) and extraction (routine limits innovation to incremental optimization). Can exit specific professional communities but faces reputational cost and knowledge depreciation.
constraint_indexing:constraint_classification(routine_as_substitute_master, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(routine_as_substitute_master_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(routine_as_substitute_master, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(routine_as_substitute_master, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(routine_as_substitute_master, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(routine_as_substitute_master_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts agency capacity — the worker's ability to deliberate, question, and revise procedures atrophies as routine becomes constitutive of professional identity. This is not as severe as debt bondage or physical coercion (which would be 0.70+), but it is substantial: the worker loses the capacity for autonomous judgment while experiencing this loss as skill acquisition. The value reflects that some routine is genuinely functional (coordination benefit) but much of it forecloses deliberation unnecessarily. Suppression (0.68): High. Barriers to exit include identity fusion (the worker's self-concept is constituted through routine mastery), market expectations (clients demand standardized deliverables), professional community enforcement (peer review and certification require routine compliance), and cognitive sunk costs (deliberative capacity has atrophied, making exit cognitively expensive). The worker is structurally mobile (could change careers, relocate, retrain) but functionally trapped by internalized framing. Theater ratio (0.45): Moderate. Some routine compliance is genuinely functional — standardized procedures enable coordination, quality control, and knowledge transfer. But a significant portion is performative: compliance rituals that maintain professional boundaries and signal competence without serving the routine's original coordination purpose. The theater has increased over the interval as productivity frameworks have codified routines at finer granularity, creating compliance overhead that exceeds coordination benefit.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — routine displacing deliberation — appears as servitude (snare) from the trapped worker's perspective, as mixed coordination-extraction (tangled rope) from the constrained contractor's and senior professional's perspectives, as pure coordination (rope) from the efficiency system's perspective, as a temporary problem with a sunset (scaffold) from the cooperative movement's perspective, and as immutable cognitive necessity (mountain) from the analytical observer's naturalized view. The gap between the powerless/identity_locked perspective (snare) and the institutional/arbitrage perspective (rope) is the constraint's core extraction mechanism: what the beneficiary experiences as coordination (routine enables scalable productivity) the victim experiences as agency loss (routine forecloses judgment). The identity-lock is critical: the worker cannot exit not because of material barriers but because exit would require abandoning the professional identity constituted through routine mastery. The scaffold perspective (cooperative movement) reveals that the constraint is not immutable — democratic workplace governance and job rotation can restore judgment to routine — but the sunset timeline is generational (20-40 years), not biographical. The analytical observer's mountain classification is a false summit: habit formation is a cognitive mechanism, but the constraint's extractiveness derives from institutional arrangements that prevent workers from revisiting routines, not from habit formation per se.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomous worker (powerless/identity_locked) is the primary victim with high directionality (d ≈ 0.89): identity-locked exit combined with victim status produces high d, which feeds into high experienced extraction via the sigmoid f(d). The worker is structurally mobile but cognitively trapped — could leave the profession but cannot see this option from within the identity frame that constitutes competence as routine mastery. The self-employed contractor (moderate/constrained) has moderate directionality (d ≈ 0.55): constrained exit plus victim status produces moderate d, reflecting mixed experience of coordination benefit and extraction. The organizational efficiency system (institutional/arbitrage) is the primary beneficiary with low directionality (d ≈ 0.05): arbitrage exit plus beneficiary status produces low d, yielding negative effective extraction — the system captures productivity gains. The worker cooperative movement (organized/mobile) has low-moderate directionality (d ≈ 0.35): mobile exit plus mixed beneficiary/victim status (benefits from coordination, bears cost of building alternatives) produces moderate d. The senior professional (powerful/mobile) has moderate directionality (d ≈ 0.50): mobile exit plus mixed beneficiary/victim status produces symmetric d, reflecting balanced experience of coordination and extraction. The analytical observer (analytical/analytical) has the canonical analytical directionality (d ≈ 0.72), but the mountain classification is a false summit — the constraint's extractiveness derives from contingent institutional arrangements, not from immutable cognitive necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing routine-as-coordination (genuine efficiency mechanism) from routine-as-servitude (agency displacement). The organizational efficiency system's rope classification is legitimate: from its perspective, routine solves the coordination problem of distributed labor. The autonomous worker's snare classification is also legitimate: from their perspective, routine has replaced external authority as the binding mechanism, and the identity-lock prevents exit. The mandatrophy is not 'which classification is correct?' but 'which structural position are you measuring from?' The constraint exhibits both coordination function (routine enables prediction and scalability) and asymmetric extraction (routine atrophies deliberative agency for workers while concentrating productivity gains for efficiency systems). The tangled_rope classification from moderate/constrained and powerful/mobile perspectives captures this hybridity. The scaffold perspective (cooperative movement) shows that the extraction is not inherent to routine per se but to the institutional separation of routine execution from routine revision authority. The mountain classification (analytical observer) is a false summit — it naturalizes the constraint by attributing it to immutable cognitive necessity (habit formation) rather than to contingent power relations (who controls routine revision).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberation_frequency_threshold,
    'What frequency of deliberative judgment distinguishes healthy habit formation from agency-eroding routine?',
    'Longitudinal studies tracking decision-making frequency, cognitive flexibility measures, and innovation capacity across workers with varying routine intensity. Comparison of self-employed vs employed workers controlling for task complexity.',
    'If threshold is high (daily deliberation required): most employment relationships are extractive. If threshold is low (monthly deliberation sufficient): routine-as-master affects only extreme cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_frequency_threshold, empirical, 'Deliberation frequency threshold for distinguishing habit from servitude').

omega_variable(
    routine_revision_authority,
    'Does the capacity to revise routines require formal authority or can it emerge from tacit knowledge and informal practice?',
    'Ethnographic studies of workplace innovation; comparison of routine modification rates in hierarchical vs flat organizations; analysis of bottom-up process improvement initiatives.',
    'If formal authority required: extraction is structural and tied to ownership. If tacit revision possible: extraction is cultural and addressable through norm change without ownership restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(routine_revision_authority, empirical, 'Whether routine revision requires formal authority or can emerge informally').

omega_variable(
    identity_lock_reversibility,
    'Can workers who have internalized routine-as-competence recover deliberative agency, or is the identity fusion permanent within a biographical timeframe?',
    'Career transition studies; retraining program outcomes; psychological research on professional identity reconstruction; sabbatical and job rotation effects on cognitive flexibility.',
    'If reversible: identity_locked classification overstates permanence; constraint is constrained-level with high psychological cost. If irreversible: identity_locked classification is accurate; extraction includes permanent agency loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity fusion with routine is reversible within biographical time').

omega_variable(
    automation_paradox_resolution,
    'Does automation of routine tasks restore deliberative agency or does it shift routine to meta-level (routine selection of automation tools, routine monitoring of automated processes)?',
    'Studies of knowledge workers post-automation adoption; comparison of judgment frequency before and after task automation; analysis of whether freed cognitive resources are allocated to deliberation or to new routines.',
    'If automation restores agency: scaffold perspective is strengthened (technological sunset). If automation shifts routine to meta-level: extraction mechanism is self-replicating and scaffold perspective is aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(automation_paradox_resolution, empirical, 'Whether automation eliminates routine-as-master or shifts it to meta-level').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(routine_as_substitute_master, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(routine_tr_t0, routine_as_substitute_master, theater_ratio, 0, 0.25).
narrative_ontology:measurement(routine_tr_t15, routine_as_substitute_master, theater_ratio, 15, 0.35).
narrative_ontology:measurement(routine_tr_t30, routine_as_substitute_master, theater_ratio, 30, 0.45).
narrative_ontology:measurement(routine_tr_t45, routine_as_substitute_master, theater_ratio, 45, 0.52).
narrative_ontology:measurement(routine_tr_t60, routine_as_substitute_master, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(routine_be_t0, routine_as_substitute_master, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(routine_be_t15, routine_as_substitute_master, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(routine_be_t30, routine_as_substitute_master, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(routine_be_t45, routine_as_substitute_master, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(routine_be_t60, routine_as_substitute_master, base_extractiveness, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(routine_as_substitute_master, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of structural_autonomy_access (mountain) — the baseline accessibility of autonomous work arrangements. Structural_autonomy_access establishes the floor: some workers have no access to self-employment or remote work and face routine-as-master within traditional employment. Routine_as_substitute_master describes the additional constraint that operates even when structural autonomy is accessible: the self-employed worker or remote employee who has escaped the boss but remains bound by internalized routine. The two constraints have different epsilon values because they measure different observables: structural_autonomy_access measures access barriers (material, legal, credentialing), while routine_as_substitute_master measures agency atrophy within accessible autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
