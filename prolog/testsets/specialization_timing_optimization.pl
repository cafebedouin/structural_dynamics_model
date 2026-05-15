% ============================================================================
% CONSTRAINT STORY: specialization_timing_optimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_specialization_timing_optimization, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: specialization_timing_optimization
 *   human_readable: Specialization Timing Optimization in Expertise Development
 *   domain: cognitive_science/expertise_development/learning_theory
 *
 * SUMMARY:
 *   The specialization timing optimization constraint governs when
 *   individuals commit to domain-specific expertise development and how that
 *   timing interacts with environment kindness to produce performance
 *   outcomes. The constraint coordinates legitimate developmental pathways
 *   through structured training, credentialing systems, and talent
 *   identification mechanisms. Simultaneously, it extracts from those whose
 *   developmental trajectory doesn't match the prescribed timeline: late
 *   bloomers are misclassified as lacking talent; early specializers in
 *   wicked environments (where feedback is delayed or misleading) invest
 *   years in deliberate practice that doesn't produce expertise;
 *   sampling-phase learners face institutional pressure to commit before
 *   exploration is complete. The constraint's extractiveness has increased
 *   over the 45-year interval as competitive pressure has pushed
 *   specialization earlier (youth sports academies, childhood coding
 *   programs, early college major declaration) while research on environment
 *   kindness and the benefits of sampling has accumulated but not yet
 *   translated into institutional reform. Theater ratio reflects the gap
 *   between the coordination narrative (optimal timing produces expertise)
 *   and the reality (timing interacts with environment type in ways
 *   institutions don't measure).
 *
 * KEY AGENTS:
 *   - Late Bloomers Misclassified as Failures: Primary victim (powerless/identity_locked) — internalized failure narrative despite developmentally appropriate sampling phase; identity fused with 'too late' framing
 *   - Wicked-Environment Early Specializers: Primary victim (powerless/trapped) — committed early to domains where deliberate practice doesn't produce expertise; trapped by sunk costs and credential lock-in
 *   - Sampling-Phase Learners: Secondary victim (moderate/constrained) — face institutional pressure to specialize while still exploring; bear opportunity cost of premature foreclosure
 *   - Early-Specialization Industrial Complex: Primary beneficiary (institutional/arbitrage) — youth academies, conservatories, pipeline programs capture enrollment revenue and prestige from producing prodigies
 *   - Kind-Environment Specialists: Secondary beneficiary (powerful/mobile) — early specialization worked as advertised in domains with immediate accurate feedback (chess, classical music, mathematics)
 *   - Range Advocacy Coalition: Organized agents (organized/constrained) — promote sampling and delayed specialization; benefit from constraint's failures as evidence but marginalized in policy debates
 *   - Developmental Psychology Research Community: Institutional observer (institutional/mobile) — building empirical understanding of environment kindness and critical periods; sees constraint as temporary problem being resolved through better science
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function contaminated by environment-blind application and institutional capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(specialization_timing_optimization, 0.48).
domain_priors:suppression_score(specialization_timing_optimization, 0.52).
domain_priors:theater_ratio(specialization_timing_optimization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(specialization_timing_optimization, extractiveness, 0.48).
narrative_ontology:constraint_metric(specialization_timing_optimization, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(specialization_timing_optimization, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(specialization_timing_optimization, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(specialization_timing_optimization, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(specialization_timing_optimization, tangled_rope).
narrative_ontology:human_readable(specialization_timing_optimization, "Specialization Timing Optimization in Expertise Development").
narrative_ontology:topic_domain(specialization_timing_optimization, "cognitive_science/expertise_development/learning_theory").

domain_priors:requires_active_enforcement(specialization_timing_optimization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(specialization_timing_optimization, early_specialization_industrial_complex).
narrative_ontology:constraint_beneficiary(specialization_timing_optimization, kind_environment_specialists).
narrative_ontology:constraint_victim(specialization_timing_optimization, late_bloomers_misclassified_as_failures).
narrative_ontology:constraint_victim(specialization_timing_optimization, wicked_environment_early_specializers).
narrative_ontology:constraint_victim(specialization_timing_optimization, sampling_phase_learners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE BLOOMER (SNARE) — Identity-locked by internalized narrative of being 'too late' or 'not talented enough' despite structural evidence that sampling phase was developmentally appropriate. The constraint extracts career opportunity and self-concept. Cannot exit because identity has fused with the failure narrative imposed by early-specialization ideology.
constraint_indexing:constraint_classification(specialization_timing_optimization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: WICKED-ENVIRONMENT EARLY SPECIALIZER (SNARE) — Committed early to a domain where feedback is delayed, noisy, or misleading (entrepreneurship, stock picking, clinical psychology). Trapped by sunk costs and credential lock-in. The constraint promised expertise through deliberate practice but delivered false confidence and career fragility. Maximum extraction — no exit and no benefit.
constraint_indexing:constraint_classification(specialization_timing_optimization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: SAMPLING-PHASE LEARNER (TANGLED ROPE) — Faces institutional pressure to specialize (college major selection, career track commitment) while still exploring domains. Benefits from the coordination function (structured pathways exist once commitment is made) but bears extraction through premature foreclosure and opportunity cost of paths not taken. Constrained by institutional timelines and social expectations but not fully trapped.
constraint_indexing:constraint_classification(specialization_timing_optimization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EARLY-SPECIALIZATION INDUSTRIAL COMPLEX (ROPE) — Youth sports academies, music conservatories, STEM pipeline programs, coding bootcamps for children. Benefits from the constraint through enrollment revenue and prestige of producing prodigies. Experiences the timing prescription as coordination: funneling talent efficiently. Net beneficiary with arbitrage exit — can pivot to different age cohorts or domains as markets shift.
constraint_indexing:constraint_classification(specialization_timing_optimization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: KIND-ENVIRONMENT SPECIALIST (ROPE) — Committed early to a domain with immediate, accurate feedback (chess, classical music performance, gymnastics, mathematics). The constraint worked as advertised: early specialization plus deliberate practice produced elite performance. Experiences low extraction because the coordination function (structured training pathways) delivered genuine skill. Mobile exit — can leverage expertise across related domains or transition to coaching/teaching.
constraint_indexing:constraint_classification(specialization_timing_optimization, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: RANGE ADVOCACY COALITION (TANGLED ROPE) — Researchers and educators promoting sampling, interdisciplinary exploration, and delayed specialization (Epstein's Range thesis, liberal arts advocates, generalist career models). Organized but constrained by institutional inertia favoring early tracking. Benefits from the constraint's failures (each misclassified late bloomer is evidence for their position) but also bears extraction through marginalization in policy debates dominated by kind-environment success stories.
constraint_indexing:constraint_classification(specialization_timing_optimization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DEVELOPMENTAL PSYCHOLOGY RESEARCH COMMUNITY (SCAFFOLD) — Sees the constraint as a temporary coordination problem being resolved through better empirical understanding of environment kindness, critical periods, and transfer effects. The one-size-fits-all timing prescription is giving way to domain-specific and individual-difference-sensitive models. Sunset mechanism: as research accumulates, prescriptive norms will be replaced by diagnostic tools that match specialization timing to environment type and learner characteristics.
constraint_indexing:constraint_classification(specialization_timing_optimization, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint coordinates legitimate developmental pathways (structured training, credentialing, talent identification) while simultaneously extracting from those whose developmental trajectory doesn't match the prescribed timeline. The extraction is asymmetric: kind-environment early specializers benefit; wicked-environment early specializers and late bloomers in any environment bear costs. The coordination function is real but contaminated by environment-blind application and institutional capture by early-specialization advocates.
constraint_indexing:constraint_classification(specialization_timing_optimization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(specialization_timing_optimization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(specialization_timing_optimization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(specialization_timing_optimization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(specialization_timing_optimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(specialization_timing_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts career opportunity from late bloomers (misclassified as failures), wasted investment from wicked-environment early specializers (deliberate practice that doesn't produce expertise), and opportunity cost from sampling-phase learners (premature foreclosure). The extraction is asymmetric: kind-environment early specializers benefit while others bear costs. However, extraction is not maximal because the coordination function is real — structured pathways do exist and do produce expertise when environment type matches timing prescription. Suppression (0.52): Moderate. Institutional timelines (college major declaration, career track selection), social expectations (parental pressure, peer comparison), and sunk cost dynamics create significant barriers to exit. Late bloomers face identity-level suppression (internalized failure narrative). But suppression is not total — some individuals do successfully specialize late or pivot after early commitment. Theater ratio (0.65): Moderate-high. The gap between coordination narrative and reality has grown as competitive pressure pushes specialization earlier while research reveals the importance of environment kindness and sampling benefits. Institutions measure commitment timing but not environment type or transfer effects. The theater is the performance of 'optimal development' without the diagnostic tools to match timing to context.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon appears differently depending on environment type and developmental trajectory. Kind-environment specialists see coordination (Rope) — early specialization plus deliberate practice delivered expertise as promised. The early-specialization industrial complex sees coordination (Rope) — funneling talent efficiently through structured pathways. Late bloomers and wicked-environment early specializers see extraction (Snare) — the constraint misclassified them or delivered false promises. Sampling-phase learners and the range advocacy coalition see mixed coordination and extraction (Tangled Rope) — genuine pathways exist but are contaminated by premature commitment pressure. The developmental psychology research community sees a temporary problem with a sunset (Scaffold) — better empirical understanding will enable domain-specific and individual-difference-sensitive timing prescriptions. The analytical observer sees the full hybrid structure (Tangled Rope) — real coordination function contaminated by environment-blind application and institutional capture. The perspectival gap reveals that 'optimal timing' is not a single number but a function of environment kindness, individual learning trajectory, and institutional flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Late bloomers are victims with identity_locked exit — the constraint has fused their identity with a failure narrative, making exit psychologically impossible even when structurally feasible. Wicked-environment early specializers are victims with trapped exit — sunk costs and credential lock-in create material barriers. Sampling-phase learners are victims with constrained exit — institutional timelines and social pressure create high but surmountable costs. The early-specialization industrial complex is a beneficiary with arbitrage exit — can pivot to different age cohorts or domains as markets shift. Kind-environment specialists are beneficiaries with mobile exit — can leverage expertise across related domains. The range advocacy coalition is organized with constrained exit — has collective agency but faces institutional inertia. The developmental psychology research community is institutional with mobile exit — building knowledge that will eventually reform the constraint. The analytical observer uses analytical exit to see the full structure. Each agent's directionality value is derived from their structural position — power level, exit options, and relationship to the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that specialization timing is neither pure coordination nor pure extraction — it is a tangled rope whose classification depends on the match between timing, environment type, and individual trajectory. For kind-environment early specializers, the constraint is coordination (structured pathways that deliver expertise). For wicked-environment early specializers and late bloomers, the constraint is extraction (misclassification and wasted investment). The coordination function is real: deliberate practice does produce expertise in domains with immediate accurate feedback, and structured training pathways do reduce search costs. The extraction is also real: the one-size-fits-all timing prescription ignores environment kindness, the institutional timeline forces premature commitment, and the failure narrative suppresses late-blooming talent. The mandatrophy is resolved by recognizing that the constraint's type is indexical — it depends on which agent you measure from and what environment type they're in. The analytical classification (Tangled Rope) captures the hybrid structure: genuine coordination contaminated by asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    environment_kindness_diagnostic,
    'Can environment kindness be reliably diagnosed prospectively, or only retrospectively after observing whether early specialization succeeded?',
    'Longitudinal studies tracking specialization timing decisions and outcomes across domains; development of validated environment kindness assessment instruments; comparison of prospective predictions vs retrospective classifications',
    'If prospectively diagnosable: the constraint becomes pure coordination (prescribe early specialization for kind environments, delayed for wicked). If only retrospective: the constraint remains extractive because learners cannot know which path to take until after the critical window has closed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environment_kindness_diagnostic, empirical, 'Whether environment kindness can be diagnosed before specialization commitment').

omega_variable(
    critical_period_existence,
    'Do genuine neuroplastic critical periods exist for most domains, or is the ''window of opportunity'' narrative itself a constructed constraint?',
    'Neuroscience research on domain-specific plasticity timelines; case studies of late-entry experts achieving elite performance; cross-cultural comparison of age-at-specialization norms and performance distributions',
    'If critical periods are real and domain-general: early specialization is coordination (aligning with biological constraints). If critical periods are domain-specific or non-existent: early specialization is extraction (artificial scarcity of opportunity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_period_existence, empirical, 'Whether neuroplastic critical periods constrain specialization timing').

omega_variable(
    transfer_benefit_quantification,
    'What is the magnitude of transfer benefit from sampling multiple domains before specialization, and does it offset the deliberate practice deficit?',
    'Controlled studies comparing matched cohorts with different sampling-to-specialization ratios; measurement of analogical reasoning, creative problem-solving, and domain adaptation in specialists vs generalists; career longevity and pivot success rates',
    'If transfer benefits are large: late specialization is coordination (building cognitive flexibility). If transfer benefits are negligible: late specialization is costly delay and early specialization is coordination (maximizing domain-specific skill).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_benefit_quantification, empirical, 'Whether sampling-phase exploration provides transferable cognitive benefits').

omega_variable(
    institutional_timeline_necessity,
    'Are institutional commitment timelines (college major declaration, career track selection) structurally necessary, or could they be redesigned to accommodate later specialization without efficiency loss?',
    'Natural experiments from institutions with flexible vs rigid specialization timelines; economic analysis of coordination costs for delayed commitment; comparison of educational systems with different specialization norms (US liberal arts vs European early tracking)',
    'If timelines are structurally necessary: the constraint is coordination with unavoidable extraction from late bloomers. If timelines are contingent: the constraint is extractive institutional design that could be reformed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_timeline_necessity, conceptual, 'Whether institutional specialization timelines are structurally necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(specialization_timing_optimization, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spec_timing_theater_1970, specialization_timing_optimization, theater_ratio, 0, 0.45).
narrative_ontology:measurement(spec_timing_theater_1985, specialization_timing_optimization, theater_ratio, 15, 0.55).
narrative_ontology:measurement(spec_timing_theater_2000, specialization_timing_optimization, theater_ratio, 30, 0.62).
narrative_ontology:measurement(spec_timing_theater_2015, specialization_timing_optimization, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(spec_timing_extract_1970, specialization_timing_optimization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spec_timing_extract_1985, specialization_timing_optimization, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(spec_timing_extract_2000, specialization_timing_optimization, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(spec_timing_extract_2015, specialization_timing_optimization, base_extractiveness, 45, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(specialization_timing_optimization, identity_coordination).
narrative_ontology:boltzmann_floor_override(specialization_timing_optimization, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is downstream of environment_kindness_spectrum (mountain) — the immutable structural difference between kind environments (immediate accurate feedback) and wicked environments (delayed misleading feedback) determines whether early specialization produces expertise or false confidence. The specialization timing constraint has its own extractiveness reflecting the institutional and social dynamics that enforce timing prescriptions without measuring environment type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
