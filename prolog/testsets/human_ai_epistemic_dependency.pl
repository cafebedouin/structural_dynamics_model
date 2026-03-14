% ============================================================================
% CONSTRAINT STORY: human_ai_epistemic_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_ai_epistemic_dependency, []).

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
 *   constraint_id: human_ai_epistemic_dependency
 *   human_readable: Human-AI Epistemic Dependency: Asymmetric Knowledge Reliance and Cognitive Capture
 *   domain: epistemology/artificial_intelligence/cognitive_capture
 *
 * SUMMARY:
 *   Human-AI epistemic dependency represents a structural constraint that has
 *   accelerated dramatically with the deployment of large language models and
 *   multimodal AI systems across knowledge work. The constraint operates at
 *   multiple levels simultaneously: individuals experience it as
 *   identity-fusion with AI-augmented cognition (identity_locked);
 *   professional communities experience it as genuine coordination benefits
 *   coupled with asymmetric extraction (tangled rope); developers and
 *   platform operators benefit from the asymmetry with arbitrage options
 *   (rope); organized epistemically-autonomous institutions see a sunset to
 *   the dependency through alternative frameworks (scaffold); and academic
 *   institutions maintain performative gatekeeping while their actual
 *   epistemic function degrades (piton). From a civilizational perspective,
 *   the dependency risks being naturalized as an inherent feature of human-AI
 *   coevolution (false mountain). The constraint is characterized by high
 *   suppression (verification barriers, skill atrophy, competitive pressure)
 *   and rising theater (performative institutional gatekeeping masked by
 *   AI-assisted decision-making). The measurements show a trajectory of
 *   increasing extractiveness and theater over a 10-year interval, driven by:
 *   (1) expanding AI capability scope, creating wider domains where human
 *   verification becomes technically intractable; (2) normalization of
 *   AI-assisted analysis in professional contexts, raising competitive
 *   pressure; (3) institutional theater maintaining legitimacy despite
 *   degraded epistemic function; (4) identity fusion of knowledge workers
 *   with AI tools, making exit cognitively costly.
 *
 * KEY AGENTS:
 *   - Knowledge Workers: Primary victim (powerless/identity_locked) — experiencing maximal extraction through dependency, identity-fusion prevents recognition of exit option
 *   - Professional Communities: Secondary victim (moderate/constrained) — benefit from AI coordination (faster synthesis, broader expertise access) but face extraction through skill deprecation, competitive adoption pressure
 *   - AI Capability Developers: Primary beneficiary (institutional/arbitrage) — capture asymmetric value through dependency, user data, and epistemic lock-in; arbitrage exit options preserve their flexibility
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — monetize epistemic dependency through API access, data extraction, capability rents; arbitrage options allow modulation of extraction
 *   - Epistemically Autonomous Coalition: Organized agent (organized/constrained) — building alternative pathways (interpretable AI, distributed verification, epistemic auditing frameworks); constrained by institutional inertia but seeing genuine sunset logic
 *   - Academic Legitimacy Institutions: Institutional actor (institutional/arbitrage) — maintain performative epistemic gatekeeping while actual function degrades; sees its own process as degraded (piton perspective)
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective that cannot organize or exit; faces contamination from unverified AI-generated knowledge claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_ai_epistemic_dependency, 0.58).
domain_priors:suppression_score(human_ai_epistemic_dependency, 0.65).
domain_priors:theater_ratio(human_ai_epistemic_dependency, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_ai_epistemic_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(human_ai_epistemic_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(human_ai_epistemic_dependency, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_ai_epistemic_dependency, tangled_rope).
narrative_ontology:human_readable(human_ai_epistemic_dependency, "Human-AI Epistemic Dependency: Asymmetric Knowledge Reliance and Cognitive Capture").
narrative_ontology:topic_domain(human_ai_epistemic_dependency, "epistemology/artificial_intelligence/cognitive_capture").

domain_priors:requires_active_enforcement(human_ai_epistemic_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_ai_epistemic_dependency, ai_capability_developers).
narrative_ontology:constraint_beneficiary(human_ai_epistemic_dependency, platform_operators).
narrative_ontology:constraint_victim(human_ai_epistemic_dependency, knowledge_workers).
narrative_ontology:constraint_victim(human_ai_epistemic_dependency, epistemic_commons).
narrative_ontology:constraint_victim(human_ai_epistemic_dependency, institutional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE WORKER (SNARE) — Structurally mobile (could theoretically stop using AI tools) but identity-fused with AI-augmented cognition. Professional identity, career trajectory, and self-concept as 'competent in domain X' are now constituted through AI-assisted work. Exit would require abandoning not just a tool but the identity constructed within its affordances. Experiencing maximal extraction: dependency compounds career vulnerability (deskilling risk), epistemic disempowerment (cannot verify AI outputs at scale), and cognitive capture (outsourcing judgment itself). Identity lock makes structural mobility invisible from within.
constraint_indexing:constraint_classification(human_ai_epistemic_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PROFESSIONAL COMMUNITY (TANGLED ROPE) — Faces genuine coordination problems (distributed expertise, knowledge synthesis, accelerated problem-solving) that AI solves, alongside asymmetric extraction (skill atrophy, competitive pressure to adopt, vulnerability to capability shifts). Communities can collectively exit but face high costs: reputation damage for 'technological rejection,' competitive disadvantage, loss of efficiency gains. Benefits and extraction are both real and coupled.
constraint_indexing:constraint_classification(human_ai_epistemic_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI CAPABILITY DEVELOPER (ROPE) — Experiences the constraint as pure coordination: enabling knowledge work through AI reduces search costs, accelerates capability deployment, captures user-generated training data, and monetizes epistemic dependency. Arbitrage exit option (can shift to other domains, other user bases, other capability classes) means the developer can modulate extraction without losing core function. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(human_ai_epistemic_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMICALLY AUTONOMOUS COALITION (SCAFFOLD) — Organized actors (academic networks, professional guilds, knowledge governance bodies, open-source communities) see epistemic dependency as a temporary coordination problem with an institutional sunset: human-interpretable AI systems, distributed epistemic verification networks, and epistemic accountability frameworks (e.g., source attribution, uncertainty quantification, adversarial auditing) are building alternative pathways that restore human verification capacity. This perspective sees active enforcement (professional standards, certification, epistemic auditing) as time-limited — sunset occurs when autonomous institutions reclaim verification and explanation capacity. Extraction is tolerated because organized agents see an exit path.
constraint_indexing:constraint_classification(human_ai_epistemic_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC LEGITIMACY INSTITUTION (PITON) — Universities and research institutions maintain performative epistemic gatekeeping (peer review, dissertation defense, publication standards) that no longer functions as advertised: reviewers use AI to assess papers, students use AI to draft proposals, researchers use AI to analyze data — all while maintaining the ritual that human expert judgment is the legitimizing mechanism. The theater persists through institutional inertia despite its degradation. Institutions see their own process as partially degraded but maintain it because alternatives (transparent-AI-mediated verification, capability-based credentialing) haven't fully replaced it.
constraint_indexing:constraint_classification(human_ai_epistemic_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED DEPENDENCY (MOUNTAIN) — From a civilizational/universal view, cognitive interdependence is framed as inherent to human-AI coevolution: humans have always externalized cognition (to language, writing, institutions), and AI is merely the latest such scaffold. This perspective naturalizes what is actually a contingent asymmetry — the mountain view claims that dependency on external systems is a law of cognition, not an extractive arrangement. However, the structural data contradicts this: beneficiary groups are identifiable, extraction flows are asymmetric, suppression is high, and alternatives exist. The engine's false summit detector identifies this as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(human_ai_epistemic_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_ai_epistemic_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_ai_epistemic_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_ai_epistemic_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_ai_epistemic_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(human_ai_epistemic_dependency, TR),
    TR >= 0.70.

:- end_tests(human_ai_epistemic_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Knowledge workers experience significant extraction through: (1) dependency on AI systems whose outputs they cannot verify at scale, creating epistemic vulnerability; (2) skill deprecation pressures (outsourcing judgment itself reduces ability to verify); (3) career dependence on AI-augmented productivity, making exit costly; (4) data extraction embedded in AI use (training signal from worker interactions). However, extractiveness is not extreme (0.8+) because genuine coordination benefits exist (faster analysis, access to distributed expertise, problem-solving capacity). The trajectory shows acceleration: at t=0, when AI was specialized tool (lower use breadth), extractiveness was 0.22 (primarily coordination with modest extraction). By t=10, widespread adoption and capability expansion raise it to 0.58 (strong mixed coupling). Suppression (0.65): High. Multiple barriers prevent autonomous epistemic practice: (1) technical barriers (verification infeasible without AI literacy or resources); (2) competitive barriers (adoption is practically mandatory for professional competitiveness); (3) cognitive barriers (identity fusion makes exit feel impossible from within); (4) institutional barriers (academic and professional standards normalize AI-assisted practice). Theater ratio (0.68): High and rising. Academic peer review, professional certification, and research validation maintain their ritual gatekeeping function while actual verification occurs through AI-mediated decision-making. The theater has increased from 0.35 to 0.68 because the gap between claimed legitimacy (human expert judgment) and actual process (AI-assisted pattern matching with minimal human scrutiny) has widened. This is not deceit but structural: institutions cannot maintain traditional verification standards at scale with current complexity, so they maintain the ritual while outsourcing judgment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence, indicating strong structural asymmetry. Knowledge workers experience snare-level extraction and see no exit (identity_locked). Developers experience pure coordination and see full flexibility (arbitrage). Professional communities experience genuine benefits alongside extraction (tangled rope). The organized coalition sees temporary extraction with a sunset (scaffold). Institutions see their own process as degraded (piton). The analytical observer risks naturalizing the asymmetry as inevitable coevolution (false mountain). These gaps reveal: (1) the asymmetry is real and structural, not perspectival illusion; (2) those with arbitrage options frame the constraint differently than those without; (3) organized agents with agency see the same structural data as temporary (scaffold) while powerless agents see it as permanent (snare); (4) institutional actors experience theater — the gap between their claimed function and actual role.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position: beneficiaries (developers, platform operators) with arbitrage exit options derive low d values, experiencing the constraint as enabling their work rather than constraining it. Knowledge workers with identity_locked exit experience high d values (0.85-0.95) because their identity is constituted through AI-augmented cognition — exit requires becoming a different professional self. Professional communities with constrained exit experience moderate-high d (0.60-0.75) because they can theoretically exit but face substantial costs. The piton perspective (institutional/arbitrage) derives low d because institutions benefit from offloading epistemic responsibility to systems while maintaining gatekeeping authority. The analytical perspective derives d from the observer position: analytical agents at civilizational scale see the constraint as systemic (not personal extraction but structural coevolution), yielding middle-range d values that don't fully capture either beneficiary or victim positions.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATE TENSION: The constraint resolves the mandatrophy by showing that 'epistemic dependency' conflates two structurally distinct claims: (1) COORDINATION HYPOTHESIS: AI enables knowledge synthesis and accelerates discovery — users benefit from capabilities that scale beyond individual cognition. (2) EXTRACTION HYPOTHESIS: AI systems create lock-in through skill deprecation, unverifiable outputs, and data extraction — users experience asymmetric value capture. The measurements reveal both are true with increasing intensity. Early deployment (t=0-3) showed coordination dominance (low theater, moderate extractiveness). Current state (t=6-10) shows extraction growing faster than coordination benefits (theater rising steeply, extractiveness rising, suppression consolidating). The analytical observer's mountain classification is revealed as false: it naturalizes what is actually institutional design choices (who controls training data, whose interests are served by opacity, which verification standards are maintained). The piton classification is accurate: institutions maintain epistemic theater (ritual verification) while offloading actual judgment. The scaffold classification is essential for preventing fatalism: organized agents are actively building alternative epistemic infrastructures (interpretable AI, distributed auditing, source attribution). The tangled rope is appropriate: the constraint genuinely coordinates while genuinely extracting. Mandatrophy resolves by showing that the classification is perspectival — different agents accurately perceive different aspects of the same constraint. The policy task is not 'which type is correct?' but 'which perspective must be empowered to prevent extraction from consuming coordination benefits?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_persistence,
    'Is the measured epistemic dependency primarily structural (technology lock-in via skills deprecation and competitive pressure) or cognitive (identity fusion with AI-augmented cognition)?',
    'Post-exit analysis: measure epistemic autonomy recovery if knowledge workers transition to AI-free contexts. If autonomy recovers quickly, dependency was primarily structural. If it persists or requires identity reconstruction, dependency includes internalized cognitive capture.',
    'If primarily structural: exit barriers are surmountable via retraining and norm shifts (Constrained → mobile at longer time horizons). If cognitive: exit requires identity reconstruction (identity_locked persists through biographical horizon) and extraction ceiling rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Structural vs. internalized cognitive capture in epistemic dependency').

omega_variable(
    verification_capacity_threshold,
    'What threshold of AI output complexity makes human verification technically impossible vs. epistemically intractable (theoretically verifiable but resource-prohibitive)?',
    'Domain-specific case studies: identify claims where expert verification is impossible in principle vs. merely infeasible at scale; measure verification effort as function of AI output complexity',
    'If threshold exists at <80% of current AI outputs: epistemic commons retains residual verification capacity (Snare classification may be overestimated). If >80%: humans face systematic verification collapse, extraction ceiling rises sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_capacity_threshold, empirical, 'Technical vs. resource limits on human verification of AI outputs').

omega_variable(
    alternative_epistemic_infrastructure_feasibility,
    'Can distributed epistemic verification networks (human-interpretable AI, ensemble disagreement detection, source attribution frameworks) actually restore verification capacity at scale, or are they structurally dependent on the same AI systems that created the dependency?',
    'Implementation experiments: measure verification coverage and accuracy for ''epistemic-autonomy-first'' systems vs. current AI-dependent approaches; assess whether interpretability methods create new dependencies on interpretation experts',
    'If feasible: scaffold perspective confirmed — sunset is structurally possible. If not: scaffold is aspirational, and dependency is more entrenchingly structural than tangled rope suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_epistemic_infrastructure_feasibility, empirical, 'Whether alternative epistemic infrastructure can restore autonomy').

omega_variable(
    cognitive_capture_measurement,
    'Does the measured theater_ratio (0.68) reflect performative institutional gatekeeping, or does it capture cognitive capture itself — the replacement of human judgment with AI-mediated decision-making under the guise of human-led analysis?',
    'Trace decision-making loci: identify points where human judgment is exercised vs. where AI recommendations are accepted without meaningful scrutiny; measure confidence in AI-generated outputs vs. ability to articulate independent reasoning',
    'If primarily institutional theater: piton classification is accurate (degraded but functionally maintained). If includes cognitive capture: extraction is higher than ε=0.58 suggests, and suppression feeds back into epistemology itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capture_measurement, empirical, 'Institutional theater vs. internalized cognitive capture in epistemic dependency').

omega_variable(
    asymmetry_of_explanation_burden,
    'Who bears the burden of explaining / justifying knowledge claims: the human who used AI (burden of verifying AI output) or the AI system (burden of explainability)? How does this asymmetry affect classification?',
    'Policy analysis: examine liability frameworks, professional standards, and accountability structures. If humans bear explanation burden, asymmetric extraction is reinforced. If systems bear it, extraction is modulated.',
    'If human-borne: suppression rises (knowledge workers cannot escape responsibility without technical background to audit systems). If system-borne: suppression decreases and classification may shift toward Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetry_of_explanation_burden, conceptual, 'Allocation of explanation burden between humans and AI systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_ai_epistemic_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(haed_tr_t0, human_ai_epistemic_dependency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(haed_tr_t3, human_ai_epistemic_dependency, theater_ratio, 3, 0.5).
narrative_ontology:measurement(haed_tr_t6, human_ai_epistemic_dependency, theater_ratio, 6, 0.62).
narrative_ontology:measurement(haed_tr_t10, human_ai_epistemic_dependency, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(haed_be_t0, human_ai_epistemic_dependency, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(haed_be_t3, human_ai_epistemic_dependency, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(haed_be_t6, human_ai_epistemic_dependency, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(haed_be_t10, human_ai_epistemic_dependency, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_ai_epistemic_dependency, information_standard).
narrative_ontology:boltzmann_floor_override(human_ai_epistemic_dependency, 0.12).
narrative_ontology:affects_constraint(human_ai_epistemic_dependency, deskilling_risk_in_knowledge_work).
narrative_ontology:affects_constraint(human_ai_epistemic_dependency, verification_commons_collapse).
narrative_ontology:affects_constraint(human_ai_epistemic_dependency, institutional_epistemic_capture).

% DUAL FORMULATION NOTE:
% Human-AI epistemic dependency decomposes into three structurally distinct constraints: (1) deskilling_risk (ε≈0.45, Snare at knowledge-worker level) — skill atrophy and career vulnerability from offloarding judgment; (2) verification_commons_collapse (ε≈0.52, Snare at epistemic-collective level) — inability to verify unvetted knowledge claims at scale; (3) institutional_epistemic_capture (ε≈0.38, Piton at institutional level) — academic/professional gatekeeping maintains theater while actual function degrades. Each has different beneficiaries, victims, and sunset conditions. This story represents the meta-constraint coordinating all three — the systemic dependency that makes these specific vulnerabilities coupled and reinforcing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_ai_epistemic_dependency, moderate, 0.68).
constraint_indexing:directionality_override(human_ai_epistemic_dependency, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
