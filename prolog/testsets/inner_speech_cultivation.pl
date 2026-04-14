% ============================================================================
% CONSTRAINT STORY: inner_speech_cultivation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inner_speech_cultivation, []).

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
 *   constraint_id: inner_speech_cultivation
 *   human_readable: Inner Speech Cultivation as Metacognitive Discipline
 *   domain: philosophy_of_mind/social_psychology/intellectual_autonomy
 *
 * SUMMARY:
 *   Inner speech cultivation — the active mental discipline required to
 *   prevent solitude from becoming private imprisonment by untested fixations
 *   — operates as a tangled rope constraint. It solves a genuine coordination
 *   problem: without metacognitive monitoring, isolated agents can spiral
 *   into echo-chamber fixation where inherited beliefs feel like examined
 *   principles. The Socratic tradition, contemplative practices, and
 *   therapeutic frameworks all recognize this risk and provide tools for
 *   self-examination. However, the constraint also extracts asymmetrically:
 *   those with access to metacognitive training (education, therapy,
 *   philosophical communities) experience the discipline as beneficial skill
 *   development, while those without access experience it as an impossible
 *   demand that pathologizes their cognitive isolation. The constraint
 *   requires active enforcement through cultural norms about the 'examined
 *   life' and 'critical thinking,' and it has both clear beneficiaries
 *   (philosophical communities, therapeutic frameworks, reflective
 *   practitioners) and clear victims (cognitively isolated individuals,
 *   untrained solitaries, fixation-prone agents). The theater ratio (0.38)
 *   reflects moderate performative content: some agents perform metacognitive
 *   vocabulary without genuine self-examination, and some cultural discourse
 *   about 'critical thinking' substitutes tribal affiliation for actual
 *   principle-testing. The extractiveness has increased over the interval
 *   (0.35 → 0.48) as cultural expectations for metacognitive sophistication
 *   have risen faster than access to training, widening the gap between those
 *   who can meet the demand and those who cannot.
 *
 * KEY AGENTS:
 *   - Cognitively Isolated Individuals: Primary victims (powerless/identity_locked) — lack metacognitive vocabulary to distinguish examined from inherited beliefs; solitude becomes fixation trap
 *   - Untrained Solitaries: Secondary victims (moderate/constrained) — have some exposure to metacognitive practices but face high costs to sustained discipline; mixed experience of benefit and burden
 *   - Fixation-Prone Agents: Secondary victims (moderate/constrained) — struggle with obsessive thought patterns or ideological rigidity; the discipline both protects and exhausts
 *   - Reflective Practitioners: Primary beneficiaries (organized/mobile) — have acquired metacognitive tools through education, therapy, or contemplative practice; experience as skill development
 *   - Philosophical Communities: Primary beneficiaries (institutional/arbitrage) — benefit from inner speech cultivation as coordination mechanism producing reliable interlocutors
 *   - Therapeutic Frameworks: Scaffold providers (institutional/arbitrage) — see cultivation as temporary support for developing autonomous metacognition with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes genuine coordination function and asymmetric extraction; identifies as tangled rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inner_speech_cultivation, 0.48).
domain_priors:suppression_score(inner_speech_cultivation, 0.52).
domain_priors:theater_ratio(inner_speech_cultivation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inner_speech_cultivation, extractiveness, 0.48).
narrative_ontology:constraint_metric(inner_speech_cultivation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(inner_speech_cultivation, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inner_speech_cultivation, tangled_rope).
narrative_ontology:human_readable(inner_speech_cultivation, "Inner Speech Cultivation as Metacognitive Discipline").
narrative_ontology:topic_domain(inner_speech_cultivation, "philosophy_of_mind/social_psychology/intellectual_autonomy").

domain_priors:requires_active_enforcement(inner_speech_cultivation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inner_speech_cultivation, reflective_practitioners).
narrative_ontology:constraint_beneficiary(inner_speech_cultivation, philosophical_communities).
narrative_ontology:constraint_beneficiary(inner_speech_cultivation, therapeutic_frameworks).
narrative_ontology:constraint_victim(inner_speech_cultivation, cognitively_isolated_individuals).
narrative_ontology:constraint_victim(inner_speech_cultivation, untrained_solitaries).
narrative_ontology:constraint_victim(inner_speech_cultivation, fixation_prone_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COGNITIVELY ISOLATED INDIVIDUAL (SNARE) — Identity-locked in untested belief structures with no external reality-testing contacts. The requirement for inner speech discipline appears as an impossible demand when the agent lacks the metacognitive vocabulary to distinguish examined from inherited beliefs. Solitude becomes private imprisonment by fixations that feel like authentic self-discovery. High extraction: the constraint demands work the agent cannot perform without tools they don't have.
constraint_indexing:constraint_classification(inner_speech_cultivation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: UNTRAINED SOLITARY (TANGLED ROPE) — Has some exposure to metacognitive practices (journaling, therapy, philosophical reading) but faces high costs to sustained practice. The constraint provides genuine coordination benefit (prevents echo-chamber fixation) but also extracts significant cognitive labor. Can exit through community engagement or structured practice, but at biographical cost of confronting cherished beliefs. Mixed experience: the discipline both protects and burdens.
constraint_indexing:constraint_classification(inner_speech_cultivation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHILOSOPHICAL COMMUNITY (ROPE) — Benefits from inner speech cultivation as a coordination mechanism that produces reliable interlocutors. The Socratic tradition, contemplative practices, and therapeutic frameworks all depend on agents who can examine their own reasoning. Low extraction: the community experiences this as a beneficial standard that enables productive discourse. Arbitrage exit: can choose which metacognitive traditions to adopt.
constraint_indexing:constraint_classification(inner_speech_cultivation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFLECTIVE PRACTITIONER (ROPE) — Has acquired metacognitive tools through education, therapy, or contemplative practice. Experiences inner speech cultivation as a coordination benefit: the discipline prevents fixation and enables principle-testing. Mobile exit: can choose among different metacognitive frameworks (Stoic self-examination, Buddhist mindfulness, psychoanalytic free association, philosophical dialectic). Low extraction: the work feels like skill development rather than imposed burden.
constraint_indexing:constraint_classification(inner_speech_cultivation, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FIXATION-PRONE AGENT (TANGLED ROPE) — Recognizes the need for metacognitive discipline but struggles with obsessive thought patterns, rumination, or ideological rigidity. The constraint provides genuine benefit (prevents runaway fixation) but also extracts high cognitive cost. Constrained exit: can access therapeutic or philosophical resources, but the work of distinguishing examined from inherited beliefs is effortful and often painful. The discipline both protects and exhausts.
constraint_indexing:constraint_classification(inner_speech_cultivation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: THERAPEUTIC FRAMEWORK (SCAFFOLD) — Sees inner speech cultivation as a temporary support structure for developing autonomous metacognition. Psychotherapy, philosophical counseling, and contemplative training all aim to internalize the discipline so that external scaffolding becomes unnecessary. Sunset logic: successful cultivation makes the explicit practice obsolete as metacognitive monitoring becomes automatic. Low extraction with declining trajectory as the agent internalizes the skill.
constraint_indexing:constraint_classification(inner_speech_cultivation, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes that inner speech cultivation solves a genuine coordination problem (preventing solitude from becoming echo-chamber fixation) but also imposes asymmetric costs. Those with access to metacognitive training (education, therapy, philosophical communities) experience the constraint as beneficial skill development. Those without access experience it as an impossible demand that pathologizes their cognitive isolation. The constraint is structurally a tangled rope: real coordination function, real extraction, active enforcement through cultural norms about 'examined life' and 'critical thinking.'
constraint_indexing:constraint_classification(inner_speech_cultivation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inner_speech_cultivation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inner_speech_cultivation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inner_speech_cultivation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inner_speech_cultivation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(inner_speech_cultivation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint demands significant cognitive labor to distinguish examined from inherited beliefs, monitor metacognitive processes, and test principles against exceptions. For agents with metacognitive training, this work is manageable and beneficial. For agents without training, the demand is extractive — they lack the tools to comply but face cultural judgment for failing to live an 'examined life.' The value reflects that roughly half the cognitive cost is coordination overhead (genuine skill development) and half is extractive burden (impossible demand on untrained agents). Suppression (0.52): Moderate. Significant barriers include lack of access to metacognitive training (education, therapy, philosophical communities), cultural pathologization of solitude, and the cognitive difficulty of self-examination for fixation-prone agents. However, suppression is not total — multiple traditions (Stoic, Buddhist, psychoanalytic, philosophical) offer accessible entry points, and some agents develop metacognitive skills through informal means (journaling, reading, conversation). Theater ratio (0.38): Moderate. Some performative content exists: agents who use metacognitive vocabulary without genuine self-examination, cultural discourse that substitutes tribal affiliation for principle-testing, and therapeutic frameworks that ritualize self-examination without producing autonomy. However, the core practice (metacognitive monitoring, belief examination, principle-testing) has substantial functional content — it genuinely prevents fixation for trained agents.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a classic tangled rope perspectival structure. Beneficiaries with metacognitive training see coordination (rope) — the discipline enables productive discourse and prevents fixation. Therapeutic frameworks see temporary support with sunset logic (scaffold) — the goal is to internalize the skill. Victims without training see extraction (snare from identity_locked perspective, tangled_rope from constrained perspective) — the demand is impossible to meet without tools they don't have. The analytical observer sees the full structure: genuine coordination function, genuine extraction, active enforcement through cultural norms. The gap between the reflective practitioner's rope and the cognitively isolated individual's snare is the diagnostic signal — the same structural phenomenon appears as beneficial skill development or impossible burden depending on access to training.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reflects asymmetric access to metacognitive training. Beneficiaries (philosophical communities, therapeutic frameworks, reflective practitioners) experience low extraction because they have the tools to comply with the discipline and gain coordination benefits. Victims (cognitively isolated individuals, untrained solitaries, fixation-prone agents) experience high extraction because they lack the tools to comply but face cultural expectations. The identity_locked exit option for cognitively isolated individuals reflects that their fixations feel like authentic self-discovery — they cannot see the untested nature of their beliefs from within their cognitive isolation. The scaffold perspective from therapeutic frameworks reflects genuine sunset logic: successful cultivation internalizes the discipline, making explicit practice unnecessary. The analytical observer's tangled_rope classification reflects recognition of both the genuine coordination function (preventing echo-chamber fixation) and the asymmetric extraction (impossible demand on untrained agents).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that inner speech cultivation is neither pure coordination (rope) nor pure extraction (snare) but a hybrid (tangled_rope). The coordination function is real: metacognitive monitoring prevents solitude from becoming echo-chamber fixation, and multiple philosophical and therapeutic traditions converge on this practice. The extraction is also real: the discipline demands cognitive labor that is manageable for trained agents but impossible for untrained agents, and cultural norms pathologize failure to comply. The tangled_rope classification prevents two errors: (1) naturalizing the constraint as pure skill development (rope) ignores the asymmetric costs on untrained agents, and (2) dismissing the constraint as pure cultural imposition (snare) ignores the genuine coordination benefit. The analytical perspective identifies the constraint as tangled_rope, matching the claimed_type, which confirms that the classification is structurally accurate rather than perspectival artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metacognitive_floor_threshold,
    'What minimum level of metacognitive capacity is required before inner speech cultivation becomes beneficial rather than extractive?',
    'Longitudinal studies tracking metacognitive skill development; identification of threshold below which self-examination increases rather than decreases fixation (rumination, obsessive thought patterns)',
    'If threshold is high: many agents experience the constraint as pure extraction (snare) because they lack prerequisite skills. If threshold is low: most agents can benefit from cultivation with minimal training.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metacognitive_floor_threshold, empirical, 'Minimum metacognitive capacity for beneficial cultivation').

omega_variable(
    solitude_fixation_causality,
    'Does solitude cause fixation, or does pre-existing fixation-proneness drive solitude-seeking?',
    'Controlled studies comparing fixation development in randomly-assigned solitary vs social conditions; personality trait analysis of solitude-seekers vs solitude-avoiders',
    'If solitude causes fixation: inner speech cultivation is a necessary protective mechanism (rope/scaffold from more perspectives). If fixation drives solitude: the constraint misidentifies the causal arrow and may pathologize healthy solitude-seeking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(solitude_fixation_causality, empirical, 'Causal direction between solitude and fixation').

omega_variable(
    examined_belief_stability,
    'Do examined beliefs actually exhibit greater stability under exception-testing than inherited beliefs, or is the distinction itself a philosophical fiction?',
    'Experimental philosophy studies measuring belief revision rates under counterevidence; comparison of self-reported ''examined'' vs ''inherited'' beliefs in longitudinal tracking',
    'If examined beliefs are more stable: the coordination benefit is real and measurable. If no difference: the entire framework may be performative (theater ratio higher than measured).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(examined_belief_stability, empirical, 'Whether examined beliefs show greater stability than inherited beliefs').

omega_variable(
    internalization_timeline,
    'How long does it take for metacognitive monitoring to become automatic (internalized) rather than effortful?',
    'Skill acquisition studies in contemplative practice, psychotherapy, and philosophical training; identification of practice duration thresholds for automaticity',
    'If timeline is short (months): scaffold perspective is accurate for most agents. If timeline is long (years/decades): many agents experience sustained extraction before reaching coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_timeline, empirical, 'Timeline for metacognitive skill internalization').

omega_variable(
    cultural_framing_extraction,
    'How much of the measured extraction derives from the cultural framing of ''examined life'' as moral imperative rather than from the metacognitive practice itself?',
    'Cross-cultural comparison of metacognitive practices with different moral valences; separation of practice effects from framing effects in experimental studies',
    'If framing drives extraction: the constraint is more extractive than necessary (could be reframed as skill rather than duty). If practice drives extraction: the cognitive cost is inherent to the discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_framing_extraction, conceptual, 'Extraction from moral framing vs practice itself').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inner_speech_cultivation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inner_speech_tr_t0, inner_speech_cultivation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(inner_speech_tr_t3, inner_speech_cultivation, theater_ratio, 3, 0.3).
narrative_ontology:measurement(inner_speech_tr_t6, inner_speech_cultivation, theater_ratio, 6, 0.35).
narrative_ontology:measurement(inner_speech_tr_t10, inner_speech_cultivation, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(inner_speech_be_t0, inner_speech_cultivation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inner_speech_be_t3, inner_speech_cultivation, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(inner_speech_be_t6, inner_speech_cultivation, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(inner_speech_be_t10, inner_speech_cultivation, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inner_speech_cultivation, identity_coordination).

% DUAL FORMULATION NOTE:
% Inner speech cultivation is downstream of both social_conformity_infrastructure (which creates the baseline cognitive environment from which solitude deviates) and autonomy_as_refusal_work (which frames the metacognitive discipline as a form of intellectual autonomy). The constraint is distinct from its upstream dependencies: social conformity provides the inherited beliefs that inner speech must examine, and autonomy-as-refusal provides the framing that makes the discipline legible as self-governance rather than self-policing. Inner speech cultivation has its own extractiveness (0.48) reflecting the cognitive cost of metacognitive monitoring, separate from the extractiveness of conformity infrastructure or refusal work.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
