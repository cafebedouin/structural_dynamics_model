% ============================================================================
% CONSTRAINT STORY: dunning_kruger_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dunning_kruger_effect, []).

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
 *   constraint_id: dunning_kruger_effect
 *   human_readable: Dunning-Kruger Effect (Cognitive Bias of Self-Assessment)
 *   domain: social/cognitive
 *
 * SUMMARY:
 *   The Dunning-Kruger effect describes a structural tension in knowledge
 *   production and organizational selection where people with limited
 *   competence systematically overestimate their abilities. This constraint
 *   operates as a Tangled Rope: it contains both a genuine coordination
 *   function (confidence is a necessary signal for learning and risk-taking)
 *   and an extraction mechanism (institutions exploit the
 *   confidence-competence gap to select incompetent leaders, contaminating
 *   epistemic and organizational systems). The effect is more extractive when
 *   organizations treat confidence as a reliable signal of competence and
 *   build selection mechanisms around self-assessment. The constraint
 *   exhibits different classifications from different perspectives: the
 *   epistemic commons sees pure extraction (Snare), competent underconfident
 *   agents see extraction (Snare), overconfident actors experience short-term
 *   coordination benefits masking long-term competence costs (Tangled Rope),
 *   gatekeeping institutions benefit from the asymmetry (Rope), competence
 *   assessment movements are building exits (Scaffold), and analytical
 *   observers debate whether the effect is real or performatively invoked
 *   (Piton). Theater ratio has increased over time as the concept has been
 *   popularized in popular psychology and organizational management, with
 *   many invocations becoming ritualistic rather than analytically rigorous.
 *
 * KEY AGENTS:
 *   - Overconfident Low-Skill Actors: Primary beneficiary in short term (powerful/arbitrage) — receive disproportionate opportunities, authority, and attention due to confidence signaling
 *   - Epistemic Commons: Primary victim (powerless/trapped) — collective knowledge production contaminated by confident false claims; no exit mechanism
 *   - Competent Underconfident Agents: Secondary victim (moderate/constrained) — career advancement blocked by reluctance to claim expertise; trapped between self-knowledge and competitive signaling requirements
 *   - Organizations/Institutions: Secondary beneficiary (institutional/arbitrage) — exploit confidence-competence gap for selection; benefit from asymmetry until incompetence materializes
 *   - Competence Assessment Movement: Organized agent (organized/mobile) — building objective assessment tools and feedback mechanisms that reduce reliance on self-assessment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — debates robustness of effect and whether concept has become performatively invoked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dunning_kruger_effect, 0.38).
domain_priors:suppression_score(dunning_kruger_effect, 0.48).
domain_priors:theater_ratio(dunning_kruger_effect, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dunning_kruger_effect, extractiveness, 0.38).
narrative_ontology:constraint_metric(dunning_kruger_effect, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dunning_kruger_effect, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dunning_kruger_effect, tangled_rope).
narrative_ontology:human_readable(dunning_kruger_effect, "Dunning-Kruger Effect (Cognitive Bias of Self-Assessment)").
narrative_ontology:topic_domain(dunning_kruger_effect, "social/cognitive").

domain_priors:requires_active_enforcement(dunning_kruger_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dunning_kruger_effect, high_confidence_low_skill_actors).
narrative_ontology:constraint_beneficiary(dunning_kruger_effect, institutional_gatekeepers_exploiting_asymmetry).
narrative_ontology:constraint_victim(dunning_kruger_effect, epistemic_commons).
narrative_ontology:constraint_victim(dunning_kruger_effect, low_confidence_competent_agents).
narrative_ontology:constraint_victim(dunning_kruger_effect, organizations_suffering_from_skill_mismatch).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The collective reliability of knowledge production is contaminated by confident incompetents whose overestimation spreads false claims. No exit mechanism; cannot organize. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(dunning_kruger_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETENT UNDERCONFIDENT AGENT (SNARE) — Trapped by self-doubt in competitive environments where confidence signals expertise. Career advancement blocked by reluctance to claim expertise they actually possess. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(dunning_kruger_effect, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OVERCONFIDENT LOW-SKILL ACTOR (TANGLED ROPE) — Benefits from illusory superiority in the short term (receives opportunities, authority, attention). Coordination function: their confidence attracts followers and creates coalition. But extraction occurs when incompetence materializes. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.14. Net slight beneficiary due to early-stage arbitrage before reality collapse.
constraint_indexing:constraint_classification(dunning_kruger_effect, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL GATEKEEPER (ROPE) — Organizations benefit from Dunning-Kruger effect through selection mechanisms: confident actors self-select into leadership positions they're not qualified for, but their confidence creates the appearance of capability. The constraint operates as coordination mechanism for gatekeepers to extract effort from organizations. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary; sees the effect as useful selection heuristic.
constraint_indexing:constraint_classification(dunning_kruger_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPETENCE ASSESSMENT MOVEMENT (SCAFFOLD) — Structured skill assessments, 360-degree feedback, and validated competence tests are building alternative pathways that reduce reliance on self-assessment. These mechanisms have sunset logic: as objective assessment tools mature and become institutionalized, the Dunning-Kruger bias loses power. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.25.
constraint_indexing:constraint_classification(dunning_kruger_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — The Dunning-Kruger effect is often invoked as an explanation for failures in knowledge production and organizational dysfunction, but the narrative has become ritualistic and oversimplified. The effect is frequently cited without rigorous validation; post-hoc explanations attribute failures to 'they didn't know what they didn't know' rather than examining structural incentives. theater_ratio=0.65 reflects performative invocation. Researchers debate whether the effect is robust or a statistical artifact. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.57.
constraint_indexing:constraint_classification(dunning_kruger_effect, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dunning_kruger_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dunning_kruger_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dunning_kruger_effect, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(dunning_kruger_effect, TR),
    TR >= 0.70.

:- end_tests(dunning_kruger_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Dunning-Kruger effect creates asymmetric knowledge about competence, enabling extraction through misallocation of opportunities and authority. However, the extraction is not maximal because: (1) the effect is temporary — reality eventually reveals incompetence, (2) many organizations are implementing objective assessment tools, (3) feedback mechanisms in high-stakes domains (academic peer review, engineering safety) reduce the effect's power. Theater ratio (0.65): Moderate-high. The concept has become ritualized in organizational literature and pop psychology. Many invocations are post-hoc explanations rather than precise empirical descriptions. Researchers debate whether the original effect (Kruger & Dunning 1999) robustly replicates or is a statistical artifact. The invocation has exceeded the empirical grounding, creating performative theater. Suppression (0.48): Moderate. The constraint suppresses alternatives through: self-doubt creating exit barriers, institutional reliance on confidence signals, organizational cultures that reward assertiveness over accuracy. But suppression is incomplete — competence assessment tools and objective feedback mechanisms are reducing the effect's hold.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap centered on the temporal dimension. Overconfident low-skill actors experience immediate benefits and short-term success (arbitrage), making the constraint appear as positive coordination. Competent underconfident agents experience immediate extraction and suppressed opportunity. The epistemic commons experiences accumulating damage (false claims in literature). Organizations experience a J-curve: short-term apparent benefit (confident selection) followed by long-term competence crisis. The Competence Assessment Movement experiences the constraint as a temporary problem with structural solutions. The analytical observer sees performative ritualization of a concept whose empirical status is contested. The perspectival gaps reveal that the constraint operates on different timescales for different agents: immediate beneficiary for the overconfident, immediate victim for the competent underconfident, medium-term victim for organizations, long-term victim for epistemic reliability.
 *
 * DIRECTIONALITY LOGIC:
 *   Overconfident low-skill actors: Beneficiary + arbitrage → d≈0.35, f(d)≈0.30. Moderate beneficiary due to early-stage confidence arbitrage before competence failure. Epistemic commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Competent underconfident agents: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction due to suppressed opportunity and self-doubt barriers. Institutional gatekeepers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; extract through selection asymmetries. Competence assessment movement: Mobile + organized → d≈0.50, f(d)≈0.65. Symmetric; both costs and benefits as they build alternative pathways. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Piton classification from theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The Dunning-Kruger constraint resolves mandatrophy by distinguishing between the coordination function (confidence enables risk-taking and learning in uncertain environments) and the extraction mechanism (institutions exploit confidence signals to make incompetent selections). The Tangled Rope classification captures this hybrid: genuine coordination benefit (confidence creates positive feedback for learning) coupled with asymmetric extraction (institutions and overconfident actors benefit at the expense of competent underconfident agents and epistemic reliability). The scaffold perspective (competence assessment movement) is not aspirational but already structurally present — 360-degree feedback, skill certifications, and objective assessments have sunset logic for the traditional confidence-based selection. The piton perspective reveals that the concept's explanatory power has exceeded its empirical grounding in recent years, becoming a convenient post-hoc narrative rather than a precise diagnostic tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dk_effect_robustness,
    'Is the Dunning-Kruger effect a robust, replicable cognitive bias or a statistical artifact of confidence distributions in low-skill populations?',
    'Meta-analysis of replication studies; correlation between confidence and actual performance across skill levels; examination of whether effect persists with alternative measurement methods',
    'If robust: constraint classification stands (Tangled Rope with moderate extraction). If artifact: the constraint is primarily institutional (organizations treating it as real even if not empirically grounded), shifting classification toward Piton (performative citation) or Snare (institutions using false belief to justify incompetent selection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dk_effect_robustness, empirical, 'Whether Dunning-Kruger effect is a robust cognitive bias or measurement artifact').

omega_variable(
    confidence_actual_competence_decoupling,
    'In high-uncertainty domains (politics, business strategy, research frontiers), how much of the confidence-competence decoupling is inherent cognitive bias vs rational response to genuinely unpredictable environments?',
    'Comparison of confidence-competence gap in domains with clear feedback (chess, mathematics) vs unclear feedback (strategic decision-making, novel research); analysis of whether overconfidence is adaptive in uncertain domains',
    'If cognitive bias dominant: Dunning-Kruger is a constraint on rational self-assessment. If rational response to uncertainty: the ''bias'' is partly a feature of navigating inherent unpredictability, and institutional solutions should focus on feedback mechanisms, not individual recalibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confidence_actual_competence_decoupling, conceptual, 'Whether confidence-competence gap is bias or rational response to uncertainty').

omega_variable(
    selection_mechanism_feedback_loop,
    'Does institutional reliance on confidence signals as a proxy for competence create a positive feedback loop that amplifies the Dunning-Kruger effect at the organizational level?',
    'Historical analysis of selection outcomes in organizations with high vs low reliance on self-assessment; tracking of promotion rates for overconfident vs appropriately confident actors; measurement of organizational performance variance with selection method',
    'If feedback loop confirmed: the constraint is Tangled Rope with institutional enforcement (organizations systematically selecting overconfident actors into positions of power). If no loop: Dunning-Kruger is an individual-level phenomenon with limited organizational leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_mechanism_feedback_loop, empirical, 'Whether institutional selection mechanisms amplify Dunning-Kruger effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dunning_kruger_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dk_tr_t0, dunning_kruger_effect, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dk_tr_t5, dunning_kruger_effect, theater_ratio, 5, 0.52).
narrative_ontology:measurement(dk_tr_t10, dunning_kruger_effect, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(dk_be_t0, dunning_kruger_effect, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dk_be_t5, dunning_kruger_effect, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(dk_be_t10, dunning_kruger_effect, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dunning_kruger_effect, enforcement_mechanism).
narrative_ontology:affects_constraint(dunning_kruger_effect, selection_bias_in_hierarchies).
narrative_ontology:affects_constraint(dunning_kruger_effect, confidence_competence_asymmetry_in_expertise_markets).
narrative_ontology:affects_constraint(dunning_kruger_effect, institutional_knowledge_gatekeeping).

% DUAL FORMULATION NOTE:
% Dunning-Kruger effect as a cognitive bias (ε≈0.15, individual-level observation) differs structurally from Dunning-Kruger as an institutional selection mechanism (ε≈0.38, organizational amplification). This story focuses on the latter — the constraint as a system-level phenomenon enabled by organizational reliance on confidence signals. The cognitive bias itself (individual miscalibration) is a prerequisite but not sufficient for the constraint; institutional enforcement of confidence-based selection is required for significant extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dunning_kruger_effect, institutional, 0.08).
constraint_indexing:directionality_override(dunning_kruger_effect, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
