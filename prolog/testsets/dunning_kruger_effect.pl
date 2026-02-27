% ============================================================================
% CONSTRAINT STORY: dunning_kruger_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The Dunning-Kruger effect describes a structural tension in learning and
 *   self-assessment: agents with limited competence in a domain lack the very
 *   metacognitive capacity needed to recognize their incompetence. This
 *   creates a coordination problem (how to enable learning when learners
 *   cannot perceive their own gaps?) and an extraction mechanism
 *   (low-competence agents make high-stakes decisions based on false
 *   confidence, damaging institutions and peer-correction mechanisms). The
 *   constraint exhibits hybrid character: partly a natural feature of
 *   learning curves (the early phase where confidence outpaces actual
 *   ability), partly institutional (organizations could reduce the effect
 *   with better assessment and feedback, but organizational inertia preserves
 *   performative confidence rituals). The empirical status has shifted
 *   dramatically: the original Dunning-Kruger papers (1999-2000) reported a
 *   strong, universal effect; meta-analyses (Schaeffer et al. 2024) show the
 *   effect is much smaller, domain-dependent, and partly an artifact of
 *   regression to the mean. Yet the cultural invocation of 'Dunning-Kruger'
 *   persists — used to explain performance gaps in organizations without
 *   addressing the underlying assessment or feedback systems. This
 *   perspectival gap between the weakened empirical claim and the persistent
 *   institutional narrative is captured in the theater_ratio (0.65), which
 *   has increased over the interval as the effect's empirical status has
 *   become contested while its rhetorical use has remained stable.
 *
 * KEY AGENTS:
 *   - Low-Competence Novices: Primary victims (powerless/trapped) — trapped in metacognitive blindness; cannot perceive gap between confidence and actual ability; bear real-world costs of false-confidence decisions
 *   - Institutional Quality Assurance: Primary victim (institutional/constrained) — their ability to assess, select, and develop talent is systematically distorted by inflated self-assessments; must enforce verification systems to bypass the bias
 *   - Expert Correctors: Secondary actor (moderate/constrained) — peers, mentors, and supervisors attempt feedback but face suppression and defensive reactions from low-competence agents
 *   - Confidence Marketers: Primary beneficiary (institutional/arbitrage) — self-help industry, motivational speakers, rapid-credentialing programs profit by reinforcing overconfidence; experience the effect as pure coordination
 *   - Psychology Research Community: Institutional observer (analytical/analytical) — original discoverers of the effect; now maintain a more complex position as meta-analyses reveal the effect is smaller, domain-dependent, and partly artifactual
 *   - Organizational Culture: Institutional actor (institutional/constrained) — invokes Dunning-Kruger performatively to explain performance gaps without addressing underlying assessment or development systems
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
narrative_ontology:constraint_beneficiary(dunning_kruger_effect, low_competence_agents_short_term).
narrative_ontology:constraint_beneficiary(dunning_kruger_effect, confidence_marketers).
narrative_ontology:constraint_victim(dunning_kruger_effect, institutional_quality_assurance).
narrative_ontology:constraint_victim(dunning_kruger_effect, peer_correction_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVICE (SNARE) — Low-competence agents trapped in genuine illusion of competence. Cannot perceive the gap between self-assessment and actual ability. Maximum extraction: they make decisions based on false confidence, suffer real-world consequences (failed projects, damaged relationships, career setbacks), yet the mechanism preventing learning (metacognitive blindness) is intrinsic to their position. No exit option — cannot climb the learning curve while trapped in the illusion.
constraint_indexing:constraint_classification(dunning_kruger_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EXPERT CORRECTING (ROPE) — Moderate-power agents (peers, mentors, supervisors) who see the gap and attempt correction face suppression: low-competence agents reject feedback, dismiss corrections as jealousy or gatekeeping, and may retaliate against those who challenge their confidence. Experts experience the constraint as coordination difficulty (how to enable learning without triggering defensive reactions?). Suppression is significant but not total — some feedback does get through; constrained exit (can partially withdraw from mentoring role but at professional/social cost).
constraint_indexing:constraint_classification(dunning_kruger_effect, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL QA (TANGLED ROPE) — Organizations implementing assessment, hiring, and promotion systems bear dual extraction: (1) They must coordinate accurate ability assessment (coordination function); (2) They suffer systematic distortion of signals — low-competence agents present inflated self-assessments that undermine selection and development processes (extraction). Active enforcement required: interviews, tests, probation periods, 360-degree feedback all exist to bypass the Dunning-Kruger distortion. The institutional victim here is the quality of information used for resource allocation and decision-making.
constraint_indexing:constraint_classification(dunning_kruger_effect, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONFIDENCE MARKETERS (ROPE) — Entities profiting from false confidence (motivational speakers, rapid-credentialing programs, self-help industries, overconfident consultants) experience the constraint as pure coordination: they are solving the market demand for confidence-boosting messages. They benefit from the Dunning-Kruger effect by selling courses, books, and seminars that reinforce overconfidence. No real victims in their frame — they coordinate supply and demand. Arbitrage exit: can shift between markets easily.
constraint_indexing:constraint_classification(dunning_kruger_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PSYCHOLOGY RESEARCH (PITON) — The original Dunning-Kruger papers (1999-2000) identified a robust, measurable phenomenon with high replicability. But over 25 years, the consensus has shifted: meta-analyses (Schaeffer et al. 2024) show the effect is much smaller than originally claimed, highly dependent on task domain, and partly an artifact of regression to the mean. The scientific consensus has degraded — the theatrical invocation of 'Dunning-Kruger' persists in popular culture and organizations despite the empirical effect being much weaker than popular belief assumes. Theater ratio (0.65) reflects that the label persists performatively in organizational contexts even as the underlying empirical claim has become contested.
constraint_indexing:constraint_classification(dunning_kruger_effect, piton,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational timescale, the Dunning-Kruger effect is best understood as a tension between metacognitive capacity and skill complexity: agents lack the very competence needed to recognize incompetence (coordination function — they cannot self-assess without a reference standard they do not possess), AND this creates systematic extraction as they make high-stakes decisions in domains where they lack grounding. The constraint is hybrid: partly natural (learning always has a lag phase where the learner is unaware of what they don't know), partly institutional (organizations could reduce the effect with better assessment, but don't). Theater ratio reflects that organizations often invoke 'Dunning-Kruger' as an explanation for performance gaps without actually implementing metacognitive training or better assessment.
constraint_indexing:constraint_classification(dunning_kruger_effect, tangled_rope,
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
    constraint_indexing:constraint_classification(dunning_kruger_effect, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.38): Moderate. The Dunning-Kruger effect extracts from institutional quality assurance and peer-correction mechanisms by distorting self-assessment signals. But the extraction is not as high as the original panic about the effect suggested (early estimates ~0.50). Meta-analytic evidence shows the effect is smaller than originally claimed, more domain-dependent, and partly an artifact of regression to the mean. The extractiveness reflects the real but limited distortion of assessment signals — institutions can partially overcome it with appropriate verification. Suppression (0.48): Moderate. Low-competence agents actively resist feedback (defensive reactions, dismissal of corrections as jealousy), creating barriers to learning. But suppression is not absolute — some agents do receive and integrate feedback; some organizations have built effective assessment systems. Theater ratio (0.65): Moderate-high. The increase over the interval reflects that the empirical status of the effect has become contested (meta-analyses showing smaller magnitude, domain-dependency, regression artifacts) while its cultural invocation has remained performatively stable. Organizations use 'Dunning-Kruger' to explain performance gaps without implementing the metacognitive or assessment interventions that would actually reduce the effect.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how indexical perspective reveals different structural realities. The novice experiences pure extraction (Snare) — they cannot perceive the gap and suffer real consequences. The expert attempting correction experiences coordination difficulty (Rope) — how to enable learning despite the novice's defensive reactions? The institution experiences hybrid extraction-coordination (Tangled Rope) — they must coordinate accurate assessment while being systematically distorted by false-confidence signals. The confidence industry experiences pure coordination (Rope) — they are simply meeting market demand for confidence-boosting content. The psychology research community has shifted from the original mountain-like certainty (robust, universal effect) to tangled-rope complexity (domain-dependent, partly artifactual, contested). The organizational culture experiences piton dynamics — invoking the effect performatively without implementing the structural changes that would reduce it. The perspectival gap reveals that the 'Dunning-Kruger effect' is not a single constraint but a cluster of related phenomena: a natural learning-lag (inevitable phase where confidence outpaces ability), a communication/feedback problem (how to convey gaps to agents who cannot perceive them), an institutional assessment problem (organizations not implementing verification), and a cultural narrative (popular invocation detached from current empirical status).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position in the extraction flow. Low-competence novices are full targets (d ≈ 0.95) because they cannot exit the illusion; the mechanism preventing learning (metacognitive blindness) is intrinsic to their position. They have no arbitrage or mobile exit options — trapped (d → high f(d) → high χ). Institutional quality assurance systems are constrained victims (d ≈ 0.65) because they must invest in verification mechanisms to overcome the signal distortion; they cannot simply exit or arbitrage (constrained exit, victim status). Expert correctors are moderate victims (d ≈ 0.55) — they face resistance to feedback but have some ability to work around the defense mechanisms; moderate power and constrained exit. Confidence marketers are beneficiaries (d ≈ 0.15) because they profit from the effect; institutional power and arbitrage exit (can move between markets). The psychology research community moved from low-victim (d ≈ 0.20) in 1999-2000 when they 'discovered' a clear effect, to moderate observer (d ≈ 0.72) as they recognized complexities in the effect's empirical basis; analytical exit option. The organizational culture has high institutional power with mixed exit (constrained to invoke narratives, arbitrage to change assessment systems) — d ≈ 0.50 — experiencing both coordination (need to communicate about competence) and extraction (performative use without structural change).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The Dunning-Kruger constraint resolves mandatrophy by showing that the 'effect' is not a single natural law or coordination problem, but a hybrid phenomenon varying by indexical context. From the novice's perspective, it is pure extraction (Snare) — they cannot learn because the mechanism preventing learning is intrinsic to their epistemic position. From the institution's perspective, it is coordination with asymmetric extraction (Tangled Rope) — they must solve the assessment problem while being distorted by false signals. From the confidence industry's perspective, it is pure coordination (Rope) — they are meeting demand. The empirical trajectory (original discovery → meta-analytic refinement → recognition of domain-dependency and regression artifacts) tracks an institutional shift from 'mountain-like certainty' (robust, universal, natural) to 'tangled rope complexity' (real but limited, domain-dependent, partly institutional). The increasing theater ratio (0.35 → 0.65) captures this: organizations continue to invoke 'Dunning-Kruger' performatively (to explain gaps without addressing systems) even as the empirical basis has become contested. The mandatrophy is resolved by recognizing that the effect is partly natural (learning curve lag is inevitable), partly institutional (organizations could reduce it with better feedback and assessment), and partly narrative (cultural invocation has decoupled from empirical status). No single classification captures the full structure — the presheaf over different observation contexts IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regression_to_mean_artifact,
    'Is the observed overconfidence in low-competence groups primarily a genuine bias or an artifact of regression to the mean in measured performance?',
    'Statistical decomposition of meta-analyses separating true overconfidence from regression artifacts; studies using latent variable models to measure true vs measured ability',
    'If primarily artifact: effect size is much smaller (0.10-0.15 vs 0.40+); many claimed instances are false positives. If genuine bias: original claim (0.40+) is valid; intervention strategies targeting metacognition are justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regression_to_mean_artifact, empirical, 'Whether Dunning-Kruger is genuine bias or regression artifact').

omega_variable(
    domain_specificity_boundary,
    'What features of a domain determine whether Dunning-Kruger overconfidence emerges (simple domains with clear feedback) vs disappears (complex domains with indirect feedback)?',
    'Cross-domain meta-analysis; correlation between task feedback quality and observed overconfidence; studies varying feedback structure within domains',
    'If primarily domain-dependent: the ''effect'' is not a universal cognitive bias but a learning-lag phenomenon specific to domains lacking clear feedback. Interventions would target feedback systems, not cognition per se.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_boundary, empirical, 'Domain-dependent versus universal nature of overconfidence bias').

omega_variable(
    metacognitive_training_efficacy,
    'Can explicit metacognitive training (teaching people to evaluate confidence, uncertainty, and knowledge gaps) sustainably reduce overconfidence, or does the bias re-emerge over time?',
    'Longitudinal studies with metacognitive intervention groups; measurement of confidence calibration over 6-12 months post-training; replication across age groups and domains',
    'If training is effective and stable: the constraint is malleable — institutional quality assurance can incorporate metacognitive development. If training effects decay: the constraint is more structural and harder to overcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metacognitive_training_efficacy, empirical, 'Efficacy and durability of metacognitive training interventions').

omega_variable(
    cultural_confidence_norms,
    'Does the magnitude of the Dunning-Kruger effect vary significantly across cultures with different norms around self-promotion and humility?',
    'Cross-cultural replication studies in individualist vs collectivist societies; measurement of confidence-competence gap by cultural dimension (Hofstede, Schwartz)',
    'If effect is culturally variable: it is not a universal cognitive constraint but partly a cultural-institutional product. Organizational interventions would need to account for cultural context. If effect is universal: cognitive mechanism is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_confidence_norms, empirical, 'Cultural variability of overconfidence bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dunning_kruger_effect, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dk_tr_t0, dunning_kruger_effect, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dk_tr_t50, dunning_kruger_effect, theater_ratio, 50, 0.48).
narrative_ontology:measurement(dk_tr_t100, dunning_kruger_effect, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(dk_be_t0, dunning_kruger_effect, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dk_be_t50, dunning_kruger_effect, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(dk_be_t100, dunning_kruger_effect, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dunning_kruger_effect, information_standard).
narrative_ontology:affects_constraint(dunning_kruger_effect, metacognitive_illusion).
narrative_ontology:affects_constraint(dunning_kruger_effect, organizational_assessment_validity).

% DUAL FORMULATION NOTE:
% The Dunning-Kruger effect is downstream of metacognitive capacity constraints (the root cause: inability to recognize what one doesn't know) and upstream of organizational assessment and feedback systems (the institutional mechanisms that either amplify or dampen the effect). This story focuses on the social/institutional manifestation; a separate story on the cognitive mechanism (metacognitive_illusion) would have different ε and different perspectives on the neuroscience of confidence calibration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dunning_kruger_effect, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
