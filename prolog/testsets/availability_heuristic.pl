% ============================================================================
% CONSTRAINT STORY: availability_heuristic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_availability_heuristic, []).

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
 *   constraint_id: availability_heuristic
 *   human_readable: Availability Heuristic (as exploited by information systems)
 *   domain: cognitive/social/economic
 *
 * SUMMARY:
 *   The availability heuristic is a cognitive shortcut where individuals
 *   estimate probability or frequency by the ease with which examples come to
 *   mind. This heuristic itself is not a constraint — it is a cognitive
 *   mechanism. But when information systems deliberately design to exploit
 *   this mechanism by controlling salience, visibility, and narrative
 *   recency, a structural extraction emerges. Media outlets, social
 *   platforms, and algorithmic recommenders amplify vivid,
 *   emotionally-charged, recent information disproportionately to its
 *   statistical importance. Decision-makers systematically overweight
 *   available examples, leading to misallocation of attention, capital,
 *   policy, and risk management across economic, political, and health
 *   domains. The constraint exhibits tangled rope structure: it has a genuine
 *   coordination component (media/platforms use salience to aggregate
 *   audience attention efficiently) but paired with asymmetric extraction
 *   (individual decision-makers systematically misallocate resources due to
 *   overweighting vivid information). The theater ratio (0.64) reflects that
 *   much discourse about availability bias frames it as inevitable cognitive
 *   architecture rather than as a deliberately designed feature of
 *   information systems. Over the 20-year interval (spanning roughly
 *   2005-2025), the extractiveness has increased from 0.35 to 0.58 as
 *   algorithmic curation has become more sophisticated and pervasive, and as
 *   media competition has intensified pressure to maximize salience capture.
 *
 * KEY AGENTS:
 *   - Individual Decision-Makers: Primary victims (powerless/trapped) — bear systematic decision errors from overweighting salient information; cannot exit the heuristic
 *   - Media Institutions & Platforms: Primary beneficiaries (institutional/arbitrage) — profit from salience capture and attention monopolization; extract audience focus
 *   - Informed Deliberators: Secondary victims (moderate/constrained) — can partially exit through debiasing but face high cognitive and information costs
 *   - Data Literacy Movement: Organized agents (organized/constrained) — building educational and infrastructural alternatives; represent sunset pathway
 *   - Epistemic Accuracy: Abstract victim (powerless/trapped) — collective epistemic commons damaged by systematically distorted information distributions
 *   - Algorithmic Recommender Systems: Institutional amplifiers (institutional/arbitrage) — not original exploiters but dramatically amplify availability bias effects
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing exploitation as immutable cognitive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(availability_heuristic, 0.58).
domain_priors:suppression_score(availability_heuristic, 0.68).
domain_priors:theater_ratio(availability_heuristic, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(availability_heuristic, extractiveness, 0.58).
narrative_ontology:constraint_metric(availability_heuristic, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(availability_heuristic, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(availability_heuristic, tangled_rope).
narrative_ontology:human_readable(availability_heuristic, "Availability Heuristic (as exploited by information systems)").
narrative_ontology:topic_domain(availability_heuristic, "cognitive/social/economic").

domain_priors:requires_active_enforcement(availability_heuristic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(availability_heuristic, attention_capturing_actors).
narrative_ontology:constraint_beneficiary(availability_heuristic, sensational_information_producers).
narrative_ontology:constraint_victim(availability_heuristic, decision_makers_relying_on_availability).
narrative_ontology:constraint_victim(availability_heuristic, epistemic_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL DECISION-MAKER (SNARE) — Cognitive architecture is constitutively vulnerable to availability bias. Cannot exit the heuristic; bears full cost of systematic overweighting vivid/recent/sensational information. No alternative mental mechanism provided; suppression is cognitive — the bias is built into human attention. High experienced extraction.
constraint_indexing:constraint_classification(availability_heuristic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMED DELIBERATOR (TANGLED ROPE) — Some exit option through deliberate debiasing (seeking base-rate data, historical context, statistical literacy). But exit costs are high: requires cognitive effort, access to reliable information infrastructure, and time pressure that often prevents bias correction. Mixed experience: benefits from fast heuristic when accuracy matters less; extracted from when high-stakes decisions rely on availability.
constraint_indexing:constraint_classification(availability_heuristic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDIA/PLATFORM INSTITUTION (ROPE) — Experiences availability heuristic as a coordination mechanism: capturing attention via vivid content is their core coordination function. Beneficiary; can arbitrage by switching narrative framing, controlling what becomes salient. Net coordination benefit — the constraint enables their primary function of audience capture.
constraint_indexing:constraint_classification(availability_heuristic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA LITERACY MOVEMENT (SCAFFOLD) — Organized agents (educators, journalists, platforms implementing friction/disclosure) are building cognitive infrastructure for bias-resistant decision-making. Education, statistical literacy programs, and algorithmic transparency are creating sunset pathways. Suppression declining as alternative cognition structures mature; high theater early (debiasing programs are aspirational) but real functional reduction over generational timescale.
constraint_indexing:constraint_classification(availability_heuristic, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EVOLUTIONARY PSYCHOLOGY FRAME (PITON) — 'Availability bias is adaptive heuristic for ancestral environments' is largely performative. The frame once explained why the bias exists; now it functions as justification for why nothing can change. Availability bias persists institutionally not because it's optimal, but because information systems profit from it. Theater ratio high; functional explanation degraded.
constraint_indexing:constraint_classification(availability_heuristic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE ARCHITECTURE VIEW (MOUNTAIN) — From a civilizational perspective, availability heuristic emerges necessarily from bounded attention and memory: human minds have finite cognitive capacity; attention is inherently selective; recent/vivid information is computationally cheaper to retrieve. This perspective sees the bias as an immutable feature of human cognition. However, the structural data contradicts this — the extraction and suppression metrics reflect systematic exploitation by information systems, not immutable cognitive law.
constraint_indexing:constraint_classification(availability_heuristic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(availability_heuristic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(availability_heuristic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(availability_heuristic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(availability_heuristic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(availability_heuristic, TR),
    TR >= 0.70.

:- end_tests(availability_heuristic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The availability heuristic enables significant extraction because information systems can predictably and systematically manipulate which examples become salient. The extraction is not total (people have some access to corrective information, some deliberative capacity) but substantial — the 20-year trend shows increasing extractiveness as algorithms have become more sophisticated. Starting value (0.35) reflects when exploitation was primarily through media editorial choice; current value (0.58) reflects algorithmic amplification. Suppression (0.68): High. Significant barriers to resisting availability bias include: (a) cognitive constraints (attention is finite and automatic); (b) information asymmetry (individuals cannot easily access alternative salience landscapes); (c) time pressure (bias-resistant decision-making requires deliberation time often unavailable); (d) social proof feedback (when salient information spreads widely, it appears more reliable). Theater ratio (0.64): Moderate-high. Considerable performative content in how availability bias is discussed. Evolutionary psychology framing ('this bias was adaptive in ancestral environments') functions as justification for accepting the bias as natural rather than as a design feature to resist. Media literacy programs ('be aware of availability bias') are largely aspirational — awareness alone doesn't eliminate the bias when information systems are still deliberately designed to maximize salience.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence. The media/platform institution sees availability heuristic as a legitimate coordination mechanism (Rope) — they solve the problem of capturing scattered attention into collective focus. The individual decision-maker sees pure extraction (Snare) — their decisions are systematically biased toward sensationalism, recency, and vividness without their participation in or benefit from this bias. The data literacy movement sees a solvable temporary problem (Scaffold) — educational and infrastructural interventions can build bias-resistant decision-making. The evolutionary psychology frame sees an immutable feature (Piton, degraded Mountain) — the constraint persists because it's 'natural,' not because it serves function. The analytical observer risks seeing cognitive law (Mountain) — availability bias as inherent to bounded rationality — when the structural data reveals systematic exploitation as a contingent institutional choice. The informed deliberator occupies the middle (Tangled Rope) — experiencing both the bias and the capacity to sometimes resist it. This perspectival range is diagnostic: when a single structural phenomenon appears as pure coordination to beneficiaries and pure extraction to victims, with organized agents building escape routes, the true classification is hybrid (Tangled Rope) with degradation (Piton) in the performative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is determined by structural position relative to the constraint. Media institutions and algorithmic systems experience low directionality (d ≈ 0.15) because they benefit from availability exploitation — the constraint subsidizes their attention capture function. Individual decision-makers experience high directionality (d ≈ 0.85) because they are structurally vulnerable and cannot exit: the bias is embedded in their cognitive architecture and information environment simultaneously. Informed deliberators (d ≈ 0.50) experience mixed extraction because they can partially exit through debiasing but at high cost. The analytical observer at the civilizational level (d ≈ 0.72) sees a natural law ('bounded attention implies salience bias') but the structural data reveals this as partial truth weaponized: salience is not just a cognitive constraint but an engineered feature of information systems. The data literacy movement (d ≈ 0.45) experiences moderate extraction because they both suffer from and benefit from the current state — they have organized agency and are building exit pathways.
 *
 * MANDATROPHY ANALYSIS:
 *   The availability heuristic resolves the mandatrophy by disambiguating the cognitive mechanism from the institutional exploitation. The cognitive heuristic (bounded attention, automatic salience weighting) is a real feature of human information processing — not inherently extractive, just efficient given constraints. But when information systems deliberately design to exploit this heuristic through algorithmic curation, sensational framing, and attention monopoly, a structural extraction emerges on top of the neutral cognitive feature. The tangled rope classification captures this hybrid: genuine coordination (audience aggregation, efficient attention direction) paired with asymmetric extraction (decision-maker vulnerability, misallocated resources). The increasing extractiveness over 20 years (0.35→0.58) reflects not that the cognitive heuristic became more powerful, but that information systems became more sophisticated at exploiting it. The scaffold perspective is crucial: data literacy, algorithmic transparency, and disclosure friction are building countermeasures that reduce suppression over generational horizon. The evolutionary psychology piton (frame claiming adaptiveness) must be rejected as performative — the constraint is neither immutable nor evolutionary advantage, but architectural choice by attention-capturing systems. The mountain perspective is a false summit: availability bias is NOT an immutable cognitive law but a contingent feature of how attention is economically organized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptive_vs_exploited_threshold,
    'When does availability heuristic transition from adaptive decision-making shortcut to exploited bias?',
    'Comparative analysis of decision outcomes in low-information environments vs high-information environments; measurement of heuristic success rates by domain and information density',
    'If always exploited: snare from all perspectives. If adaptively useful in some contexts: rope or scaffold from beneficiary perspectives becomes legitimate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_vs_exploited_threshold, conceptual, 'Threshold between adaptive heuristic and exploited bias').

omega_variable(
    algorithmic_amplification_causality,
    'Do algorithmic recommendation systems exploit pre-existing availability bias or actively amplify it beyond natural cognitive limits?',
    'Controlled experiments comparing information selection in human-only vs algorithm-curated environments; measurement of salience divergence between algorithmic feeds and diverse information sources',
    'If exploit only: extractiveness ≤ 0.40 (snare as contingent institutional arrangement). If amplify: extractiveness ≥ 0.65 (snare as co-produced by cognitive and technological coupling).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_amplification_causality, empirical, 'Whether algorithms amplify or merely exploit availability bias').

omega_variable(
    debiasing_infrastructure_scalability,
    'Are data literacy and statistical education interventions scalable to population level or inherently limited to epistemic elites?',
    'Longitudinal tracking of cognitive bias prevalence across education levels and access to debiasing resources; measurement of base-rate reasoning improvement in intervention vs control populations',
    'If scalable: scaffold sunset is real; suppression declines over generational horizon. If elite-limited: suppression persists; constraint remains snare for majority, rope for educated minority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debiasing_infrastructure_scalability, empirical, 'Scalability of debiasing interventions to population level').

omega_variable(
    attention_economy_necessity,
    'Is the availability-heuristic exploitation a necessary feature of attention-based economic models or contingent institutional choice?',
    'Historical analysis of media and platform economics; comparison of business models that profit from vs defend against availability bias; examination of platform design alternatives',
    'If necessary: extractiveness is inherent to attention economies (piton/mountain boundary). If contingent: extractiveness reflects extractive platform choice (snare/tangled rope distinction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_economy_necessity, conceptual, 'Whether bias exploitation is necessary to attention-economy models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(availability_heuristic, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(avail_tr_t0, availability_heuristic, theater_ratio, 0, 0.42).
narrative_ontology:measurement(avail_tr_t10, availability_heuristic, theater_ratio, 10, 0.55).
narrative_ontology:measurement(avail_tr_t20, availability_heuristic, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(avail_be_t0, availability_heuristic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(avail_be_t10, availability_heuristic, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(avail_be_t20, availability_heuristic, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(availability_heuristic, information_standard).
narrative_ontology:affects_constraint(availability_heuristic, attention_capture_asymmetry).
narrative_ontology:affects_constraint(availability_heuristic, algorithmic_salience_curation).
narrative_ontology:affects_constraint(availability_heuristic, media_sensationalism_cycle).

% DUAL FORMULATION NOTE:
% Availability heuristic is a family of constraints: (1) the cognitive mechanism (bounded attention, automatic salience weighting) is a neutral feature of information processing; (2) the institutional exploitation (algorithmic amplification, sensational framing, attention monopoly) creates structural extraction. These are distinct constraints with different epsilon values. Story focuses on the exploitation layer (ε=0.58, Tangled Rope). Upstream cognitive heuristic could be decomposed as Mountain (ε≤0.25) if analyzed purely as cognitive architecture. Network effects represent downstream consequences of availability exploitation on attention distribution, algorithmic feedback, and media business models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(availability_heuristic, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
