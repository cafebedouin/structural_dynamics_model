% ============================================================================
% CONSTRAINT STORY: framing_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_framing_effect, []).

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
 *   constraint_id: framing_effect
 *   human_readable: The Framing Effect in Choice Architecture
 *   domain: social/political/technological
 *
 * SUMMARY:
 *   The framing effect represents a fundamental structural constraint on
 *   decision autonomy across social, political, and technological domains.
 *   Since Tversky and Kahneman's foundational work (1981), framing has been
 *   recognized as a robust cognitive bias where logically equivalent choices
 *   produce systematically different decisions based on how options are
 *   presented (as losses vs gains, risks vs certainties, etc.). However, the
 *   framing effect exhibits a dual nature: it can function as pure extraction
 *   (choice architects manipulating subjects through frames), as coordination
 *   (legitimate use of frames to clarify complex decisions), or as an
 *   immutable property of cognition itself. The constraint has intensified
 *   over the past 60 years through three mechanisms: (1) exponential growth
 *   in choice architecture applications (from policy to product design to
 *   algorithmic recommendation), (2) increasing sophistication of framing
 *   techniques enabled by behavioral science research, and (3) theater ratio
 *   growth as marketing industries elaborate performative narratives around
 *   framing. The current interval (years 0-60, approximately 1981-2041)
 *   captures the lifecycle from initial experimental discovery through
 *   widespread application and early resistance through transparency and
 *   digital tools. The theater ratio has increased from 0.50 (research-phase
 *   framing was relatively explicit) to 0.65 (institutional applications bury
 *   framing in complexity) because choice architects now deploy frames
 *   implicitly, relying on subjects' cognitive blindness rather than
 *   transparent persuasion.
 *
 * KEY AGENTS:
 *   - Choice Subjects: Primary victims (powerless/trapped) — ordinary people making decisions in contexts where frames are selected by institutional actors; experience involuntary reframing despite awareness of the bias.
 *   - Choice Architects: Primary beneficiary (institutional/arbitrage) — policy designers, UX designers, marketers, politicians who benefit from framing as a low-cost coordination/control tool.
 *   - Behavioral Economists: Secondary actor (moderate/constrained) — researchers who studied framing effects and are now institutionally pressured to apply findings; experience both career advancement (benefit) and extraction pressure (expectation to show dramatic interventions).
 *   - Regulatory/Transparency Coalition: Organized agents (organized/constrained) — behavioral ethics boards, consumer protection agencies, transparency advocates seeking to manage framing through disclosure mandates and standardization.
 *   - Marketing/Advertising Industry: Institutional actor (institutional/arbitrage) — maintains elaborate frameworks (brand positioning, narrative construction) that institutionalize framing effects; system persists through inertia despite reduced functionality.
 *   - Digital Transparency Movement: Organized agents (organized/mobile) — fact-checkers, algorithmic literacy educators, open-data platforms building frame-neutral alternatives; represent the sunset trajectory for framing constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(framing_effect, 0.52).
domain_priors:suppression_score(framing_effect, 0.68).
domain_priors:theater_ratio(framing_effect, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(framing_effect, extractiveness, 0.52).
narrative_ontology:constraint_metric(framing_effect, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(framing_effect, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(framing_effect, tangled_rope).
narrative_ontology:human_readable(framing_effect, "The Framing Effect in Choice Architecture").
narrative_ontology:topic_domain(framing_effect, "social/political/technological").

domain_priors:requires_active_enforcement(framing_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(framing_effect, choice_architects).
narrative_ontology:constraint_beneficiary(framing_effect, policy_implementers).
narrative_ontology:constraint_victim(framing_effect, choice_subjects).
narrative_ontology:constraint_victim(framing_effect, decision_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHOICE SUBJECT (SNARE) — Cannot exit framing manipulation; cognitive bias is involuntary and persistent even when disclosed. Lacks metacognitive access to the reframing mechanism. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(framing_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BEHAVIORAL ECONOMIST (TANGLED ROPE) — Constrained by institutional pressure to publish and apply findings; also benefits from framing research funding, career advancement, and ability to influence policy. Experiences both extraction (pressure to show dramatic effects) and coordination (advancing collective knowledge of decision science). d≈0.58, f(d)≈0.80, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(framing_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHOICE ARCHITECT (ROPE) — Benefits from framing effects as a coordination tool: using frames to align citizen behavior with policy goals (e.g., organ donation defaults, retirement savings opt-out). Experiences the constraint as enabling efficient implementation. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(framing_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (TANGLED ROPE) — Organized agents (behavioral ethics boards, consumer protection agencies, transparency advocates) seek to manage framing through disclosure mandates and frame standardization. Faces pushback from choice architects (loss of efficacy) but also coordinates public discourse around choice design. Benefits from legitimacy gained through regulatory role. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(framing_effect, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MARKETING/ADVERTISING SYSTEM (PITON) — Framing is the foundational technology of advertising; the industry maintains elaborate institutional frameworks (brand positioning, narrative construction, emotional appeals) that are largely performative theater masking the core framing mechanism. Theater_ratio=0.65 reflects that much advertising ritual is aesthetic rather than informative. The system persists through inertia and because direct alternatives (transparent, unframed choice) would reduce industry value extraction.
constraint_indexing:constraint_classification(framing_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL TRANSPARENCY MOVEMENT (SCAFFOLD) — Organized agents (fact-checkers, algorithmic literacy educators, open-data platforms) are building tools and norms that reduce framing effectiveness: visual information comparisons, frame-neutral decision support systems, media literacy education. These represent a sunset trajectory for the framing constraint — as frame-neutral alternatives mature, the extraction mechanism loses force. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.14. Low effective extraction because the coalition has agency and growing structural alternatives.
constraint_indexing:constraint_classification(framing_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, framing effects may appear to be invariant features of human cognition: all decisions require some frame, and frame selection is inevitable. This perspective risks naturalizing what is actually a contingent institutional arrangement (choice architects have power to select frames; choice subjects do not). Engine false summit detector will flag this: ε=0.52, suppression=0.68 contradict mountain classification.
constraint_indexing:constraint_classification(framing_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(framing_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(framing_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(framing_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(framing_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(framing_effect, TR),
    TR >= 0.70.

:- end_tests(framing_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The framing effect enables choice architects to steer decisions without changing objective conditions, creating asymmetric benefit. The constraint is not maximally extractive (0.52 rather than 0.75+) because framing works within bounds — subjects retain some decision agency and can partially overcome framing through effort. Increasing from 0.35 to 0.52 over the interval reflects both wider application and deeper institutional embedding. Suppression (0.68): High structural barriers prevent subjects from escaping framing. Cognitive bias is involuntary, persists even after disclosure, and is nearly universal across populations. Choice architects control frame presentation with few institutional checks. Suppression has remained relatively constant because the cognitive mechanism is stable; growth is in application breadth rather than individual barrier strength. Theater ratio (0.65): Marketing and institutional applications emphasize emotional narratives, brand positioning, and aesthetic framing around the core mechanism. Explicit research-phase framing (1980s-1990s) was more transparent; institutional application (2000s-present) obscures the frame in elaborate narratives. The ratio has grown because institutions now hide frames under complexity and beauty rather than presenting them explicitly.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The choice subject experiences pure extraction (Snare) — they have no exit option and bear full cognitive cost. The choice architect experiences pure coordination (Rope) — framing is their legitimate tool for aligning behavior with goals. The behavioral economist experiences mixed extraction and coordination (Tangled Rope) — they benefit from research status while experiencing pressure to show dramatic effects. The regulatory coalition experiences mixed extraction and enforcement (Tangled Rope) — they coordinate transparency norms but face extraction pressure from choice architects' resistance. The advertising system experiences degradation (Piton) — elaborate ritual persists through inertia despite known inefficiency. The digital transparency movement experiences temporary constraint (Scaffold) — they are building frame-neutral alternatives that will eventually reduce extraction. The analytical observer risks naturalizing framing as an immutable cognitive law (Mountain) — but the structural data (extractiveness rising over time, suppression based on institutional power, theater ratio increasing) reveals this as false: framing extraction depends on architectural choices, not cognitive universality.
 *
 * DIRECTIONALITY LOGIC:
 *   Choice subjects: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cognitive bias is involuntary; disclosure does not eliminate effect; subjects cannot exit framing decisions. Choice architects: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. They control frame selection and capture asymmetric decision advantage. Behavioral economists: Both victim (pressure to apply findings) + beneficiary (career advancement) + constrained exit → d≈0.58, f(d)≈0.80. Mixed extraction-coordination. Regulatory coalition: Organized + constrained → d≈0.50, f(d)≈0.65. Moderate extraction. They coordinate transparency norms but cannot fully counteract architect incentives. Transparency movement: Organized + mobile → d≈0.35, f(d)≈0.35. Low extraction. They have agency and structural alternatives emerging. Marketing system: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification derives from theater gate (0.65 ≥ 0.70 threshold close), not chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit — observer naturalizes contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The framing effect resolves mandatrophy through perspectival decomposition. The constraint is neither 'pure extraction' nor 'immutable law' but rather a contingent institutional arrangement that functions differently depending on structural position. From the choice subject's perspective, it is extraction (Snare). From the choice architect's perspective, it is legitimate coordination (Rope). From the regulatory perspective, it is hybrid (Tangled Rope) requiring active enforcement to manage. From the transparency perspective, it is temporary (Scaffold) with a plausible sunset as frame-neutral alternatives mature. The false summit detector correctly identifies the 'naturalization' view as a mountain — when institutional actors present framing as 'inherent to human cognition' rather than 'our design choice,' they are using natural law language to evade accountability. The mandatrophy is resolved by accepting that all six types are structurally legitimate perspectives, and the system's actual constraint is the power asymmetry that allows architects to choose which perspective to occupy while subjects are trapped in theirs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    involuntariness_threshold,
    'At what disclosure level does framing cease to be involuntary extraction and become informed coordination?',
    'Experimental studies measuring decision change under explicit frame labeling; comparison of frame-naive vs frame-aware populations; longitudinal tracking of choice stability post-disclosure',
    'If disclosure threshold is low (< 30% effectiveness loss): framing approaches Rope classification across more perspectives. If high (> 70% persistence): framing remains Snare-dominant even with transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(involuntariness_threshold, empirical, 'Disclosure threshold for frame effects to become transparent').

omega_variable(
    frame_neutrality_existence,
    'Do truly frame-neutral choice presentations exist, or is frame-neutrality itself a frame?',
    'Analysis of information presentation formats that claim neutrality; identification of hidden framing in ostensibly neutral comparisons; cross-cultural variation in ''neutral'' presentation',
    'If frame-neutrality is impossible: the constraint shifts from extraction mechanism to inevitable structural feature (Mountain logic). If frame-neutral presentations exist: framing is contingent on architectural choice (Rope or Scaffold logic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frame_neutrality_existence, conceptual, 'Whether truly frame-neutral choice presentations are possible').

omega_variable(
    beneficiary_asymmetry_durability,
    'Do framing effects persist when choice architects and subjects have aligned incentives (mutual benefit from accurate choice)?',
    'Comparison of framing robustness in adversarial (choice architect benefits from manipulation) vs aligned (mutual benefit) settings; medical informed consent studies where accurate choice serves both parties',
    'If effects persist: framing is cognitive constraint independent of incentive structure (Mountain). If effects diminish: framing extraction depends on asymmetric power and interests (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_durability, empirical, 'Durability of framing effects under aligned incentives').

omega_variable(
    alternative_decision_technology_effectiveness,
    'Do digital decision support systems (neutral comparison tools, algorithmic advisors, visual information displays) actually reduce framing vulnerability at scale?',
    'Population-level studies of frame-resilient decision-making post-intervention; measurement of frame effect size before/after access to neutral tools; tracking of adoption rates and sustained usage',
    'If effective: scaffold perspective confirmed — frame-neutral alternatives are materially reducing extraction. If ineffective: the digital transparency movement is aspirational rather than structural, and framing remains dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_decision_technology_effectiveness, empirical, 'Effectiveness of digital decision support in reducing framing vulnerability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(framing_effect, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frame_tr_t0, framing_effect, theater_ratio, 0, 0.5).
narrative_ontology:measurement(frame_tr_t30, framing_effect, theater_ratio, 30, 0.58).
narrative_ontology:measurement(frame_tr_t60, framing_effect, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(frame_be_t0, framing_effect, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(frame_be_t30, framing_effect, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(frame_be_t60, framing_effect, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(framing_effect, information_standard).
narrative_ontology:affects_constraint(framing_effect, choice_architecture_governance).
narrative_ontology:affects_constraint(framing_effect, algorithmic_opacity).
narrative_ontology:affects_constraint(framing_effect, informed_consent_institutional_capture).

% DUAL FORMULATION NOTE:
% The framing effect decomposes into two structurally distinct constraints: (1) the cognitive bias itself (neurological limit on frame-neutral perception — approaches Mountain), and (2) the institutional choice to exploit that bias without disclosure (Snare/Tangled Rope). This story addresses the institutional exploitation form. The cognitive limit form would be a separate story with lower ε and no beneficiary/victim declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(framing_effect, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
