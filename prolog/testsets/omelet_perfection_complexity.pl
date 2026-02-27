% ============================================================================
% CONSTRAINT STORY: omelet_perfection_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_omelet_perfection_complexity, []).

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
 *   constraint_id: omelet_perfection_complexity
 *   human_readable: The French Omelet Paradox (Chasing Perfection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The French omelet paradox reveals how seemingly simple tasks become
 *   vehicles for extractive complexity myths. The constraint operates through
 *   the gap between the objective simplicity of the underlying task (heat
 *   management in a pan) and the subjective complexity internalized by
 *   practitioners (mastery requires decades, innate talent, French
 *   tradition). The paradox is that the simplicity is what makes the
 *   constraint extractive: because omelets are genuinely easy to make
 *   adequately, the perfection myth must be continually reinforced to justify
 *   the existence of culinary gatekeepers and to maintain the psychological
 *   drive for impossible refinement. Culinary institutions benefit from
 *   practitioners chasing an asymptotic ideal that is never reached. Novice
 *   cooks internalize the myth and experience perpetual inadequacy. The
 *   demystification movement (YouTube educators, explicit instruction,
 *   physics-based explanations) represents a structural challenge to the
 *   constraint's extraction mechanism — as knowledge becomes freely
 *   accessible, the scarcity value of 'mastery' erodes. The theater ratio has
 *   increased over the interval (0.52 → 0.68) because the constraint's
 *   primary function has shifted from genuine technique transmission to
 *   performative aspiration (cooking shows, Instagram food photography, the
 *   ritual of trying and failing). The base extractiveness has also increased
 *   (0.28 → 0.38) as the gap between adequate and ideal has widened — modern
 *   culinary media emphasizes aesthetic perfection more than nutrition or
 *   taste.
 *
 * KEY AGENTS:
 *   - Novice Cook: Primary victim (powerless/trapped) — internalizes perfection myth; trapped in refinement regress with no clear success criteria
 *   - Home Cook Community: Secondary victim (moderate/constrained) — benefits from shared learning but also faces hierarchy of tacit knowledge and perpetual inadequacy
 *   - Culinary Institutions: Primary beneficiary (institutional/arbitrage) — professional chefs, cooking schools, food media monetize aspiration and gatekeep technique
 *   - Demystification Movement: Organized challenger (organized/mobile) — social media educators reducing theater through explicit instruction; building alternative learning pathways
 *   - Culinary Traditionalist Narrative: Institutional inertia (institutional/arbitrage) — myth of French tradition and innate talent maintained through repetition despite atrophied function
 *   - Professional Chef: Mixed position (organized/mobile) — benefits from exclusivity but constrained by performance expectations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees underlying thermodynamics as simple, risks naturalizing social myth as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(omelet_perfection_complexity, 0.38).
domain_priors:suppression_score(omelet_perfection_complexity, 0.52).
domain_priors:theater_ratio(omelet_perfection_complexity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omelet_perfection_complexity, extractiveness, 0.38).
narrative_ontology:constraint_metric(omelet_perfection_complexity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(omelet_perfection_complexity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(omelet_perfection_complexity, tangled_rope).
narrative_ontology:human_readable(omelet_perfection_complexity, "The French Omelet Paradox (Chasing Perfection)").
narrative_ontology:topic_domain(omelet_perfection_complexity, "social/psychological").

domain_priors:requires_active_enforcement(omelet_perfection_complexity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(omelet_perfection_complexity, culinary_gatekeepers).
narrative_ontology:constraint_beneficiary(omelet_perfection_complexity, perfection_myth_maintainers).
narrative_ontology:constraint_victim(omelet_perfection_complexity, novice_practitioners).
narrative_ontology:constraint_victim(omelet_perfection_complexity, aspiring_mastery_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVICE COOK (SNARE) — Trapped in an infinite regress of refinement. No exit from the goal (perfect omelet) but also no clear criteria for success. Each attempt reveals new failure modes: heat timing, pan selection, utensil angle, butter temperature, egg freshness, humidity. The extraction mechanism is the implicit demand for mastery without pedagogical guidance. High suppression because failure is interpreted as personal inadequacy rather than task complexity.
constraint_indexing:constraint_classification(omelet_perfection_complexity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HOME COOK COMMUNITY (TANGLED ROPE) — Experiences both coordination benefit and extraction asymmetry. The shared goal (improving technique) generates community learning. But the constraint also enforces hierarchy: those with more tacit knowledge (professional training, family tradition, time investment) occupy asymmetric advantage. Constrained exit because cooking is embedded in cultural identity and family practice. Some benefit from shared recipes and techniques; some bear the cost of perpetual inadequacy relative to impossible standards.
constraint_indexing:constraint_classification(omelet_perfection_complexity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CULINARY INSTITUTIONS AND MEDIA (ROPE) — Professional chefs, cooking shows, culinary schools, and food media benefit from the perfection-chasing dynamic. They arbitrage access to secrets and technique: broadcast imperfection (entertaining failures), withhold mastery (enroll in classes), monetize aspiration (sell cookbooks, streaming content, equipment). Exit is free — they can pivot to different cuisines or techniques. The constraint appears to them as pure coordination: enabling knowledge transfer and setting standards.
constraint_indexing:constraint_classification(omelet_perfection_complexity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEMYSTIFICATION MOVEMENT (SCAFFOLD) — Social media cooking educators (YouTube, TikTok, Instagram) are building alternative pathways that bypass the culinary establishment's gatekeeping. 'Anyone can make a French omelet' creators show real failures, explain physical principles, reduce theater. The constraint appears as temporary: as accessible knowledge accumulates, the mystique of perfection erodes. This perspective has a genuine sunset — the constraint's extraction mechanism weakens as demystification succeeds. Suppression is lower because the movement provides explicit learning pathways.
constraint_indexing:constraint_classification(omelet_perfection_complexity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CULINARY TRADITIONALIST NARRATIVE (PITON) — The myth that French omelets require innate talent or decades of apprenticeship persists through repetition and institutional inertia. The actual functional requirement — rapid heat dissipation, low-protein coagulation rate — is simple physics. But the performative content (the ritual of 'learning' from masters, the appeal to French tradition, the mystique of technique) maintains the constraint even as its real coordination function has atrophied. Theater ratio is high because the narrative persists despite being undermined by demystification.
constraint_indexing:constraint_classification(omelet_perfection_complexity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PROFESSIONAL CHEF (TANGLED ROPE) — Organized practitioners see the constraint as a coordination mechanism (setting standards, enabling technique sharing, building craft identity) but also as an extraction mechanism (defending exclusivity, gatekeeping recipes, maintaining scarcity of 'true' mastery). Professional chefs benefit from the myth of complexity because it justifies their premium position. But they also incur costs: expectation of perfection, pressure to innovate, career risk if demystified as 'just technique.' Mobile exit because chefs can pivot to other specializations or drop the perfection standard.
constraint_indexing:constraint_classification(omelet_perfection_complexity, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational perspective, achieving consistent results in rapid heat transfer systems has objective constraints: Maillard reaction onset temperature (155°C), protein denaturation kinetics, thermal diffusivity of dairy products, pan geometry effects on heat distribution. These are physical limits, not social constraints. But the structural data (high suppression, beneficiaries/victims, theater ratio > 0.60) indicates this is not a mountain — the analyst risks naturalizing a contingent institutional arrangement (the perfection myth) as an immutable physical law. The actual thermodynamics are simple; the constraint's extracted complexity is social.
constraint_indexing:constraint_classification(omelet_perfection_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omelet_perfection_complexity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(omelet_perfection_complexity, TR),
    TR >= 0.70.

:- end_tests(omelet_perfection_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts attention, time, and psychological energy from practitioners, but extraction is not as severe as pure snare dynamics would suggest. Much of the perceived complexity is self-imposed through perfection standards. The extraction mechanism is primarily psychological (aspiration that exceeds objective need) rather than coercive. Suppression (0.52): Moderate-high. Barriers to entry are real but not insurmountable: specialized knowledge exists but is increasingly accessible; cultural gatekeeping persists but is eroding; failure is psychologically costly but not materially catastrophic. Theater ratio (0.68): High and rising. The constraint's primary function has shifted from technique transmission to performative display. Cooking shows, Instagram, culinary competition culture emphasize presentation and aspiration over nutritional adequacy or ease. The theater has increased as demystification advances — the more the physics becomes clear, the more the myth must emphasize intuition, tradition, and aesthetic refinement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals perspectival inversion: the novice sees snare (pure extraction, trapped goal, infinite regress), while the institution sees rope (pure coordination, enabling knowledge transfer). The gap indicates asymmetric information: the institution knows the task is simple and maintains complexity through gatekeeping; the novice internalizes the myth and experiences it as genuine difficulty. The demystification movement's existence is itself evidence of the extraction mechanism — the movement would not exist if the constraint were truly about inherent complexity. Its emergence (perspective 4, scaffold) represents organized challenge to the institution's asymmetric advantage (perspective 3, rope). The professional chef (perspective 6) demonstrates that perspectives at the same power level can have opposite experiences of the same constraint: chefs benefit from the perfection myth but are also constrained by it (must maintain standards, cannot fail publicly). The traditionalist narrative (perspective 5) is piton — performative maintenance of a function that has largely atrophied. The mountain perspective (analytical) is a false summit — the constraint risks being naturalized as an immutable feature of cooking or human psychology, when the structural data reveals it as a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. Culinary institutions are beneficiaries with arbitrage exit — they can pivot to other cuisines or stop gatekeeping at zero cost. Their d is low (~0.10), producing negative chi: the constraint subsidizes them. Novice cooks are victims with trapped exit — they cannot leave the goal (learning to cook) but face infinite regress in the path. Their d is high (~0.90), producing maximum chi: they bear full extraction cost. Home cooks are constrained victims with moderate power — some agency in learning pathways but also some benefit from community. Their d is moderate (~0.65), producing moderate chi. Professional chefs have mobile exit but benefit from the myth — mixed directionality (~0.48), moderate chi. Demystification advocates have mobile exit and derive benefit from reducing suppression — low-moderate d (~0.35), low chi. The analytical observer is external (analytical/analytical) — not subject to extraction, d ≈ 0.72 by canonical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that 'impossible standards' are simultaneously real social structures and remediable through demystification. The novice's snare is not false — the trap is genuine given the information environment the novice inhabits. The institution's rope is also not false — knowledge coordination is a real function. But the gap between them is neither inevitable nor immutable. The demystification movement's scaffold perspective shows the exit path: as information barriers erode, the extraction mechanism loses force. The mandatrophy resolution is structural, not semantic: the question is not 'is this really a snare or really a rope?' but 'under what information and institutional conditions does each perspective's experience become dominant?' The piton perspective (traditionalist narrative) detects what has atrophied: the constraint's primary function (technique transmission) has been partially replaced by secondary functions (aspiration maintenance, status signaling). The false mountain perspective reveals the anti-pattern: naturalizing institutional contingency as physical law. This constraint serves as a diagnostic example of how indexical classification detects and dissolves apparent paradoxes — the French omelet is neither inherently complex nor entirely simple; complexity is contingent on institutional structures that are observable and remediable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_transfer_threshold,
    'What fraction of French omelet technique is genuinely tacit (embodied, intuitive) versus explicitly transferable through written or video instruction?',
    'Randomized controlled trial: teaching omelet technique via explicit instruction alone vs expert apprenticeship; measure success rate and time to competence',
    'If > 80% tacit: the perfection complexity is partly structural (harder to teach). If < 30% tacit: the mystique is primarily social (gatekeeping myth). If 30-80%: tangled rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_transfer_threshold, empirical, 'Proportion of French omelet technique that is tacit vs explicit').

omega_variable(
    demystification_rate_acceleration,
    'Is the demystification movement actually reducing barrier to entry and time-to-competence, or merely distributing the perfection-chasing myth more broadly?',
    'Longitudinal comparison of learning curves for home cooks pre-internet vs post-YouTube; measure competence as binary (can produce acceptable omelet) not as subjective quality rating',
    'If acceleration confirmed: scaffold sunset is real. If no acceleration: demystification is theater, constraint persists, downgrade scaffold to piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demystification_rate_acceleration, empirical, 'Whether demystification movement actually accelerates learning curves').

omega_variable(
    perfection_standard_cultural_variation,
    'Does the perfection myth vary significantly across culinary traditions, or is the push toward impossible standards universal?',
    'Comparative ethnographic analysis: study home cooking practices and aspiration levels in French tradition vs Japanese tamagoyaki vs American scrambled vs Spanish tortilla cultures',
    'If cultural variation is high: constraint is contingent institutional practice, not psychological universal. If universal: constraint reflects deeper psychological tendency (sunk cost, goal escalation), altering piton/scaffold analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perfection_standard_cultural_variation, empirical, 'Cultural variation in omelet perfection standards').

omega_variable(
    satisfaction_threshold_gap,
    'What is the gap between the objective quality threshold for a ''good enough'' omelet and the subjective quality threshold practitioners aspire to?',
    'Blind taste tests: distinguish expert evaluation (nutritional adequacy, safety, flavor) from practitioner self-evaluation (perceived match to aesthetic ideal)',
    'If gap > 0.4 (on 0-1 scale): large extraction mechanism through perpetual inadequacy feeling. If gap < 0.2: mastery is achievable, less snare-like. Shapes theater ratio interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(satisfaction_threshold_gap, empirical, 'Gap between objective quality adequacy and subjective aspiration level').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(omelet_perfection_complexity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omelet_tr_t0, omelet_perfection_complexity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(omelet_tr_t15, omelet_perfection_complexity, theater_ratio, 15, 0.62).
narrative_ontology:measurement(omelet_tr_t30, omelet_perfection_complexity, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(omelet_be_t0, omelet_perfection_complexity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(omelet_be_t15, omelet_perfection_complexity, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(omelet_be_t30, omelet_perfection_complexity, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(omelet_perfection_complexity, information_standard).
narrative_ontology:affects_constraint(omelet_perfection_complexity, aspiration_gap_dynamics).
narrative_ontology:affects_constraint(omelet_perfection_complexity, culinary_gatekeeping_hierarchy).

% DUAL FORMULATION NOTE:
% The French omelet paradox decomposes into two distinct constraints: (1) the actual thermodynamic/technique complexity (low extractiveness, ~0.12); (2) the social myth of perfection (moderate extractiveness, ~0.38, this story). These are linked: the myth persists because the underlying task IS genuinely simple, creating psychological pressure to internalize external standards as intrinsic difficulty. The upstream thermodynamic constraint has near-zero extractiveness and would classify as rope or mountain; this story focuses on the institutional extraction mechanism that weaponizes the simplicity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(omelet_perfection_complexity, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
