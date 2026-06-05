% ============================================================================
% CONSTRAINT STORY: shitty_feedback_handling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shitty_feedback_handling, []).

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
 *   constraint_id: shitty_feedback_handling
 *   human_readable: The Protocol for Handling Shitty Feedback
 *   domain: social/professional
 *
 * SUMMARY:
 *   The protocol for handling shitty feedback is a ubiquitous social
 *   constraint in organizational and professional contexts where
 *   feedback-delivery skills are rare, but feedback-giving is
 *   institutionalized as a management practice. The constraint exhibits a
 *   tension between its coordination function (organizations need mechanisms
 *   to communicate performance expectations and enable skill development) and
 *   its extraction function (authority figures benefit from a low-skill
 *   mechanism that maintains asymmetric power while appearing benign). Shitty
 *   feedback itself — vague criticism, emotional loaded delivery, lack of
 *   specific actionable direction, timing that humiliates rather than
 *   educates — is not inherent to human communication; it persists because it
 *   serves the structural interests of those who give it while suppressing
 *   the options of those who receive it. The constraint demonstrates how a
 *   coordination mechanism can be captured by extraction without changing its
 *   formal label or institutional legitimacy. The feedbacker experiences it
 *   as simple communication; the feedback-recipient experiences it as
 *   suppression and power abuse; the organization experiences it as both a
 *   learning mechanism and a degradation of its epistemic capacity. The
 *   theater ratio (0.58) reflects the performative nature of formal feedback
 *   protocols: managers go through scheduled feedback conversations to
 *   satisfy HR requirements and create documentation for legal protection,
 *   often without genuine intention to develop the recipient's skills.
 *
 * KEY AGENTS:
 *   - Feedback Recipients: Primary victims (powerless/trapped) — bear psychological and career costs of shitty feedback; cannot safely refuse or challenge without jeopardy
 *   - Managers/Authority Figures: Primary beneficiaries (institutional/arbitrage) — benefit from low-skill coordination mechanism; maintain power asymmetry; can exit to other roles
 *   - Organizations/HR Systems: Secondary actor (institutional/constrained) — require feedback mechanisms for coordination but also extract through suppressed voice and defensive culture
 *   - Feedback-Skills Movement: Organized beneficiary (organized/mobile) — profit from demand for training, coaching, frameworks; also provide genuine value
 *   - Organizational Learning Capacity: Victim collective (powerless/trapped) — abstract commons bearing extraction cost without representation
 *   - Psychological-Safety Frameworks: Alternative pathway (organized/mobile) — emerging sunset mechanism offering escape from shitty-feedback extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shitty_feedback_handling, 0.52).
domain_priors:suppression_score(shitty_feedback_handling, 0.65).
domain_priors:theater_ratio(shitty_feedback_handling, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shitty_feedback_handling, extractiveness, 0.52).
narrative_ontology:constraint_metric(shitty_feedback_handling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(shitty_feedback_handling, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shitty_feedback_handling, tangled_rope).
narrative_ontology:human_readable(shitty_feedback_handling, "The Protocol for Handling Shitty Feedback").
narrative_ontology:topic_domain(shitty_feedback_handling, "social/professional").

domain_priors:requires_active_enforcement(shitty_feedback_handling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shitty_feedback_handling, authority_figures).
narrative_ontology:constraint_beneficiary(shitty_feedback_handling, feedback_avoiders).
narrative_ontology:constraint_beneficiary(shitty_feedback_handling, organizational_hierarchy).
narrative_ontology:constraint_victim(shitty_feedback_handling, feedback_recipients).
narrative_ontology:constraint_victim(shitty_feedback_handling, organizational_learning_capacity).
narrative_ontology:constraint_victim(shitty_feedback_handling, psychological_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEEDBACK RECIPIENT (SNARE) — Structurally trapped by organizational hierarchy and employment dependency. Receives poorly-articulated criticism (shitty feedback) without meaningful recourse. Cannot exit the feedback relationship without career jeopardy. High suppression of alternative response paths: silence/accept or lose standing. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.59.
constraint_indexing:constraint_classification(shitty_feedback_handling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZATION / INSTITUTIONAL LEARNING (TANGLED ROPE) — The organization structurally benefits from feedback mechanisms (coordination function: information flow, performance correction, skill development) BUT simultaneously extracts costs through shitty feedback: demoralizes recipients, suppresses dissent (suppression of alternative viewpoints), creates defensive cultures, and degrades actual learning. Active enforcement required: HR policies mandate feedback; managers are held accountable for giving it. Constrained exit for the organization — cannot abandon feedback without losing coordination function, but also cannot easily escape the shitty-feedback extraction dynamic without skill investment. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(shitty_feedback_handling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANAGERS / AUTHORITY FIGURES (ROPE) — Benefit from the protocol as a coordination mechanism: gives them a socially-acceptable framework for exercising power and controlling behavior. Shitty feedback serves as social performance ('I gave feedback, I'm managing') with minimal skill investment. Experience low suppression because they set the terms. Arbitrage exit: can move to other management roles; the protocol travels with them. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary position due to institutional power and arbitrage exit.
constraint_indexing:constraint_classification(shitty_feedback_handling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FEEDBACK-SKILLS MOVEMENT (TANGLED ROPE) — Organized actors (executive coaches, HR consultants, training vendors, agile/radical candor communities) see shitty feedback as both a coordination failure AND an extraction opportunity. They benefit from the extraction (market demand for coaching, seminars, tools to fix shitty feedback) while also coordinating genuine skill improvement. Mobile exit for sophisticated organizations — move to alternative feedback cultures (psychological safety, blameless postmortems, radical candor frameworks). d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.25. Organized power enables negotiation of terms.
constraint_indexing:constraint_classification(shitty_feedback_handling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL HR FEEDBACK PROTOCOL (PITON) — Once-functional coordination mechanism (360 reviews, performance reviews, feedback cycles) that has degraded into theatrical compliance. Theater_ratio=0.58: much activity is performative (documentation for legal protection, annual checkbox compliance) rather than functionally improving feedback quality. Managers go through motions ('I'll give feedback in the Tuesday sync') without skill or care. The formal protocol persists through institutional inertia and legal-defense requirements, not because it works. Constrained exit: cannot abandon formal feedback without litigation risk.
constraint_indexing:constraint_classification(shitty_feedback_handling, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZATIONAL LEARNING CAPACITY (SNARE) — Abstract collective good. Shitty feedback undermines organizational learning: recipients become defensive, suppress voice, hide mistakes, avoid admitting unknowing. The organization extracts from its own epistemic commons. No entity advocates for learning capacity; no voice can demand exit. Trapped by the very structure that produces shitty feedback. d≈0.94, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(shitty_feedback_handling, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: PSYCHOLOGICAL-SAFETY FRAMEWORKS (SCAFFOLD) — Organized movements (blameless postmortems, radical candor, nonviolent communication, Agile Retrospectives) offer temporary alternative coordination mechanisms with an explicit sunset: as these norms mature, the shitty-feedback protocol's extraction mechanism should lose force. Has sunset clause: psychological safety norms aim to replace command-and-control feedback with reciprocal learning. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.13. Low effective extraction because the framework acknowledges temporary status and aims to degrade the shitty-feedback extraction over time.
constraint_indexing:constraint_classification(shitty_feedback_handling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — Risk of false summit: characterizing shitty feedback as inherent to human communication ('people are bad at giving feedback, it's just how humans are'). This perspective naturalizes what is actually a contingent training/skill/incentive problem. The base metrics (ε=0.52, suppression=0.65, theater=0.58) contradict mountain classification — this is not an irreducible constraint. However, the naturalization is a real rhetorical move that legitimates inaction.
constraint_indexing:constraint_classification(shitty_feedback_handling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shitty_feedback_handling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shitty_feedback_handling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shitty_feedback_handling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shitty_feedback_handling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shitty_feedback_handling, TR),
    TR >= 0.70.

:- end_tests(shitty_feedback_handling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Shitty feedback extracts from recipients through psychological harm, suppressed voice, and defensive behavior, but not as severely as a pure snare because some feedback is genuinely useful and the constraint serves legitimate organizational coordination. The extraction has increased over time (0.35→0.52) as formal feedback protocols have proliferated without corresponding skill investment, creating more shitty feedback events. Suppression (0.65): High. Feedback recipients face substantial barriers to exit: employment dependency, career risk from challenging authority, organizational norms that frame pushback as defensive/unprofessional. The feedback relationship is framed as unidirectional (authority→recipient) with no legitimate upward accountability channel for feedback quality. Theater ratio (0.58): Moderate-high. Formal feedback processes (annual reviews, 360s, scheduled one-on-ones) are substantially performative: managers fulfill them to satisfy HR documentation requirements and legal defense needs, not necessarily to develop skills. The theater has increased over time (0.42→0.58) as compliance documentation has become more central than actual skill transfer.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here reveals how the same structural phenomenon appears benign from the authority position and extractive from the recipient position. The manager sees a coordination mechanism ('I'm communicating expectations'). The recipient sees suppression and power abuse ('I can't push back without career risk'). The organization sees both coordination and a degraded learning system. The feedback-skills industry sees an opportunity to capture extraction through coaching. The psychological-safety movement sees a temporary problem with a sunset pathway. The organization's learning capacity sees pure extraction (defensive culture suppresses voice). The naturalized view risks seeing shitty feedback as inherent to human nature ('people are just bad communicators') rather than a contingent institutional arrangement. The constraint demonstrates how indexical classification reveals the hidden extraction beneath the coordination rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Feedback recipients: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — trapped by employment and hierarchy. Managers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary due to institutional power and ability to move to other roles without changing their feedback behavior. Organization (learning): Victim + trapped → d≈0.94, f(d)≈1.40. Abstract commons cannot exit; extraction is structural. Organization (institution): Mixed position + constrained → d≈0.55, f(d)≈0.75. Constrained because they cannot fully exit feedback mechanisms without losing coordination function, but also cannot fully accept shitty feedback without organizational damage. Feedback-skills movement: Beneficiary + mobile → d≈0.40, f(d)≈0.40. Organized actors can negotiate terms and see alternative pathways; benefit from both training market AND genuine skill improvement. Psychological-safety frameworks: Mobile organized actors → d≈0.35, f(d)≈0.32. Mobile because they can shift organizational norms; low effective extraction because they aim for sunset.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disaggregating the feedback protocol into its structural components: (1) Legitimate coordination function — organizations need performance communication mechanisms — (2) Extraction function — authority figures benefit from low-skill mechanisms that maintain power asymmetry — (3) Theatrical compliance — formal processes satisfy legal/HR requirements without functional feedback quality. The mandatrophy question is: 'Is this Rope (pure coordination) or Snare (pure extraction) or something hybrid?' The structural answer is Tangled Rope: the constraint genuinely serves coordination AND genuinely extracts. The fake natural-law framing ('people are just bad at giving feedback, it's human nature') is a false mountain — shitty feedback is not inherent; it persists because the current institutional arrangements benefit those who give it and suppress those who receive it. The Scaffold perspective is real: psychological-safety frameworks offer a genuine escape pathway with a sunset clause, but the sunset only materializes if organizations actively invest in skill development and power redistribution, not just add new frameworks on top of old extraction. The fake optimism framing ('once we train managers, everything improves') is the false scaffold — if the underlying incentive structures remain unchanged, training becomes performative cover for continued extraction. The Piton perspective captures this risk: psychological-safety rhetoric becomes a new theater while shitty feedback persists in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shitty_feedback_definition_boundary,
    'Where is the boundary between ''poorly-delivered feedback'' (fixable via skill training) and ''extraction masquerading as feedback'' (structural power abuse)?',
    'Comparative analysis of feedback content, emotional valence, and outcome for recipients. Does recipient report skill gain or psychological harm? Do they change behavior or withdraw?',
    'If boundary is skill-based: constraint is Scaffold (solvable via training, sunset as skills improve). If boundary is structural power abuse: constraint is Snare (requires organizational power redistribution, not training).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shitty_feedback_definition_boundary, empirical, 'Boundary between skill-delivery failure and structural extraction').

omega_variable(
    feedback_recipient_voice_capacity,
    'Can feedback recipients safely push back or request clarification without career jeopardy? Is suppression of response options actual or performative?',
    'Study organizational cultures: do recipients who challenge feedback face retaliation? Do they report psychological safety to provide upward feedback on downward feedback quality?',
    'If safe pushback is possible: suppression is lower than 0.65, constraint reclassifies (possibly Rope). If career risk is real: suppression threshold is correct, snare classification valid for recipients.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_recipient_voice_capacity, empirical, 'Whether recipients can safely challenge feedback quality').

omega_variable(
    alternative_learning_pathway_viability,
    'Do alternative feedback mechanisms (peer feedback, self-directed learning, blameless postmortems) actually replace top-down shitty feedback, or do they exist in parallel while maintaining the extraction?',
    'Longitudinal organizational study: when psychological-safety frameworks are adopted, does downward shitty feedback actually decline? Or do both coexist with the new frameworks becoming additional theater?',
    'If alternatives are viable: Scaffold perspective is real, sunset is achievable. If both coexist: Piton risk — psychological-safety frameworks become performative cover for unchanged extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_learning_pathway_viability, empirical, 'Whether alternative feedback pathways can truly replace top-down shitty feedback').

omega_variable(
    skill_vs_incentive_asymmetry,
    'Is shitty feedback primarily a skill gap (managers lack training in constructive feedback) or an incentive misalignment (managers benefit from delivering harsh feedback to maintain power)?',
    'Intervention study: does skill training (communication workshops, feedback coaching) reduce shitty feedback quality when incentive structures remain unchanged? Do managers apply skills once trained?',
    'If skill-driven: training + practices reduce ε to ~0.25 (Scaffold). If incentive-driven: training has minimal effect, extraction persists (Snare), and requires restructuring power dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_vs_incentive_asymmetry, conceptual, 'Whether shitty feedback is driven by skill gaps or incentive misalignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shitty_feedback_handling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shit_tr_t0, shitty_feedback_handling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(shit_tr_t10, shitty_feedback_handling, theater_ratio, 10, 0.51).
narrative_ontology:measurement(shit_tr_t20, shitty_feedback_handling, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(shit_be_t0, shitty_feedback_handling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shit_be_t10, shitty_feedback_handling, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(shit_be_t20, shitty_feedback_handling, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shitty_feedback_handling, information_standard).
narrative_ontology:affects_constraint(shitty_feedback_handling, psychological_safety_commons).
narrative_ontology:affects_constraint(shitty_feedback_handling, organizational_learning_suppression).
narrative_ontology:affects_constraint(shitty_feedback_handling, power_asymmetry_in_professional_hierarchy).

% DUAL FORMULATION NOTE:
% The shitty-feedback protocol represents a bundle of structurally distinct claims: (1) feedback-delivery skill scarcity (ε≈0.15, Scaffold), (2) feedback-extraction asymmetry (ε≈0.65, Snare), (3) theatrical compliance in formal protocols (ε≈0.58, Piton), (4) organizational learning commons degradation (ε≈0.72, Snare). This story treats the integrated constraint at ε=0.52 (Tangled Rope), but individual organizational contexts may emphasize different ε values depending on whether skill investment or power restructuring is the active margin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shitty_feedback_handling, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
