% ============================================================================
% CONSTRAINT STORY: attachment_system_dysregulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attachment_system_dysregulation, []).

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
 *   constraint_id: attachment_system_dysregulation
 *   human_readable: Attachment System Dysregulation and Relational Extraction
 *   domain: interpersonal_psychology/developmental_trauma
 *
 * SUMMARY:
 *   Attachment system dysregulation describes a constraint that operates
 *   within intimate relationships where one agent's identity is fused with
 *   the role of meeting another agent's emotional and relational needs, often
 *   at the cost of their own autonomy, reality-testing, and authentic
 *   self-expression. This constraint exhibits the full range of Deferential
 *   Realism classification types from different structural positions. The
 *   same dysregulated relationship pattern — where one agent bears
 *   disproportionate psychological labor while the other benefits from
 *   continued availability and responsiveness — appears as pure extraction
 *   (Snare), a mixed coordination-extraction hybrid (Tangled Rope), a
 *   coordination mechanism (Rope), a temporary problem being solved by
 *   therapeutic intervention (Scaffold), a degraded caregiving ritual
 *   maintained by institutional inertia (Piton), or an immutable law of human
 *   development (Mountain), depending on the observer's structural position
 *   relative to the extraction flow. The constraint's extractiveness (0.58)
 *   reflects that the primary beneficiary captures emotional and relational
 *   resources during an extended period, while the attached agent's costs
 *   accumulate. The theater ratio (0.65) indicates that the dysregulation is
 *   partly concealed through performative reconciliation cycles and
 *   narratives of mutual care that mask asymmetric psychological labor. The
 *   suppression (0.72) reflects both structural barriers (economic
 *   dependency, social isolation, geographic constraint) and internalized
 *   barriers (identity fusion, diminished sense of alternatives, catastrophic
 *   thinking about separation).
 *
 * KEY AGENTS:
 *   - Attached Agent: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with the attachment role; bearers of disproportionate emotional labor and surveillance burden
 *   - Attachment Figure: Primary beneficiary (institutional/arbitrage) — receives sustained emotional availability, care, and responsiveness; has exit options through alternative attachment sources or detachment
 *   - Secondary Witnesses (Family/Close Others): Secondary victims/witnesses (moderate/constrained) — observe both coordination function and extraction mechanism; constrained by family obligation and identity; bear costs of systemic dysregulation
 *   - Therapeutic Intervention (Therapists, Mentors, Support Groups): Organized agents (organized/mobile) — perceive dysregulation as temporary problem with sunset clause; building secure attachment pathways and earned security alternatives
 *   - Institutional Caregiving System (Schools, Social Services, Medical): Institutional actors (institutional/constrained) — maintain performative attachment-based care rituals; see their own processes as degraded but maintained through regulatory inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent psychosocial patterns as immutable laws of human development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attachment_system_dysregulation, 0.58).
domain_priors:suppression_score(attachment_system_dysregulation, 0.72).
domain_priors:theater_ratio(attachment_system_dysregulation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attachment_system_dysregulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(attachment_system_dysregulation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(attachment_system_dysregulation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attachment_system_dysregulation, tangled_rope).
narrative_ontology:human_readable(attachment_system_dysregulation, "Attachment System Dysregulation and Relational Extraction").
narrative_ontology:topic_domain(attachment_system_dysregulation, "interpersonal_psychology/developmental_trauma").

domain_priors:requires_active_enforcement(attachment_system_dysregulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attachment_system_dysregulation, primary_attachment_figure).
narrative_ontology:constraint_victim(attachment_system_dysregulation, attached_agent_emotional_autonomy).
narrative_ontology:constraint_victim(attachment_system_dysregulation, relationship_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTACHED AGENT (SNARE) — Structurally mobile (could relocate, has income options, legal protections) but identity-fused with the relationship. The agent's self-concept is constituted through the attachment role ('the loyal one,' 'the caregiver,' 'the one who understands them'). Exit would require not just leaving but becoming a different person. Maximum experienced extraction because the identity lock prevents exercising structural mobility. The binding mechanism is cognitive rather than material.
constraint_indexing:constraint_classification(attachment_system_dysregulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: SECONDARY WITNESS / FAMILY SYSTEM (TANGLED ROPE) — Other family members or close associates witness both the coordination function (the attachment figure and attached agent do coordinate care, shared resources, emotional support) and the extractive mechanism (one agent bears disproportionate psychological labor, surveillance, emotional regulation burden). These witnesses are constrained by family obligation and identity; they benefit from family stability but bear costs of the dysregulation.
constraint_indexing:constraint_classification(attachment_system_dysregulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ATTACHMENT FIGURE (ROPE) — Benefits from continued availability and emotional responsiveness of the attached agent. Experiences the constraint as coordination: sustaining the relationship enables their own emotional regulation, care, and social functioning. The beneficiary has arbitrage options (can shift attachment to other agents or sources). Net beneficiary — extraction flows toward them; they perceive this as legitimate coordination.
constraint_indexing:constraint_classification(attachment_system_dysregulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: THERAPEUTIC INTERVENTION / ORGANIZED CARE (SCAFFOLD) — Therapists, mentors, support groups, and trauma-informed practitioners see attachment dysregulation as a temporary coordination failure with a sunset clause. Secure attachment formation, earned security, and therapeutic re-parenting are building alternative attachment pathways that bypass the dysregulated primary relationship. The intervention perceives low effective extraction because therapeutic agents have agency and see a concrete exit path: the goal is attachment repair or conscientious separation.
constraint_indexing:constraint_classification(attachment_system_dysregulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: INSTITUTIONAL CAREGIVING SYSTEM (PITON) — Schools, medical systems, and social services maintain performative attachment-based care rituals: home visits, parental involvement requirements, family-centered planning. These institutions persist largely through regulatory inertia and ideology ('family first,' 'blood is thicker') despite evidence that dysregulated primary attachments often require institutional intervention and supported separation. The institutional system sees its own process as degraded — maintained because alternatives haven't fully replaced it, not because family-based care is always therapeutic.
constraint_indexing:constraint_classification(attachment_system_dysregulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a universalized perspective, attachment dysregulation appears as an immutable feature of human relational biology: early attachment templates are fixed, identity is constituted through primary relationships, and separation inevitably causes psychological harm. This perspective naturalizes contingent psychosocial patterns as laws of development. However, the structural data contradicts the mountain classification — the engine's false summit detector identifies this as naturalization of what are actually flexible neurobiological and social processes amenable to intervention and conscious restructuring.
constraint_indexing:constraint_classification(attachment_system_dysregulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attachment_system_dysregulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attachment_system_dysregulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attachment_system_dysregulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attachment_system_dysregulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attachment_system_dysregulation, TR),
    TR >= 0.70.

:- end_tests(attachment_system_dysregulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary beneficiary captures emotional and relational resources while the attached agent bears disproportionate psychological labor, monitoring, and identity-subordination costs. The extractiveness is not as severe as pure predation (Snare maximum ~0.90) because genuine coordination functions exist — the relationship does provide care and emotional regulation to both parties. The value reflects that much of the 'extraction' is organized around a coordination function (sharing emotional labor, mutual support) that is genuine but asymmetric. Suppression (0.72): High. Significant barriers to exit include economic dependency, social isolation through the attachment figure's control of external relationships, internalized beliefs about the agent's responsibility for the attachment figure's wellbeing, identity fusion making exit psychologically catastrophic, and diminished contact with reality-testing relationships. Structural barriers (material dependency) combine with internalized barriers (cognitive capture, identity lock) to create high total suppression. Theater ratio (0.65): Moderate-high. The dysregulation is concealed through narrative frameworks ('they need me,' 'nobody else could handle them,' 'we're a team') and performative reconciliation cycles that temporarily restore perceived closeness and shared purpose. The theatrical component masks the underlying extraction mechanism — intermittent reinforcement creates the appearance of mutual care punctuated by crisis.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between the beneficiary's experience (Rope: a coordination mechanism they depend on for emotional regulation) and the victim's experience (Snare: pure extraction with cognitive barriers to exit) and the observer's risk (Mountain: mistaking a contingent pattern for an immutable law). The identity-lock exit option creates a diagnostic signal: the attached agent is not materially trapped but cognitively captured. If the identity frame shifted — if the agent could reimagine themselves as separate, autonomous, and worthy of care outside the relationship — the classification would change. The gap reveals that the binding mechanism is not material constraint but internalized identity fusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position: power level, exit options, and relationship to the extraction flow. The pipeline computes d from these parameters. The attachment figure as beneficiary with arbitrage options experiences low d (~0.15), producing negative or minimal effective extraction (χ). The attached agent as victim with identity_locked exit experiences high d (~0.89), producing high experienced extractiveness via f(d). Secondary witnesses as moderate agents with constrained exit experience moderate d (~0.55). The identity-lock exit option is critical here: the attached agent is structurally mobile (could relocate, has income options, legal protections) but cognitively trapped by identity fusion with the relationship role. This creates a perspectival signal unique to identity-locked constraints: the agent has more structural mobility than a trapped agent would, but less functional freedom than a constrained agent would, because the binding mechanism is cognitive rather than material. The piton classification derives from the theater ratio gate rather than from high χ. The mountain classification at the analytical context is perspectival — the engine's false summit detector identifies it as naturalization of a flexible psychosocial pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that attachment dysregulation is genuinely a Tangled Rope — it contains both a coordination function (emotional regulation, shared care) AND asymmetric extraction (disproportionate psychological labor, identity subordination, surveillance). The key insight is that identity-locked agents experience this hybrid as pure Snare (because their identity frame makes the coordination benefit invisible — they see only their obligation) while institutional beneficiaries experience it as Rope (because they perceive the coordination function as genuine). The resolution is not 'which type is correct' but understanding that the constraint's structure contains both elements. The therapy/intervention goal is to decouple the coordination function (emotional regulation, mutual care) from the extraction mechanism (identity subordination, asymmetric labor) — enabling the attached agent to access secure attachment without the extraction. The scaffold perspective confirms this is possible: secure secondary attachments, earned security development, and therapeutic re-parenting demonstrate that the coordination and extraction are separable. The false summit (mountain perspective) naturalizes what is actually a changeable psychosocial arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'Is the attached agent''s inability to exit due to internalized identity-fusion or to genuine material/social barriers?',
    'Post-separation trajectory analysis: if suppression persists after material barriers are removed (relocation, financial independence established, legal protections secured), the suppression is partially internalized. If suppression declines markedly after material barrier removal, the constraint was primarily structural.',
    'If identity-locked: the constraint''s effective suppression is higher than the structural measure suggests — the agent carries the suppression internally and remains vulnerable to re-entanglement. If constrained: standard exit barriers suffice for classification; identity lock is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Whether suppression is identity-fused or structurally material').

omega_variable(
    coordination_function_authenticity,
    'Does the dysregulated attachment actually coordinate care and emotional regulation, or is the ''coordination'' function purely secondary to the extraction mechanism?',
    'Behavioral analysis of shared activities and resource exchanges; controlled observation of relationship dynamics when identity-lock is temporarily bracketed (therapy context, crisis response); assessment of whether decoupling care provision from identity would preserve the coordination functions.',
    'If genuine coordination exists: constraint remains Tangled Rope (mixed coordination and extraction). If coordination is epiphenomenal to extraction: constraint should be reclassified as Snare. This determines whether the therapeutic goal is relationship repair or conscientious separation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether attachment dysregulation contains genuine coordination or only extraction').

omega_variable(
    intergenerational_transmission_inevitability,
    'Is attachment dysregulation necessarily transmitted across generations, or are intervention points that break the cycle?',
    'Longitudinal studies of children who experienced dysregulated primary attachments but received secure secondary attachments (foster care, mentorship, therapy); measurement of earned security development; tracking of parenting outcomes in next generation.',
    'If transmission is inevitable: the mountain perspective gains credence — attachment patterns are fixed developmental templates. If intervention points exist: the scaffold perspective is confirmed — secure attachment formation and therapeutic re-parenting are real pathways, making the constraint structurally changeable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_transmission_inevitability, empirical, 'Whether attachment dysregulation is intergenerationally invariant').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'What proportion of the measured suppression (0.72) is structural (economic dependency, geographic isolation, legal barriers) versus internalized (belief that one deserves the treatment, isolation from reality-testing contacts, identity fusion)?',
    'Qualitative analysis of cognitive patterns preventing exit: catastrophic thinking about separation, diminished sense of self, reduced contact with relationships outside the dyad. Quantitative measurement of internal versus external barriers listed by the agent.',
    'If primarily structural: standard material support (housing, income, legal protection) may suffice. If significantly internalized: therapeutic work on identity deconstruction and reality-testing becomes critical, and risk of re-entanglement after material exit remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Proportion of suppression that is internalized versus structural').

omega_variable(
    cyclical_vs_linear_dysregulation_trajectory,
    'Does attachment dysregulation follow a cyclical pattern (tension-incident-reconciliation-calm-tension) that reinforces the identity lock through intermittent reinforcement, or does it show linear degradation?',
    'Time-series analysis of relationship quality measures; tracking of reconciliation cycles and their emotional/behavioral signatures; assessment of whether the intermittent reinforcement pattern itself functions as the extraction mechanism.',
    'If cyclical: the intermittent reinforcement is itself the extraction mechanism, and measurements should show oscillating extractiveness and theater_ratio values. The piton classification (maintained through inertia and theater) is then specifically the performance of reconciliation cycles. If linear: the suppression and extraction accumulate monotonically, and the constraint shows degradation without recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyclical_vs_linear_dysregulation_trajectory, empirical, 'Whether dysregulation follows cyclical or linear trajectory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attachment_system_dysregulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attach_tr_t0, attachment_system_dysregulation, theater_ratio, 0, 0.5).
narrative_ontology:measurement(attach_tr_t3, attachment_system_dysregulation, theater_ratio, 3, 0.58).
narrative_ontology:measurement(attach_tr_t6, attachment_system_dysregulation, theater_ratio, 6, 0.65).
narrative_ontology:measurement(attach_tr_t9, attachment_system_dysregulation, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(attach_be_t0, attachment_system_dysregulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attach_be_t3, attachment_system_dysregulation, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(attach_be_t6, attachment_system_dysregulation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(attach_be_t9, attachment_system_dysregulation, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attachment_system_dysregulation, attachment_coordination).
narrative_ontology:boltzmann_floor_override(attachment_system_dysregulation, 0.1).
narrative_ontology:affects_constraint(attachment_system_dysregulation, identity_subordination_in_relationships).
narrative_ontology:affects_constraint(attachment_system_dysregulation, intermittent_reinforcement_extraction).
narrative_ontology:affects_constraint(attachment_system_dysregulation, intergenerational_trauma_transmission).

% DUAL FORMULATION NOTE:
% Attachment system dysregulation decomposes into multiple structurally distinct constraints depending on the observable selected. This story addresses the primary relationship dynamic (identity fusion + emotional labor extraction). Downstream constraints include the specific mechanisms: intermittent reinforcement cycles (identity_locked agents are particularly vulnerable to variable reward schedules), identity subordination in professional/relational contexts (how identity lock generalizes beyond the primary relationship), and intergenerational transmission (whether dysregulation patterns persist in offspring of dysregulated dyads). Each has its own ε value reflecting its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attachment_system_dysregulation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
