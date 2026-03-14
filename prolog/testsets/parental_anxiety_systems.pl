% ============================================================================
% CONSTRAINT STORY: parental_anxiety_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parental_anxiety_systems, []).

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
 *   constraint_id: parental_anxiety_systems
 *   human_readable: Parental Anxiety Systems: Coordination of Child Safety with Extraction of Parental Identity
 *   domain: interpersonal/family/psychological
 *
 * SUMMARY:
 *   Parental anxiety systems represent a structurally mixed constraint in
 *   which genuine child safety coordination becomes entangled with parental
 *   identity maintenance and emotional regulation. The constraint exhibits
 *   the full spectrum of DR types depending on observer position: the child
 *   experiences pure extraction (snare); the anxious parent experiences
 *   genuine coordination overlaid with identity extraction (tangled rope);
 *   the healthy autonomy-respecting parent experiences pure coordination
 *   (rope); developmental psychology community sees a temporary problem with
 *   evidence-based solutions (scaffold); cultural narratives maintain the
 *   constraint through performative institutional mechanisms (piton); the
 *   civilizational analytical view risks naturalizing what is culturally
 *   contingent (false summit). The constraint is particularly diagnostically
 *   rich because it demonstrates how identity fusion transforms a legitimate
 *   functional need (child safety) into an extractive mechanism that persists
 *   even when functional justification declines. The measurements show
 *   increasing theater and extractiveness over the interval (0–15 years),
 *   indicating that as children age and actual risk decreases, the parental
 *   anxiety system becomes increasingly performative and extractive rather
 *   than functionally necessary.
 *
 * KEY AGENTS:
 *   - Child (Developmental Period): Primary victim (powerless/trapped) — structurally dependent, no exit option, bears full cost of constrained autonomy
 *   - Adult Child (Post-Independence): Secondary victim (powerless/identity_locked) — structurally mobile but cognitively trapped by internalized anxiety framing; identity constructed within the constraint
 *   - Anxious Parent: Primary beneficiary and executor (moderate/constrained) — experiences genuine safety coordination need AND identity reinforcement through anxiety management; suppression is internalized as justified vigilance
 *   - Healthy Parent Alternative: Contrast agent (moderate/mobile) — demonstrates that safety coordination can occur without extractive identity fusion; can exit the anxious identification while maintaining genuine coordination
 *   - Developmental Psychology Community: Organized reform agent (organized/constrained) — building evidence-based alternatives with sunset mechanism; constrained by cultural resistance and institutional inertia
 *   - Cultural Anxiety Narrative System: Institutional maintainer (institutional/arbitrage) — perpetuates constraint through schools, media, safety industries, liability structures; profits from parental anxiety amplification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parental_anxiety_systems, 0.58).
domain_priors:suppression_score(parental_anxiety_systems, 0.65).
domain_priors:theater_ratio(parental_anxiety_systems, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parental_anxiety_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(parental_anxiety_systems, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(parental_anxiety_systems, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parental_anxiety_systems, tangled_rope).
narrative_ontology:human_readable(parental_anxiety_systems, "Parental Anxiety Systems: Coordination of Child Safety with Extraction of Parental Identity").
narrative_ontology:topic_domain(parental_anxiety_systems, "interpersonal/family/psychological").

domain_priors:requires_active_enforcement(parental_anxiety_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parental_anxiety_systems, anxious_parent_identity_investment).
narrative_ontology:constraint_victim(parental_anxiety_systems, child_autonomy_development).
narrative_ontology:constraint_victim(parental_anxiety_systems, parental_wellbeing_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHILD AUTONOMY DEVELOPMENT (SNARE) — Structurally trapped within the parental anxiety system; bears the full cost of constrained independence and excessive supervision. Child has no exit option (dependency on parent) and no advocate within the system. Suppression is complete: the child cannot challenge the constraint without jeopardizing the attachment relationship itself. Extractiveness is maximum because the extraction mechanism (parental anxiety management) operates at the child's expense with zero coordination benefit to the child.
constraint_indexing:constraint_classification(parental_anxiety_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CHILD'S ADULT SELF (IDENTITY-LOCKED SNARE) — Even after the child reaches adulthood and structural mobility (arbitrage options), they remain bound by identity fusion with the parental anxiety system. The adult child has internalized the anxious framing ('the world is dangerous,' 'I am fragile,' 'parental vigilance = love'). Exit would require abandoning the identity constructed within the constraint. Structurally mobile but cognitively trapped. The classification is snare because the identity lock mechanism itself becomes extractive: the adult child now self-enforces the constraint that the parent once imposed.
constraint_indexing:constraint_classification(parental_anxiety_systems, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: ANXIOUS PARENT (TANGLED ROPE) — Constrained by genuine need for child safety (coordination function) but experiences relief and identity reinforcement through anxiety management (extraction function). The parent genuinely wants the child safe AND extracts identity value and emotional regulation from the role of vigilant protector. Suppression is high: parental fear feels justified ('what if something happens?'); exit costs are severe (identity dissolution, guilt). But genuine coordination exists — reasonable safety supervision is real, extraction is asymmetric overlay on legitimate function.
constraint_indexing:constraint_classification(parental_anxiety_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: HEALTHY AUTONOMY-RESPECTING PARENT (ROPE) — Parent experiences the safety coordination problem differently when not fused with anxiety identity. Child safety supervision + age-appropriate independence = pure coordination with low extraction overhead. This parent can exit the anxious identification mechanism and maintain genuine safety coordination. Extractiveness approaches rope thresholds because beneficiary and victim interests partially align: child gets both safety AND autonomy development.
constraint_indexing:constraint_classification(parental_anxiety_systems, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: DEVELOPMENTAL PSYCHOLOGY COMMUNITY (SCAFFOLD) — Organized agents (family therapists, developmental researchers, parenting educators) are building alternative frameworks for safety-autonomy integration. Sunset clause: evidence-based parenting interventions, trauma-informed practice, and attachment security research all point toward phased reduction of parental anxiety systems as children develop. The constraint has an exit path — families adopting these frameworks show measurable improvements. Theater is moderate: psychological education requires sustained engagement, but the function (transforming anxiety from extraction to coordination) is genuine.
constraint_indexing:constraint_classification(parental_anxiety_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: CULTURAL ANXIETY NARRATIVE SYSTEM (PITON) — Broader cultural framing of parenting ('helicopter parenting is vigilant love,' 'any autonomy risks child safety,' 'good parents manage anxiety through control') persists through institutional inertia despite psychological evidence against it. School surveillance, social media risk narratives, and parental guilting maintain the constraint through cultural performance rather than genuine functional necessity. Theater ratio reflects the gap between the elaborate anxiety-management performativity (safety equipment, scheduled activities, monitoring systems) and actual risk reduction. The constraint persists because institutions profit from parental anxiety (safety products, educational services, institutional liability reduction) even as the functional justification degrades.
constraint_indexing:constraint_classification(parental_anxiety_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — From a universal perspective, some parental concern is inherent to caregiving: risk detection is biologically adaptive, and parental anxiety has evolutionary roots. However, the structural data reveals this as a false summit. The biological predisposition is real, but the modern anxiety system is culturally amplified and extractively maintained. The framing 'parental anxiety is natural law' naturalizes what is actually a contingent institutional arrangement shaped by information systems, institutional incentives, and identity narratives.
constraint_indexing:constraint_classification(parental_anxiety_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parental_anxiety_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parental_anxiety_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parental_anxiety_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parental_anxiety_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parental_anxiety_systems, TR),
    TR >= 0.70.

:- end_tests(parental_anxiety_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The parental anxiety system extracts identity value, emotional regulation capacity, and control from the parent at the child's autonomy cost. The extraction intensifies as the child ages (0.35→0.58 over the interval), because the functional justification for restrictive control declines while the parent's identity investment in the vigilant-protector role may increase. Suppression (0.65): High. Child is trapped by structural dependency (young) or identity lock (adult). Parental anxiety is framed as justified concern ('I just want to keep them safe'), making suppression feel legitimate rather than coercive. Theater ratio (0.68): Moderate-high. As children age and objective risk decreases, the apparatus of parental monitoring and control becomes increasingly performative — excessive surveillance, safety protocols, and risk aversion that exceed functional necessity. The rise in theater over the interval (0.45→0.68) reflects this: the functional safety coordination declines while the anxiety management theater intensifies. Claimed type (Tangled Rope): Correct at the parental perspective level. Genuine safety coordination exists (rope function) but is asymmetrically overlaid with identity extraction (snare mechanism). The tangled rope accurately captures the mixture.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence reveals the constraint's complexity. Child sees snare; parent sees rope; analyst sees false summit; reform community sees scaffold. The gap exposes that 'parental protection' is a polysemous concept: genuine coordination vs identity-extracted anxiety vs cultural performance are three different constraints wearing the same label. The expansion of extractiveness and theater over the 15-year interval indicates the functional justification (actual child risk) is declining while the extraction mechanism (parental identity investment) is increasing. This is classic Goodhart drift: the anxiety system's original function (safety coordination) is being crowded out by its proxy metric (parental control behavior).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the anxious parent: d ≈ 0.48 (beneficiary with constrained exit). Parent benefits from identity reinforcement and anxiety relief through the constraint (low d) but constrained exit means they cannot easily abandon the anxiety-management identity (raises d to moderate). The child experiences d ≈ 0.92 (victim with no exit) when young, transitioning to d ≈ 0.85 (victim with identity-locked exit) as adult. The healthy parent alternative shows d ≈ 0.50 (symmetric: both parent and child benefit from safe autonomy development). The developmental psychology community derives d ≈ 0.35 (organized, constrained, advocating for reform) — positioned as partial beneficiary of alternative frameworks. Overrides not required because the structural relationships are clearly differentiated through beneficiary/victim declarations and exit_options variation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that parental anxiety systems are structurally mixed: genuine coordination of child safety + asymmetric extraction of parental identity + performative cultural maintenance. The tangled rope classification is stable because it captures all three components. The false summit (mountain) perspective is diagnostic: when an observer naturalizes parental anxiety as inherent to caregiving, they are obscuring the extractive identity mechanism that transforms legitimate protective instinct into identity-fused control. The scaffold perspective (developmental psychology with sunset) is real and measurable — families adopting evidence-based frameworks show reduced parental anxiety alongside improved child autonomy. The piton perspective (cultural narrative maintenance) explains why the constraint persists despite growing evidence that it is functionally unnecessary and developmentally harmful. Mandatrophy is resolved by recognizing that the constraint is not 'pure coordination being mislabeled' but 'mixed coordination and extraction being normalized as pure protection.' The decomposition into separate constraint stories (one for safety coordination, one for identity extraction, one for cultural narrative maintenance) is recommended but not required — the tangled rope single story captures the mixture accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_extraction_boundary,
    'At what point does legitimate parental protective instinct transition into identity-extracted anxiety maintenance?',
    'Longitudinal tracking of parental anxiety levels post-child-independence; measurement of identity dissolution threat when parent reduces anxiety behaviors; correlation between parental identity fusion and child autonomy delay',
    'If boundary is sharp: clear diagnostic criterion for snare vs rope classification. If boundary is fuzzy: extractive mechanism is difficult to identify and address clinically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_extraction_boundary, empirical, 'Boundary between protective instinct and identity-extracted anxiety').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.65) structural (child''s actual dependency) or internalized (child''s constructed helplessness) or both?',
    'Post-independence tracking: does suppression persist after child exits structural dependency? Measure autonomy development delay among children with anxious parents vs controls; identify separation anxiety and guilt patterns in adult children.',
    'If primarily structural: suppression is temporary and remediable by development. If primarily internalized: suppression persists after structural dependency ends, indicating identity-lock mechanism is active. Mixed mechanism requires two-phase intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized or both').

omega_variable(
    cyclical_reinforcement_pattern,
    'Is the parental anxiety cycle (tension → safety violation → panic → control → temporary calm → renewed anxiety) itself the extraction mechanism, or a side effect of identity fusion?',
    'Analysis of anxiety cycles in families: measure cycle frequency, triggering events, pattern change with therapeutic intervention; compare intermittent reinforcement schedule to operant conditioning literature on persistence.',
    'If cycle is the mechanism: short-term crisis intervention breaks the pattern. If cycle is symptom of identity lock: intervention must address identity reconstruction, not just behavior change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyclical_reinforcement_pattern, empirical, 'Whether anxiety cycles are mechanism or symptom').

omega_variable(
    institutional_profit_extraction,
    'How much of the modern parental anxiety system is maintained by institutional profit incentives (safety products, educational services, institutional liability reduction) vs genuine functional necessity?',
    'Market analysis: quantify parental safety spending growth vs actual child risk reduction; identify institutional entities benefiting from anxiety amplification; compare anxiety levels and safety spending across regulatory environments with different liability structures.',
    'If significant institutional profit: piton classification is correct; constraint persists through institutional inertia despite degraded function. If minimal institutional profit: anxiety system has genuine functional anchors; constraint is harder to dislodge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_profit_extraction, empirical, 'Institutional profit incentives maintaining anxiety system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parental_anxiety_systems, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pxs_tr_t0, parental_anxiety_systems, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pxs_tr_t5, parental_anxiety_systems, theater_ratio, 5, 0.62).
narrative_ontology:measurement(pxs_tr_t10, parental_anxiety_systems, theater_ratio, 10, 0.68).
narrative_ontology:measurement(pxs_tr_t15, parental_anxiety_systems, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(pxs_be_t0, parental_anxiety_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pxs_be_t5, parental_anxiety_systems, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(pxs_be_t10, parental_anxiety_systems, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(pxs_be_t15, parental_anxiety_systems, base_extractiveness, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parental_anxiety_systems, attachment_coordination).
narrative_ontology:boltzmann_floor_override(parental_anxiety_systems, 0.12).
narrative_ontology:affects_constraint(parental_anxiety_systems, childhood_autonomy_development).
narrative_ontology:affects_constraint(parental_anxiety_systems, parental_identity_construction).
narrative_ontology:affects_constraint(parental_anxiety_systems, cultural_risk_narrative_amplification).

% DUAL FORMULATION NOTE:
% Parental anxiety systems can be decomposed into three structurally distinct constraints: (1) safety_coordination_genuine (ε≈0.15, Rope) — legitimate coordination of child protection; (2) parental_anxiety_identity_extraction (ε≈0.72, Snare) — identity-fused anxiety management at child's autonomy cost; (3) cultural_anxiety_performance (ε≈0.58, Piton) — institutional maintenance of anxiety narratives through performative safety systems. The single tangled_rope story presented here captures the mixture at the family level. Decomposition into separate stories is recommended for detailed analysis of each mechanism's operation and reversibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parental_anxiety_systems, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
