% ============================================================================
% CONSTRAINT STORY: identity_internalization_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_identity_internalization_dynamics, []).

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
 *   constraint_id: identity_internalization_dynamics
 *   human_readable: Identity Internalization Dynamics: Cognitive Binding Through Self-Concept Fusion
 *   domain: psychology/cognition/social_structures
 *
 * SUMMARY:
 *   Identity internalization dynamics represent a distinct binding mechanism
 *   where agents become structurally mobile yet functionally trapped because
 *   their self-concept — their identity — has become fused with the
 *   constraint. This constraint operates across domains: professional
 *   identity (surgeons cannot imagine themselves not-surgeons), relational
 *   identity (caregivers fused with caregiving roles), ideological identity
 *   (believers whose worldview makes apostasy literally unthinkable), and
 *   institutional identity (organizations that have become the policies they
 *   were created to oversee). The mechanism is cognitive rather than
 *   material: unlike trapped agents who face physical barriers to exit,
 *   identity-locked agents face an internal barrier — exit would require
 *   becoming a different person. The constraint extracts real costs (limited
 *   autonomy, constrained behavior, cognitive closure) while maintaining
 *   coordination function (role clarity, social coherence, institutional
 *   stability). The theater ratio increases over time as the original
 *   coordination function degrades and role performance becomes increasingly
 *   performative — identities persist not because they solve problems but
 *   because agents have internalized them as immutable self-definitions.
 *
 * KEY AGENTS:
 *   - Identity-Locked Agents: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused; experience maximum suppression because they enforce constraint against themselves
 *   - Identity Enforcers: Primary beneficiaries (institutional/arbitrage) — professional organizations, cultural institutions, family systems that benefit from internalized role compliance without active enforcement costs
 *   - Partially Aware Agents: Secondary victims (moderate/constrained) — perceive both coordination function and extraction; have limited agency but visible exit costs
 *   - Identity Liberation Movements: Organized actors (organized/mobile) — therapeutic communities, consciousness-raising networks, social movements working to make identity contingency visible
 *   - Cultural Historians: Analytical observers (analytical/constrained) — identify degraded coordination function and high theater in many identity-internalization constraints
 *   - Essentialist Philosophers: False-summit observers (analytical/analytical) — risk naturalizing contingent institutional identities as immutable laws of cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(identity_internalization_dynamics, 0.58).
domain_priors:suppression_score(identity_internalization_dynamics, 0.62).
domain_priors:theater_ratio(identity_internalization_dynamics, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(identity_internalization_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(identity_internalization_dynamics, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(identity_internalization_dynamics, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(identity_internalization_dynamics, tangled_rope).
narrative_ontology:human_readable(identity_internalization_dynamics, "Identity Internalization Dynamics: Cognitive Binding Through Self-Concept Fusion").
narrative_ontology:topic_domain(identity_internalization_dynamics, "psychology/cognition/social_structures").

domain_priors:requires_active_enforcement(identity_internalization_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(identity_internalization_dynamics, identity_enforcers).
narrative_ontology:constraint_beneficiary(identity_internalization_dynamics, role_gatekeepers).
narrative_ontology:constraint_victim(identity_internalization_dynamics, identity_locked_agents).
narrative_ontology:constraint_victim(identity_internalization_dynamics, cognitive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-FUSED AGENT (SNARE) — The agent's self-concept is constituted through the constraint. They cannot imagine themselves outside the role/identity the constraint assigns them. Structurally mobile (could change jobs, leave communities, shift beliefs) but identity-fused such that exit is psychologically unthinkable. Maximum experienced extraction because the binding is internal — the agent enforces the constraint against themselves. Zero perceived alternatives.
constraint_indexing:constraint_classification(identity_internalization_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: PARTIALLY AWARE AGENT (TANGLED ROPE) — Agent perceives both the coordination function (genuine benefits from role clarity, social belonging, career structure) and the extraction (limitations on autonomy, narrow permitted behaviors, identity constraints). Exit costs are high but visible. Some agency, some benefit — mixed experience that creates perspective on the hybrid nature of the constraint.
constraint_indexing:constraint_classification(identity_internalization_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IDENTITY ENFORCER (ROPE) — Institutional actors (professional organizations, cultural institutions, family systems, ideological communities) benefit from internalized role compliance. They experience the constraint as pure coordination: agents regulate their own behavior according to internalized identity norms, reducing enforcement costs. Benefits flow toward enforcers; perceived extraction is minimal from their vantage point.
constraint_indexing:constraint_classification(identity_internalization_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CULTURAL HISTORIAN (PITON) — Long-term view reveals that many identity internalization constraints have degraded from their original coordination function. Professional licensing that once served quality assurance now serves gatekeeping theater. Family role prescriptions that coordinated household economics now perform inherited patterns with minimal function. Gender norms that organized reproduction now persist through cultural inertia. The constraint's theater_ratio is high — much performative identity maintenance substitutes for real function.
constraint_indexing:constraint_classification(identity_internalization_dynamics, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IDENTITY LIBERATION MOVEMENT (TANGLED ROPE) — Organized agents (social movements, therapeutic communities, consciousness-raising networks) see identity internalization as both a coordination problem (identities do coordinate social roles) and an extraction mechanism (identities constrain autonomy and serve dominant groups). They work to make identity contingency visible — that identities are constructed, not inherent. Moderate power and exit options enable agency; effectiveness limited by counter-enforcement.
constraint_indexing:constraint_classification(identity_internalization_dynamics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ESSENTIALIST VIEW (MOUNTAIN) — From a civilizational analytical position, identity internalization might appear immutable — humans necessarily have identities, necessarily internalize social roles, necessarily experience constraint from identity commitments. The binding appears as a law of cognition/psychology. However, the structural data contradicts pure mountain classification. The constraint's extractiveness (0.58), suppression (0.62), and beneficiary/victim declarations reveal that much of what appears 'natural' is contingent institutional machinery maintained through enforcement. False summit detection should flag this perspective as naturalization.
constraint_indexing:constraint_classification(identity_internalization_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(identity_internalization_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(identity_internalization_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(identity_internalization_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(identity_internalization_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(identity_internalization_dynamics, TR),
    TR >= 0.70.

:- end_tests(identity_internalization_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising over time (0.28 → 0.58). Initial extractiveness is low because agents entering identity-locked constraints often benefit from role clarity, social belonging, and institutional support. Over time, extractiveness rises as the constraint's function degrades and identity internalization becomes increasingly rigid. By biographical maturity, agents bear full suppression costs while perceiving diminishing coordination benefits. Suppression (0.62): High and enduring. Identity-locked suppression is particularly severe because it is internalized — agents don't perceive external barriers but rather perceived immutability ('this is who I am'). Unlike material suppression (economic barriers, legal prohibition) which can be overcome by removing barriers, identity-locked suppression persists because the agent carries it internally. Theater ratio (0.55): Moderate and rising (0.35 → 0.55). Many identity-internalization constraints begin with genuine coordination function (professional licensing ensures quality, family roles organize childcare, gender norms coordinate reproduction). Over generations, the coordination function degrades as contexts change (professional licensing now primarily gates competition rather than ensuring quality; childcare is professionalized; reproduction is decoupled from gender). Role performance persists through cultural inertia and identity internalization rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The magnitude of perspectival disagreement reveals the constraint's extraction mechanism. Enforcers classify as rope (pure coordination); locked agents classify as snare (pure extraction). This gap is larger than most tangled_rope constraints because the mechanism is so asymmetric: internalized compliance requires zero enforcement cost from institutional actors while producing maximum suppression for locked agents. The gap is also revealing because it shows that the same structural phenomenon can be genuinely experienced as coordination (by enforcers who benefit from self-regulating agents) and as extraction (by agents who have become locked into roles). Neither perspective is false — they are both structurally accurate from their positions. The constraint's true nature is revealed only when viewed from multiple positions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Identity internalization extracts by making agents internalize their constraint: they do to themselves what external enforcers would need to do. This is maximally efficient extraction because the target enforces against themselves. The beneficiary (identity enforcer) gets compliance without paying enforcement costs. The victim (identity-locked agent) pays suppression costs without perceiving an external enforcer to blame — they blame themselves. This inversion is why identity-locked suppression is so durable: agents believe they cannot exit because of who they are, not because of structural barriers. Overcoming identity-locked suppression requires not removing barriers (though that helps) but reconstructing identity itself, which is psychologically destabilizing. The constraint's extractiveness rises over time because initial coordination benefits degrade (professional licensing no longer improves quality; family roles no longer organize necessary labor; gender norms no longer predict reproductive outcomes) while identity internalization deepens, leaving pure extraction without coordination justification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in identity internalization dynamics is the risk of misclassifying internalized extraction as pure coordination (rope). The constraint IS coordination in the sense that internalized role compliance does coordinate social behavior. But it is also extraction because agents are locked into roles that serve institutional interests more than their own. The tangled_rope classification resolves this: the constraint genuinely coordinates behavior (beneficiary perspective), genuinely extracts resources/autonomy (victim perspective), and genuinely requires active enforcement (through socialization, identity formation, norm reinforcement). The theater ratio rising over time is diagnostic: if the constraint were pure coordination (rope), theater should be minimal and stable. Rising theater signals degraded function — role performance persists through cultural momentum and identity internalization, not through coordination necessity. The analytical perspective risks a false summit (mountain) by naturalizing identity as immutable law rather than recognizing it as contingent institutional machinery. The framework prevents this by requiring that mountain classifications include accessibility_collapse ≥ 0.85, resistance ≤ 0.15, and emerges_naturally = true. Identity internalization is none of these — it emerges through socialization (not naturally), it has high resistance (identity liberation movements successfully create contingency awareness), and accessibility to exit is not collapsed (many agents do leave identities, though at high psychological cost).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'At what threshold does high-cost exit (constrained) transform into identity-fused exit (identity_locked)? Is the boundary sharp or continuous?',
    'Longitudinal study: track agents attempting identity exit; measure whether suppression persists after removing structural barriers (job loss, social ostracism, etc.). If suppression continues post-barrier-removal, classification shifts toward identity_locked.',
    'If boundary is sharp: perspectival gap reveals identity-fusion as a distinct binding mechanism. If continuous: identity_locked is a matter of degree, not kind — complicates threshold-based classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Boundary between constrained and identity_locked exit').

omega_variable(
    internalization_mechanism_causality,
    'Does identity internalization cause role compliance, or does prolonged role compliance cause identity internalization? Does the causal direction matter for classification?',
    'Developmental research: track identity formation across populations with different role assignment timings; analyze whether early socialization into roles produces stronger identity fusion than later role adoption.',
    'If internalization causes compliance: the constraint is fundamentally cognitive, and identity_locked is the primary mechanism. If compliance causes internalization: the constraint is fundamentally structural, and identity_locked is a secondary effect of prolonged extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_mechanism_causality, empirical, 'Causal direction of internalization vs role compliance').

omega_variable(
    collective_vs_individual_identity_lock,
    'Is identity lock fundamentally individual (each agent''s self-concept fused with their role) or collective (group members fused with group identity)? Are these the same mechanism?',
    'Comparative analysis: populations with individual identity fusion (craft professionals identifying as their specialty) vs collective identity fusion (tribal members identifying as tribe members). Do they show different suppression profiles, exit dynamics, or recovery trajectories?',
    'If fundamentally different: may require separate constraint stories (individual_identity_lock vs collective_identity_lock). If same mechanism at different scales: single story captures both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_vs_individual_identity_lock, conceptual, 'Whether identity lock operates at individual or collective level').

omega_variable(
    identity_contingency_as_destabilization,
    'Does making identity contingency salient (teaching that identities are constructed) reliably reduce identity-lock suppression, or does it paradoxically increase anxiety and entrenchment?',
    'Intervention studies: expose identity-locked populations to contingency framing and measure shifts in identity fusion, perceived suppression, and expressed desire for exit at multiple time points.',
    'If contingency-awareness reduces lock: liberation movements are effective and the constraint is epistemically vulnerable. If it increases entrenchment: the constraint is robust against awareness interventions, suggesting deeper psychological embedding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_contingency_as_destabilization, empirical, 'Effect of identity contingency awareness on suppression').

omega_variable(
    therapeutic_identity_reconstruction_feasibility,
    'Can agents who have become deeply identity-locked (professional identity, relational identity, ideological identity) successfully reconstruct identity post-exit without pathological outcomes?',
    'Longitudinal mental health tracking: monitor agents who exit identity-locked constraints (career changes, cult exit, divorce, ideological deprogramming); measure identity continuity, psychological stability, and reintegration timelines.',
    'If reconstruction is feasible with support: identity-locked status is reversible, and therapeutic intervention is viable. If reconstruction fails or produces persistent instability: identity locks may be durable beyond exit, complicating exit cost calculus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_identity_reconstruction_feasibility, empirical, 'Feasibility of identity reconstruction post-exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(identity_internalization_dynamics, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(identity_int_tr_t0, identity_internalization_dynamics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(identity_int_tr_t3, identity_internalization_dynamics, theater_ratio, 3, 0.45).
narrative_ontology:measurement(identity_int_tr_t6, identity_internalization_dynamics, theater_ratio, 6, 0.52).
narrative_ontology:measurement(identity_int_tr_t9, identity_internalization_dynamics, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(identity_int_be_t0, identity_internalization_dynamics, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(identity_int_be_t3, identity_internalization_dynamics, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(identity_int_be_t6, identity_internalization_dynamics, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(identity_int_be_t9, identity_internalization_dynamics, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(identity_internalization_dynamics, identity_coordination).
narrative_ontology:boltzmann_floor_override(identity_internalization_dynamics, 0.12).
narrative_ontology:affects_constraint(identity_internalization_dynamics, professional_licensing_gatekeeping).
narrative_ontology:affects_constraint(identity_internalization_dynamics, gender_role_enforcement).
narrative_ontology:affects_constraint(identity_internalization_dynamics, ideological_commitment_lock).
narrative_ontology:affects_constraint(identity_internalization_dynamics, relational_identity_fusion).
narrative_ontology:affects_constraint(identity_internalization_dynamics, organizational_identity_capture).

% DUAL FORMULATION NOTE:
% Identity internalization is a meta-constraint that operates through multiple domain-specific instantiations. Professional identity lock (surgeons), relational identity fusion (partners in abusive relationships), ideological commitment (religious believers, political ideologues), gender identity internalization, and organizational identity capture are all expressions of the same underlying mechanism: agents become locked into roles through internalization of identity. This story models the mechanism itself; domain-specific stories model particular instantiations with different ε values, beneficiary/victim declarations, and social contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(identity_internalization_dynamics, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
