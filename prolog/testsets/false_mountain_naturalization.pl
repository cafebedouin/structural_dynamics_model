% ============================================================================
% CONSTRAINT STORY: false_mountain_naturalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_false_mountain_naturalization, []).

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
 *   constraint_id: false_mountain_naturalization
 *   human_readable: False Mountain Naturalization in Social Constraint Perception
 *   domain: social_psychology/structural_power/epistemic_capture
 *
 * SUMMARY:
 *   False mountain naturalization is the cognitive mechanism by which
 *   socially constructed constraints are misperceived as natural law,
 *   preventing collective action that could transform them. This constraint
 *   operates at the intersection of epistemic capture and structural power:
 *   agents trapped in high-extraction positions waste agency on acceptance
 *   rather than transformation because they cannot perceive the constraint as
 *   mutable. The naturalization is not accidental — it is actively maintained
 *   by beneficiaries who gain from converting arbitrary power into perceived
 *   legitimacy. The constraint exhibits a diagnostic Type I error pattern:
 *   agents systematically misclassify constraints with ε > 0.15 (mutable,
 *   extractive) as ε ≈ 0.0 (immutable, natural law). Verbal naturalization
 *   patterns ('that's just how things are,' 'it's human nature,' 'some people
 *   are just like that') and behavioral resignation despite available
 *   collective action opportunities are the primary observables. The
 *   constraint is downstream of indexical_extraction_asymmetry: the
 *   asymmetric extraction creates the incentive structure for naturalization
 *   (beneficiaries naturalize to legitimize; victims naturalize to cope), and
 *   the naturalization in turn stabilizes the asymmetry by preventing
 *   resistance.
 *
 * KEY AGENTS:
 *   - Stigmatized Agents (identity_locked): Primary victims (powerless/identity_locked) — internalize naturalization; perceive social constraint as unchangeable; waste agency on acceptance rather than transformation; structurally mobile but cognitively trapped
 *   - Materially Trapped Agents: Primary victims (powerless/trapped) — face both naturalization and genuine structural barriers; naturalization compounds material entrapment by preventing formation of exit strategies
 *   - Enforcers of Social Stigma: Primary beneficiaries (institutional/arbitrage) — gain status and resource allocation advantage from naturalization maintaining hierarchy; experience constraint as coordination (shared understanding of 'natural' order)
 *   - Institutional Gatekeepers: Secondary beneficiaries and victims (moderate/constrained) — middle-management enforcers who benefit from positional authority but are trapped in enforcement role by career path dependence; mixed coordination and extraction
 *   - Consciousness-Raising Coalition: Organized resistance (organized/mobile) — building counter-narratives that denaturalize the constraint; see naturalization as temporary epistemic problem with generational sunset
 *   - Analytical Observer (False Summit): Risks reproducing naturalization at meta-level by classifying constraint as mountain ('humans naturally form hierarchies'); false summit detector will flag this as naturalization of contingent arrangement
 *   - Structural Analyst: Correct analytical classification (analytical/analytical) — recognizes genuine coordination function coexisting with asymmetric extraction; sees constraint as mutable but entrenched tangled_rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(false_mountain_naturalization, 0.58).
domain_priors:suppression_score(false_mountain_naturalization, 0.72).
domain_priors:theater_ratio(false_mountain_naturalization, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(false_mountain_naturalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(false_mountain_naturalization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(false_mountain_naturalization, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(false_mountain_naturalization, snare).
narrative_ontology:human_readable(false_mountain_naturalization, "False Mountain Naturalization in Social Constraint Perception").
narrative_ontology:topic_domain(false_mountain_naturalization, "social_psychology/structural_power/epistemic_capture").

domain_priors:requires_active_enforcement(false_mountain_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(false_mountain_naturalization, enforcers_of_social_stigma).
narrative_ontology:constraint_beneficiary(false_mountain_naturalization, institutional_gatekeepers).
narrative_ontology:constraint_victim(false_mountain_naturalization, stigmatized_agents_who_internalize_label).
narrative_ontology:constraint_victim(false_mountain_naturalization, collective_action_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STIGMATIZED AGENT (SNARE) — Identity-locked by internalized naturalization. Perceives social constraint as unchangeable natural law. Wastes agency on acceptance rather than transformation. The identity lock is cognitive: the agent has structural mobility (could organize, could exit the local context) but cannot perceive this from within the naturalized frame. High extraction experienced as inevitable rather than imposed.
constraint_indexing:constraint_classification(false_mountain_naturalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: MATERIALLY TRAPPED AGENT (SNARE) — Faces genuine structural barriers (economic dependency, geographic isolation, legal constraints) in addition to naturalization. Cannot exit even if the cognitive frame broke. The naturalization compounds material entrapment: the agent believes their structural position is natural law rather than contingent arrangement, preventing even the formation of exit strategies.
constraint_indexing:constraint_classification(false_mountain_naturalization, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: STIGMA ENFORCER (ROPE) — Benefits from naturalization maintaining social hierarchy. Experiences the constraint as coordination: shared understanding of 'natural' social order enables efficient allocation of status and resources. The naturalization is functional for this agent — it converts arbitrary power into perceived legitimacy. Low effective extraction because extraction flows toward this position.
constraint_indexing:constraint_classification(false_mountain_naturalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL GATEKEEPER (TANGLED ROPE) — Middle-management enforcers who both benefit from and are constrained by naturalization. They gain positional authority from maintaining the stigma but are also trapped in the enforcement role by career path dependence. Genuine coordination function (maintaining institutional coherence) coexists with asymmetric extraction (perpetuating arbitrary hierarchy). Constrained exit: could leave the role but at significant career cost.
constraint_indexing:constraint_classification(false_mountain_naturalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ORGANIZED RESISTANCE (SCAFFOLD) — Collective action groups (consciousness-raising movements, mutual aid networks, counter-narrative campaigns) see naturalization as a temporary epistemic problem with a sunset. They are building alternative framings that denaturalize the constraint: 'this is not natural law, this is policy, and policy can change.' The scaffold logic: as more agents recognize the constraint as contingent rather than natural, collective action becomes possible and the extraction mechanism loses force. Estimated sunset: 1-2 generations for counter-narratives to achieve critical mass in the affected population.
constraint_indexing:constraint_classification(false_mountain_naturalization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — From a detached analytical position, one might classify this as mountain: 'humans naturally form hierarchies,' 'stigma is an evolutionary adaptation,' 'some inequality is inevitable.' This is the naturalization trap at the meta-level — the analytical observer reproducing the same cognitive error the trapped agents make. The engine's false summit detector will flag this: base extractiveness (0.58) far exceeds mountain threshold (0.25), suppression (0.72) far exceeds mountain threshold (0.05), and the constraint requires active enforcement. The mountain classification is perspectival naturalization, not structural reality.
constraint_indexing:constraint_classification(false_mountain_naturalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: STRUCTURAL ANALYST (TANGLED ROPE) — The correct analytical classification. Recognizes genuine coordination function (stigma does coordinate social boundaries and resource allocation) coexisting with asymmetric extraction (naturalization prevents collective action that could redistribute power). The constraint is mutable but entrenched. Active enforcement required (stigma must be continuously performed and policed). This perspective sees both the coordination benefit (for those inside the boundary) and the extraction cost (for those outside), without naturalizing either.
constraint_indexing:constraint_classification(false_mountain_naturalization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(false_mountain_naturalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(false_mountain_naturalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(false_mountain_naturalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(false_mountain_naturalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(false_mountain_naturalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Stigmatized agents bear significant costs (wasted agency, foregone collective action, internalized shame, material disadvantage) while enforcers capture benefits (status, resource allocation priority, legitimacy). The extraction is not total (some agents resist naturalization, some contexts have weaker enforcement) but is substantial and systematic. The value reflects that naturalization prevents transformation that would redistribute power — the counterfactual without naturalization is collective action that reduces extraction. Suppression (0.72): High. Multiple mechanisms suppress alternatives: internalized identity lock (agent cannot imagine exit), social policing (enforcement of stigma), institutional gatekeeping (career and resource access conditional on compliance), and epistemic closure (counter-narratives are marginalized). Suppression is not total — consciousness-raising movements do exist and do shift frames — but is strong enough to prevent most agents from accessing denaturalization. Theater ratio (0.48): Moderate. Some enforcement is genuinely functional (maintaining group boundaries, coordinating resource allocation) but a significant portion is performative (ritualized stigma displays, virtue signaling, enforcement theater that exceeds coordination necessity). The theater has increased over the interval as naturalization has become more entrenched and enforcement has become more ritualized. The moderate value reflects that this is not a pure piton (the constraint still has real extraction function) but has substantial performative overlay.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the core diagnostic feature of this constraint. Stigmatized agents with identity_locked exit see snare — high extraction, no exit, naturalized as inevitable. Enforcers see rope — coordination that legitimately allocates status and resources according to 'natural' differences. Institutional gatekeepers see tangled_rope — genuine coordination function (maintaining institutional coherence) coexisting with extraction (perpetuating arbitrary hierarchy). The organized coalition sees scaffold — temporary epistemic problem with a generational sunset as counter-narratives denaturalize the constraint. The analytical observer risks seeing mountain — 'humans naturally form hierarchies, stigma is evolutionary adaptation' — which is the naturalization trap at the meta-level. The structural analyst sees tangled_rope — mutable but entrenched, requiring active enforcement, with separable coordination and extraction components. The gap between the identity_locked agent's snare and the enforcer's rope is the extraction asymmetry. The gap between the false summit (analytical mountain) and the structural analyst's tangled_rope is the naturalization mechanism itself — the constraint's primary function is to make itself appear immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Stigmatized agents with identity_locked exit are victims with low power — the derivation chain produces high d (approaching 1.0), yielding high experienced extraction. The identity lock is cognitive rather than material: the agent has structural mobility (could organize with other stigmatized agents, could exit the local context at some cost) but cannot perceive this from within the naturalized frame. Exit would require not just paying a cost but abandoning the internalized identity ('I am the kind of person this constraint applies to'). Materially trapped agents are victims with trapped exit — even higher d, maximum experienced extraction. Enforcers with arbitrage exit are beneficiaries with institutional power — low d (approaching 0.0), yielding low or negative experienced extraction (they benefit from the constraint). Institutional gatekeepers are both beneficiaries (positional authority) and victims (career lock-in) with constrained exit — moderate d, yielding moderate experienced extraction. The organized coalition has mobile exit and mixed beneficiary/victim status — they bear costs of resistance but also benefit from solidarity and counter-narrative community — moderate d. The analytical observer at civilizational/universal scale risks naturalizing (mountain classification) but the structural analyst at generational/global scale correctly identifies the tangled_rope structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the snare classification from the victim's perspective and the rope classification from the beneficiary's perspective are both structurally accurate readings of the same constraint from different positions. The mandatrophy is not 'is this coordination or extraction?' but 'for whom is it coordination and for whom is it extraction?' The enforcer genuinely experiences coordination — naturalization does solve a coordination problem for them (efficient status allocation without constant negotiation). The stigmatized agent genuinely experiences extraction — naturalization prevents collective action that could redistribute power. The tangled_rope classification from the structural analyst perspective captures both: there is a real coordination function (boundary maintenance, resource allocation) AND asymmetric extraction (naturalization prevents transformation). The scaffold perspective from the organized coalition is also structurally sound — consciousness-raising movements are building alternative framings with a generational sunset logic. The mountain classification from the detached analytical observer is the naturalization trap — it reproduces at the meta-level the same cognitive error the trapped agents make. The false summit detector flags this: base extractiveness far exceeds mountain threshold, suppression far exceeds mountain threshold, and the constraint requires active enforcement. The 'natural law' framing is itself the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_reversibility_threshold,
    'At what point does internalized naturalization become irreversible within a biographical timeframe?',
    'Longitudinal studies of consciousness-raising intervention effectiveness; measurement of identity-frame plasticity across age cohorts and exposure durations',
    'If reversible within 2-5 years of intervention: identity_locked agents can be reclassified as constrained (exit becomes thinkable). If irreversible: identity_locked is functionally equivalent to trapped for biographical timescales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_reversibility_threshold, empirical, 'Whether internalized naturalization can be reversed within biographical time').

omega_variable(
    collective_action_threshold,
    'What fraction of the stigmatized population must denaturalize before collective action becomes viable?',
    'Historical analysis of successful denaturalization movements (civil rights, disability rights, LGBTQ+ rights); identification of tipping points where naturalized constraints became recognized as policy',
    'If threshold < 15%: scaffold perspective is structurally sound — small organized minorities can shift frames. If threshold > 40%: scaffold is aspirational — naturalization is too entrenched for generational sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Critical mass required for denaturalization to enable collective action').

omega_variable(
    enforcement_mechanism_visibility,
    'Does making enforcement mechanisms visible (showing that stigma requires active policing) reliably trigger denaturalization?',
    'Experimental studies of frame-shifting interventions; comparison of naturalization persistence when enforcement is visible vs invisible',
    'If visibility triggers denaturalization: the constraint is more fragile than suppression score suggests — it depends on enforcement invisibility. If visibility does not trigger denaturalization: the cognitive lock is deeper than enforcement awareness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_visibility, empirical, 'Whether enforcement visibility breaks naturalization frame').

omega_variable(
    coordination_function_separability,
    'Can the genuine coordination function (boundary maintenance, resource allocation) be preserved while eliminating the extractive naturalization component?',
    'Case studies of successful stigma reduction that maintained group coherence; analysis of whether denaturalization necessarily destabilizes coordination or can be decoupled',
    'If separable: tangled_rope can be untangled into rope (pure coordination without extraction). If inseparable: the coordination function is itself extractive — the boundary maintenance requires naturalization to function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_separability, conceptual, 'Whether coordination and extraction components can be decoupled').

omega_variable(
    intergenerational_transmission_mechanism,
    'Is naturalization transmitted primarily through explicit socialization or through structural position inheritance?',
    'Comparison of naturalization persistence in agents who inherit structural position vs those who experience social mobility; measurement of explicit vs implicit transmission pathways',
    'If explicit socialization dominates: counter-narrative interventions can break transmission. If structural position dominates: mobility interventions required — changing the narrative without changing material conditions will not denaturalize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Primary mechanism of naturalization transmission across generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(false_mountain_naturalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmn_theater_initial, false_mountain_naturalization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fmn_theater_early, false_mountain_naturalization, theater_ratio, 3, 0.42).
narrative_ontology:measurement(fmn_theater_mid, false_mountain_naturalization, theater_ratio, 6, 0.48).
narrative_ontology:measurement(fmn_theater_late, false_mountain_naturalization, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(fmn_extract_initial, false_mountain_naturalization, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fmn_extract_early, false_mountain_naturalization, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(fmn_extract_mid, false_mountain_naturalization, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(fmn_extract_late, false_mountain_naturalization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(false_mountain_naturalization, identity_coordination).
narrative_ontology:affects_constraint(false_mountain_naturalization, indexical_extraction_asymmetry).

% DUAL FORMULATION NOTE:
% False mountain naturalization is downstream of indexical_extraction_asymmetry. The asymmetric extraction creates the incentive structure for naturalization: beneficiaries naturalize to legitimize their position, victims naturalize to cope with inescapable extraction. The naturalization in turn stabilizes the asymmetry by preventing collective action that could redistribute power. The two constraints form a reinforcing loop: extraction → naturalization → stabilized extraction. They are modeled as separate stories because they have different ε values (indexical_extraction_asymmetry has lower ε, reflecting that some of the asymmetry is genuine coordination; false_mountain_naturalization has higher ε, reflecting that the naturalization itself is primarily extractive) and different primary observables (asymmetry is measured by power differentials and resource flows; naturalization is measured by Type I error rates and verbal patterns).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(false_mountain_naturalization, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
