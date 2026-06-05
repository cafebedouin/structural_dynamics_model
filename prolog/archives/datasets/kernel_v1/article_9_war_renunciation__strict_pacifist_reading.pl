% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 War Renunciation (Strict Pacifist Reading)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution contains the categorical phrase
 *   'never be maintained' regarding war and military forces. The strict
 *   pacifist reading interprets this as an absolute prohibition: organized
 *   military capacity is constitutionally impermissible under all
 *   circumstances, including collective defense and self-defense. This
 *   reading represents one coherent instantiation of the kernel 'war
 *   renunciation' — a commitment to non-militarism encoded in constitutional
 *   text. The constraint exhibits substantial tension between pacifist
 *   constituencies (who benefit from constitutional institutionalization of
 *   their moral commitment) and state security planners (who face absolute
 *   prohibition on defensive military organization). The reading generates a
 *   tangled rope structure: it provides genuine coordination benefit within
 *   international peace frameworks while imposing real extraction on state
 *   autonomy and regional deterrence capacity. Over the 50-year interval
 *   measured here, the theater_ratio has risen (from 0.22 to 0.48) as
 *   constitutional jurisprudence has expanded permissible military activity
 *   through reinterpretation, creating widening gaps between textual reading
 *   and institutional practice. Suppression_requirement has also risen (0.65
 *   to 0.72) as external security threats increase, forcing the constraint to
 *   rely more heavily on formal enforcement and constitutional fiction to
 *   maintain its force.
 *
 * KEY AGENTS:
 *   - State Security Planners: Primary victims (powerless/trapped) — face absolute prohibition on organizing defensive military capacity; no legitimate exit within the reading's framework
 *   - Regional Alliance Partners (US, NATO analogs): Secondary beneficiaries with mixed experience (organized/constrained) — benefit from host state's non-militarism but subsidize its defense; experience tangled rope
 *   - Pacifist Constituencies: Primary beneficiaries (institutional/arbitrage) — their normative commitment is institutionalized at constitutional level; experience rope
 *   - International Peace Infrastructure (UN, arms control bodies): Organized beneficiaries (organized/constrained) — see the reading as part of global demilitarization transition; experience scaffold
 *   - Constitutional Court / Judicial System: Institutional enforcer (institutional/constrained) — maintains the reading through formal decisions while jurisprudential practice erodes its force; experience piton
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both the genuine coordination function and the real extraction structure; classifies as tangled rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.58).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.72).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 War Renunciation (Strict Pacifist Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '7da78e1c-2630-47b3-a53b-6af1aa873f50').
narrative_ontology:cs_kernel_codification('7da78e1c-2630-47b3-a53b-6af1aa873f50', fixed_text).
narrative_ontology:cs_authority_grounding('7da78e1c-2630-47b3-a53b-6af1aa873f50', lineage).
narrative_ontology:cs_interpretation_layer_present('7da78e1c-2630-47b3-a53b-6af1aa873f50').
narrative_ontology:cs_reading_relation('7da78e1c-2630-47b3-a53b-6af1aa873f50', article_9_war_renunciation__inherent_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('7da78e1c-2630-47b3-a53b-6af1aa873f50', article_9_war_renunciation__collective_self_defense_reading, coexists_with).
narrative_ontology:cs_axiom('7da78e1c-2630-47b3-a53b-6af1aa873f50', foundational, military_organization_categorically_impermissible).
narrative_ontology:cs_axiom_status(military_organization_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7da78e1c-2630-47b3-a53b-6af1aa873f50', military_organization_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('7da78e1c-2630-47b3-a53b-6af1aa873f50', foundational, security_achievable_through_non_military_means).
narrative_ontology:cs_axiom_status(security_achievable_through_non_military_means, holdable).
narrative_ontology:cs_axiom_grounding('7da78e1c-2630-47b3-a53b-6af1aa873f50', security_achievable_through_non_military_means, conventional).
narrative_ontology:cs_reference_frame('7da78e1c-2630-47b3-a53b-6af1aa873f50', absolute_war_renunciation).
narrative_ontology:cs_drift_state('7da78e1c-2630-47b3-a53b-6af1aa873f50', contemporary_security_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7da78e1c-2630-47b3-a53b-6af1aa873f50', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_constituencies).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, international_peace_movements).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, constitutional_legitimacy_via_moral_standing).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, defense_planners).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, regional_deterrence_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE SECURITY PLANNER (SNARE) — Faces absolute prohibition on organizing defensive military capacity. Exit would require abandoning constitutional fidelity or challenging the reading itself (politically costly or structurally foreclosed). Maximum extraction: the state's security autonomy is entirely subordinated to alliance dependence or vulnerability. No legitimate defense alternative exists within the reading's framework.
constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL ALLIANCE PARTNERS (TANGLED ROPE) — Benefit from the host state's constitutional commitment to non-militarism (reduces arms race pressure, enables cost-sharing on collective defense). Simultaneously bear extraction: they subsidize security for a state that cannot organize its own defense. Mixed coordination (collective security framework) with asymmetric burden.
constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: PACIFIST CONSTITUENCIES (ROPE) — Primary beneficiaries. The reading institutionalizes their moral commitment at the constitutional level. Gain legitimacy, legal standing, and political influence. Experience the constraint as coordination: the state's refusal to militarize aligns with their normative goals. No extraction experienced — pure coordination benefit.
constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL PEACE INFRASTRUCTURE (SCAFFOLD) — UN mechanisms, collective security agreements, arms control frameworks benefit from and reinforce the reading's logic. Experience the reading as temporary support for a broader transition to non-military security paradigm. See the constraint as part of a sunset toward global demilitarization (eventually all states adopt similar logic). Currently constrained by regional military competition but perceive genuine exit path.
constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT (PITON) — Maintains the reading through formal decisions but with increasing perceptual degradation. Theater ratio rises as security threats mount and reinterpretation pressures accumulate (Article 9 jurisprudence shows expanding loopholes: Self-Defense Forces, collective defense, weapons development). The court continues performance of strict pacifism while structural enforcement erodes. Piton classification reflects degraded function obscured by theatrical maintenance of original reading.
constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the strict pacifist reading as a genuine coordination mechanism (states voluntarily renouncing military capacity can align on collective security) with embedded extraction (the state's security autonomy is subordinated to alliance partners and international infrastructure). The reading is not a natural law but a contingent institutional choice with real structural consequences. Classification reflects both the coordination function and the asymmetric burden distribution.
constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_9_war_renunciation__strict_pacifist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, TR),
    TR >= 0.70.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The strict pacifist reading imposes substantial costs on state security autonomy — the state cannot organize any military capacity, must rely entirely on alliance partners, and faces vulnerability to regional threats that might be addressable through defensive forces. However, the extraction is not maximal (snare-level, ≥ 0.66) because the reading does provide genuine coordination benefits within international peace frameworks, and alliance relationships provide actual (if asymmetric) security. The reading genuinely coordinates collective security at a global scale. Suppression (0.72): High. The constraint requires active enforcement through constitutional law and judicial interpretation. Rising suppression over the interval reflects increasing pressure: as external security threats mount (regional militarization, adversarial power growth), maintaining the absolute prohibition requires more forceful enforcement and broader reinterpretation to prevent defection. The suppression is structural — it comes from the categorical nature of the prohibition, not from negotiable trade-offs. Theater ratio (0.48): Moderate, rising. Initial theater is low because the reading was coherent with constitutional design and matched public normative commitment. Over the interval, theater rises as jurisprudence expands permissible military activity (SDF constitutional, collective defense permissible, weapons development legitimate) while maintaining the fiction of absolute pacifism. The gap between textual reading ('never be maintained') and institutional practice (substantial Self-Defense Forces) creates performative content — the court performs adherence to the reading while permitting the substance to expand.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from identical base properties. State security planners (snare) experience categorical prohibition with no legitimate exit — pure extraction at maximum compression. Pacifist constituencies (rope) experience institutionalization of their normative commitment — pure coordination benefit at no perceived cost. Regional alliance partners (tangled rope) experience mixed coordination (collective security framework) with asymmetric burden (they subsidize the host's defense). International peace infrastructure (scaffold) perceives the reading as temporary support for a broader global demilitarization transition with genuine sunset logic — as international institutions mature, all states eventually adopt similar commitments. Constitutional court (piton) maintains the reading through formal doctrine while jurisprudential practice expands permissible military activity, creating theatrical maintenance of a constraint whose functional force has degraded. The analytical observer (tangled rope) sees both the coordination function and the extraction structure, without presupposing either the pacifist constituency's normative commitment or the security planner's vulnerability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural relationship to the extraction flow. Victims with trapped exit (security planners) experience d ≈ 0.95, producing maximum f(d) ≈ 1.42. Beneficiaries with arbitrage exit (pacifist constituencies) experience d ≈ 0.15, producing negative f(d) ≈ -0.01. Organized agents with constrained exit (regional partners) experience d ≈ 0.55, producing moderate f(d) ≈ 0.75. The analytical observer experiences d ≈ 0.72, producing f(d) ≈ 1.15. These values feed χ = ε × f(d) × σ(S) with scope modifier σ(S)=1.0 (national scope at primary perspective). The resulting effective extractiveness varies substantially across perspectives, explaining the perspectival gap: the same structural data produces snare classification for trapped security planners and rope classification for arbitrage beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy — the strict pacifist reading is coherent and unmixed. It classifies as tangled rope because it genuinely coordinates collective security while imposing real extraction on state autonomy. The coordination function is authentic (alliance partners do reduce costs of collective defense), not theatrical. The extraction is authentic (the state's security autonomy is subordinated to alliance dependence). The rising theater_ratio reflects JUDICIAL THEATER (constitutional court maintaining the reading while practice expands), not mandatrophy in the classical sense. The piton perspective (constitutional court itself) correctly identifies that the court performs adherence while permitting substance to expand — this is inertial maintenance of an institution whose primary function has atrophied. But from the analytical perspective, the reading's function remains intact: it coordinates international peace infrastructure and pacifist constituencies genuinely, even as it imposes real costs on security planning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_defense_definition_boundary,
    'Does the strict pacifist reading permit any military capability at all, or does ''never be maintained'' foreclose even strictly defensive forces?',
    'Textual analysis of Article 9''s phrasing; comparison to pacifist vs. rearmament case law; examination of what ''self-defense'' means in the reading''s own commitments (non-military only, or defense-only military permitted)',
    'If ''never'' means no military capacity whatsoever: reading forecloses all alternative readings (self-defense must be purely non-military). If ''defense-only'' capacity permitted: reading coexists with inherent-right reading (differ on scope, not principle). This is THE critical axiom boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_defense_definition_boundary, conceptual, 'Whether strict pacifism permits any defensive military capacity or forecloses all military organization').

omega_variable(
    alliance_dependence_extraction_magnitude,
    'Is alliance dependence a legitimate coordination mechanism or extractive subordination of security autonomy?',
    'Structural comparison to other reading''s security frameworks; measurement of burden-sharing asymmetry (military expenditure, strategic autonomy, decision-making power within alliances); historical analysis of whether alliance partners increase or decrease security outcomes for the renunciating state',
    'If extraction < 0.40: reading is rope (pure coordination). If extraction > 0.60: reading is snare-adjacent (high suppression via vulnerability). Current classification (tangled_rope at 0.58) assumes genuine coordination benefit alongside real autonomy loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_dependence_extraction_magnitude, empirical, 'Degree to which alliance dependence constitutes extractive subordination vs. legitimate coordination').

omega_variable(
    judicial_theater_accumulation,
    'As constitutional court jurisprudence expands reinterpretation (SDF constitutional, collective defense permissible, weapons development legitimate), does the strict pacifist reading transition from functioning constraint to theatrical maintenance?',
    'Longitudinal analysis of constitutional court decisions granting exemptions and expansions; measurement of gap between textual reading (''never be maintained'') and actual armed capacity (SDF personnel, budget, capability); assessment of whether court maintains the fiction while permitting the substance',
    'If piton diagnosis confirmed: theater_ratio should rise above 0.7 and extractiveness should fall (the constraint no longer functions, merely performs). This would trigger reclassification toward piton. If reading retains genuine force: theater_ratio remains moderate and the constraint functions despite jurisprudential pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_theater_accumulation, empirical, 'Whether constitutional court exemptions degrade the strict pacifist reading into performative theater').

omega_variable(
    reading_committer_frame,
    'Is this reading instantiating a genuinely held constitutional commitment, or is it one move in a strategic game where the reading''s authority is instrumentalized to achieve other political ends?',
    'Historical examination of when the reading was adopted and by whom (occupation-era imposition vs. autonomous adoption); contemporary political discourse about whether renewal of the reading reflects genuine normative commitment or historical inertia; comparison to states with similar pacifist readings and their security outcomes',
    'If the reading is strategically instrumentalized (e.g., adopted under occupation, now maintained for diplomatic rather than normative reasons): the beneficiary classification shifts. The primary beneficiary becomes strategic actors (alliance partners, post-war occupation powers) rather than pacifist constituencies. This changes directionality and may alter classification at some perspectives. If genuinely held: beneficiary classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_frame, preference, 'Whether the strict pacifist reading represents genuine normative commitment or instrumental maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a9sp_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(a9sp_tr_t25, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(a9sp_tr_t50, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(a9sp_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(a9sp_be_t25, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 25, 0.53).
narrative_ontology:measurement(a9sp_be_t50, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(a9sp_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(a9sp_su_t25, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(a9sp_su_t50, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, sdf_constitutional_legitimacy).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, regional_deterrence_asymmetry).

% DUAL FORMULATION NOTE:
% Article 9 war renunciation is a kernel with three readings. This constraint models the strict pacifist reading (absolute prohibition). Sibling constraints model the inherent-right reading (defensive military permissible) and collective-self-defense reading (alliance participation permissible). The three readings have different ε values: strict pacifist (0.58), inherent-right (~0.35), collective-self-defense (~0.42). They also have different victim/beneficiary structures. They coexist as live positions in constitutional jurisprudence — no single reading has foreclosed the others, though each reading's axioms would foreclose the others IF a single framework could hold only one. The constraint family captures the presheaf of readings over the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__strict_pacifist_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
