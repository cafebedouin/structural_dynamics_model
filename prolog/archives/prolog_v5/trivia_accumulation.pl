% ============================================================================
% CONSTRAINT STORY: trivia_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trivia_accumulation, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trivia_accumulation
 *   human_readable: Trivia Accumulation: The Gravitational Pull of Small Urgencies
 *   domain: moral_psychology/existential_philosophy/decision_theory
 *
 * SUMMARY:
 *   Trivia accumulation is the constraint by which small urgencies compound
 *   into years not through single catastrophic choice but through
 *   gravitational pull of immediate demands. Time-use diary studies
 *   consistently show gaps between stated priorities (write the book, learn
 *   the language, build the relationship) and actual time allocation (email,
 *   meetings, notifications, small tasks). Attention residue measurements
 *   reveal that even brief interruptions impose cognitive switching costs
 *   that persist for minutes to hours, fragmenting the continuous attention
 *   required for deep work. The constraint exhibits tangled rope structure:
 *   genuine coordination needs (distributed teams require communication,
 *   async work creates bottlenecks, immediate response prevents escalation)
 *   coexist with asymmetric extraction (attention residue, priority
 *   inversion, biographical drift). The theater_ratio (0.62) reflects that
 *   much of the visible busyness is performative: immediate email response
 *   and meeting attendance are used as proxies for engagement because deep
 *   work output is harder to measure. The constraint is downstream of
 *   desire_opacity (mountain) — agents cannot clearly see their own long-term
 *   priorities, making the gravitational pull of immediate demands harder to
 *   resist. The identity_locked exit option captures professionals whose
 *   self-concept is constituted through responsiveness: exit would require
 *   not just setting boundaries but abandoning the identity of being
 *   reliable, available, conscientious.
 *
 * KEY AGENTS:
 *   - Identity-Locked Professional: Primary victim (powerless/identity_locked) — professional identity fused with responsiveness; structurally mobile but cognitively trapped; years pass through gravitational accumulation
 *   - Constrained Knowledge Worker: Secondary victim (moderate/constrained) — faces real coordination needs alongside extraction; can set some boundaries at career cost; mixed experience
 *   - Platform Beneficiary: Primary beneficiary (institutional/arbitrage) — attention economy platforms and workplace communication systems capture engagement metrics and subscription revenue
 *   - Digital Minimalism Movement: Organized agents (organized/mobile) — building alternative norms with sunset logic: batch processing, async-first, protected deep work time
 *   - Productivity Theater System: Institutional actor (institutional/arbitrage) — maintains performative busyness metrics; sees own proxies as degraded but persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybrid of coordination and extraction; constraint is contingent on institutional arrangements but coordination function is real
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trivia_accumulation, 0.38).
domain_priors:suppression_score(trivia_accumulation, 0.48).
domain_priors:theater_ratio(trivia_accumulation, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trivia_accumulation, extractiveness, 0.38).
narrative_ontology:constraint_metric(trivia_accumulation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(trivia_accumulation, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trivia_accumulation, tangled_rope).
narrative_ontology:human_readable(trivia_accumulation, "Trivia Accumulation: The Gravitational Pull of Small Urgencies").
narrative_ontology:topic_domain(trivia_accumulation, "moral_psychology/existential_philosophy/decision_theory").

domain_priors:requires_active_enforcement(trivia_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trivia_accumulation, systems_demanding_immediate_response).
narrative_ontology:constraint_beneficiary(trivia_accumulation, attention_economy_platforms).
narrative_ontology:constraint_beneficiary(trivia_accumulation, workplace_urgency_culture).
narrative_ontology:constraint_victim(trivia_accumulation, long_term_self_directed_projects).
narrative_ontology:constraint_victim(trivia_accumulation, biographical_coherence).
narrative_ontology:constraint_victim(trivia_accumulation, reflective_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-LOCKED PROFESSIONAL (SNARE) — Professional identity constituted through responsiveness. Exit would require abandoning the self-concept of being reliable, available, conscientious. Structurally mobile (could change jobs, set boundaries) but identity-fused with the responsive role. The years pass not through single catastrophic choice but through gravitational accumulation of small urgencies that feel individually justified. Maximum experienced extraction because identity frame prevents seeing the pattern.
constraint_indexing:constraint_classification(trivia_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED KNOWLEDGE WORKER (TANGLED ROPE) — Faces real coordination needs (email enables collaboration, meetings solve problems, notifications prevent bottlenecks) alongside extraction (attention residue, context-switching costs, priority inversion). Can set some boundaries but at career cost. Genuine coordination function exists: immediate response systems do solve collective action problems. But asymmetric extraction also exists: the urgency culture extracts more from this agent than they gain from the coordination. Mixed experience.
constraint_indexing:constraint_classification(trivia_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM BENEFICIARY (ROPE) — Attention economy platforms and workplace communication systems benefit from immediate response culture. Experience the constraint as pure coordination: enabling connection, reducing friction, solving information asymmetries. Net beneficiary. The extraction runs toward this agent (captured attention, engagement metrics, subscription revenue) not away from them.
constraint_indexing:constraint_classification(trivia_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIGITAL MINIMALISM MOVEMENT (SCAFFOLD) — Organized agents (Cal Newport's time-blocking protocols, digital minimalism communities, right-to-disconnect legislation, attention restoration practices) see trivia accumulation as a temporary coordination failure with a sunset. Building alternative norms: batch processing, async-first communication, protected deep work time, notification architecture redesign. Low effective extraction because the coalition has agency and sees an exit path. Estimated sunset: 15-25 years for norms to mature across knowledge work sectors.
constraint_indexing:constraint_classification(trivia_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PRODUCTIVITY THEATER SYSTEM (PITON) — The visible busyness rituals (immediate email response, meeting attendance, Slack presence indicators) are largely performative. Actual productivity research shows these behaviors correlate weakly or negatively with output quality. The theater persists through institutional inertia: managers use responsiveness as a proxy for engagement because deep work is harder to measure. The system sees its own metrics as degraded — maintained because alternatives haven't replaced them, not because they work.
constraint_indexing:constraint_classification(trivia_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, trivia accumulation exhibits both genuine coordination (immediate response systems do solve real collective action problems in distributed work) and asymmetric extraction (attention residue, priority inversion, biographical drift). The constraint is not a natural law — the gravitational pull is contingent on institutional arrangements, platform design choices, and cultural norms around responsiveness. But it is also not pure extraction — the coordination function is real. Tangled Rope classification reflects the irreducible hybrid structure.
constraint_indexing:constraint_classification(trivia_accumulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trivia_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trivia_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trivia_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(trivia_accumulation, TR),
    TR >= 0.70.

:- end_tests(trivia_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The constraint extracts biographical coherence and deep work capacity, but the extraction is not as severe as pure attention-capture mechanisms because genuine coordination needs exist. The value reflects that much of the urgency culture serves real coordination functions (preventing bottlenecks, enabling collaboration) even as it imposes costs (attention residue, priority inversion). Suppression (0.48): Moderate. Significant barriers to exit include workplace norms around responsiveness, platform design (default-on notifications, presence indicators), career risk of boundary-setting, and identity fusion with the responsive role. But suppression is not total — some agents do successfully implement boundaries, and organized movements are building alternative norms. Theater ratio (0.62): Moderate-high. Much of the visible busyness (immediate email response, meeting attendance, Slack presence) is performative. Productivity research shows these behaviors correlate weakly or negatively with output quality. The theater has increased over the interval as communication platforms proliferated and responsiveness norms intensified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the gravitational pull of small urgencies — appears differently depending on the observer's structural position. The identity-locked professional sees a snare: years passing through invisible accumulation, exit blocked by identity fusion. The constrained knowledge worker sees tangled rope: real coordination needs mixed with real extraction. The platform beneficiary sees rope: pure coordination enabling connection and collaboration. The digital minimalism movement sees scaffold: a temporary problem with a sunset as alternative norms mature. The productivity theater system sees piton: degraded metrics persisting through inertia. The analytical observer sees tangled rope at the civilizational level: the coordination function is real (distributed work requires communication) but the extraction is also real (attention residue, biographical drift). No single type is 'the' answer — the presheaf over the observation site IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity-locked professional experiences maximum extraction because their identity frame prevents seeing the pattern — each small urgency feels individually justified, and the gravitational accumulation is invisible from within the responsive identity. The constrained knowledge worker experiences moderate extraction — they see the coordination-extraction hybrid and can set some boundaries, but at cost. The platform beneficiary experiences negative extraction (net benefit) — they capture the attention and engagement that flows through the urgency culture. The digital minimalism movement experiences low extraction because they have organized exit paths and see a sunset. The productivity theater system experiences low extraction because they benefit from the performative metrics even while recognizing their degradation. The analytical observer sees the irreducible hybrid structure — genuine coordination coexisting with asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that trivia accumulation is neither pure coordination (rope) nor pure extraction (snare) but an irreducible hybrid. The coordination function is genuine: immediate response systems do solve collective action problems in distributed work (preventing bottlenecks, enabling collaboration, reducing information asymmetries). But the extraction is also genuine: attention residue imposes cognitive costs, priority inversion prevents deep work, biographical drift compounds over years. The constraint requires active enforcement (workplace norms, platform defaults, career incentives) and exhibits both beneficiaries (platforms, urgency culture) and victims (long-term projects, biographical coherence). The tangled rope classification prevents both false naturalization (this is just how modern work is) and false demonization (this is pure manipulation). The constraint is contingent on institutional arrangements but serves real coordination needs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_residue_threshold,
    'At what magnitude does attention residue from task-switching cross from coordination cost to extractive mechanism?',
    'Longitudinal cognitive performance studies measuring residue decay rates; correlation between switching frequency and deep work capacity; identification of individual variation in switching costs',
    'If threshold is low (residue clears quickly): much of the experienced extraction is subjective framing rather than structural cost. If threshold is high (residue persists): the coordination function is overwhelmed by cognitive overhead, shifting classification toward snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_residue_threshold, empirical, 'Threshold at which attention residue becomes extractive rather than coordination cost').

omega_variable(
    biographical_coherence_measurement,
    'How do we measure the gap between stated long-term priorities and actual time allocation in a way that distinguishes preference change from structural drift?',
    'Repeated time-use diary studies with retrospective priority assessment; longitudinal tracking of project completion rates vs initiation rates; qualitative interviews distinguishing ''I changed my mind'' from ''I ran out of time''',
    'If the gap is primarily preference change: trivia accumulation is a coordination mechanism helping agents discover their true priorities through revealed preference. If the gap is primarily structural drift: trivia accumulation is an extraction mechanism preventing agents from pursuing their stable long-term goals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biographical_coherence_measurement, conceptual, 'Whether priority-allocation gaps reflect preference change or structural drift').

omega_variable(
    identity_lock_reversibility,
    'Is the identity fusion with responsiveness reversible through boundary-setting practice, or does it require identity reconstruction?',
    'Intervention studies: boundary-setting training vs identity-focused therapy for professionals reporting responsiveness-based burnout; measurement of sustained behavior change and subjective well-being at 6-month and 2-year follow-up',
    'If reversible through practice: the identity lock is shallower than the classification suggests, and exit_options should be downgraded to constrained for many agents. If requires identity reconstruction: the identity lock is structural, and the snare classification from the identity-locked perspective is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity fusion with responsiveness is reversible through practice').

omega_variable(
    async_coordination_sufficiency,
    'Do async-first communication protocols (batch email processing, async standups, document-based decision-making) preserve coordination function while reducing extraction, or do they introduce new coordination failures?',
    'Comparative organizational studies: teams using async-first vs synchronous-default protocols; measurement of decision quality, project completion rates, coordination overhead, and individual deep work capacity',
    'If async protocols preserve coordination: the scaffold perspective is confirmed — the sunset is real and the constraint is temporary. If async protocols introduce new failures: the coordination function is more tightly coupled to immediate response than the scaffold perspective assumes, and the constraint is more durable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(async_coordination_sufficiency, empirical, 'Whether async-first protocols preserve coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trivia_accumulation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trivia_tr_t0, trivia_accumulation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(trivia_tr_t5, trivia_accumulation, theater_ratio, 5, 0.48).
narrative_ontology:measurement(trivia_tr_t10, trivia_accumulation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(trivia_tr_t15, trivia_accumulation, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(trivia_be_t0, trivia_accumulation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(trivia_be_t5, trivia_accumulation, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(trivia_be_t10, trivia_accumulation, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(trivia_be_t15, trivia_accumulation, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trivia_accumulation, resource_allocation).

% DUAL FORMULATION NOTE:
% Trivia accumulation is downstream of desire_opacity (mountain) — agents cannot clearly see their own long-term priorities, making the gravitational pull of immediate demands harder to resist. The upstream constraint (desire_opacity) is a natural law of human psychology; the downstream constraint (trivia_accumulation) is a contingent institutional arrangement that exploits that natural law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
