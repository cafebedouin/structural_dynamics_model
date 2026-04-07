% ============================================================================
% CONSTRAINT STORY: coordination_fatigue
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_fatigue, []).

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
 *   constraint_id: coordination_fatigue
 *   human_readable: The Consensus Exhaustion Loop
 *   domain: social/organizational/technological
 *
 * SUMMARY:
 *   The Consensus Exhaustion Loop describes a structural constraint in
 *   hyper-connected organizations where the energy required to maintain
 *   synchronization, approval workflows, and real-time consensus exceeds the
 *   creative output capacity of individual contributors. This is not a
 *   coordination problem in the classic sense — the infrastructure (Slack,
 *   Teams, Zoom, project management platforms) solves genuine distributed
 *   team challenges. Rather, it is a hybrid constraint where coordination
 *   infrastructure has been extended beyond functional necessity into an
 *   extraction apparatus that captures attention, enforces constant
 *   availability, and subordinates asynchronous creative work to synchronous
 *   consensus rituals. The constraint exhibits theater ratio increasing over
 *   time (0.42 → 0.68) as organizations layer more consensus ceremonies
 *   without removing old ones, and extractiveness increasing (0.28 → 0.54) as
 *   the coordination overhead becomes harder to avoid. Different
 *   organizational actors experience this constraint differently: exhausted
 *   contributors see a trap with no exit, mid-level practitioners benefit
 *   from consensus authority while submitting to it from above,
 *   infrastructure vendors see pure coordination mechanism, gatekeepers see
 *   necessity for their roles, and reform coalitions see a temporary problem
 *   being solved by async-first alternatives. The analytical observer sees a
 *   tangled rope: genuine coordination function paired with asymmetric
 *   extraction of attention and time.
 *
 * KEY AGENTS:
 *   - Exhausted Contributors: Primary victims (powerless/trapped) — embedded in networks with 60-70% overhead allocation; cannot exit without severe career penalty
 *   - Mid-Level Practitioners: Secondary victim-beneficiary (moderate/constrained) — enforce consensus demands on subordinates while submitting to them from above; mixed extraction
 *   - Coordination Infrastructure Providers: Beneficiary (institutional/arbitrage) — platform vendors benefit from subscription and attention capture; experience constraint as pure coordination
 *   - Consensus Gatekeepers: Beneficiary (organized/arbitrage) — project managers, scrum masters benefit from expanded consensus authority; role necessity depends on synchronization requirements
 *   - Async-First Reform Coalition: Organized agents (organized/constrained) — distributed organizations and remote-work advocates building structural alternatives with sunset pathway
 *   - Legacy Meeting Culture: Institutional inertia (institutional/constrained) — synchronous consensus rituals persist through institutional momentum; largely performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies both genuine coordination function and extractive overhead; tangled rope classification reflects both
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_fatigue, 0.54).
domain_priors:suppression_score(coordination_fatigue, 0.62).
domain_priors:theater_ratio(coordination_fatigue, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_fatigue, extractiveness, 0.54).
narrative_ontology:constraint_metric(coordination_fatigue, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coordination_fatigue, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_fatigue, tangled_rope).
narrative_ontology:human_readable(coordination_fatigue, "The Consensus Exhaustion Loop").
narrative_ontology:topic_domain(coordination_fatigue, "social/organizational/technological").

domain_priors:requires_active_enforcement(coordination_fatigue).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coordination_fatigue, coordination_infrastructure_providers).
narrative_ontology:constraint_beneficiary(coordination_fatigue, consensus_gatekeepers).
narrative_ontology:constraint_victim(coordination_fatigue, creative_workers).
narrative_ontology:constraint_victim(coordination_fatigue, marginal_contributors).
narrative_ontology:constraint_victim(coordination_fatigue, organizational_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED CONTRIBUTOR (SNARE) — Individual contributors embedded in hyper-connected organizations face trapped exit: leaving the network severs career access, project visibility, and professional identity. Consensus demands (meetings, approval loops, asynchronous acknowledgment cycles) consume 60-70% of available attention. Suppression is near-total: alternatives (offline work, autonomous projects, exit to non-networked roles) carry severe career penalties. The constraint extracts cognitive surplus under the guise of coordination necessity.
constraint_indexing:constraint_classification(coordination_fatigue, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-LEVEL PRACTITIONER (TANGLED ROPE) — Team leads and specialists benefit from visibility and consensus authority within their domain, but face constrained exit: dropping from visibility loops reduces career mobility and project influence. Mixed extraction: they both enforce consensus demands on subordinates (beneficiary position) and submit to them from above (victim position). Suppression is moderate — can negotiate some offline time and async alternatives, but institutional pressure remains significant.
constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COORDINATION INFRASTRUCTURE PROVIDER (ROPE) — Platform and tool vendors (Slack, Teams, Asana, Zoom, etc.) experience the constraint as pure coordination mechanism. The tools solve a genuine problem: distributed team synchronization. These providers have arbitrage exit (can pivot to other markets, serve alternative clients). Net beneficiary position: they extract value through subscription and attention capture, but this is reciprocal — the tools genuinely enable some coordination benefit. Low suppression from their view.
constraint_indexing:constraint_classification(coordination_fatigue, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSENSUS GATEKEEPER (ROPE) — Project managers, scrum masters, and organizational compliance roles benefit from the consensus infrastructure: their authority and necessity depend on synchronization requirements. They experience the constraint as coordination, not extraction. They have arbitrage exit (can move to other organizations, climb to leadership, transition to strategic roles). The gatekeeper role itself creates demand for consensus-maintenance services.
constraint_indexing:constraint_classification(coordination_fatigue, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ASYNC-FIRST REFORM COALITION (SCAFFOLD) — Distributed organizations (GitLab, Automattic, others) and remote-work advocates are building structural alternatives to synchronous consensus. Async-first protocols, documented decision-making, and trust-based autonomy represent a sunset pathway. Suppression appears high (changing norms requires sustained effort), but the coalition has agency and organizational power. Theater is present (some async work remains performatively documented), but lower than sync-consensus theater. The constraint classification as scaffold is conditional on the sunset clause: these alternatives must mature and scale.
constraint_indexing:constraint_classification(coordination_fatigue, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: LEGACY MEETING CULTURE (PITON) — Synchronous meetings and real-time consensus rituals persist through institutional inertia despite evidence of low functional value for creative work. Many organizations maintain daily standups, all-hands meetings, and consensus ceremonies primarily for performative reasons: visible alignment, institutional ritual, theater of productivity. Theater ratio (0.68) reflects that much consensus overhead is maintenance of organizational appearances rather than functional coordination. The constraint persists because exit from the ritual carries reputational cost, not because the ritual is functionally necessary.
constraint_indexing:constraint_classification(coordination_fatigue, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, some coordination overhead is inherent to distributed work. But the current instantiation exceeds functional necessity by a wide margin. Coordination does solve genuine problems (synchronization, alignment, accountability), so Rope would be plausible. But the extraction component — attention capture, surveillance through constant availability, forced synchronization on creative timelines — elevates it to Tangled Rope. Suppression (0.62) is structural: the constraint depends on enforcing real-time responsiveness norms and making offline work costly.
constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_fatigue_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coordination_fatigue, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_fatigue, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coordination_fatigue, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coordination_fatigue, TR),
    TR >= 0.70.

:- end_tests(coordination_fatigue_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The constraint extracts attention and creative time through mandatory synchronous participation, approval cycles, and visibility requirements. The initial value (0.28) reflects that coordination infrastructure had legitimate necessity in distributed work. The trajectory to 0.54 shows accumulation: as organizations added more platforms, more meetings, and more consensus layers without removing old infrastructure, the extraction component exceeded functional coordination. Suppression (0.62): Moderate-high. Barriers to opting out include career penalties (reduced visibility damages promotion), social/cultural pressure (async work seen as disengagement), and technical lock-in (organizational processes hardcoded around synchronous tools). Alternatives exist (async protocols, deep work blocks, offline periods) but are costly to implement against institutional norms. Theater ratio (0.68): High. Many consensus ceremonies are primarily performative: daily standups confirm what asynchronous status updates already documented, all-hands meetings broadcast information already in channels, approval loops validate decisions already substantively made. The theater increased over time as organizations added new rituals without retiring old ones, creating a compound performance overhead.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp divergence between beneficiary and victim perspectives. The infrastructure provider and gatekeeper see a Rope — a coordination mechanism solving distributed team challenges. The exhausted contributor sees a Snare — mandatory participation with no exit. The mid-level practitioner sees both simultaneously (Tangled Rope) — they benefit from consensus authority yet chafe under consensus demands. The async-first coalition sees a Scaffold — the constraint is real but temporary; alternative protocols are building a sunset pathway. The legacy meeting culture perspective reveals the Piton classification: synchronous consensus rituals persist through institutional inertia despite low functional value (high theater ratio), maintained because exit carries reputational cost. The analytical observer resolves this as Tangled Rope: the constraint has genuine coordination function (preventing misalignment in distributed teams) but also extracts attention and creative time beyond functional necessity. The perspectival gap reveals that this is not simply a coordination problem to solve, but an extraction apparatus dressed in coordination language.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position: who benefits from the constraint and who bears its costs. Exhausted contributors have d ≈ 0.95 (full victims + trapped exit): they cannot leave the network without career damage, and the constraint extracts their time. Gatekeepers have d ≈ 0.05 (full beneficiaries + arbitrage exit): the constraint creates demand for their role and they can move to other organizations if needed. Infrastructure providers have d ≈ 0.15 (beneficiaries + arbitrage): they capture value through subscriptions but experience the constraint as coordination mechanism. Mid-level practitioners have d ≈ 0.50 (both beneficiary and victim + constrained exit): they gain authority from consensus gatekeeping but also submit to consensus demands from above. The async-first coalition has d ≈ 0.45 (organized victims with exit path): they see the constraint as solvable through structural change. The analytical observer has d ≈ 0.72 (observational position): they see both coordination and extraction functioning simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false Snare classification (pure extraction) by possessing genuine coordination function — synchronization mechanisms do solve real distributed team problems. It avoids false Rope classification (pure coordination) by exhibiting asymmetric extraction: contributors bear costs (attention, forced synchronicity, availability requirements) while beneficiaries (infrastructure vendors, gatekeepers, management) capture value. The Tangled Rope classification correctly captures that this is hybrid: coordination infrastructure has been extended into an extraction apparatus. The theater ratio (0.68) and increasing trajectory (0.42 → 0.68) show organizational drift toward performative consensus, indicating the coordination function is atrophying relative to the extraction component — a potential future Piton decay. The async-first scaffold perspective suggests the constraint is not inevitable: alternative coordination models (documented decisions, trust-based autonomy, asynchronous workflows) can perform equivalent coordination with dramatically lower theater and extraction overhead. The mandatrophy is resolved by distinguishing (a) the legitimate coordination problem these tools solve, (b) the extractive overhead that has accumulated around them, and (c) the organizational drift from functional coordination toward performative consensus theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_overhead_threshold,
    'What percentage of work time should coordination infrastructure legitimately consume before it crosses from enabling to extractive?',
    'Longitudinal study of output per capita vs coordination time allocation; comparison of high-coordination orgs vs async-first orgs with matched project complexity; worker productivity and creative output metrics',
    'If threshold is 20-30%: current 60-70% allocation is clearly extractive. If threshold is 50%+: might be legitimate for distributed work. Classification hinges on this empirical discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_overhead_threshold, empirical, 'Threshold for distinguishing coordination from coordination overhead').

omega_variable(
    async_viability_at_scale,
    'Can fully async-first protocols maintain sufficient coordination for complex interdependent projects at enterprise scale (>1000 people)?',
    'Multi-year case studies of scaling async organizations; measurement of decision velocity, alignment fidelity, and error rates in async vs sync cohorts; analysis of project failure rates by decision-making mode',
    'If viable: scaffold sunset is structural and real; many organizations could achieve dramatic productivity gains. If not viable: coordination fatigue is a genuine tradeoff rather than extractive overhead — classification drops to Rope from many perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(async_viability_at_scale, empirical, 'Whether async-first protocols scale to enterprise complexity').

omega_variable(
    synchronization_bias_in_knowledge_work,
    'Is the preference for synchronous consensus in organizations a functional requirement or a selection bias inherited from pre-digital management theory?',
    'Historical analysis of consensus norms; comparison of decision quality between sync and async modes for matched problem classes; organizational learning curves in async transition',
    'If functional requirement: coordination fatigue represents a genuine constraint on distributed scale. If inherited bias: the constraint is a rent-seeking apparatus disguised as necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synchronization_bias_in_knowledge_work, conceptual, 'Whether synchronization requirements are structural or inherited bias').

omega_variable(
    attention_scarcity_vs_coordination_scarcity,
    'Is the primary constraint in modern organizations coordination scarcity (hard to align people) or attention scarcity (hard to focus people on productive work)?',
    'Measurement of misalignment costs vs distraction costs in organizations with high and low coordination overhead; analysis of projects that failed due to insufficient coordination vs those derailed by coordination theater',
    'If coordination scarcity dominates: current overhead justified. If attention scarcity dominates: coordination fatigue is a principal cause of underproductivity, elevating extraction classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_scarcity_vs_coordination_scarcity, empirical, 'Whether coordination or attention is the binding constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_fatigue, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coord_fatigue_tr_t0, coordination_fatigue, theater_ratio, 0, 0.42).
narrative_ontology:measurement(coord_fatigue_tr_t5, coordination_fatigue, theater_ratio, 5, 0.55).
narrative_ontology:measurement(coord_fatigue_tr_t10, coordination_fatigue, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(coord_fatigue_be_t0, coordination_fatigue, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(coord_fatigue_be_t5, coordination_fatigue, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(coord_fatigue_be_t10, coordination_fatigue, base_extractiveness, 10, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_fatigue, enforcement_mechanism).
narrative_ontology:affects_constraint(coordination_fatigue, attention_economy_capture).
narrative_ontology:affects_constraint(coordination_fatigue, asynchronous_work_viability).
narrative_ontology:affects_constraint(coordination_fatigue, manager_role_necessity).
narrative_ontology:affects_constraint(coordination_fatigue, creative_output_capacity).

% DUAL FORMULATION NOTE:
% The consensus exhaustion loop can be decomposed into (1) the synchronous coordination requirement (ε ≈ 0.25, Rope), which solves genuine distributed team problems, and (2) the attention capture and forced-availability extraction mechanism (ε ≈ 0.72, Snare), which represents organizational rent-seeking. The current constraint story models the hybrid tangled rope where both operate simultaneously. Upstream constraints (manager role necessity, organizational hierarchy) drive demand for consensus gatekeeping. Downstream constraints (creative output capacity, attention scarcity) show the impact of coordination overhead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coordination_fatigue, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
