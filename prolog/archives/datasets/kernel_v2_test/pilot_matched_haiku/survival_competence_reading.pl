% ============================================================================
% CONSTRAINT STORY: survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_survival_competence_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: survival_competence_reading
 *   human_readable: Ritual Encodes and Transmits Adaptive Capacity for Persecution-Survival
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the
 *   catastrophe_memory_kernel: the survival_competence_reading. The kernel is
 *   a contested claim about how ritual functions in persecuted communities —
 *   specifically, how ritual encodes and transmits adaptive capacity for
 *   surviving persecution. This reading interprets ritual as a survival
 *   technology: a structured practice that rehearses persecution-response
 *   patterns, transmits tacit knowledge of evasion and resilience, and
 *   maintains group cohesion under threat. The reading is held by communities
 *   for whom persecution is an active or recent historical reality, and by
 *   scholars who emphasize ritual's functional role in collective adaptation.
 *   Sibling readings (symbol_continuity_reading, trauma_encoding_reading,
 *   boundary_maintenance_reading) interpret the same ritual practices through
 *   different lenses: as narrative continuity, as psychological processing of
 *   trauma, or as boundary maintenance. These readings coexist in
 *   contemporary discourse — different parties hold different readings, and
 *   no single framework has foreclosed the others. The
 *   survival_competence_reading is structurally distinct from its siblings
 *   because it grounds ritual's legitimacy in operational efficacy: does the
 *   ritual actually transmit skills and knowledge that improve survival under
 *   persecution? This reading produces a tangled_rope classification because
 *   ritual simultaneously coordinates genuine survival-competence
 *   transmission AND extracts significant costs (time, emotional labor,
 *   embodied vulnerability, identity fusion) from practitioners.
 *
 * KEY AGENTS:
 *   - Persecuted Community Members: Primary victims (powerless/identity_locked) — bear the cost of ritual maintenance while survival competence is abstract and collective; identity fused with ritual practice
 *   - Ritual Practitioners with Moderate Agency: Secondary agents (moderate/constrained) — experience both coordination function and extraction; can theoretically exit but face high costs
 *   - Religious Institution / Ritual Authority: Primary beneficiary (institutional/arbitrage) — benefits from ritual's perceived efficacy; has agency to modify or adapt ritual
 *   - Diaspora Adaptation Coalition: Organized agents (organized/mobile) — see ritual as transitional mechanism with sunset logic; building alternative pathways for identity maintenance
 *   - Secularized Institutional Ritual: Institutional actor (institutional/arbitrage) — maintains ritual through inertia in contexts where persecution has ended; theater ratio high
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the survival_competence_reading as immutable law rather than as one constructed interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(survival_competence_reading, 0.38).
domain_priors:suppression_score(survival_competence_reading, 0.42).
domain_priors:theater_ratio(survival_competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(survival_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(survival_competence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(survival_competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(survival_competence_reading, "Ritual Encodes and Transmits Adaptive Capacity for Persecution-Survival").
narrative_ontology:topic_domain(survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(survival_competence_reading, '568c1850-d4d6-4b45-a5e1-0f98487b850e').
narrative_ontology:cs_kernel_codification('568c1850-d4d6-4b45-a5e1-0f98487b850e', distributed).
narrative_ontology:cs_authority_grounding('568c1850-d4d6-4b45-a5e1-0f98487b850e', lineage).
narrative_ontology:cs_interpretation_layer_present('568c1850-d4d6-4b45-a5e1-0f98487b850e').
narrative_ontology:cs_reading_relation('568c1850-d4d6-4b45-a5e1-0f98487b850e', survival_competence_reading__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('568c1850-d4d6-4b45-a5e1-0f98487b850e', survival_competence_reading__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('568c1850-d4d6-4b45-a5e1-0f98487b850e', survival_competence_reading__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('568c1850-d4d6-4b45-a5e1-0f98487b850e', foundational, ritual_transmits_survival_competence).
narrative_ontology:cs_axiom_status(ritual_transmits_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('568c1850-d4d6-4b45-a5e1-0f98487b850e', ritual_transmits_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('568c1850-d4d6-4b45-a5e1-0f98487b850e', foundational, persecution_threat_justifies_ritual_cost).
narrative_ontology:cs_axiom_status(persecution_threat_justifies_ritual_cost, holdable).
narrative_ontology:cs_axiom_grounding('568c1850-d4d6-4b45-a5e1-0f98487b850e', persecution_threat_justifies_ritual_cost, instrumental).
narrative_ontology:cs_reference_frame('568c1850-d4d6-4b45-a5e1-0f98487b850e', persecution_as_active_threat).
narrative_ontology:cs_drift_state('568c1850-d4d6-4b45-a5e1-0f98487b850e', contemporary_legal_emancipation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('568c1850-d4d6-4b45-a5e1-0f98487b850e', '').
narrative_ontology:cs_kernel_id(survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(survival_competence_reading, community_resilience_under_threat).
narrative_ontology:constraint_victim(survival_competence_reading, assimilation_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(survival_competence_reading, ritual_practitioners_moderate_agency).
narrative_ontology:constraint_beneficiary(survival_competence_reading, religious_institution).
narrative_ontology:constraint_victim(survival_competence_reading, persecuted_community_members).
narrative_ontology:constraint_victim(survival_competence_reading, ritual_practitioners_moderate_agency).
narrative_ontology:constraint_vindicates(survival_competence_reading, ritual_as_survival_technology).
narrative_ontology:constraint_vindicates(survival_competence_reading, embodied_memory_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in ritual practices that encode survival responses to persecution. Bear the cost of ritual maintenance (time, emotional labor, embodied vulnerability) while the survival competence transmitted is abstract and collective. Identity fused with ritual practice — cannot exit without abandoning the community identity that ritual constitutes. Experience the constraint as snare: trapped by identity lock, bearing maximum extraction.
narrative_ontology:constraint_stakeholder(survival_competence_reading, persecuted_community_members, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(survival_competence_reading, persecuted_community_members, observer).

% Actively practice and transmit ritual; experience both genuine coordination function (ritual rehearses survival responses, transmits tacit knowledge) and extraction (time, emotional investment, embodied vulnerability). Can theoretically exit but face high costs: loss of community, loss of transmitted knowledge, loss of identity frame. Constrained exit produces moderate experienced extraction.
narrative_ontology:constraint_stakeholder(survival_competence_reading, ritual_practitioners_moderate_agency, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(survival_competence_reading, ritual_practitioners_moderate_agency, beneficiary).

% Keeper and authority of ritual practice. Benefits from ritual's perceived efficacy in transmitting survival knowledge and maintaining group cohesion. Has arbitrage options: can modify ritual, can shift emphasis, can adapt to new threat environments. Experiences the constraint as rope: pure coordination, net beneficiary. Authority and continuity depend on ritual's perceived efficacy.
narrative_ontology:constraint_stakeholder(survival_competence_reading, religious_institution, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(survival_competence_reading, religious_institution, beneficiary).

% Organized agents (diaspora communities, interfaith networks, secular cultural preservationists) see ritual as a transitional mechanism. Encodes survival competence during persecution, but as threat environment changes (legal emancipation, integration, assimilation pressure), ritual's function shifts from survival-training to identity-maintenance. Has agency and sees exit path: ritual can be reframed as cultural heritage rather than persecution-response. Building alternative pathways for identity maintenance.
narrative_ontology:constraint_stakeholder(survival_competence_reading, diaspora_adaptation_coalition, agenda_setter,
    organized, generational, mobile, global).

% External structural force (legal integration, cultural homogenization, economic incentives for cultural assimilation) that creates the boundary-maintenance problem ritual addresses. Not an agent but a structural condition. Included in narrative for completeness; excluded from beneficiary/victim derivation because it is not an actor that collects from or bears costs of the constraint.
narrative_ontology:constraint_stakeholder(survival_competence_reading, assimilation_pressure, excluded,
    powerful, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(survival_competence_reading, assimilation_pressure).

% In contexts where persecution has ended or legal protections are established, ritual persists through institutional inertia despite atrophied survival function. Maintained theatrically as cultural identity marker rather than as active survival-competence transmission. Has arbitrage options but chooses to maintain ritual as performance of continuity. Theater ratio high because original function is no longer operationally necessary.
narrative_ontology:constraint_stakeholder(survival_competence_reading, secularized_institutional_ritual, agenda_setter,
    institutional, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual encodes and transmits adaptive capacity for surviving persecution — specific knowledge about evasion, resilience, community protection, and identity maintenance under threat. The coordination problem is: how does a persecuted community preserve and transmit survival knowledge across generations when persecution creates barriers to normal knowledge transmission (disruption, displacement, trauma)? Ritual solves this by embedding survival knowledge in embodied practice, narrative, and symbolic action that can be transmitted even under threat.
% TRANSFER_FUNCTION: Ritual transfers embodied knowledge, emotional resilience, and group identity from elder practitioners to younger members. It also transfers the burden of maintaining group boundaries against assimilation pressure. The flow is from religious institution (keeper of ritual authority) to community members (practitioners), and from elder practitioners to younger practitioners. The transfer includes both benefit (survival knowledge, group identity, community belonging) and cost (time, emotional labor, embodied vulnerability, identity fusion).
% ABSENT_VOICES: Assimilated community members who have exited the ritual practice are absent from the conversation. Their perspective — that ritual is unnecessary, that survival competence can be maintained through secular means, that the costs of ritual exceed the benefits — is not represented in the survival_competence_reading. Also absent: persecutors and assimilationist forces, whose perspective is that ritual should be eliminated or suppressed. The survival_competence_reading is authored from the perspective of communities for whom persecution is an active or recent historical reality.
% DISAPPEARANCE_RATIONALE: In contexts where persecution is active or recent, the constraint's disappearance would require alternative mechanisms for transmitting survival knowledge and maintaining group identity — the world would rearrange itself around new coordination mechanisms. In contexts where persecution has ended and legal protections are established, the constraint's disappearance would leave identity-maintenance functions to be performed by other mechanisms (secular cultural practice, institutional identity programs, diaspora networks) — the world would rearrange itself but less dramatically. The verdict is contested because different parties disagree about whether the survival-competence function is still operationally necessary.
% FOUNDING_PROBLEM: How does a persecuted community preserve and transmit survival knowledge across generations when persecution creates barriers to normal knowledge transmission? Ritual emerged as a solution: embedding survival knowledge in embodied practice, narrative, and symbolic action that can be transmitted even under threat, and that strengthens group identity and cohesion in the face of external pressure.
% FOUNDING_PROBLEM_CORROBORATION: Communities for whom persecution is an active or recent historical reality attest that the founding problem is live: survival knowledge transmission remains operationally necessary. Scholars of collective memory and ritual practice corroborate that ritual functions as a survival technology in persecuted communities. However, assimilated community members and secular scholars attest that the founding problem is dead in contexts where legal protections are established and assimilation pressure has declined. The contest is not empirical (whether ritual transmits knowledge) but interpretive (whether the founding problem remains live or has become historical).
narrative_ontology:disappearance_verdict(survival_competence_reading, contested).
narrative_ontology:founding_problem_status(survival_competence_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERSECUTED COMMUNITY MEMBER (SNARE) — Identity fused with ritual practice; cannot exit without abandoning the community identity that ritual constitutes. Bears the cost of ritual maintenance (time, emotional labor, embodied participation) while the survival competence it encodes is abstract and collective. Trapped by identity lock: the ritual IS how they know who they are.
constraint_indexing:constraint_classification(survival_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RITUAL PRACTITIONER WITH MODERATE AGENCY (TANGLED ROPE) — Experiences genuine coordination function (ritual rehearses survival responses, transmits tacit knowledge of persecution-evasion) alongside extraction (ritual demands time, emotional investment, embodied vulnerability). Can theoretically exit but faces high costs: loss of community, loss of transmitted knowledge, loss of identity frame. Constrained exit produces moderate experienced extraction.
constraint_indexing:constraint_classification(survival_competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTION / RITUAL AUTHORITY (ROPE) — Experiences the constraint as pure coordination: ritual is the mechanism through which the institution preserves and transmits survival knowledge across generations. The institution has arbitrage options (can modify ritual, can shift emphasis, can adapt to new threat environments). Net beneficiary — the institution's authority and continuity depend on ritual's perceived efficacy. Extraction runs toward the institution, not away.
constraint_indexing:constraint_classification(survival_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIASPORA ADAPTATION COALITION (SCAFFOLD) — Organized agents (diaspora communities, interfaith networks, secular cultural preservationists) see ritual as a transitional mechanism: it encodes survival competence during persecution, but as threat environment changes (legal emancipation, integration, assimilation pressure), the ritual's function shifts from survival-training to identity-maintenance. The coalition has agency and sees an exit path: ritual can be reframed as cultural heritage rather than persecution-response. Sunset logic: as legal protections strengthen, the survival-competence reading becomes less functionally necessary, though identity-maintenance reading persists.
constraint_indexing:constraint_classification(survival_competence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SECULARIZED INSTITUTIONAL RITUAL (PITON) — In contexts where persecution has ended or legal protections are established, ritual persists through institutional inertia despite atrophied survival function. The ritual is maintained theatrically: performed as cultural identity marker rather than as active survival-competence transmission. Theater ratio high because the original function (rehearsing persecution-response) is no longer operationally necessary, yet the ritual persists as performance of continuity. Piton classification derives from degraded function maintained as institutional theater.
constraint_indexing:constraint_classification(survival_competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, ritual transmission of survival knowledge is an immutable feature of human collective memory: all persecuted groups develop ritual encoding of adaptive responses, and this is a natural law of cultural evolution. However, this perspective risks naturalizing what is actually a contingent institutional arrangement. The survival-competence reading itself is a constructed interpretation of ritual's function, not a discovered natural law. The engine's false summit detector will identify this as naturalization of a reading-specific claim.
constraint_indexing:constraint_classification(survival_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(survival_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(survival_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(survival_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(survival_competence_reading, TR),
    TR >= 0.70.

:- end_tests(survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The survival_competence_reading frames ritual as a genuine coordination mechanism (transmitting survival knowledge) with embedded extraction (time, emotional labor, identity costs). The extractiveness is not as high as pure snare (0.70+) because the coordination function is real — ritual does transmit adaptive knowledge. But it is not as low as pure rope (0.10) because the costs to practitioners are substantial and asymmetrically distributed. The trajectory shows declining extractiveness over the interval (0.52 → 0.35) reflecting that as threat environment changes (legal protections, diaspora integration), the survival-competence function becomes less operationally necessary, though the ritual persists. Suppression (0.42): Moderate. Barriers to exit include identity fusion, community dependence, and loss of transmitted knowledge. But suppression is not total — some practitioners do exit, and legal protections reduce the structural barriers to leaving. The trajectory shows declining suppression (0.68 → 0.35) as threat environment improves. Theater ratio (0.35): Moderate-low. In active persecution contexts, ritual's theater is low because the survival-competence function is operationally necessary — the rehearsal is not performative, it is functional. As threat environment changes, theater ratio rises (0.15 → 0.35) because the ritual persists as identity marker rather than as active survival training. The trajectory reflects the reading's own sunset logic: ritual transitions from survival-competence encoding to identity-maintenance performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same ritual practice classifies differently depending on the observer's structural position. The persecuted community member sees snare (identity-locked, no exit, pure extraction of embodied participation). The moderate practitioner sees tangled_rope (genuine coordination function alongside extraction). The religious institution sees rope (pure coordination, net beneficiary). The diaspora coalition sees scaffold (temporary mechanism with sunset). The secularized institution sees piton (degraded function maintained as theater). The analytical observer risks seeing mountain (natural law of cultural evolution) but this is a false summit — the survival_competence_reading is a constructed interpretation, not a discovered natural law. The perspectival gaps reveal that the constraint's classification depends on whether the observer experiences the threat environment as active (snare/tangled_rope) or historical (scaffold/piton/rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the survival-competence reading. Persecuted community members with identity-locked exit experience maximum extraction (d ≈ 1.0) because they cannot exit and bear full costs. Moderate practitioners with constrained exit experience moderate extraction (d ≈ 0.6) because they have some agency and some benefit from coordination. The religious institution with arbitrage exit experiences low or negative extraction (d ≈ 0.2) because it benefits from ritual's perceived efficacy and can modify or adapt it. The diaspora coalition with mobile exit experiences low extraction (d ≈ 0.3) because it has agency and sees alternative pathways. The secularized institution with arbitrage exit experiences low extraction (d ≈ 0.1) because it maintains ritual through inertia rather than active extraction. The analytical observer with analytical exit experiences no extraction (d ≈ 0.5) because analysis is symmetric. The engine computes effective extraction (χ) from these directionality values and the constraint's scope; larger scope (global) amplifies extraction for targets and damps it for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The survival_competence_reading resolves mandatrophy by clarifying that ritual's mandate (transmit survival competence under persecution) is contingent on the threat environment. As threat environment changes (legal protections, diaspora integration, assimilation pressure declines), the mandate becomes obsolete, but the ritual persists through institutional inertia and identity fusion. The reading's own sunset logic (scaffold perspective) acknowledges that the survival-competence function is transitional — as legal protections strengthen and alternative identity-maintenance mechanisms emerge, the ritual's operational necessity declines. The piton perspective captures the degraded state: ritual persists as performance of continuity rather than as active survival training. The reading does not collapse into mandatrophy because it explicitly acknowledges the contingency of its own function — the survival-competence reading is valid only in contexts where persecution is an active or recent threat. In contexts where persecution has ended, the reading's mandate has outlived its function, and the constraint transitions to piton (degraded function) or scaffold (transitional mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_vs_identity_maintenance,
    'Is ritual''s primary function the encoding of survival competence under persecution, or the maintenance of group identity and boundary?',
    'Historical analysis of ritual content and practice during active persecution vs. post-persecution contexts; ethnographic documentation of what practitioners report as the ritual''s purpose; comparison of ritual modification patterns when threat environment changes',
    'If survival-competence is primary: this reading (tangled_rope with moderate extraction) is correct. If identity-maintenance is primary: the trauma_encoding_reading or boundary_maintenance_reading becomes the dominant framing, and extractiveness may be lower (pure coordination) or higher (pure extraction depending on boundary costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_vs_identity_maintenance, conceptual, 'Whether ritual''s function is survival-competence encoding or identity-maintenance').

omega_variable(
    kernel_reading_contest,
    'This constraint is ONE reading of the catastrophe_memory_kernel. What distinguishes the survival_competence_reading from its sibling readings (symbol_continuity_reading, trauma_encoding_reading, boundary_maintenance_reading)?',
    'Comparative analysis of the four readings'' axioms and reference frames. The survival_competence_reading grounds legitimacy in operational efficacy (does the ritual actually transmit skills and knowledge that improve survival under persecution?). Sibling readings ground legitimacy in different claims: symbol_continuity in narrative coherence, trauma_encoding in psychological processing, boundary_maintenance in group cohesion. These are not empirically resolvable as ''which is true'' — they are different readings of the same kernel, held by different parties.',
    'If survival_competence_reading is adopted: ritual is justified by its operational function, extractiveness is moderate (coordination + cost), and the constraint is tangled_rope. If trauma_encoding_reading is adopted: ritual is justified by its psychological function, extractiveness may be lower (pure coordination) or higher (extraction if trauma processing is coercive). If boundary_maintenance_reading is adopted: ritual is justified by its social function, extractiveness depends on boundary costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: survival_competence vs. sibling readings').

omega_variable(
    assimilation_pressure_mechanism,
    'Is assimilation pressure a genuine victim of the survival-competence reading, or is assimilation pressure the reading''s own structural target?',
    'Clarify the causal direction: does ritual encoding of survival competence RESIST assimilation (assimilation is the victim, ritual is the constraint that prevents it), or does ritual encoding of survival competence REQUIRE assimilation pressure as its justification (assimilation pressure is the reading''s own constructed necessity)? Historical analysis of ritual persistence in contexts where assimilation pressure has declined.',
    'If assimilation pressure is a genuine victim: the constraint''s function is to maintain group boundaries against external pressure, and the reading is correctly framed. If assimilation pressure is a constructed necessity: the reading may be using assimilation threat to justify ritual extraction, and the reading is itself extractive (snare rather than tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilation_pressure_mechanism, empirical, 'Whether assimilation pressure is victim or constructed necessity').

omega_variable(
    identity_lock_vs_constrained_exit,
    'For persecuted community members, is the binding mechanism identity-lock (cognitive/identity fusion) or constrained exit (material barriers to leaving the community)?',
    'Ethnographic study of exit narratives: do people who leave the community report that they ''became a different person'' (identity-lock) or that they ''faced too high a cost'' (constrained exit)? Post-exit trajectory analysis: do people who leave maintain ritual practice in modified form (suggesting identity-lock persists) or abandon it entirely (suggesting material barriers were primary)?',
    'If identity-lock: the powerless perspective''s classification as snare is correct, and the binding mechanism is cognitive rather than structural. If constrained exit: the classification may shift toward tangled_rope (moderate agency) rather than snare (no agency). The distinction matters for understanding whether ritual''s extraction is structural or internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Identity-lock vs. constrained exit mechanism for persecuted community members').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(survival_competence_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, survival_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(surv_tr_t2, survival_competence_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(surv_tr_t4, survival_competence_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(surv_tr_t6, survival_competence_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(surv_tr_t8, survival_competence_reading, theater_ratio, 8, 0.35).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, survival_competence_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(surv_be_t2, survival_competence_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(surv_be_t4, survival_competence_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(surv_be_t6, survival_competence_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(surv_be_t8, survival_competence_reading, base_extractiveness, 8, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(surv_su_t0, survival_competence_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(surv_su_t4, survival_competence_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(surv_su_t8, survival_competence_reading, suppression_requirement, 8, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(survival_competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(survival_competence_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(survival_competence_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(survival_competence_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four structurally distinct constraint stories, each representing a different reading of how ritual functions in persecuted communities. The survival_competence_reading (this story) emphasizes operational efficacy and produces tangled_rope classification. Sibling readings emphasize narrative continuity (rope), psychological processing (tangled_rope), or boundary maintenance (snare/tangled_rope). Each reading has its own ε value, its own beneficiary/victim structure, and its own classification. The readings are linked via network.affects_constraints to enable contamination propagation analysis — if one reading's purity degrades, the system can predict which siblings will be affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
