% ============================================================================
% CONSTRAINT STORY: symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbol_continuity_reading, []).

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
 *   constraint_id: symbol_continuity_reading
 *   human_readable: Ritual Preserves Symbolic Continuity and Collective Identity Across Time
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes ritual as a mechanism for preserving symbolic
 *   continuity and collective identity across time. The
 *   symbol_continuity_reading emphasizes the genuine coordination function:
 *   ritual provides a shared symbolic vocabulary, anchors collective memory,
 *   and transmits identity across generations. This reading is one of four
 *   structurally distinct interpretations of the catastrophe_memory_kernel —
 *   a contested claim about what ritual practices do in the aftermath of
 *   collective trauma or social disruption. The symbol_continuity_reading
 *   focuses on symbolic transmission and identity maintenance, treating
 *   ritual as a coordination mechanism with low extractiveness. The
 *   constraint exhibits a perspectival range from rope (genuine coordination)
 *   to snare (coercive participation) to piton (performative continuity in
 *   secularized contexts) to mountain (naturalized as inherent to human
 *   society). The key structural tension is between the constraint's genuine
 *   coordination function (transmitting meaning and identity) and its
 *   suppression of adaptive modification (ritual rigidity costs). The theater
 *   ratio increases over the interval as secularization advances and ritual's
 *   original functional meaning attenuates, leaving primarily performative
 *   identity-maintenance.
 *
 * KEY AGENTS:
 *   - Ritual Community (Practitioners): Organized agents (organized/mobile) — experience ritual as genuine coordination; benefit from shared symbolic vocabulary and collective memory anchoring
 *   - Religious Institution (Custodian): Institutional beneficiary (institutional/arbitrage) — maintains ritual framework; benefits from stable symbolic transmission; has exit options but chooses to maintain
 *   - Initiated Member (Identity-Fused): Powerless agent (powerless/identity_locked) — identity constituted through ritual role; bears cost of ritual rigidity while institution collects benefit of stable transmission
 *   - Coerced Participant (Structurally Trapped): Powerless victim (powerless/trapped) — trapped by family obligation or social sanction; bears cost of participation; experiences constraint as pure extraction
 *   - Secularized Institution (Performative): Institutional actor (institutional/arbitrage) — maintains ritual forms primarily for identity continuity and institutional legitimacy; sees own ritual as degraded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent features of human society
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbol_continuity_reading, 0.15).
domain_priors:suppression_score(symbol_continuity_reading, 0.2).
domain_priors:theater_ratio(symbol_continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbol_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(symbol_continuity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(symbol_continuity_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbol_continuity_reading, rope).
narrative_ontology:human_readable(symbol_continuity_reading, "Ritual Preserves Symbolic Continuity and Collective Identity Across Time").
narrative_ontology:topic_domain(symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbol_continuity_reading, 'f24b3bc1-d899-4cdb-9f66-9954f07dbc7b').
narrative_ontology:cs_kernel_codification('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', distributed).
narrative_ontology:cs_authority_grounding('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', lineage).
narrative_ontology:cs_interpretation_layer_present('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b').
narrative_ontology:cs_reading_relation('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', symbol_continuity_reading__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', symbol_continuity_reading__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', symbol_continuity_reading__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', foundational, symbolic_continuity_primary_function).
narrative_ontology:cs_axiom_status(symbolic_continuity_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', symbolic_continuity_primary_function, conventional).
narrative_ontology:cs_axiom('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', secondary, ritual_form_preservation_necessary).
narrative_ontology:cs_axiom_status(ritual_form_preservation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', ritual_form_preservation_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', symbolic_continuity_mandate).
narrative_ontology:cs_drift_state('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', contemporary_secularization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f24b3bc1-d899-4cdb-9f66-9954f07dbc7b', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, tradition_continuity).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, community_identity_maintenance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, ritual_community).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, religious_institution).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, initiated_member_identity_locked).
narrative_ontology:constraint_victim(symbol_continuity_reading, initiated_member_identity_locked).
narrative_ontology:constraint_victim(symbol_continuity_reading, coerced_participant).
narrative_ontology:constraint_vindicates(symbol_continuity_reading, symbolic_transmission_hypothesis).
narrative_ontology:constraint_vindicates(symbol_continuity_reading, collective_memory_stabilization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized practitioners who participate in ritual to maintain shared symbolic vocabulary and collective identity. They experience ritual as enabling — it solves the real problem of transmitting meaning across generations. They benefit from the coordination function and have the option to leave the community if they choose, but they choose to stay because the ritual serves their interests.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, ritual_community, beneficiary,
    organized, generational, mobile, local).

% The institutional custodian (church, temple, mosque, synagogue) that maintains and administers ritual practice. The institution sets the agenda for ritual form and meaning, and benefits from the constraint's operation — it provides the framework for symbolic continuity. The institution has arbitrage options (could modify rituals, could exit the tradition) but chooses to maintain them because they solve a genuine coordination problem and serve the institution's core function.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, religious_institution, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(symbol_continuity_reading, religious_institution, beneficiary).

% An individual whose identity is constituted through ritual participation. They benefit from the ritual's coordination function — it provides meaning, belonging, and identity. But they bear the cost of ritual rigidity: adaptive modification is suppressed in the name of symbolic continuity. Their identity is fused with the ritual role, making exit psychologically impossible even if structurally available. They are both beneficiary (of symbolic continuity) and payer (of ritual rigidity costs).
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, initiated_member_identity_locked, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(symbol_continuity_reading, initiated_member_identity_locked, beneficiary).

% An individual trapped in ritual participation by family obligation, social sanction, or economic dependency. They bear the cost of participation (time, conformity, suppression of alternative identity) while the institution and tradition collect the benefit. Exit is materially blocked by family pressure, social ostracism, or economic consequences. The ritual's symbolic function is irrelevant to them; they experience the constraint as pure extraction.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, coerced_participant, payer,
    powerless, biographical, trapped, local).

% The abstract good of symbolic continuity and collective identity transmission across generations. This is not an actor but a proposition — the constraint's operation vindicates the claim that ritual preserves symbolic continuity. Tradition-continuity benefits from the constraint's operation but collects no rents and has no agency.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, tradition_continuity, beneficiary,
    powerless, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(symbol_continuity_reading, tradition_continuity).

% The capacity for adaptive ritual modification in response to environmental, social, or technological change. Adaptive modification is suppressed in the name of symbolic continuity — ritual forms are maintained unchanged even when adaptation would improve community resilience. Adaptive modification would object if it had a voice, but it is excluded from the conversation about ritual's proper form.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, adaptive_modification, excluded,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(symbol_continuity_reading, adaptive_modification).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: How to maintain shared symbolic meaning and collective identity across generational change, in the face of social disruption, cultural loss, or the natural attrition of memory over time.
% TRANSFER_FUNCTION: Ritual transfers symbolic meaning, identity markers, and collective memory from one generation to the next. It also transfers authority from the religious institution to participants, and transfers the cost of ritual rigidity (suppression of adaptive modification) from the institution to participants.
% ABSENT_VOICES: Adaptive modification would object if it had a voice — the constraint suppresses ritual adaptation in the name of symbolic continuity. Secular communities that have abandoned ritual practice would object to the naturalization of ritual as necessary for identity. Individuals who have exited ritual communities would testify to the identity-lock mechanism that makes exit psychologically difficult.
% DISAPPEARANCE_RATIONALE: If ritual practice disappeared overnight, communities would need to develop alternative mechanisms for transmitting symbolic meaning and anchoring collective identity. Some communities might develop secular rituals or commemorative practices; others might lose symbolic continuity and experience identity fragmentation. The constraint's disappearance would force rearrangement of how communities maintain intergenerational meaning transmission.
% FOUNDING_PROBLEM: How to preserve collective identity and symbolic meaning in the aftermath of catastrophe, social disruption, or generational change. Ritual emerged as a mechanism for encoding and transmitting meaning that survives individual memory loss and social upheaval.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and historians attest that ritual practice is universal across human societies and that it serves identity and memory functions. Survivors of collective trauma (genocide, displacement, cultural suppression) testify that ritual practice is essential for maintaining identity and transmitting meaning to subsequent generations. However, secular communities that have abandoned ritual practice also testify that identity and meaning can be maintained through non-ritual mechanisms (narrative, institutional practice, secular commemoration). The founding problem is live but contested — different parties dispute whether ritual is necessary or merely one possible solution.
narrative_ontology:disappearance_verdict(symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(symbol_continuity_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RITUAL COMMUNITY (ROPE) — Organized practitioners experience ritual as genuine coordination: the constraint solves the real problem of transmitting identity and meaning across generations. Participants benefit from shared symbolic vocabulary and collective memory anchoring. Exit is mobile — individuals can leave the community, but the constraint itself is perceived as enabling rather than extractive. Low experienced extraction because the coordination function is authentic and participants are net beneficiaries of symbolic continuity.
constraint_indexing:constraint_classification(symbol_continuity_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTION (ROPE) — The institutional custodian (church, temple, mosque, synagogue) experiences ritual as coordination mechanism that stabilizes collective identity and ensures intergenerational transmission. The institution benefits from the constraint's operation — it provides the framework for symbolic continuity — but this is a legitimate coordination benefit, not extraction. The institution has arbitrage options (could modify rituals, could exit the tradition) but chooses to maintain them because they solve a genuine coordination problem. Extraction is minimal because the institution's interests align with the community's.
constraint_indexing:constraint_classification(symbol_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: INITIATED MEMBER (TANGLED ROPE) — An individual whose identity is constituted through ritual participation experiences the constraint as both coordination and extraction. The ritual genuinely coordinates community belonging and transmits meaning (coordination function). But the individual is identity-locked: their self-concept is fused with the ritual role, making exit psychologically impossible even if structurally available. They bear the cost of ritual rigidity — adaptive modification is suppressed in the name of symbolic continuity — while the institution collects the benefit of stable transmission. This is tangled rope: real coordination layered with asymmetric extraction via identity lock.
constraint_indexing:constraint_classification(symbol_continuity_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 4: COERCED PARTICIPANT (SNARE) — An individual trapped in ritual participation by family obligation, social sanction, or economic dependency experiences the constraint as pure extraction. The ritual's symbolic function is irrelevant to them; they bear the cost of participation (time, conformity, suppression of alternative identity) while the institution and tradition collect the benefit. Exit is materially blocked by family pressure, social ostracism, or economic consequences. The coordination story is cover; the constraint's persistence depends on coercion and suppression of alternatives.
constraint_indexing:constraint_classification(symbol_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: SECULARIZED INSTITUTION (PITON) — A religious institution in a secularized context that maintains ritual forms primarily for identity continuity and institutional legitimacy, with the original functional meaning largely atrophied. The ritual persists through institutional inertia and theatrical maintenance of tradition rather than because it solves an active coordination problem. The institution sees its own ritual as degraded — maintained because alternatives haven't fully replaced it and because ritual still anchors community identity, but no longer because the ritual's original function (spiritual transformation, cosmic ordering, divine communion) is operative. Theater ratio is high because the performative identity-maintenance function has replaced the original functional meaning.
constraint_indexing:constraint_classification(symbol_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, ritual's role in preserving symbolic continuity and collective identity appears as an immutable feature of human social organization. All known human societies use ritual to anchor identity and transmit meaning across generations. This perspective sees the constraint as a natural law of collective memory: symbolic continuity requires ritual performance, and ritual performance requires repetition and continuity. However, this naturalizes what is actually a contingent institutional arrangement. The engine's false summit detector will identify this as a false summit, revealing that 'ritual is necessary for identity' is a constructed claim that benefits tradition-custodians, not a law of nature.
constraint_indexing:constraint_classification(symbol_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbol_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbol_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbol_continuity_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(symbol_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The symbol_continuity_reading emphasizes the genuine coordination function of ritual — transmitting symbolic meaning and anchoring collective identity. The constraint solves a real coordination problem: how to maintain shared meaning and identity across generational change. Extractiveness is low because the primary beneficiary is tradition-continuity itself (a proposition, not an actor collecting rents), and participating communities are net beneficiaries of the coordination function. The modest extractiveness reflects the cost of ritual rigidity — adaptive modification is suppressed in the name of symbolic continuity. Suppression (0.20): Low-moderate. Ritual participation is generally voluntary for organized communities and mobile participants. Suppression is higher for identity-locked and trapped participants who face psychological or material barriers to exit. The average suppression reflects that most participants experience ritual as enabling rather than coercive, but a significant minority experience it as constraining. Theater ratio (0.35): Moderate. In traditional contexts, ritual's performative and functional dimensions are integrated — the performance IS the function. In secularized contexts, the original functional meaning (spiritual transformation, cosmic ordering) has largely atrophied, leaving primarily performative identity-maintenance. The theater ratio increases over the interval as secularization advances. At t=0 (traditional context), theater is low because performance and function are unified. At t=6 (secularized context), theater is higher because the performative identity-maintenance function has replaced the original functional meaning.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a wide perspectival range driven by the participant's structural position and identity relationship to the ritual. The organized community sees rope — genuine coordination solving the real problem of intergenerational symbolic transmission. The religious institution sees rope — the constraint enables the institution's core function of maintaining tradition. The identity-locked initiated member sees tangled rope — the constraint both coordinates community belonging and extracts via psychological binding. The coerced participant sees snare — the constraint is pure extraction, with the coordination story as cover. The secularized institution sees piton — the ritual persists through inertia and theatrical maintenance, not because it solves an active coordination problem. The analytical observer risks seeing mountain — naturalizing ritual as an inherent feature of human society. The perspectival gap reveals that the same structural phenomenon (ritual performance) is experienced as coordination, extraction, or degraded performance depending on the observer's power, exit options, and identity relationship to the ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the constraint. Beneficiaries with arbitrage options (the religious institution) experience low d — they benefit from the constraint and can exit if they choose. Organized communities with mobile exit experience moderate d — they benefit from coordination but could leave. Identity-locked participants experience high d — they are structurally mobile but psychologically trapped, bearing the cost of ritual rigidity. Trapped participants experience maximum d — they bear the full cost of participation with no exit option. The engine derives d from beneficiary/victim declarations and exit options, then applies the sigmoid f(d) to produce experienced extractiveness chi. The symbol_continuity_reading's low base extractiveness (0.15) reflects that the primary beneficiary is tradition-continuity itself (a proposition, not an actor), and most participants are net beneficiaries of the coordination function. The tangled rope and snare perspectives show higher chi because those agents bear costs (ritual rigidity, coerced participation) while the institution collects benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the symbol_continuity_reading from sibling readings of the catastrophe_memory_kernel. The symbol_continuity_reading's mandate is to preserve symbolic continuity and collective identity across time. This mandate is live and functional — communities do use ritual to transmit meaning and anchor identity. The constraint does not exhibit mandatrophy in the sense of a dead mandate persisting through inertia. However, the piton perspective reveals a secondary mandatrophy: in secularized contexts, the original functional meaning of ritual (spiritual transformation, cosmic ordering) has atrophied, leaving primarily performative identity-maintenance. The institution maintains the ritual form through theatrical performance rather than because the original function is operative. This is not mandatrophy of the symbol_continuity_reading itself, but rather a degradation of the ritual's original functional meaning in contexts where secularization has advanced. The symbol_continuity_reading remains structurally valid — ritual does preserve symbolic continuity — but in secularized contexts, this function is increasingly performative rather than substantive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_necessity_vs_contingency,
    'Is ritual''s role in preserving symbolic continuity a necessary feature of human collective memory, or a contingent institutional arrangement that benefits tradition-custodians?',
    'Comparative analysis of identity transmission mechanisms across cultures and historical periods; examination of secular communities that maintain collective identity without ritual; study of identity persistence in diaspora communities that have abandoned ritual practice',
    'If necessary: mountain classification confirmed — ritual is inherent to collective memory. If contingent: false summit detected — the ''natural law'' framing naturalizes what is actually an institutional arrangement that benefits tradition-continuity and suppresses adaptive modification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_necessity_vs_contingency, conceptual, 'Whether ritual is necessary for symbolic continuity or a contingent institutional arrangement').

omega_variable(
    adaptive_modification_suppression,
    'Does the constraint''s emphasis on symbolic continuity systematically suppress adaptive modification of ritual forms, and at what cost to community resilience?',
    'Historical analysis of ritual rigidity vs adaptive capacity in communities facing environmental, social, or technological change; comparison of communities that maintain strict continuity vs those that permit adaptive modification; measurement of community resilience outcomes',
    'If suppression is substantial: the constraint extracts from adaptive capacity (victims include community resilience and innovation). If suppression is minimal: the constraint is genuine coordination without significant extraction cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptive_modification_suppression, empirical, 'Whether symbolic continuity emphasis suppresses adaptive ritual modification').

omega_variable(
    identity_lock_mechanism_scope,
    'What proportion of ritual participants are identity-locked (self-concept fused with ritual role) vs structurally mobile but choosing participation?',
    'Ethnographic study of exit narratives; comparison of exit costs reported by identity-locked vs mobile participants; analysis of identity reconstruction in post-ritual communities',
    'If identity-lock is widespread: tangled rope classification dominates — the constraint extracts via psychological binding. If identity-lock is rare: rope classification dominates — the constraint is genuine coordination with voluntary participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_scope, empirical, 'Proportion of participants who are identity-locked vs structurally mobile').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the catastrophe_memory_kernel. What structural differences distinguish this symbol_continuity_reading from the survival_competence_reading, trauma_encoding_reading, and boundary_maintenance_reading?',
    'Comparative analysis of the four readings'' ε values, beneficiary structures, and victim sets. The symbol_continuity_reading emphasizes symbolic transmission (low ε, beneficiary is tradition-continuity itself). The survival_competence_reading emphasizes practical knowledge transmission (moderate ε, beneficiary is adaptive capacity). The trauma_encoding_reading emphasizes psychological processing (moderate-high ε, victim is unprocessed trauma). The boundary_maintenance_reading emphasizes group identity boundaries (moderate ε, beneficiary is group cohesion, victim is adaptive inclusion).',
    'Each reading produces a different constraint story with different ε values and different beneficiary/victim structures. The readings coexist as different parties'' interpretations of the same kernel (catastrophe memory practices). No single reading is ''correct'' — the readings are different structural claims about what the same ritual practice does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural differences between symbol_continuity_reading and sibling readings of catastrophe_memory_kernel').

omega_variable(
    false_summit_mountain_candidate,
    'Is the mountain classification (ritual as natural law of collective memory) a genuine natural law or a false summit that naturalizes institutional arrangements benefiting tradition-custodians?',
    'Examination of whether the mountain classification persists when beneficiaries are declared. The schema requires omegas when a mountain declares beneficiaries — this omega documents the natural-law vs constructed ambiguity. If the constraint''s operation depends on institutional enforcement (tradition-custodians suppressing modification), it is not a natural law. If the constraint persists without enforcement (communities spontaneously maintain ritual), it approaches natural law status.',
    'If false summit: the engine reclassifies to tangled_rope (default override) — ritual is coordination layered with extraction, not a natural law. If genuine mountain: the classification stands — ritual is inherent to collective memory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, conceptual, 'Whether mountain classification is genuine natural law or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbol_continuity_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symcont_tr_t0, symbol_continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(symcont_tr_t2, symbol_continuity_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(symcont_tr_t4, symbol_continuity_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(symcont_tr_t6, symbol_continuity_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(symcont_be_t0, symbol_continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(symcont_be_t2, symbol_continuity_reading, base_extractiveness, 2, 0.13).
narrative_ontology:measurement(symcont_be_t4, symbol_continuity_reading, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(symcont_be_t6, symbol_continuity_reading, base_extractiveness, 6, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(symcont_su_t0, symbol_continuity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(symcont_su_t2, symbol_continuity_reading, suppression_requirement, 2, 0.18).
narrative_ontology:measurement(symcont_su_t4, symbol_continuity_reading, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(symcont_su_t6, symbol_continuity_reading, suppression_requirement, 6, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(symbol_continuity_reading, survival_competence_reading).
narrative_ontology:affects_constraint(symbol_continuity_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(symbol_continuity_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The symbol_continuity_reading is one of four structurally distinct readings of the catastrophe_memory_kernel. Each reading has its own ε value, beneficiary/victim structure, and classification. The readings coexist as different parties' interpretations of the same kernel. The network links show that this reading affects (and is affected by) the sibling readings — they compete for interpretive authority over what ritual practices do.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
