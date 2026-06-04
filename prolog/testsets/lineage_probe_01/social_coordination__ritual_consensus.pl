% ============================================================================
% CONSTRAINT STORY: social_coordination__ritual_consensus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_coordination__ritual_consensus, []).

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
 *   constraint_id: social_coordination__ritual_consensus
 *   human_readable: Social Coordination Through Ritual and Sacralized Consensus
 *   domain: political/social
 *
 * SUMMARY:
 *   Large-scale social coordination through ritual and sacralized consensus
 *   operates through a distinctive mechanism: defection becomes cognitively
 *   unavailable rather than externally punished. Heterodox members cannot
 *   imagine themselves outside the sacralized frame because their identity is
 *   constituted through ritual participation and consensus membership. Ritual
 *   specialists benefit from maintaining and interpreting the consensus, but
 *   the coordination dividend they enable is genuine — collective action,
 *   resource pooling, mutual defense, and social cohesion all depend on the
 *   shared sacralized worldview. This constraint is ONE READING of a
 *   contested kernel about how social coordination is achieved: governance
 *   (formal authority), kinship obligation (inherited reciprocity), market
 *   exchange (voluntary price-driven coordination), and ritual consensus
 *   (cognitive unavailability of defection) are four competing explanations
 *   for the same phenomenon. This reading instantiates the
 *   ritual/sacralization mechanism as primary, with very high suppression at
 *   the level of imagination and low extractiveness because the benefit flows
 *   are bidirectional and embedded in genuine coordination function. The
 *   theater ratio has increased over the historical interval as secular
 *   critique has mounted and the sacralization has required increasing
 *   performative reinforcement to maintain apparent inevitability.
 *
 * KEY AGENTS:
 *   - Heterodox Members: Primary victim (powerless/identity_locked) — identity fused with ritual consensus; defection cognitively unavailable
 *   - Ritual Specialists: Primary beneficiary (institutional/arbitrage) — maintain and interpret sacralized consensus; capture status and legitimacy benefits
 *   - Marginal Believers: Secondary victim (moderate/constrained) — maintain public compliance for access to collective goods but privately dissent
 *   - Reform Coalition: Organized agents (organized/mobile) — build alternative coordination mechanisms (formal law, markets, secular governance) with sunset logic
 *   - Historicized Ritual: Institutional perspective (institutional/constrained) — post-reform societies where ritual persists as vestigial performance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent ritual mechanism as immutable feature of human cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_coordination__ritual_consensus, 0.28).
domain_priors:suppression_score(social_coordination__ritual_consensus, 0.82).
domain_priors:theater_ratio(social_coordination__ritual_consensus, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_coordination__ritual_consensus, extractiveness, 0.28).
narrative_ontology:constraint_metric(social_coordination__ritual_consensus, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(social_coordination__ritual_consensus, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_coordination__ritual_consensus, tangled_rope).
narrative_ontology:human_readable(social_coordination__ritual_consensus, "Social Coordination Through Ritual and Sacralized Consensus").
narrative_ontology:topic_domain(social_coordination__ritual_consensus, "political/social").

domain_priors:requires_active_enforcement(social_coordination__ritual_consensus).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(social_coordination__ritual_consensus, '2991eb5a-2ba1-4364-92f2-423d6b82c385').
narrative_ontology:cs_kernel_codification('2991eb5a-2ba1-4364-92f2-423d6b82c385', distributed).
narrative_ontology:cs_authority_grounding('2991eb5a-2ba1-4364-92f2-423d6b82c385', practice).
narrative_ontology:cs_interpretation_layer_present('2991eb5a-2ba1-4364-92f2-423d6b82c385').
narrative_ontology:cs_reading_relation('2991eb5a-2ba1-4364-92f2-423d6b82c385', social_coordination__governance, coexists_with).
narrative_ontology:cs_reading_relation('2991eb5a-2ba1-4364-92f2-423d6b82c385', social_coordination__kinship_obligation, coexists_with).
narrative_ontology:cs_reading_relation('2991eb5a-2ba1-4364-92f2-423d6b82c385', social_coordination__market_exchange, coexists_with).
narrative_ontology:cs_axiom('2991eb5a-2ba1-4364-92f2-423d6b82c385', foundational, cognitive_unavailability_sufficient_for_coordination).
narrative_ontology:cs_axiom_status(cognitive_unavailability_sufficient_for_coordination, holdable).
narrative_ontology:cs_axiom_grounding('2991eb5a-2ba1-4364-92f2-423d6b82c385', cognitive_unavailability_sufficient_for_coordination, empirically_contingent).
narrative_ontology:cs_axiom('2991eb5a-2ba1-4364-92f2-423d6b82c385', foundational, ritual_specialists_legitimate_interpreters).
narrative_ontology:cs_axiom_status(ritual_specialists_legitimate_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('2991eb5a-2ba1-4364-92f2-423d6b82c385', ritual_specialists_legitimate_interpreters, conventional).
narrative_ontology:cs_reference_frame('2991eb5a-2ba1-4364-92f2-423d6b82c385', sacralized_consensus_frame).
narrative_ontology:cs_drift_state('2991eb5a-2ba1-4364-92f2-423d6b82c385', contemporary_secular_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2991eb5a-2ba1-4364-92f2-423d6b82c385', '').
narrative_ontology:cs_kernel_id(social_coordination__ritual_consensus, social_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_coordination__ritual_consensus, ritual_specialists).
narrative_ontology:constraint_beneficiary(social_coordination__ritual_consensus, consensus_maintainers).
narrative_ontology:constraint_victim(social_coordination__ritual_consensus, heterodox_members).
narrative_ontology:constraint_victim(social_coordination__ritual_consensus, cognitive_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HETERODOX MEMBER (SNARE) — Identity fused with community membership and ritual participation. Defection is literally unthinkable because the agent's self-concept is constituted through the sacralized consensus. No external punishment needed — the agent cannot imagine themselves outside the ritual frame without experiencing identity dissolution. High suppression (0.82) operates at the level of imagination, not enforcement machinery. The agent is structurally mobile (could physically leave) but identity-locked (cannot conceptually exit).
constraint_indexing:constraint_classification(social_coordination__ritual_consensus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RITUAL SPECIALISTS (ROPE) — Maintain and interpret the sacralized consensus. Experience the constraint as pure coordination: the ritual specialists' role is to keep the consensus functioning, and they benefit from the coordination dividend (social cohesion, resource pooling, collective action capacity). Extraction is minimal and bidirectional — they give legitimacy to the ritual, they receive status and material support. Arbitrage exit: they could join another community but choose this one because their expertise and status are concentrated here.
constraint_indexing:constraint_classification(social_coordination__ritual_consensus, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: MARGINAL BELIEVER (TANGLED ROPE) — Maintains public consensus compliance for access to collective goods (resource pooling, mutual defense, ritual protection) but privately doubts or rejects the sacralized framing. Faces high costs to exit: social ostracism, loss of resource access, kinship damage. But also genuinely benefits from coordination functions — the ritual generates real collective capacity. Mixed experience: constrained by high exit costs, but experiences genuine coordination benefit alongside extraction of conformity.
constraint_indexing:constraint_classification(social_coordination__ritual_consensus, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents (missionaries, enlightenment movements, state bureaucracies) seeking to replace sacralized consensus with alternative coordination mechanisms (formal law, individual conscience, market exchange, secular governance). See the ritual constraint as temporary — the scaffold is the transition period where reform institutions are building parallel coordination capacity. Exit path: communities that adopt the reform framework exit the ritual constraint without losing coordination function. Sunset clause: as reform institutions mature, their coordination capacity will exceed the ritual mechanism, and the sacralized consensus will become optional rather than binding.
constraint_indexing:constraint_classification(social_coordination__ritual_consensus, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICIZED RITUAL (PITON) — From the perspective of societies that have already experienced the reform transition, the ritual appears as vestigial performance: maintaining ceremonial aspects (national anthems, oath-taking, commemorative rituals) without the cognitive unavailability that characterized the original sacralized consensus. The ritual persists through institutional inertia and nostalgic attachment, but its coordination function has been substantially replaced by formal institutions. Theater ratio (0.65) captures the performative residue: the ritual continues because 'this is how we do things,' not because defection is cognitively unavailable.
constraint_indexing:constraint_classification(social_coordination__ritual_consensus, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the sacralization of consensus and cognitive unavailability of defection appears as an inherent feature of how large-scale coordination has always worked: humans are fundamentally ritual creatures, and coordination through shared meaning-making is an immutable property of social organization itself. This perspective naturalizes the constraint as emerging inevitably from human cognitive architecture. However, the structural data contradicts the mountain classification — the engine's false summit detector identifies this as naturalization of a contingent institutional arrangement. The fact that reform movements can and do replace ritual consensus with alternative coordination mechanisms (formal law, markets, deliberative governance) proves the constraint is not immutable.
constraint_indexing:constraint_classification(social_coordination__ritual_consensus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_coordination__ritual_consensus_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_coordination__ritual_consensus, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_coordination__ritual_consensus, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_coordination__ritual_consensus, TR),
    TR >= 0.70.

:- end_tests(social_coordination__ritual_consensus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low and diffuse. The constraint's extraction is not concentrated or coercive — it operates through identity fusion and cognitive capture rather than resource asymmetry. Ritual specialists benefit from their legitimacy role, but the benefit is proportional to their actual contribution to coordination. The low extractiveness reflects that this is a genuinely mixed coordination-extraction mechanism: real collective goods are produced (social cohesion, mutual defense, resource pooling), and the specialists' role in maintaining those goods is real, not purely parasitic. Suppression (0.82): Very high, but operating at the level of imagination rather than enforcement. The cognitive unavailability of defection is the mechanism — heterodox members literally cannot imagine themselves outside the sacralized frame without experiencing identity dissolution. This is suppression of the highest order (nothing to punish because the alternative is unthinkable), yet it is not enforced by external coercion. Theater ratio (0.65): Moderate-high, rising over time. As secular critique has mounted and the sacralization has become contested, ritual specialists must work harder to maintain the appearance of inevitability. The ritual increasingly requires performative reinforcement rather than operating through taken-for-granted inevitability. The rising trajectory reflects the constraint's destabilization as alternative coordination mechanisms become thinkable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification from a single structural mechanism. The heterodox member experiences snare-level extraction through cognitive capture (identity_locked exit). The ritual specialists experience pure coordination (rope) — their role is genuinely coordinative and they benefit proportionally. The marginal believer experiences mixed benefit and constraint (tangled_rope) — they benefit from collective goods but are suppressed by conformity pressure. The reform coalition sees a temporary problem with a sunset (scaffold) — alternative coordination mechanisms are being built that will render sacralization optional. Historicized societies see vestigial performance (piton) — the ritual continues through inertia after its coordination function has been replaced. The civilizational analytical observer risks naturalizing the mechanism as immutable (mountain) — but the structural evidence shows it is contingent and replaceable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position. Heterodox members are victims of cognitive capture with identity_locked exit — high d (0.89) → high f(d) (1.28). Ritual specialists are beneficiaries with arbitrage exit — low d (0.05) → negative f(d) (-0.12). Marginal believers are mixed victims and beneficiaries with constrained exit — moderate d (0.55) → moderate f(d) (0.75). Reform coalition are organized agents with mobile exit — moderate d (0.40) → moderate f(d) (0.40). The directionality derivation shows why each perspective experiences different effective extractiveness chi despite identical base ε: the victims experience high chi, the beneficiaries experience low or negative chi, the mixed agents experience moderate chi. No overrides needed — the structural data flows correctly through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint resolves mandatrophy by being explicit about which reading of the social_coordination kernel it instantiates. The ritual_consensus reading proposes that large-scale coordination is achieved through sacralized consensus and cognitive unavailability of defection — not through governance authority (governance reading), kinship obligation (kinship reading), or market exchange (market reading). The constraint's extractiveness (0.28) and suppression (0.82) are consistent with this reading: low extractiveness because genuine coordination benefits flow, high suppression because cognitive capture operates at the level of imagination. The mandatrophy is resolved by showing that the constraint is a legitimate reading of a contested kernel, not a failed attempt to classify a single phenomenon. The false summit detector will correctly identify the analytical observer's mountain as naturalization of the kernel choice, revealing that the apparent inevitability of sacralized consensus depends on accepting this particular reading of what coordination requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_unavailability_vs_enforcement,
    'Is the suppression mechanism genuinely cognitive (alternatives unthinkable) or functionally equivalent to enforcement (high cost of defection creates apparent inevitability)?',
    'Analysis of members who have left communities: do they report ''I suddenly could imagine alternatives'' (cognitive shift) or ''I finally could afford the cost'' (structural shift)? Observation of defection rates under stress: do they spike when enforcement capacity fails (indicating suppression is enforcement-mediated) or remain stable (indicating cognitive lock)?',
    'If cognitive: the constraint is a genuine cognitive capture mechanism (snare from powerless perspective). If enforcement-mediated: the constraint is a tangled rope with suppression that appears cognitive but is structural. Classification of powerless perspective shifts from snare to tangled_rope if enforcement-based.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_unavailability_vs_enforcement, empirical, 'Whether suppression is cognitive unavailability or high-cost enforcement').

omega_variable(
    sacralization_necessity_for_coordination,
    'Does the coordination function genuinely depend on sacralization and cognitive unavailability, or can secular alternative mechanisms (formal law, deliberative process, market exchange) achieve the same coordination outcomes?',
    'Comparative analysis: societies that have transitioned from sacralized to secular coordination mechanisms. Did coordination capacity drop during transition (sacralization necessary) or remain stable or improve (sacralization is one implementation of coordination, not essential)? Rate of successful collective action pre/post transition.',
    'If sacralization necessary: the constraint is a mountain for large-scale coordination (unavoidable bottleneck). If alternatives work: the constraint is a contingent institutional arrangement, and the mountain classification is a false summit. Affects classification of analytical perspective and network claims about constraint inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacralization_necessity_for_coordination, empirical, 'Whether sacralization is necessary for large-scale coordination function').

omega_variable(
    ritual_specialist_extraction_degree,
    'How much benefit do ritual specialists extract through their monopoly on interpretation, relative to the coordination dividend they enable?',
    'Measurement of resource flows: what material/status benefits do ritual specialists capture relative to community size? Counterfactual analysis: what coordination outcomes would occur if interpretation authority were distributed (democratized ritual interpretation) vs. concentrated?',
    'If extraction is substantial: the rope classification is optimistic, and the constraint should be reclassified as tangled_rope with ritual specialists as primary beneficiaries and the wider community as secondary victims. If extraction is negligible: rope classification is correct, and specialists'' role is genuinely coordinative rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_specialist_extraction_degree, empirical, 'Magnitude of ritual specialist extraction relative to coordination benefit').

omega_variable(
    kernel_reading_contest,
    'Which reading of the social_coordination kernel best describes the actual mechanism through which large-scale societies achieve coordination?',
    'Historical and comparative institutional analysis: do governance institutions (formal law), kinship webs (extended obligation), market mechanisms (price signals), or ritual/sacralized consensus (cognitive unavailability) provide the primary coordination function across different societies and historical periods? Likely answer: all four readings capture real elements; no single reading is exclusively correct. The contest is not resolvable through empirical data alone — it depends on which elements the observer privileges as primary.',
    'If governance dominates: ritual_consensus reading appears as a secondary or vestigial mechanism, and the constraint should be reclassified as piton or rope. If ritual/sacralization dominates: the reading is correct as tangled_rope. If multiple readings coexist: the constraint is part of a distributed kernel where different institutional contexts instantiate different readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which social_coordination kernel reading captures the actual primary mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_coordination__ritual_consensus, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ritual_tr_t0, social_coordination__ritual_consensus, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ritual_tr_t3, social_coordination__ritual_consensus, theater_ratio, 3, 0.5).
narrative_ontology:measurement(ritual_tr_t6, social_coordination__ritual_consensus, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(ritual_be_t0, social_coordination__ritual_consensus, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ritual_be_t3, social_coordination__ritual_consensus, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(ritual_be_t6, social_coordination__ritual_consensus, base_extractiveness, 6, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(ritual_su_t0, social_coordination__ritual_consensus, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(ritual_su_t3, social_coordination__ritual_consensus, suppression_requirement, 3, 0.83).
narrative_ontology:measurement(ritual_su_t6, social_coordination__ritual_consensus, suppression_requirement, 6, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_coordination__ritual_consensus, identity_coordination).
narrative_ontology:affects_constraint(social_coordination__ritual_consensus, social_coordination__governance).
narrative_ontology:affects_constraint(social_coordination__ritual_consensus, social_coordination__kinship_obligation).
narrative_ontology:affects_constraint(social_coordination__ritual_consensus, social_coordination__market_exchange).

% DUAL FORMULATION NOTE:
% The social_coordination kernel decomposes into four constraint stories instantiating different readings: governance (formal authority as primary), kinship_obligation (inherited reciprocity as primary), market_exchange (price signals as primary), and ritual_consensus (sacralized consensus as primary). Each reading has different ε, beneficiary/victim sets, and suppression mechanisms. They are linked as sibling readings of the same kernel, not as hierarchical decomposition. Each reading coexists with the others; no reading logically forecloses another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
