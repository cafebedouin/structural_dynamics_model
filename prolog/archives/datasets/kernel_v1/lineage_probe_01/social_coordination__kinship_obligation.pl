% ============================================================================
% CONSTRAINT STORY: social_coordination__kinship_obligation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_coordination__kinship_obligation, []).

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
 *   constraint_id: social_coordination__kinship_obligation
 *   human_readable: Kinship Obligation and Inherited Reciprocity as Social Coordination
 *   domain: social/political
 *
 * SUMMARY:
 *   Kinship obligation and inherited reciprocity represent one reading of the
 *   social_coordination kernel—a stabilized commitment to the claim that
 *   large-scale social coordination is achieved through extended webs of kin
 *   obligation that precede and outlast any formal authority. This reading
 *   instantiates a constraint in which obligation is ascribed rather than
 *   chosen, beneficiaries are identifiable (lineage elders and patrilineal
 *   authority structures), and victims are identifiable (junior members and
 *   out-marrying individuals constrained by inherited obligation). The
 *   constraint coordinates legitimate social functions—labor sharing,
 *   resource management, child-rearing, old-age support—while simultaneously
 *   extracting resources, controlling life-course decisions, and suppressing
 *   alternatives. The tension between coordination and extraction is the
 *   defining structural feature: kinship obligation cannot be classified as
 *   pure rope (coordination) because it forecloses alternatives and
 *   concentrates authority; it cannot be classified as pure snare
 *   (extraction) because it genuinely solves coordination problems that
 *   formal authority and market exchange often fail to solve. The constraint
 *   is tangled_rope: it requires active enforcement (community sanction,
 *   resource control, social death for defection), it generates both real
 *   coordination benefits and real extraction costs, and the distribution of
 *   these costs and benefits is asymmetric (elders benefit, juniors bear
 *   costs).
 *
 * KEY AGENTS:
 *   - Lineage Elders: Primary beneficiaries (institutional/arbitrage) — control resources, make decisions, receive deference and support in old age
 *   - Junior Members: Primary victims (powerless/trapped) — face life-course control, deferred autonomy, resource subordination, and compounding extraction across generations
 *   - Out-Marrying Members: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused with kinship membership; exit requires social death and identity dissolution
 *   - Lateral Kin (Cousins, Collateral): Mixed position (moderate/constrained) — benefit from kinship network while bearing obligation costs; constrained exit because alternatives carry significant status loss
 *   - Diaspora Community: Organized actors (organized/constrained) — out-migrants maintaining kinship ties while developing alternative coordination mechanisms; showing incipient scaffold dynamics
 *   - Nation-State Authority: Institutional actor (institutional/mobile) — formally claims authority monopoly but relies on kinship obligation for social coordination; maintains theatrical recognition while deferring actual enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing kinship obligation as immutable law when it is actually a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_coordination__kinship_obligation, 0.38).
domain_priors:suppression_score(social_coordination__kinship_obligation, 0.72).
domain_priors:theater_ratio(social_coordination__kinship_obligation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_coordination__kinship_obligation, extractiveness, 0.38).
narrative_ontology:constraint_metric(social_coordination__kinship_obligation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(social_coordination__kinship_obligation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_coordination__kinship_obligation, tangled_rope).
narrative_ontology:human_readable(social_coordination__kinship_obligation, "Kinship Obligation and Inherited Reciprocity as Social Coordination").
narrative_ontology:topic_domain(social_coordination__kinship_obligation, "social/political").

domain_priors:requires_active_enforcement(social_coordination__kinship_obligation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(social_coordination__kinship_obligation, '7d751b67-b4bb-4fbb-a753-48012711a784').
narrative_ontology:cs_kernel_codification('7d751b67-b4bb-4fbb-a753-48012711a784', distributed).
narrative_ontology:cs_authority_grounding('7d751b67-b4bb-4fbb-a753-48012711a784', lineage).
narrative_ontology:cs_interpretation_layer_present('7d751b67-b4bb-4fbb-a753-48012711a784').
narrative_ontology:cs_reading_relation('7d751b67-b4bb-4fbb-a753-48012711a784', social_coordination__governance, coexists_with).
narrative_ontology:cs_reading_relation('7d751b67-b4bb-4fbb-a753-48012711a784', social_coordination__market_exchange, coexists_with).
narrative_ontology:cs_reading_relation('7d751b67-b4bb-4fbb-a753-48012711a784', social_coordination__ritual_consensus, coexists_with).
narrative_ontology:cs_axiom('7d751b67-b4bb-4fbb-a753-48012711a784', foundational, kinship_obligation_precedence).
narrative_ontology:cs_axiom_status(kinship_obligation_precedence, holdable).
narrative_ontology:cs_axiom_grounding('7d751b67-b4bb-4fbb-a753-48012711a784', kinship_obligation_precedence, conventional).
narrative_ontology:cs_axiom('7d751b67-b4bb-4fbb-a753-48012711a784', foundational, inherited_reciprocity_necessity).
narrative_ontology:cs_axiom_status(inherited_reciprocity_necessity, overridden).
narrative_ontology:cs_axiom_grounding('7d751b67-b4bb-4fbb-a753-48012711a784', inherited_reciprocity_necessity, instrumental).
narrative_ontology:cs_reference_frame('7d751b67-b4bb-4fbb-a753-48012711a784', lineage_authority_primacy).
narrative_ontology:cs_drift_state('7d751b67-b4bb-4fbb-a753-48012711a784', contemporary_modernized_societies, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d751b67-b4bb-4fbb-a753-48012711a784', '').
narrative_ontology:cs_kernel_id(social_coordination__kinship_obligation, social_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_coordination__kinship_obligation, lineage_elders).
narrative_ontology:constraint_beneficiary(social_coordination__kinship_obligation, patrilineal_authority).
narrative_ontology:constraint_victim(social_coordination__kinship_obligation, junior_members).
narrative_ontology:constraint_victim(social_coordination__kinship_obligation, out_marrying_members).
narrative_ontology:constraint_victim(social_coordination__kinship_obligation, identity_constrained_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OUT-MARRYING MEMBER (SNARE) — Structurally mobile (could physically leave) but identity-fused with lineage membership. Exit would require abandoning kinship identity, severing social bonds, and becoming a pariah in both origin and destination communities. The obligation is experienced as internal (cannot conceive of themselves outside the kinship frame) even though the binding mechanism is external (community sanction, disinheritance, social death). High suppression of alternatives; no meaningful exit path within the identity frame.
constraint_indexing:constraint_classification(social_coordination__kinship_obligation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: JUNIOR MEMBER (SNARE, TRAPPED VARIANT) — At generational timescale, junior members face accumulated extraction through life-course control: deferred marriage, labor obligation, resource exclusion, and subordination. The constraint is experienced as a mountain at biographical scale (appears immutable because authority claims it as tradition) but as snare at generational scale (the extraction accumulates and compounds). Material barriers to exit: no independent income, no housing options, no legal standing to claim resources. Pure extraction with suppression enforced through economic dependency.
constraint_indexing:constraint_classification(social_coordination__kinship_obligation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: LATERAL KIN (TANGLED ROPE) — Cousins and collateral relatives occupy a mixed position: they benefit from kinship coordination (claims on labor, resources, social standing within the network) but also bear costs (obligations to juniors, constraints on marriage choices, subordination to lineage decisions). The constraint coordinates their collective action (managing shared resources, labor exchange) while extracting asymmetric control through authority concentration. Constrained exit because alternatives exist (individual careers, nuclear family) but carry significant cost (status loss, resource cuts, community ostracism).
constraint_indexing:constraint_classification(social_coordination__kinship_obligation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LINEAGE ELDER (ROPE) — Experiences the kinship system as pure coordination: mobilizing labor, managing resources, maintaining stability across generations. The elder's authority is grounded in their role as coordinator and custodian of collective welfare. They benefit substantially (resource control, decision power, status) but also see their benefits as compensation for coordination burden. The constraint appears as a functional necessity rather than extraction. Arbitrage exit: elders can leverage their position across domains (political councils, trade networks, marriage alliances) without abandoning kinship authority.
constraint_indexing:constraint_classification(social_coordination__kinship_obligation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: DIASPORA COMMUNITY (TANGLED ROPE, ORGANIZED) — Out-migrants who maintain kinship ties while developing alternative coordination mechanisms. They experience the constraint as mixed: kinship networks remain functionally critical for remittance routing, marriage alliance, and identity maintenance, but they also perceive alternatives (formal contract, individual achievement, voluntary association) that reduce suppression. Constrained exit because abandoning kinship ties carries substantial cost (resource loss, identity dissolution, community sanction), but also potential for negotiated reformation of obligations (reduced labor duty, renegotiated resource claims). This perspective shows incipient scaffold logic: kinship obligation may be transitioning from pure extraction toward negotiated coordination in diaspora contexts.
constraint_indexing:constraint_classification(social_coordination__kinship_obligation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: NATION-STATE AUTHORITY (PITON) — Formally claims monopoly on law and authority but relies on kinship obligation systems to achieve social coordination at scales the state cannot reach. State enforcement of kinship obligation is largely theatrical: courts recognize customary law, police defer to elder mediation, and formal legal authority ratifies rather than creates kinship sanction. The theater ratio is high (0.62) because the state's formal involvement is performative—the real enforcement happens through community action, social sanction, and resource control. The constraint persists through institutional inertia: kinship obligation preceded the state and outlasts formal legal authority, but the state maintains theatrical recognition to avoid the administrative cost of direct governance.
constraint_indexing:constraint_classification(social_coordination__kinship_obligation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, kinship obligation appears as an immutable law of human organization: all large-scale societies have relied on kinship networks, inheritance rules, and reciprocity norms as the foundation for coordination. This perspective sees the constraint as emerging naturally from the structure of human reproduction and social bonding. However, the declared beneficiaries (lineage elders) and the measurable suppression (0.72) suggest this is a false summit—the naturalization obscures that kinship obligation is a contingent institutional arrangement that benefits specific agents and extracts from others.
constraint_indexing:constraint_classification(social_coordination__kinship_obligation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_coordination__kinship_obligation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_coordination__kinship_obligation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_coordination__kinship_obligation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_coordination__kinship_obligation, TR),
    TR >= 0.70.

:- end_tests(social_coordination__kinship_obligation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The kinship obligation system extracts control over life-course decisions (marriage, labor, resource use) and concentrates economic authority in elders, but much of the extraction is non-monetary—it is control rather than wealth transfer. Resource extraction is less severe than in debt-trap or labor-exploitation constraints because kinship obligation also provides welfare functions (old-age support, childcare, subsistence insurance). The measurement trajectory shows rising extractiveness (0.28 → 0.38 → 0.45) as the system encounters alternatives and modernization pressures; the constraint requires increasing enforcement as the functional justification weakens. Suppression (0.72): High. Kinship obligation is ascribed rather than chosen, and alternatives are heavily suppressed through social sanction, disinheritance, and community ostracism. Defection from obligation carries severe cost: loss of identity, resource access, marriage alliance, and social standing. Exit options are severely constrained, particularly for out-marrying members who experience identity_locked exit (internal fusion with kinship frame prevents conceiving of exit despite material ability). Suppression rises over the measurement interval (0.65 → 0.72 → 0.78) as modernization provides material alternatives that the system must actively suppress. Theater ratio (0.55): Moderate. Kinship obligation has real functional content (it does coordinate labor, manage resources, and provide welfare), but it also involves significant performative elements: ritual deference, ceremonial reciprocity, inheritance rules that exceed functional necessity, and formalized dispute resolution. The theater ratio rises (0.42 → 0.55 → 0.62) as state authority becomes more salient; the state's theatrical recognition of customary law (courts ratifying elder decisions rather than creating them) increases the performative layer. At high theater, the constraint approaches piton classification, but the functional coordination component remains substantial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows dramatic perspectival variation across the observation site. The lineage elder (institutional/arbitrage) experiences rope—genuine coordination that they perceive as fair compensation for burden. The junior member (powerless/trapped) experiences snare—pure extraction with no exit and no compensation. The out-marrying member (powerless/identity_locked) experiences snare—structurally mobile but identity-fused, unable to exercise exit even though material barriers are lower than for trapped members. The diaspora community (organized/constrained) experiences tangled_rope—kinship obligation remains functionally important for remittances and identity, but alternatives exist at acceptable cost. The nation-state (institutional/mobile) experiences piton—the state maintains theatrical recognition of kinship authority while actually relying on it for governance, aware that the system is degraded compared to formal law but not yet replaced. The analytical observer risks mountain—naturalizing kinship obligation as immutable law of human organization rather than recognizing it as a contested institutional arrangement with identifiable beneficiaries. The perspectival gaps are large and fundamental: elders see coordination; juniors see extraction; the system's functionality depends on this gap remaining unresolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position: beneficiary status, power level, and exit options. Lineage elders (institutional/arbitrage) have d ≈ 0.05 (beneficiary with easy exit) → low f(d) → low χ → rope classification. Junior members (powerless/trapped) have d ≈ 0.95 (victim with no exit) → high f(d) → high χ → snare classification. Out-marrying members (powerless/identity_locked) have d ≈ 0.89 (victim with psychological barrier to exit despite material mobility) → high f(d) → high χ → snare classification. The identity_locked exit option reveals a binding mechanism that is partially internalized: the agent is not physically trapped (they have mobility and could leave) but is trapped by identity fusion with the kinship frame. This distinction is critical: identity_locked agents at biographical timescale perceive rope (the obligation could be broken in principle), but at the moment of considering actual exit, the identity frame makes exit literally unthinkable—they experience it as snare even though the structural classification of rope is technically accurate if they could shift their identity frame. This gap between structural and perceived classification is the diagnostic signal for identity lock.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is resolved through recognizing that the kinship reading instantiates tangled_rope, not rope. If kinship obligation were pure rope (coordination with minimal extraction), there would be no conceptual tension—the system would simply be an efficient coordination mechanism. The tension arises because kinship obligation has real coordination functions AND real extractive effects, with asymmetric distribution. The false summit risk appears in perspective 7: the analytical observer might classify kinship obligation as mountain (immutable law of human organization) rather than recognizing it as a contingent institutional arrangement with identifiable beneficiaries (lineage elders) and victims (juniors, out-marrying members). The constraint story resolves this by declaring beneficiaries and victims in base_properties, triggering the false summit detection engine. The measurement trajectory showing rising suppression and rising theater ratio indicates that the constraint is degrading: as alternatives become available, the system requires increasing enforcement and increasing performative content. This pattern is diagnostic of an extraction mechanism that was previously justified as coordination but is now revealed as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kinship_obligation_vs_identity_fusion,
    'Is the binding mechanism structural (external barriers to exit) or internalized (identity fusion that makes exit unthinkable)?',
    'Post-exit behavior analysis: measure suppression persistence after material barriers are removed. If suppression persists (shame, identity loss, internal conflict despite legal freedom), the mechanism is partially internalized. If suppression collapses when barriers are removed, the mechanism is purely structural.',
    'If internalized: the constraint is identity-locked with high psychological cost to exit, even when material barriers dissolve. Reclassify exit_options from trapped to identity_locked; recognize that legal emancipation does not dissolve the obligation. If structural: the constraint is contingent on material conditions; modernization and wealth accumulation should reduce suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kinship_obligation_vs_identity_fusion, empirical, 'Structural vs. internalized binding mechanism in kinship obligation').

omega_variable(
    kinship_as_coordination_or_extraction,
    'Does kinship obligation coordinate genuine collective action (reduce transaction costs, manage shared resources) or does it primarily extract resources and control from juniors to benefit elders?',
    'Comparative institutional analysis: measure resource flows, decision authority, and welfare outcomes across kinship-coordinated vs. alternative systems (market, state, voluntary association). Assess whether coordination functions (labor sharing, child-rearing, old-age support) could be provided more efficiently through alternative mechanisms.',
    'If primarily coordinative: the constraint is legitimately tangled_rope with real cooperation benefits alongside extraction. If primarily extractive: the constraint is snare with coordination framing as cover story. If mixed but tilted: identify the balance point and whether alternatives reduce extraction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kinship_as_coordination_or_extraction, empirical, 'Balance between coordination function and extractive mechanism in kinship obligation').

omega_variable(
    reading_contest_underdetermination,
    'Which reading of the social_coordination kernel is instantiated by actual institutional practice—kinship_obligation, governance, market_exchange, or ritual_consensus?',
    'Domain ethnography: map actual coordination mechanisms in a specific community or society. Identify which mechanism(s) dominate (kinship ties, formal authority, price signals, ritual consensus). For mixed systems, estimate the proportion of coordination achieved through each mechanism at different scales.',
    'If kinship dominates: kinship_obligation reading is confirmed. If multiple readings coexist at different scales (kinship at local, governance at regional, market at transregional): the kernel permits multiple readings simultaneously, and constraint stories should reflect the scale-dependent classification (e.g., snare at local scale, rope at regional, tangled_rope at transregional). If governance or market readings dominate: kinship_obligation is residual or nostalgic, suggesting piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_underdetermination, empirical, 'Empirical dominance of kinship vs. alternative coordination readings in actual social practice').

omega_variable(
    diaspora_reformation_trajectory,
    'In diaspora communities, is kinship obligation reforming into negotiated reciprocal commitment (scaffold with sunset toward voluntary association), or is it persisting unchanged despite material conditions changing?',
    'Longitudinal ethnography of diaspora communities: track changes in obligation intensity, resource flows, marriage rules, and labor exchange over generational timescale. Measure whether diaspora youth experience the obligation as binding or aspirational.',
    'If reforming toward negotiated basis: the constraint is transitioning from snare (trapped) to tangled_rope (constrained) to potentially scaffold (with sunset). If persisting unchanged: the constraint''s extractiveness may be increasing as younger members develop alternative resources and perceive the obligation as arbitrary rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_reformation_trajectory, empirical, 'Reformation or persistence of kinship obligation in diaspora communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_coordination__kinship_obligation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kinship_tr_t0, social_coordination__kinship_obligation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(kinship_tr_t50, social_coordination__kinship_obligation, theater_ratio, 50, 0.55).
narrative_ontology:measurement(kinship_tr_t100, social_coordination__kinship_obligation, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(kinship_be_t0, social_coordination__kinship_obligation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kinship_be_t50, social_coordination__kinship_obligation, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(kinship_be_t100, social_coordination__kinship_obligation, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(kinship_su_t0, social_coordination__kinship_obligation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(kinship_su_t50, social_coordination__kinship_obligation, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(kinship_su_t100, social_coordination__kinship_obligation, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_coordination__kinship_obligation, attachment_coordination).
narrative_ontology:affects_constraint(social_coordination__kinship_obligation, social_coordination__governance).
narrative_ontology:affects_constraint(social_coordination__kinship_obligation, social_coordination__market_exchange).
narrative_ontology:affects_constraint(social_coordination__kinship_obligation, social_coordination__ritual_consensus).

% DUAL FORMULATION NOTE:
% The social_coordination kernel contains four competing readings about how large-scale coordination is achieved. Each reading instantiates a distinct constraint story with its own extractiveness value, beneficiary/victim structure, and measurement trajectory. This story (kinship_obligation) is upstream of the other readings empirically but logically symmetric with them—none forecloses the others within actual institutional practice. The network links represent the reading contest within the kernel, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
