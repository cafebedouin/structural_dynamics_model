% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__boundary_maintenance_reading, []).

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
 *   constraint_id: nicene_creed_authority__boundary_maintenance_reading
 *   human_readable: Nicene Creed Authority: Boundary Maintenance Reading
 *   domain: religious_history/theology/political_authority
 *
 * SUMMARY:
 *   The Nicene Creed (325 CE) establishes minimum doctrinal boundaries for
 *   Christian communion while claiming to permit interpretive diversity
 *   within those bounds. This constraint is one reading of a contested
 *   kernel: the Creed's authority. The boundary-maintenance reading
 *   interprets the Creed as a mechanism for preserving both doctrinal
 *   integrity and local autonomy — communities must affirm Nicene basics
 *   (homoousios, anti-Arianism) but retain interpretive freedom in areas the
 *   Creed does not address. This reading positions the Creed as a tangled
 *   rope: it coordinates the church's internal structure (benefiting orthodox
 *   maintainers and imperial authority) while extracting from extreme
 *   innovators (who face excommunication for exceeding boundaries). The
 *   reading differs structurally from two siblings: the imperial-uniformity
 *   reading (which sees the Creed primarily as a tool for political control)
 *   and the confessional reading (which treats creedal bounds as absolute and
 *   universally enforceable). The boundary-maintenance reading is
 *   historically live during the fourth and fifth centuries, when councils
 *   and bishops genuinely debated how much variation the Creed permitted and
 *   selectively enforced boundaries based on political context, theological
 *   judgment, and local stability.
 *
 * KEY AGENTS:
 *   - Orthodox Consensus Maintainers (institutional/arbitrage): Beneficiaries of boundary coordination; experience Creed as enabling unified communion structure without excessive constraint on orthodox elaboration
 *   - Extreme Theological Innovators (powerless/trapped): Victims; face excommunication or exile for exceeding doctrinal bounds; no exit available within Christian communion
 *   - Local Interpretive Traditions (moderate/constrained): Secondary victims and partial beneficiaries; constrained by requirement to affirm Nicene basics but permitted to develop local theological and liturgical elaboration
 *   - Imperial Ecclesiastical Authority (organized/constrained): Beneficiary and enforcer; extracts political legitimacy from doctrinal uniformity while coordinating church structure; enforces boundaries through imperial mechanisms (exile, property confiscation, council attendance mandates)
 *   - Organized Heterodox Coalition (organized/mobile): Secondary actor with exit options; can establish separate communion structures; experiences constraint as negotiable rather than total
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangements as inherent logical necessities; boundary maintenance appears as inevitable feature of any universal doctrinal religion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__boundary_maintenance_reading, 0.38).
domain_priors:suppression_score(nicene_creed_authority__boundary_maintenance_reading, 0.48).
domain_priors:theater_ratio(nicene_creed_authority__boundary_maintenance_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__boundary_maintenance_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(nicene_creed_authority__boundary_maintenance_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(nicene_creed_authority__boundary_maintenance_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__boundary_maintenance_reading, "Nicene Creed Authority: Boundary Maintenance Reading").
narrative_ontology:topic_domain(nicene_creed_authority__boundary_maintenance_reading, "religious_history/theology/political_authority").

domain_priors:requires_active_enforcement(nicene_creed_authority__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__boundary_maintenance_reading, '9313f991-5235-4f3e-9ea0-b1e03850a5ad').
narrative_ontology:cs_kernel_codification('9313f991-5235-4f3e-9ea0-b1e03850a5ad', formalized).
narrative_ontology:cs_authority_grounding('9313f991-5235-4f3e-9ea0-b1e03850a5ad', lineage).
narrative_ontology:cs_interpretation_layer_present('9313f991-5235-4f3e-9ea0-b1e03850a5ad').
narrative_ontology:cs_reading_relation('9313f991-5235-4f3e-9ea0-b1e03850a5ad', nicene_creed_authority__imperial_uniformity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9313f991-5235-4f3e-9ea0-b1e03850a5ad', nicene_creed_authority__confessional_reading, influences).
narrative_ontology:cs_axiom('9313f991-5235-4f3e-9ea0-b1e03850a5ad', foundational, doctrinal_bounds_permit_genuine_diversity).
narrative_ontology:cs_axiom_status(doctrinal_bounds_permit_genuine_diversity, holdable).
narrative_ontology:cs_axiom_grounding('9313f991-5235-4f3e-9ea0-b1e03850a5ad', doctrinal_bounds_permit_genuine_diversity, conventional).
narrative_ontology:cs_axiom('9313f991-5235-4f3e-9ea0-b1e03850a5ad', foundational, enforcement_selective_not_comprehensive).
narrative_ontology:cs_axiom_status(enforcement_selective_not_comprehensive, holdable).
narrative_ontology:cs_axiom_grounding('9313f991-5235-4f3e-9ea0-b1e03850a5ad', enforcement_selective_not_comprehensive, empirically_contingent).
narrative_ontology:cs_reference_frame('9313f991-5235-4f3e-9ea0-b1e03850a5ad', permissive_minimum_bounds_with_local_autonomy).
narrative_ontology:cs_drift_state('9313f991-5235-4f3e-9ea0-b1e03850a5ad', late_fourth_century_post_theodosius, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9313f991-5235-4f3e-9ea0-b1e03850a5ad', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(nicene_creed_authority__boundary_maintenance_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__boundary_maintenance_reading, orthodox_consensus_maintainers).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__boundary_maintenance_reading, imperial_ecclesiastical_authority).
narrative_ontology:constraint_victim(nicene_creed_authority__boundary_maintenance_reading, extreme_theological_innovators).
narrative_ontology:constraint_victim(nicene_creed_authority__boundary_maintenance_reading, local_interpretive_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTREME THEOLOGICAL INNOVATOR (SNARE) — Trapped by the Creed's boundary enforcement. Innovation beyond doctrinal tolerance faces excommunication, exile, or suppression. No exit available within Christian communion; leaving means abandoning religious identity and community. The Creed's logic forecloses their theological project entirely. Maximum experienced extraction.
constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL INTERPRETIVE TRADITION (TANGLED ROPE) — Constrained by the requirement to affirm Nicene boundaries while retaining local liturgical and interpretive autonomy. Moderate extraction: local practices face scrutiny and potential correction, but permitted diversity provides real agency. The tradition benefits from communion with the larger church (coordination gain) while bearing suppression costs from enforcement.
constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORTHODOX CONSENSUS MAINTAINERS (ROPE) — Experience the Creed as pure coordination mechanism. Benefits from unified doctrinal boundaries that prevent schism and consolidate authority. Exit options (modify creed, permit unlimited variation) would cost more than maintaining status quo. Net beneficiary experiencing the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: IMPERIAL ECCLESIASTICAL AUTHORITY (TANGLED ROPE) — Organized institutional actor (empire + church hierarchy) extracts political legitimacy from doctrinal uniformity while coordinating the church's internal structure. The Creed enforces both genuine coordination (unified communion structure) and extractive political control (suppression of heterodox movements that threaten imperial authority). Active enforcement reveals hybrid function.
constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ORGANIZED HETERODOX COALITION (ROPE) — Organized agents with theological alternatives (Arian networks, Nestorian communities) experience the Creed as a coordination problem, not extraction. They can exit through schism, establish separate communion structures, and build alternative institutional hierarchies. Mobility reduces experienced extraction despite suppression attempts. The Creed constrains but does not trap; they perceive negotiation space.
constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Creed appears as an inevitable feature of any religious institution that claims universal doctrinal truth: boundaries between orthodoxy and heresy must exist logically and necessarily. This perspective naturalizes the boundary enforcement as inherent to the commitment structure itself. However, the presence of identifiable beneficiaries (orthodox maintainers, imperial authority) suggests this is a false summit — the boundary's specific form and enforcement are contingent, not necessary.
constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__boundary_maintenance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nicene_creed_authority__boundary_maintenance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(nicene_creed_authority__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The boundary-maintenance reading interprets the Creed as hybrid mechanism: genuine coordination (preventing schism, preserving communion) combined with selective extraction (suppression of innovators). The extractiveness is not as high as the imperial-uniformity reading would suggest (which emphasizes coercive control) nor as low as the confessional reading (which treats bounds as absolute necessity rather than choice). At 0.38, extractiveness reflects that the Creed does coordinate authentic doctrinal consensus while also targeting specific innovators for suppression. Suppression (0.48): Moderate. The Creed enables excommunication and exile but does not create total behavioral control; local practices retain significant autonomy. Suppression increases over the measurement interval (0.28 → 0.65) as imperial enforcement mechanisms intensify and councils become more rigorous in boundary enforcement. Theater ratio (0.55): Moderate. The Creed's initial function (doctrinal preservation) is genuine, but performative elements emerge as enforcement becomes ritualized. By the late fourth century (t=100), the theater ratio rises to 0.72, reflecting that the original doctrinal clarity fades and enforcement becomes increasingly about jurisdictional control and ceremonial compliance rather than actual theological substance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same doctrinal mechanism (the Creed) appears as radically different constraints from different structural positions. The orthodox maintainers see pure coordination (Rope): the Creed solves the problem of preventing schism and maintaining communion unity. The extreme innovators see pure extraction (Snare): the Creed traps them by foreclosing theological innovation and offering no exit. Local traditions see mixed effects (Tangled Rope): they benefit from communion coordination but bear suppression costs. The imperial authority sees enforced coordination with political extraction (Tangled Rope): genuine church structure, but deployed for political control. Organized heterodox groups see a negotiable constraint (Rope): they can exit through schism and establish alternatives. The civilizational analytical observer risks seeing natural law (Mountain): 'any universal religion must have doctrinal boundaries, therefore the Creed is inevitable.' The perspectival gap reveals that boundaries exist (coordinate function is real) but their specific form and enforcement (extraction mechanism) is contingent on institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's relationship to the extraction flow. Orthodox maintainers are primary beneficiaries; they have arbitrage options (could permit unlimited variation; don't, because it costs more than maintaining boundaries) and experience negative effective extraction (the constraint subsidizes their position). Extreme innovators are primary victims; they are trapped (leaving means abandoning religious identity); they experience maximum effective extraction through the sigmoid f(d). Local traditions are secondary victims with constrained exits (could leave Christianity entirely, but at high identity/community cost); they experience moderate effective extraction. The imperial authority as organized beneficiary with constrained exits experiences moderate extraction (they benefit from control but the Creed also constrains their ability to impose arbitrary demands; they must maintain appearance of theological coherence). Organized heterodox groups with mobile exits (can schism) experience lower effective extraction than trapped individuals. The analytical observer at the civilizational level occupies a position where d ≈ 0.72 (canonical for analytical), but their analysis risks being captured by the mountain frame (naturalizing as inevitable) — this is the oracle gap (Theorem 4) instantiated: the analytical position needs the boundary-maintenance reading to see what the natural-law frame prevents them from seeing.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between the Creed's coordination function (preventing schism, enabling communion) and its extraction function (suppressing innovation, enforcing uniformity). The resolution is not 'which type is correct?' but 'which function dominates, and from whose perspective?' The boundary-maintenance reading holds both functions as structurally real: the Creed coordinates genuine doctrinal consensus while simultaneously extracting from those who exceed its bounds. The Tangled Rope classification captures both. However, the measurement trajectory reveals a drift: as the interval progresses from immediate post-Nicaea (t=0) to late fourth century (t=100), the suppression_requirement rises sharply (0.28 → 0.65) and theater_ratio rises (0.32 → 0.72). This drift suggests the constraint is drifting from genuine coordination toward degraded extraction — the original coordination function (preserving shared doctrine) persists, but increasingly buried under enforcement theater and political extraction. By t=100, the constraint approaches Snare territory (high suppression + high theater + rising extraction). This drift is captured in the omega about enforcement vs. boundaries: if the extraction is driven by enforcement intensity rather than boundary logic itself, then the boundary-maintenance reading's confidence in the 'permitted diversity' claim erodes over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_vs_enforcement_ambiguity,
    'Does the Creed''s extractiveness derive from doctrinal boundaries existing per se, or from the *specific enforcement mechanisms* used to maintain them?',
    'Comparative historical analysis: non-imperial councils that affirmed doctrinal boundaries but lacked enforcement capacity; alternative enforcement modalities (persuasion, dialogue, voluntary schism) vs coercive enforcement; measurement of extraction levels across different enforcement regimes',
    'If boundaries alone: reclassifies toward Mountain (logical necessity). If enforcement mechanisms: reclassifies toward Snare or remains Tangled Rope depending on enforcement intensity. This omega locates whether the constraint''s force is structural (logical) or institutional (political).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_vs_enforcement_ambiguity, conceptual, 'Whether extractiveness derives from boundaries themselves or from enforcement mechanisms').

omega_variable(
    interpretive_autonomy_actual_vs_claimed,
    'Do the Creed''s doctrinal bounds actually permit meaningful interpretive diversity, or is the ''permitted diversity'' an illusion maintained by bishops'' selective enforcement?',
    'Historical documentation of interpretive innovation accepted vs rejected within boundaries; correlation between theological novelty and suppression response; measurement of variation in local practices pre- and post-Creed',
    'If interpretive autonomy is real: Tangled Rope classification holds; coordination benefits are genuine. If illusory (boundaries permit no actual variation): reclassifies toward Snare; the theater of permitted diversity masks total behavioral control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_autonomy_actual_vs_claimed, empirical, 'Whether permitted diversity is substantive or performative').

omega_variable(
    alternative_boundary_mechanisms,
    'Could the same coordination function (preventing schism, preserving communion unity) be achieved through alternative mechanisms that do not extract from theological innovators?',
    'Historical counterfactual: analysis of councils that attempted dialogue-based dispute resolution; comparative study of religious traditions with inclusive boundary mechanisms; modeling of alternative enforcement structures (advisory bounds vs mandatory confession, graduated separation vs excommunication)',
    'If alternatives exist: the specific Creed is revealed as a choice point, not necessity; extractiveness is contingent on institutional preference. If no alternatives: boundaries approach necessity and mountain classification gains warrant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_boundary_mechanisms, conceptual, 'Existence of alternative boundary mechanisms with lower extraction').

omega_variable(
    reading_vs_imperial_uniformity_foreclosure,
    'Does the boundary-maintenance reading logically foreclose the imperial-uniformity reading, or do both readings represent live alternative commitments that can coexist within different institutional actors?',
    'Logical analysis: Can an actor simultaneously believe (a) boundaries exist to preserve authentic doctrine and diversity within bounds (boundary-maintenance), and (b) boundaries exist primarily to enforce political uniformity (imperial-uniformity)? If yes: coexist_with. If no: one forecloses the other.',
    'If foreclosure: the readings cannot be held together; commitment to one denies the other. If coexistence: both readings describe the same constraint accurately from different positions; the Creed serves both functions simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_imperial_uniformity_foreclosure, conceptual, 'Logical relationship between boundary-maintenance and imperial-uniformity readings').

omega_variable(
    confessional_vs_boundary_maintenance_drift,
    'Does the boundary-maintenance reading''s reference frame (permissive bounds with selective enforcement) remain coherent after the Reformation, when confessional reading emerges with rigid, universally-enforced boundaries?',
    'Historical analysis of creedal evolution: did post-Reformation confessions abandon permissiveness or redefine it? Did enforcement mechanisms intensify across the tradition family, or did different confessions maintain different enforcement? What architectural shifts enabled confessional rigidity?',
    'If confessional reading abandons boundary-maintenance''s permissiveness: boundary-maintenance reading''s reference frame erodes into the confessional reading''s opposite axiom. If they coexist at different institutional sites: the boundary-maintenance reading remains stable, merely localized to pre-Reformation Catholicism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_vs_boundary_maintenance_drift, empirical, 'Post-Reformation relationship between boundary-maintenance and confessional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__boundary_maintenance_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_bm_theater_t0_immediate_post_325, nicene_creed_authority__boundary_maintenance_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(nicene_bm_theater_t50_mid_fourth_century, nicene_creed_authority__boundary_maintenance_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(nicene_bm_theater_t100_late_fourth_century, nicene_creed_authority__boundary_maintenance_reading, theater_ratio, 100, 0.72).

% Extraction over time
narrative_ontology:measurement(nicene_bm_extractiveness_t0_immediate_post_325, nicene_creed_authority__boundary_maintenance_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nicene_bm_extractiveness_t50_mid_fourth_century, nicene_creed_authority__boundary_maintenance_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(nicene_bm_extractiveness_t100_late_fourth_century, nicene_creed_authority__boundary_maintenance_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(nicene_bm_suppression_t0_immediate_post_325, nicene_creed_authority__boundary_maintenance_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(nicene_bm_suppression_t50_mid_fourth_century, nicene_creed_authority__boundary_maintenance_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(nicene_bm_suppression_t100_late_fourth_century, nicene_creed_authority__boundary_maintenance_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__boundary_maintenance_reading, nicene_creed_authority__imperial_uniformity_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__boundary_maintenance_reading, nicene_creed_authority__confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__boundary_maintenance_reading, arianism_suppression_mechanism).
narrative_ontology:affects_constraint(nicene_creed_authority__boundary_maintenance_reading, fourth_century_imperial_ecclesiastical_authority).

% DUAL FORMULATION NOTE:
% The Nicene Creed authority kernel decomposes into three structurally distinct constraint stories: (1) boundary_maintenance_reading (this file)—moderate epsilon, hybrid coordination-extraction, permissive bounds with selective enforcement; (2) imperial_uniformity_reading—higher epsilon, emphasizes coercive control and political extraction; (3) confessional_reading—boundary-maintenance's axiom of permissive diversity is formally overridden, replaced with absolute, universally-enforced bounds. Each reading has its own epsilon, its own beneficiary/victim structure, its own measurements tracking how the Creed's function shifts. The network links trace the influence chain: boundary-maintenance coexists with imperial-uniformity (live positions across different institutions) and influences the confessional reading (which abandons boundary-maintenance's permissiveness entirely).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__boundary_maintenance_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
