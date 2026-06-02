% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Deuteronomy 7) as Contextually Superseded Constraint
 *   domain: biblical_hermeneutics/religious_ethics/commitment_systems
 *
 * SUMMARY:
 *   The herem command in Deuteronomy 7 — the divine directive to annihilate
 *   or permanently separate from subject populations in Canaan upon conquest
 *   — represents one of the most ethically intractable constraints in
 *   biblical law. The contextual-supersession reading argues that this
 *   constraint was functionally tied to the settlement period (Iron Age I
 *   population displacement and boundary establishment) and was morally
 *   superseded by the prophetic tradition's critique of covenantal
 *   conditionality (justice requirements override ethnic boundary
 *   maintenance) and by the Christian expansion of covenant beyond ethnicity.
 *   This reading acknowledges the historical reality of the command while
 *   denying its permanent authority. The constraint manifests as Tangled Rope
 *   under this reading: the prophetic supersession and universalist ethical
 *   framework provide genuine coordination benefits (textual coherence, moral
 *   consistency, inclusive identity), but the constraint's persistence in
 *   fundamentalist and literalist enforcement mechanisms generates
 *   substantial extraction (identity-fused enforcers, marginalized
 *   intermarriage populations, sectarian institutional gatekeeping). The
 *   constraint's theater has increased dramatically from ancient settlement
 *   (low theater — direct enforcement) through Talmudic elaboration (rising
 *   theater — increasingly complex halakhic reasoning that nominally
 *   preserves the obligation while practically evacuating it) to modern
 *   sectarian contexts (high theater — ritualized identity performance
 *   disconnected from functional enforcement).
 *
 * KEY AGENTS:
 *   - Prophetic Reform Tradition: Institutional beneficiary (institutional/arbitrage) — establishes authority grounding for supersession reading; reinforces institutional legitimacy through ethical coherence
 *   - Universalist Ethical Framework (Christian and Reform Jewish): Institutional beneficiary (institutional/arbitrage) — incorporates the constraint as historically-bounded directive; uses the supersession to demonstrate textual fidelity compatible with universal moral claims
 *   - Intermarriage Target Populations (Ancient Context): Primary victim (powerless/trapped) — subject to coercive separation enforced through covenant oath and settlement law
 *   - Fundamentalist Enforcer Communities (Contemporary Context): Dual-status victim (powerless-to-moderate/identity_locked) — identity fused with literal text enforcement; structurally mobile but cognitively trapped; bear extraction costs of maintaining the enforcer role
 *   - Reform-Critical Theological Community: Secondary actor (organized/constrained) — coordinates on historical-critical interpretation; bears institutional pressure and sectarian gatekeeping costs
 *   - Literal-Obedience Interpretive Apparatus: Institutional actor (institutional/arbitrage) — maintains performative reading apparatus through Talmudic elaboration and theological justification; perpetuates constraint through theater rather than functional enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.38).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.45).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Deuteronomy 7) as Contextually Superseded Constraint").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical_hermeneutics/religious_ethics/commitment_systems").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '80f57d6d-e2a4-42c5-9461-8577099c45cf').
narrative_ontology:cs_kernel_codification('80f57d6d-e2a4-42c5-9461-8577099c45cf', fixed_text).
narrative_ontology:cs_authority_grounding('80f57d6d-e2a4-42c5-9461-8577099c45cf', lineage).
narrative_ontology:cs_interpretation_layer_present('80f57d6d-e2a4-42c5-9461-8577099c45cf').
narrative_ontology:cs_reading_relation('80f57d6d-e2a4-42c5-9461-8577099c45cf', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('80f57d6d-e2a4-42c5-9461-8577099c45cf', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('80f57d6d-e2a4-42c5-9461-8577099c45cf', foundational, prophetic_universalism_overrides_ethnic_covenant).
narrative_ontology:cs_axiom_status(prophetic_universalism_overrides_ethnic_covenant, holdable).
narrative_ontology:cs_axiom_grounding('80f57d6d-e2a4-42c5-9461-8577099c45cf', prophetic_universalism_overrides_ethnic_covenant, deontological).
narrative_ontology:cs_axiom('80f57d6d-e2a4-42c5-9461-8577099c45cf', foundational, historical_contingency_revokes_eternal_applicability).
narrative_ontology:cs_axiom_status(historical_contingency_revokes_eternal_applicability, holdable).
narrative_ontology:cs_axiom_grounding('80f57d6d-e2a4-42c5-9461-8577099c45cf', historical_contingency_revokes_eternal_applicability, empirically_contingent).
narrative_ontology:cs_reference_frame('80f57d6d-e2a4-42c5-9461-8577099c45cf', prophetic_ethical_universalism).
narrative_ontology:cs_drift_state('80f57d6d-e2a4-42c5-9461-8577099c45cf', contemporary_sectarian_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80f57d6d-e2a4-42c5-9461-8577099c45cf', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, prophetic_reform_tradition).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, universalist_ethical_framework).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, intermarriage_target_populations).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_enforcer_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANCIENT INTERMARRIAGE TARGET (SNARE) — Subject populations without exit options face the herem prohibition enforced through covenant oath, settlement law, and divine sanction. Extraction is maximal: no structural alternative to separation or death is presented in the text. The constraint permits no negotiation, no consent, no arbitrage.
constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FUNDAMENTALIST ENFORCER IN CONTEMPORARY CONTEXT (SNARE via identity_locked) — Modern communities that treat the herem as an eternally binding command face severe structural extraction: their religious identity is constituted through covenant fidelity, which they understand as requiring separation enforcement. They are structurally mobile (can leave the community, can reinterpret doctrine) but identity-fused with the constraint's literal application. The constraint extracts by making enforcer fidelity to the literal text the primary mark of covenant loyalty. Chi is high because the enforcer experiences this as non-negotiable identity requirement despite having exit options at the material level.
constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: REFORM-CRITICAL THEOLOGICAL COMMUNITY (TANGLED ROPE) — Academic and progressive religious scholarship coordinating on historical-critical reading (the constraint had a settlement-period function, now superseded) benefits from epistemological coherence and ethical consistency. But constraint-enforcing traditions create significant costs: scholarly reputations damaged in sectarian contexts, families fractured by interpretive divergence, institutional pressure to maintain traditional readings. Both genuine coordination (shared interpretive method) and asymmetric extraction (gatekeeping of scholarly legitimacy, career risk) are present.
constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROPHETIC UNIVERSALIST TRADITION AS INSTITUTIONAL AUTHORITY (ROPE) — The Christian and Reform-Jewish institutional frameworks that have already adopted the contextual-supersession reading experience the constraint as pure coordination: communicating the ethical supersession (Jeremiah's critique of cult without justice, Jesus's expansion of covenant beyond ethnicity) allows believers to maintain textual fidelity while resolving the ethical contradiction. Arbitrage exit: can cite alternative biblical texts (Jonah, Ruth, universal love commands) to legitimate the reading. Net beneficiary — institutional authority reinforces itself through the coherence of this reading.
constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LITERAL-OBEDIENCE INTERPRETIVE APPARATUS (PITON) — Textual literalism regarding the herem persists in certain fundamentalist and Orthodox contexts through institutional inertia and identity-performance rather than coherent ethical reasoning. The apparatus produces theater: elaborate halakhic distinctions about when separation is mandated vs. permitted, Talmudic re-readings that nominally preserve the obligation while practically evacuating it, theological justifications that sound functional but perform identity maintenance. Theater ratio high because the interpretive work exceeds functional need — the apparatus survives primarily through ritualized transmission, not active enforcement.
constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL-LAW READING (MOUNTAIN) — From a civilizational perspective, ethical universalism is an inevitable discovery: once humans recognize the arbitrariness of ethnic boundaries and the universality of moral status, any constraint based on ethnic separation becomes logically incoherent. This perspective treats the supersession as a natural law of ethical development, making the constraint's obsolescence immutable. However, the structural data reveals a false summit: the constraint persists in specific communities because identity, institutional authority, and textual authority grounds it — not because universalism is compulsory.
constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(herem_command_dt7__contextual_supersession_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, TR),
    TR >= 0.70.

:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. In the ancient settlement context, the constraint was highly extractive (0.72) — enforced separation with coercive backing and no exit options for target populations. Under the contextual-supersession reading, extractiveness has declined because: (1) the constraint is explicitly reframed as historically-bounded rather than eternally binding, reducing the population experiencing it as absolute obligation; (2) prophetic and Christian institutional authorities have formally adopted the supersession, reducing active enforcement; (3) contemporary target populations have legal and social frameworks protecting intermarriage rights. However, extractiveness remains non-negligible (0.38) because: (1) fundamentalist and Orthodox communities still enforce separation, particularly through institutional pressure (excommunication, inheritance sanctions); (2) enforcer communities themselves bear extraction through identity-lock mechanisms; (3) the theater surrounding the constraint (elaborate justifications, conditional readings) masks the ongoing control function. Suppression (0.45): Moderate. The constraint's suppressive mechanisms have weakened dramatically (0.85 in ancient settlement → 0.45 contemporary) because exit options have multiplied: legal frameworks protect intermarriage, secular alternatives exist to religious identity, geographic and social mobility enable community exit. However, suppression remains significant within enforcer communities due to identity-lock (cognitive barriers to reinterpretation), institutional sanctions (excommunication, inheritance), and social stigma. Theater ratio (0.62): High. The constraint's contemporary manifestation is substantially theatrical. Talmudic reasoning has developed elaborate distinctions (e.g., when separation applies vs. when it's suspended, conditions under which intermarriage is permitted retroactively) that nominally preserve the obligation while practically evacuating enforcement. The theater increased over the measurement interval as the constraint transitioned from direct enforcement (settlement period) through complex theological justification (medieval/early modern period) to ritualized identity performance (contemporary fundamentalist contexts). The high theater indicates the constraint is being maintained through institutional inertia and identity affirmation rather than coherent functional reasoning.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a profound perspectival gap between the literal-enforcement perspective and the contextual-supersession perspective. The ancient target population experienced the herem as maximal extraction (Snare, 0.95 directionality). The contemporary enforcer community experiences it as identity-lock extraction (Snare via identity_locked exit options). The reform-critical theological community experiences it as mixed coordination and extraction (Tangled Rope) — they coordinate on a coherent ethical reading but face sectarian costs. The prophetic universalist institutional framework experiences it as pure coordination (Rope) — the supersession resolves an ethical contradiction and strengthens institutional legitimacy. The literal-obedience apparatus experiences it as performative (Piton) — the constraint is maintained through theater and ritualization rather than genuine functional reasoning. The analytical observer risks naturalizing the constraint as an immutable feature of monotheistic ethics (Mountain), but the structural data reveals this as a false summit — the constraint's persistence is contingent on specific institutional and identity choices, not logically necessitated.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary dramatically by agent and context. In the ancient settlement period, target populations faced d ≈ 1.0 (full extraction targets) because they bore all costs and had zero exit options — the constraint extracted completely from them. Prophetic universalist institutional authorities face d ≈ 0.1 (net beneficiaries) because the supersession strengthens their institutional coherence and legitimacy. Contemporary enforcer communities face d ≈ 0.75-0.85 (primarily victims despite institutional framing) because they bear heavy identity-fusion extraction costs, even though they nominally 'benefit' from covenant fidelity by their own framing. Reform-critical scholars face d ≈ 0.6 (mixed) because they coordinate on interpretive method but face sectarian gatekeeping costs. The piton perspective sees d ≈ 0.15 (net beneficiary through ritual performance) because the apparatus itself survives through the constraint's theatrical maintenance. These variations reflect genuine structural differences in how different agents relate to the constraint — the engine's derivation chain computes these from the declared beneficiary/victim structure and exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_supersession_mechanism,
    'Does the prophetic tradition (Jeremiah, Isaiah) actually constitute a formal supersession of the herem command, or is it reinterpretation of the herem''s purpose (justice-based separation vs. ethnicity-based)?',
    'Textual analysis of prophetic critique of cult-without-justice and universalist expansions (Jonah, Ruth, Servant Songs); determination of whether prophets explicitly reject the herem or reframe it as conditional on ethical grounds',
    'If formal supersession: this reading is well-grounded and represents genuine authority shift within tradition. If reframing only: the constraint remains active at the canonical level and the supersession is aspirational rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prophetic_supersession_mechanism, empirical, 'Whether prophetic tradition formally supersedes herem or reframes its application').

omega_variable(
    enforcement_mechanism_persistence,
    'To what degree does the herem command persist as an enforced constraint in contemporary communities that adopt this reading? Is the suppression value (0.45) accurate or does enforcement vary dramatically by sect?',
    'Ethnographic and historical analysis of enforcement patterns in Orthodox, Conservative, and fundamentalist communities; tracking of intermarriage enforcement, excommunication rates, institutional sanctions in last 50 years',
    'If enforcement persists at 0.45 level: suppression is well-calibrated. If enforcement is < 0.20 in most streams: constraint is largely theater (Piton reclassification). If enforcement > 0.70 in some sects: those communities experience higher chi, possible Snare reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_persistence, empirical, 'Contemporary enforcement patterns for herem command across Jewish and Christian communities').

omega_variable(
    ethical_universalism_contingency,
    'Is ethical universalism (the axiom grounding this reading''s supersession claim) a contingent historical development or a discoverable truth about moral reasoning? Does this distinction affect whether the constraint is truly ''superseded'' or merely ''disputed''?',
    'Philosophical analysis of universalist ethics grounding; comparison to historical emergence of universalism in other traditions (Stoicism, Confucianism, Islamic jurisprudence); assessment of whether universalism is inevitable or path-dependent',
    'If contingent: the constraint is not ''superseded'' (immutable) but rather ''contested'' (two live positions in ongoing dispute). If discoverable truth: supersession is justified and the mountain perspective''s inevitability framing holds. This determines whether the constraint is a Tangled Rope (coordination+extraction) or approaches Rope (coordination-dominant) at the civilizational scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_universalism_contingency, conceptual, 'Contingency status of ethical universalism relative to constraint supersession').

omega_variable(
    identity_locked_vs_trapped_boundary,
    'For fundamentalist enforcer communities, is the constraint''s binding force (identity fusion with literal text) genuinely structurally mobile (identity_locked) or are material barriers to exit (social ostracism, economic dependency, geographic isolation) determinative (trapped)?',
    'Comparison of exit capacity for enforcers with high vs. low social capital; tracking of communities where reinterpretation adoption is highest among those with external opportunity; assessment of whether post-exit suppression persistence indicates internalized lock',
    'If identity_locked is accurate: the enforcer sees Snare via cognitive frame. If trapped is more accurate: the enforcer is under Snare via material barriers. Classification remains Snare but resolution path differs — identity-locked requires cognitive reframing; trapped requires material resource access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_trapped_boundary, empirical, 'Whether fundamentalist enforcement is identity-fused or materially trapped').

omega_variable(
    kernel_reading_contest_structure,
    'What is the specific structural relationship between this reading (contextual supersession) and its siblings (durable separation, allegorical displacement)? Does this reading''s adoption functionally preclude the others, or can communities maintain multiple readings simultaneously?',
    'Historical and ethnographic analysis of Jewish and Christian communities; determination of whether denomination/sect primarily adopts one reading or syncretizes multiple readings in different contexts',
    'If readings genuinely foreclose each other: the kernel is actively contested with zero-sum stakes. If coexistence is possible: communities employ context-switching (herem applies in certain domains, not others) and the contest is over legitimacy ordering rather than logical foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, empirical, 'Structural relationship between contextual-supersession reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_ctx_theater_ancient_settlement, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(herem_ctx_theater_talmudic_elaboration, herem_command_dt7__contextual_supersession_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(herem_ctx_theater_modern_sectarian, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1850, 0.62).

% Extraction over time
narrative_ontology:measurement(herem_ctx_extractiveness_ancient_settlement, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(herem_ctx_extractiveness_post_prophetic, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 100, 0.45).
narrative_ontology:measurement(herem_ctx_extractiveness_reform_adoption, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1850, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(herem_ctx_suppression_ancient_settlement, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(herem_ctx_suppression_post_prophetic, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement(herem_ctx_suppression_reform_adoption, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1850, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem command kernel has three structurally distinct readings with different ε values and beneficiary/victim structures. Contextual-supersession (this file) ε=0.38, low extractiveness on intermarriage application. Durable-separation (sibling) ε≈0.65, high extractiveness on enforced separation. Allegorical-displacement (sibling) ε≈0.25, low extractiveness via metaphorical reinterpretation. These are not the same constraint viewed from different angles — their ε values differ substantially and reflect fundamentally different structural claims about the kernel's authority. See constraint family network entry for full taxonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
