% ============================================================================
% CONSTRAINT STORY: ideological_belief_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ideological_belief_systems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ideological_belief_systems
 *   human_readable: Ideological Belief Systems as Extraction and Coordination Mechanisms
 *   domain: epistemic/social/political
 *
 * SUMMARY:
 *   Ideological belief systems operate as structural constraints that
 *   coordinate collective action while simultaneously extracting cognitive
 *   resources from adherents. This constraint exhibits the full spectrum of
 *   DR classification depending on observer position: the adherent
 *   experiences identity-locked cognitive capture (snare), the leadership
 *   experiences pure coordination mechanism (rope), the institutional
 *   apparatus performs degraded ritual (piton), the intellectual is caught in
 *   professional identity fusion (tangled rope), and the epistemic commons
 *   bears unmeasured extraction costs (snare). The constraint's
 *   extractiveness has increased over the measurement interval (0.35 to 0.58)
 *   as ideological enforcement has migrated from external enforcement
 *   (education, law, media) to internalized cognitive capture. Theater ratio
 *   has increased (0.40 to 0.75) indicating that institutional ideological
 *   mechanisms are increasingly performative and disconnected from genuine
 *   coordination function. The constraint demonstrates how the same
 *   structural mechanism can appear as natural law, coordination benefit, and
 *   extraction depending on structural position.
 *
 * KEY AGENTS:
 *   - Ideological Adherent: Primary victim (powerless/identity_locked) — identity fused with belief system, experiences maximum extraction, cannot perceive exit option
 *   - Ideological Leadership: Primary beneficiary (institutional/arbitrage) — maintains ideological narrative, arbitrages between belief and action, accumulates institutional power
 *   - Institutional Apparatus: Secondary beneficiary (institutional/constrained) — schools, media, state maintain ideological enforcement through ritual and inertia
 *   - Skeptical Community Member: Mixed victim/beneficiary (moderate/constrained) — derives coordination benefit from community but bears conformity costs
 *   - Intellectual Captured by Ideology: Secondary victim (moderate/identity_locked) — professional identity fused with ideological framework, cannot pursue contradictory evidence
 *   - Epistemic Commons: Victim (powerless/trapped) — abstract collective good that cannot exit, experiences total suppression of excluded domains
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ideological_belief_systems, 0.58).
domain_priors:suppression_score(ideological_belief_systems, 0.65).
domain_priors:theater_ratio(ideological_belief_systems, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ideological_belief_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(ideological_belief_systems, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ideological_belief_systems, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ideological_belief_systems, tangled_rope).
narrative_ontology:human_readable(ideological_belief_systems, "Ideological Belief Systems as Extraction and Coordination Mechanisms").
narrative_ontology:topic_domain(ideological_belief_systems, "epistemic/social/political").

domain_priors:requires_active_enforcement(ideological_belief_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ideological_belief_systems, ideological_leadership).
narrative_ontology:constraint_beneficiary(ideological_belief_systems, institutional_gatekeepers).
narrative_ontology:constraint_victim(ideological_belief_systems, adherent_cognition).
narrative_ontology:constraint_victim(ideological_belief_systems, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADHERENT (SNARE) — Structurally mobile but identity-fused with belief system. Exit would require abandoning constitutive identity. Suppression manifests through epistemic closure, community sanctions for doubt, and internalized guilt over disloyal questioning. No material barriers but cognitive capture is complete. The adherent experiences maximum extraction: cognitive labor devoted to belief maintenance, opportunity cost of alternative knowledge pursuit, psychological cost of enforced certainty.
constraint_indexing:constraint_classification(ideological_belief_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: SKEPTICAL COMMUNITY MEMBER (TANGLED ROPE) — Derives genuine coordination benefit from shared worldview and community bonds, but also bears costs: conformity pressure, cost of expressing doubt, resource commitment to ideological performance. Exit is costly but possible (career damage, social ostracism, relocation). Both extraction and coordination are real — not fully captured by either Rope or Snare alone.
constraint_indexing:constraint_classification(ideological_belief_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IDEOLOGICAL LEADERSHIP (ROPE) — Experiences the belief system primarily as a coordination mechanism. Leadership benefits from collective action, shared purpose, and institutional legitimacy generated by ideological alignment. Leadership can arbitrage: they can shift narratives, allocate resources, or switch frameworks without fundamental identity disruption. For them, the constraint is pure coordination with institutional benefits.
constraint_indexing:constraint_classification(ideological_belief_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL APPARATUS (PITON) — Schools, media, state bureaucracy maintain ideological performance long after adaptive function has degraded. Theater ratio (0.68) reflects that much institutional ideological enforcement is ritual: flag salutes, loyalty oaths, ideological examinations that no longer serve coordination but persist through inertia. The apparatus is weakly bound to the ideology — institutions maintain performance because alternatives aren't established, not because the ideology is functionally essential.
constraint_indexing:constraint_classification(ideological_belief_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTELLECTUAL CAPTURED BY IDEOLOGY (TANGLED ROPE) — Professional identity fused with ideological framework. The intellectual experiences both genuine coordination (scholarly community, research infrastructure, intellectual tradition) and extraction (prohibition on exploring contradictory evidence, career risk of heterodoxy, cognitive capture that prevents seeing the constraint). Identity-locked exit option reflects that questioning the ideology would require abandoning professional identity. The extraction is not intentionally malicious but structurally real.
constraint_indexing:constraint_classification(ideological_belief_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: EPISTEMIC COMMONS (SNARE) — The abstract collective good of human knowledge is trapped and bears the maximum cost of ideological constraint. Epistemic commons cannot organize, has no agent to advocate for it, and experiences total suppression: entire domains of inquiry are prohibited, contradictory evidence is excluded, competing explanatory frameworks are delegitimized. From this perspective, the constraint is pure extraction masquerading as coordination — the 'shared purpose' and 'collective good' are fictions that conceal systematic extraction of cognitive resources.
constraint_indexing:constraint_classification(ideological_belief_systems, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, ideological belief systems appear as immutable features of human cognition: agents require simplifying narratives to coordinate large-scale action, and any large enough collective must enforce epistemic boundaries. Ideology appears as a natural law — humans cannot think without frames, frames require gatekeeping, gatekeeping requires suppression. However, the structural data contradicts the mountain classification — the engine's false summit detector identifies this as naturalization of socially contingent arrangements.
constraint_indexing:constraint_classification(ideological_belief_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ideological_belief_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ideological_belief_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ideological_belief_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ideological_belief_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ideological_belief_systems, TR),
    TR >= 0.70.

:- end_tests(ideological_belief_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts cognitive resources through identity fusion (identity_locked adherents), enforced conformity (suppression of doubt), opportunity cost (time devoted to belief maintenance rather than alternative knowledge), and psychological cost (internalized guilt, enforced certainty). The extraction is not a direct material transfer but a systematic redirection of human cognition toward ideology-aligned concerns. The metric reflects that ideological extraction has grown over the interval as enforcement has shifted from external (school curriculum, state propaganda) to internalized (identity fusion, cognitive capture). Suppression (0.65): High. Mechanisms include community sanctions for doubt, epistemic closure (rejection of contradictory evidence), delegitimization of competing frameworks, career risk for heterodoxy, and internalized censorship. Suppression is enforced through psychological mechanisms (identity threat, belonging anxiety) and social mechanisms (community ostracism, professional exclusion). The high suppression reflects that ideological constraint operates with minimal material coercion — the apparatus has successfully internalized enforcement through cognitive capture. Theater ratio (0.68): High. Increasing over the interval. Institutional ideological performance (patriotic rituals, loyalty oaths, ideological examinations) has become increasingly decoupled from actual coordination function. The apparatus maintains these performances through inertia and because alternatives aren't established, not because the performances serve genuine coordination. As adherent cognition becomes more directly captured (less need for external enforcement), institutional theater becomes more ornamental.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival gap between leadership and adherent perspectives. Leadership classifies as Rope (pure coordination), adherent classifies as Snare (pure extraction). The gap is not due to disagreement about facts but to different structural positions relative to the constraint. From leadership position, the coordination function is real and primary — ideology solves the problem of mobilizing large collectives. From adherent position, the extraction is real and primary — identity fusion prevents perceiving the coordination as voluntary. The gap reveals the constraint's essential structure: it appears different from different structural positions because the mechanism is asymmetric extraction disguised as symmetric coordination. Inter-institutional gap: the intellectual (moderate/identity_locked) experiences a different extraction than the adherent (powerless/identity_locked) because the intellectual has more institutional position and professional benefit — identity lock is present in both but combined with different power and exit options. Same-level lateral gap: the skeptical community member (moderate/constrained) and the intellectual (moderate/identity_locked) both occupy moderate power but experience different exit options and different classification outcomes — the differentiation reflects that structural mobility (constrained vs identity_locked) produces different perspectives despite equal nominal power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Leadership (institutional/arbitrage) experiences low d — they benefit from the constraint, can arbitrage away if incentives shift, accumulate institutional power. D ≈ 0.15, f(d) ≈ -0.01, they experience negative χ (subsidy). Adherents (powerless/identity_locked) experience high d — they bear extraction costs, cannot exit despite structural mobility (cognitive capture through identity lock), have no institutional power. D ≈ 0.89, f(d) ≈ 1.28, they experience high χ (extraction). The intellectual (moderate/identity_locked) experiences moderate-high d — professional identity fused with ideology but with more agency than pure adherents. D ≈ 0.75, f(d) ≈ 1.10. The skeptical community member (moderate/constrained) experiences moderate d — genuine exit barriers (social ostracism, career damage) but not identity lock. D ≈ 0.55, f(d) ≈ 0.75. The institutional apparatus (institutional/constrained) experiences moderate-high d — constrained by path dependency and inertia but benefits from maintaining the ideological performance. D ≈ 0.60, f(d) ≈ 0.85. The epistemic commons (powerless/trapped) experiences maximum d — cannot organize, cannot exit, experiences total suppression. D ≈ 0.95, f(d) ≈ 1.42.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by revealing how ideology operates as a coordination solution that is optimized for leadership benefit, not for symmetric problem-solving. The false positive (treating ideology as pure coordination / Rope) emerges when leadership perspectives dominate analysis — from their position, ideology genuinely solves large-scale coordination with voluntary adherence. The false negative (treating ideology as pure extraction / Snare) emerges from adherent perspectives — they experience only the extraction costs. The Tangled Rope classification is the structural truth: ideologies DO solve genuine coordination problems (large-scale action without centralized coercion) AND extract resources from adherents (cognitive labor, opportunity costs, psychological control). The tangled rope is not a compromise classification but a recognition that both functions are real and inseparable. The mandatrophy is resolved by examining the distribution of coordination benefits and extraction costs: if benefits concentrate on leadership (arbitrage options, institutional power, narrative control) while costs distribute to adherents (cognitive capture, conformity pressure, epistemic closure), the constraint is extraction asymmetrically masked as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'Below what scale does ideological coordination become genuinely necessary for collective action, versus optional?',
    'Comparative analysis of large-scale coordination: identification of successful non-ideological coordination (open-source software, scientific communities with weak ideological enforcement, decentralized networks). Threshold where ideological suppression becomes structurally required vs. merely convenient for leadership.',
    'If threshold is high (large collectives require ideology): more perspectives classify as Rope, theater_ratio interpretation shifts to ''necessary overhead.'' If threshold is low (ideology is optional even at scale): more perspectives classify as Snare, extracted cognition becomes harder to justify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Threshold scale below which ideological coordination is optional').

omega_variable(
    identity_lock_reversibility,
    'What proportion of adherents who have exited their ideological systems report that exit required cognitive reprogramming (identity lock) versus external circumstance (material constraint)?',
    'Longitudinal interviews with ex-believers; psychological assessments of cognitive capture; analysis of exit narratives to distinguish structural barriers from identity fusion.',
    'If majority report identity lock: the identity_locked exit option is correct, extraction mechanism is cognitive rather than material, adherent perspective correctly classifies as Snare. If majority report external barriers: actual exit options are more constrained/trapped than identity_locked, suppression is primarily structural not cognitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Proportion of ideological exits driven by identity lock vs. external constraint').

omega_variable(
    epistemic_commons_recovery_trajectory,
    'After ideological constraint is lifted, how quickly does the epistemic commons recover excluded domains of inquiry and contradictory evidence?',
    'Historical analysis: post-liberation science in formerly ideologically constrained societies (Soviet bloc, post-Mao China, post-apartheid South Africa). Time to reintroduction of forbidden topics, publication rates in reactivated research areas, rehabilitation of purged scholars.',
    'If recovery is fast (< 1 generational cohort): epistemic damage is containable, constraint is temporary (scaffold perspective validated). If recovery is slow (> 2 generations): epistemic commons suffers persistent degradation (snare perspective from commons view is correct, extraction is intergenerational).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_commons_recovery_trajectory, empirical, 'Recovery trajectory of epistemic commons after ideological constraint removal').

omega_variable(
    benign_vs_malignant_ideology,
    'Is the distinction between ''benign coordination ideology'' and ''malignant extractive ideology'' conceptually sharp or continuous?',
    'Specification of formal criteria: boundary detection via extractiveness metrics, suppression structure, victim identification. Test on corpus: apply DR classification to ideologies with known historical outcomes. Can we predict which ideologies will degrade to extraction versus sustaining genuine coordination?',
    'If sharp: ideologies can be classified a priori; early intervention possible. If continuous: the constraint moves along Rope-Snare spectrum over time; today''s benign coordination becomes tomorrow''s extraction as leadership incentives shift. Classification becomes temporal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(benign_vs_malignant_ideology, conceptual, 'Whether benign and malignant ideology form a sharp distinction or continuous spectrum').

omega_variable(
    cognitive_capture_versus_rational_commitment,
    'How do we distinguish between an adherent who is genuinely persuaded by ideology (rational commitment) versus cognitively captured (identity lock)?',
    'Behavioral tests: adherent response to contradictory evidence, consistency of belief under stress, ability to articulate alternative framings, willingness to engage heterodox perspectives. Hypothetical exit scenarios: would adherent exit if cost were removed? If answer changes based on framing, cognitive capture is present.',
    'If most adherents are rationally committed: classification shifts toward Rope, extraction metric is lower, suppression is consensus enforcement not cognitive capture. If most are cognitively captured: classification remains Snare, exit option correctly identity_locked, extraction mechanism is fundamentally cognitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capture_versus_rational_commitment, empirical, 'Distinction between rational commitment and cognitive capture in adherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ideological_belief_systems, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ideo_tr_t0, ideological_belief_systems, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ideo_tr_t10, ideological_belief_systems, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ideo_tr_t20, ideological_belief_systems, theater_ratio, 20, 0.68).
narrative_ontology:measurement(ideo_tr_t30, ideological_belief_systems, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(ideo_be_t0, ideological_belief_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ideo_be_t10, ideological_belief_systems, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ideo_be_t20, ideological_belief_systems, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ideo_be_t30, ideological_belief_systems, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ideological_belief_systems, identity_coordination).
narrative_ontology:affects_constraint(ideological_belief_systems, epistemic_closure_mechanism).
narrative_ontology:affects_constraint(ideological_belief_systems, leadership_authority_legitimation).
narrative_ontology:affects_constraint(ideological_belief_systems, cognitive_capture_through_belonging).

% DUAL FORMULATION NOTE:
% Ideological belief systems decompose into three structurally distinct constraints with different ε values: (1) identity_coordination (ε≈0.35, how shared identity enables large-scale coordination), (2) cognitive_capture (ε≈0.72, how belief systems fuse with personal identity), (3) epistemic_suppression (ε≈0.68, how ideological frameworks exclude contradictory evidence). This story models the unified phenomenon; downstream stories address the mechanistic decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
