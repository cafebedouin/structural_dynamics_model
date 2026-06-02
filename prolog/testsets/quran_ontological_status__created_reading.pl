% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Makhlūq Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   The 'created reading' of Qur'anic ontological status asserts that God's
 *   speech (revelation) is temporally created — produced at a specific
 *   historical moment — while God's essence transcends all temporal artifacts
 *   including the text itself. This reading emerged in early Islamic
 *   intellectual history through the Mu'tazilite schools and periodic
 *   rationalist movements, and it remains a live theological position in
 *   reform-oriented and philosophical Islamic traditions. The created reading
 *   is one among three structurally distinct claims about revelation's
 *   ontology, each producing different institutional consequences and
 *   beneficiary/victim configurations. This constraint story instantiates
 *   ONLY the created reading as a clean ε-invariant claim with its own
 *   extractiveness profile, beneficiaries, and victims. The sibling readings
 *   (uncreated/eternal reading, state-enforced creation reading) are separate
 *   constraints with separate ε values and their own JSON files — they
 *   coexist in institutional space but are not conflated here. The created
 *   reading itself classifies as tangled rope because it simultaneously
 *   functions as a coordination solution (enabling rational theology and
 *   hermeneutic flexibility) and an extraction mechanism (benefiting
 *   rationalist schools and reformers at the expense of traditionalist
 *   authority and literalist identity claims).
 *
 * KEY AGENTS:
 *   - Rationalist Theologians & Mu'tazilite Schools: Primary beneficiary (institutional/arbitrage) — the created reading grants them hermeneutic authority; can adopt or discard framework based on philosophical coherence
 *   - Literalist Communities: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with uncreated premise; exit would require becoming different people
 *   - Traditionalist Jurists: Secondary victim (moderate/constrained) — institutional authority depends on textual fixity; high cost to adopt created reading; some compensating benefit from interpretive flexibility for jurisprudential problem-solving
 *   - Reform Movements & Modernist Schools: Organized beneficiary (organized/constrained) — use created reading to navigate modernity; see it as temporary scaffold with sunset once pluralism normalizes textual meaning as collective negotiation
 *   - State Authority / Political Establishment: Institutional user (institutional/constrained) — benefits from decoupling legitimacy from literal textual law; faces constant suppression burden from literalist opposition
 *   - Institutional Memory of Theological Canon: Degraded carrier (institutional/arbitrage) — perpetuates the dispute through scholarship and transmission but original function largely atrophied; maintained by institutional inertia
 *   - Analytical Observer: Risks false summit (analytical/analytical) — views creation doctrine as logical necessity rather than historically contingent institutional claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.48).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.52).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, 'aa46884a-4932-4aae-ad29-1f2c99a7d09b').
narrative_ontology:cs_kernel_codification('aa46884a-4932-4aae-ad29-1f2c99a7d09b', formalized).
narrative_ontology:cs_authority_grounding('aa46884a-4932-4aae-ad29-1f2c99a7d09b', lineage).
narrative_ontology:cs_interpretation_layer_present('aa46884a-4932-4aae-ad29-1f2c99a7d09b').
narrative_ontology:cs_reading_relation('aa46884a-4932-4aae-ad29-1f2c99a7d09b', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa46884a-4932-4aae-ad29-1f2c99a7d09b', quran_ontological_status__state_enforced_reading, influences).
narrative_ontology:cs_axiom('aa46884a-4932-4aae-ad29-1f2c99a7d09b', foundational, divine_transcendence_compatible_with_temporal_revelation).
narrative_ontology:cs_axiom_status(divine_transcendence_compatible_with_temporal_revelation, holdable).
narrative_ontology:cs_axiom_grounding('aa46884a-4932-4aae-ad29-1f2c99a7d09b', divine_transcendence_compatible_with_temporal_revelation, deontological).
narrative_ontology:cs_axiom('aa46884a-4932-4aae-ad29-1f2c99a7d09b', foundational, rational_hermeneutics_can_access_divine_intention).
narrative_ontology:cs_axiom_status(rational_hermeneutics_can_access_divine_intention, holdable).
narrative_ontology:cs_axiom_grounding('aa46884a-4932-4aae-ad29-1f2c99a7d09b', rational_hermeneutics_can_access_divine_intention, instrumental).
narrative_ontology:cs_reference_frame('aa46884a-4932-4aae-ad29-1f2c99a7d09b', rationalist_theology_framework).
narrative_ontology:cs_drift_state('aa46884a-4932-4aae-ad29-1f2c99a7d09b', contemporary_pluralist_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aa46884a-4932-4aae-ad29-1f2c99a7d09b', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, mu_tazilite_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_rationality).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, textual_fixity_authority).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, unmediated_revelation_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITERALIST COMMUNITY (SNARE) — Structurally mobile (could adopt rationalist hermeneutics) but identity-fused with the premise that God's speech is uncreated and eternally fixed. Exit would require abandoning the foundational identity claim that direct access to divine speech is possible without interpretive mediation. Experiences the created reading as extraction of their epistemic authority and spiritual legitimacy. Maximum extraction because the binding is cognitive rather than material — the community cannot exit even when alternatives are available.
constraint_indexing:constraint_classification(quran_ontological_status__created_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: TRADITIONALIST JURISTS (TANGLED ROPE) — Face high costs to exit (institutional position depends on textual fixity, career built on literalist authority) but also benefit from coordination: the created reading enables jurisprudential flexibility when textual literal meaning conflicts with social necessity. Constrained exit but genuine mixed experience — some authority retained through interpretive privilege, but overall position weakened. Moderate extraction with compensating coordination benefit.
constraint_indexing:constraint_classification(quran_ontological_status__created_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RATIONALIST THEOLOGIANS & MU'TAZILITE SCHOOLS (ROPE) — Experience the created reading as pure coordination: it solves the theological problem of how God remains transcendent while revelation becomes historically comprehensible. Arbitrage exit available — can adopt or discard the framework based on philosophical coherence. Net beneficiary position: hermeneutic authority accrues to rational theology; interpretive flexibility enables both orthodoxy and reform. Low experienced extraction because this perspective is the reading's native home.
constraint_indexing:constraint_classification(quran_ontological_status__created_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REFORM MOVEMENTS & MODERNIST SCHOOLS (SCAFFOLD) — Organized agents with clear exit path: the created reading is a transitional framework enabling social adaptation without abandoning Islamic authority. See the uncreated reading as unsustainable in the context of modern pluralism and historical consciousness. Low effective extraction because the reform coalition has agency and sees a sunset — as textual meaning becomes collectively negotiated rather than individually revealed, the creation framework's function diminishes. Coordinating principle with enforced exit deadline.
constraint_indexing:constraint_classification(quran_ontological_status__created_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE AUTHORITY / POLITICAL ESTABLISHMENT (TANGLED ROPE) — Institutional actor using the created reading to justify political authority independent of textual prescription. Benefits from flexibility: the creation doctrine decouples state legitimacy from literal Qur'anic law, enabling rational governance. But constrained by religious opposition and periodic legitimacy crises requiring reassertion of Islamic authority. Mixed extraction: the state gains policy flexibility but must continuously suppress literalist challenge and manage religious legitimacy.
constraint_indexing:constraint_classification(quran_ontological_status__created_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL MEMORY / THEOLOGICAL CANON (PITON) — The created/uncreated dispute persists in formal Islamic theology as an historical marker of past rationalist movements (Mu'tazilites, medieval Ash'arites in rationalist mode, 19th-century reformers) but the functional purpose has largely degraded. Contemporary Islamic institutions maintain the framework through scholarly transmission and textual commentary, but the dispute's original function — adjudicating how to reconcile divine transcendence with human rationality — has shifted to new vocabularies (authenticity, cultural context, historical interpretation). Theater ratio high because the institutional apparatus (seminary curricula, theological literature, scholarly debate) persists largely through momentum rather than active enforcement or genuine coordination.
constraint_indexing:constraint_classification(quran_ontological_status__created_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From the perspective of formal logic, either God's speech is created (temporally contingent, logically consistent with divine transcendence) or uncreated (eternally necessary, logically consistent with preservation of textual fixity). These form an exclusive pair at the logical level — no third term. This perspective risks naturalizing the dichotomy as a logical law rather than recognizing it as a historically contingent interpretive problem. However, the structural data contradicts mountain classification: identifiable beneficiaries and victims exist; textual fixity authority derives from institutional power, not logical necessity; the entire dispute is contingent on accepting the theological premise that God's transcendence requires explanation through human reason.
constraint_indexing:constraint_classification(quran_ontological_status__created_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_ontological_status__created_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_ontological_status__created_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__created_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_ontological_status__created_reading, TR),
    TR >= 0.70.

:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The created reading produces asymmetric benefit: rationalist theologians and reform movements gain hermeneutic authority and policy flexibility, while traditionalist institutions and literalist communities lose their foundation claim. The extraction is not maximal (traditionalists retain some authority through interpretation; literalists retain spiritual community) but significant. Measurement trajectory shows rising extractiveness from 0.35 to 0.48 over the interval, reflecting accumulating institutional pressure as modernization increases the cost of maintaining the uncreated claim in pluralist contexts. Suppression (0.52): Moderate. Multiple mechanisms: (1) institutional pressure from modernizing states and reform movements pushing the created reading; (2) literalist counter-pressure defending uncreated orthodoxy; (3) cognitive suppression — the identity-locked binding prevents literalist communities from even recognizing the created reading as a live option. Theater ratio (0.65): Moderate-high and rising. The institutional apparatus (seminary curricula, theological commentaries, scholarly polemics) perpetuates the dispute, but its original problem-solving function has shifted. Contemporary Islamic institutions invoke the creation doctrine when needed for flexibility (state law, educational reform) but revert to uncreated language for spiritual authority. The performative content increases as the doctrine becomes tools for institutional positioning rather than genuine theological problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   The created reading exhibits maximal perspectival disagreement. Rationalist theologians classify it as enabling pure coordination (rope) — solving the problem of reconciling divine transcendence with human rationality. Literalist communities locked into the uncreated reading experience it as pure extraction (snare) of their epistemic authority and spiritual identity, with no exit option because their identity is constituted through the uncreated claim. Traditionalist jurists classify it as mixed (tangled rope) — it constrains their literal-text authority but enables jurisprudential flexibility for social problems. Reform movements see it as a temporary solution with a sunset (scaffold) — useful during the transition to pluralism but destined to be superseded by fully secular-rational frameworks. The institutional memory of the theological canon (piton) views the dispute as largely performative — perpetuated through scholarly transmission but drained of original problem-solving force. The analytical observer risks misclassifying it as logical necessity (mountain) rather than recognizing it as a historically contingent institutional claim. No single perspective is 'wrong' — all are legitimate readings of the constraint from structurally different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position relative to this specific constraint. Rationalist theologians: beneficiary with arbitrage exit → low d → negative χ. Literalist communities: victim with identity-locked exit (structurally mobile but identity-fused) → high d initially, but the identity-lock binding mechanism means they experience this constraint as immutable even though it is structurally contingent. This is the diagnostic signal of identity_locked: at biographical time, they see the constraint as mountain (unchangeable) whereas organized agents at the same temporal scale see it as rope or tangled rope (changeable in principle). Traditionalist jurists: victim with constrained exit (high cost but surmountable) → moderate-high d. Reform movements: beneficiary with constrained exit (tied to modernization trajectory, sunset if modernization reverses) → low d. State authority: beneficiary with constrained exit (political legitimacy requirements restrict arbitrage) → low-moderate d. The perspectival gap emerges: beneficiaries see coordination (rope) or temporary support (scaffold), while victims see extraction (snare for identity-locked, tangled rope for constrained). The false summit (analytical/mountain) represents the risk that logical framing naturalizes what is institutionally contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is resolved by recognizing that the created reading is one partial reading of a multi-faceted theological problem, not a universal claim about revelation. At the created reading's own level (institutional beneficiaries and victims, real authority claims), it is a genuine tangled rope: the reading simultaneously enables rational theology (genuine coordination function) and displaces traditionalist authority (genuine extraction). The mandatrophy dissolves when we stop asking 'is this coordination or extraction?' and instead ask 'for whom, and under what structural conditions?' For rationalist theologians, it is coordination. For literalist communities identity-locked to the uncreated reading, it is extraction. For traditionalists with structural flexibility, it is mixed. The reading is not mislabeled coordination — it genuinely extracts from literalists. But it is also not pure extraction because the rationalist coordination function is real. The presence of both beneficiaries and victims, both active enforcement and genuine alternative-solving, confirms tangled rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fixity_necessity,
    'Is the uncreated reading''s claim to textual fixity a logical consequence of divine authority, or a contingent institutional claim grounded in traditionalist power?',
    'Historical analysis of emergence of uncreated doctrine (Abbasid period, political consolidation of Sunni orthodoxy); comparative analysis of religious traditions without creation doctrine (some Shi''a frameworks, certain Sufi orders) showing alternative authority grounds',
    'If logical necessity: created reading forecloses the uncreated reading. If contingent institutional claim: created and uncreated readings coexist as competing readings of the same theological problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_fixity_necessity, conceptual, 'Whether textual fixity requires uncreated ontology or derives from institutional power').

omega_variable(
    divine_transcendence_mechanism,
    'Does the created reading adequately preserve divine transcendence, or does making revelation temporally contingent imply God is bound by temporal logic?',
    'Formal logical analysis of creation doctrine (al-Ghazali, Mu''tazilite arguments, contemporary Islamic philosophy); assessment of whether temporal creation is genuinely compatible with divine transcendence or merely asserts compatibility without resolving the logical tension',
    'If transcendence preserved: created reading is philosophically coherent. If not preserved: the reading''s core theological claim fails, and it functions purely as political doctrine (extraction mechanism exposed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_transcendence_mechanism, conceptual, 'Whether creation doctrine logically preserves divine transcendence').

omega_variable(
    hermeneutic_authority_recursion,
    'Once revelation is classified as created (temporally situated, historically contingent), who has authority to interpret it? Does rational theology replace textual fixity with institutional rationality as the new foundation for authority?',
    'Genealogical analysis of actual authority structures in rationalist vs literalist Islamic communities; assessment of whether ''reason'' as criterion is less institutional-capture-prone than ''textual fixity''',
    'If rational theology simply substitutes institutional rationality for textual authority: extraction mechanism is unchanged, merely disguised. If rational theology enables genuine pluralism: creation reading coordinates rather than extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_authority_recursion, empirical, 'Whether creation doctrine enables genuine interpretive pluralism or substitutes one institutional authority for another').

omega_variable(
    reading_identity_fusion,
    'Is the literalist community''s identity-lock on the uncreated reading a genuine fusion (identity constituted through textual fixity claim) or a contingent ideological commitment that could be abandoned?',
    'Ethnographic and historical analysis of literalist communities'' self-understanding; assessment of whether literalist practitioners distinguish ''textual meaning is fixed'' (metaphysical claim) from ''I am a person who believes textual meaning is fixed'' (identity claim)',
    'If genuine fusion: snare classification confirmed — community cannot exit without becoming different people. If contingent ideology: classification should be constrained or mobile (material barriers, not identity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_fusion, empirical, 'Whether literalist identity is fused with textual fixity or merely ideologically committed to it').

omega_variable(
    reform_movement_sunset_realism,
    'Is the scaffold perspective''s claim of a sunset (modern pluralism making uncreated doctrine untenable) genuinely structural, or does the uncreated reading persistently maintain institutional support across generations?',
    'Historical measurement of institutional adoption of created vs uncreated readings across 19th–21st centuries; assessment of whether literalist movements are declining or consolidating; measurement of whether ''modern pluralism'' is forcing theological change or whether theological frameworks are independent of modernity claims',
    'If sunset is real: scaffold is accurate and reform has structural path to authority transition. If uncreated reading persists: scaffold is aspirational; the created reading does not have guaranteed institutional future.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_movement_sunset_realism, empirical, 'Whether modernist sunset of uncreated doctrine is structural or aspirational').

omega_variable(
    state_legitimacy_decoupling,
    'Does state use of the created reading actually decouple political legitimacy from textual authority, or does it merely provide rhetorical cover for extraction while maintaining textual authority as reserve legitimacy claim?',
    'Historical analysis of states claiming created-reading framework (Ottoman modernization, post-colonial states) and their actual relationship to textual authority; measurement of whether states truly operate independent of Qur''anic authority claims or invoke them when needed',
    'If decoupling genuine: state tangled rope classification is accurate — real mixed coordination-extraction. If rhetorical cover: state is pure extraction mechanism using created reading as camouflage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_decoupling, empirical, 'Whether state legitimacy truly decouples from textual authority under created reading').

omega_variable(
    kernel_reading_contingency,
    'Is this the ''created reading'' of a single kernel, or does it instantiate a fundamentally different theological commitment (not a reading of the same question, but a different question about revelation)?',
    'Formal logical analysis: do created and uncreated readings occupy the same question space (same kernel, different readings) or incommensurable question spaces (different kernels)?',
    'If same kernel: created and uncreated are readings of ''what is the ontological status of revelation?'' If different kernels: created reading might be about ''how does reason relate to revelation?'' (different question). Affects classification of whether readings coexist or foreclose each other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether created reading is a reading of the same kernel or a fundamentally different theological commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quranic_created_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(quranic_created_tr_t3, quran_ontological_status__created_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(quranic_created_tr_t6, quran_ontological_status__created_reading, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(quranic_created_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(quranic_created_be_t3, quran_ontological_status__created_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(quranic_created_be_t6, quran_ontological_status__created_reading, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(quranic_created_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(quranic_created_su_t3, quran_ontological_status__created_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(quranic_created_su_t6, quran_ontological_status__created_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, islamic_legal_authority_rationalist_vs_textual).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, literalist_theological_identity_persistence).

% DUAL FORMULATION NOTE:
% The created reading is one constraint in a three-part kernel family. The uncreated reading and state-enforced creation reading are separate constraints with different ε values (uncreated ε ≈ 0.25 mountain candidate; state-enforced ε ≈ 0.62 snare candidate) and different beneficiary/victim structures. All three are linked via network.affects_constraints because they compete for institutional adoption and influence downstream constraints about Islamic legal authority and literalist identity persistence. The three readings do NOT average to a single ε — each is a structurally distinct institutional claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__created_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
