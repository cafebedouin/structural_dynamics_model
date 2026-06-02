% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Homoousios Christology (Pro-Nicene Reading)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   The homoousios (consubstantiality) Christology represents a specific
 *   reading of the incarnation claim within Christian theology: that Christ
 *   shares identical divine substance with God the Father, neither
 *   subordinate nor separate. This pro-Nicene reading emerged as the enforced
 *   orthodox position following the Council of Nicaea (325 CE), becoming
 *   institutionalized through imperial decree, episcopal anathema, and
 *   ecclesiastical punishment. The constraint describes the structural
 *   relationship between this theological claim and the institutional
 *   apparatus (imperial authority, episcopal hierarchy, anathematization)
 *   that enforces it. From the analytical perspective, homoousios appears as
 *   an immutable law of Christian monotheism. From the perspective of
 *   subordinationist bishops, it appears as pure extraction enforced through
 *   coercion. From the perspective of the imperial authority, it appears as a
 *   coordination mechanism that unifies the church. The constraint exhibits
 *   tangled rope characteristics: it contains both genuine coordination
 *   (standardizing Christological language across provinces, enabling
 *   theological coherence) and asymmetric extraction (suppressing alternative
 *   readings, rewarding alignment, punishing dissent). The measurements show
 *   extractiveness rising sharply at the council (time point 1) when
 *   enforcement machinery is activated, suppression peaking at the same
 *   moment (imperial anathema and exile provisions), and theater_ratio rising
 *   as well (the council's pronouncements acquire performative authority
 *   beyond their argumentative force). The constraint is one reading of a
 *   contested kernel — the homoousios doctrine itself remains a live site of
 *   Christian theological disagreement, with Arian and semi-Arian readings
 *   persisting as alternative instantiations of the same kernel.
 *
 * KEY AGENTS:
 *   - Imperial Authority (Constantine): Institutional beneficiary (institutional/arbitrage) — uses doctrinal standardization as governance tool; experiences constraint as coordination mechanism enabling unified church
 *   - Nicene Episcopal Coalition: Organized beneficiary (organized/arbitrage) — gains doctrinal authority and imperial support; experiences constraint as coordination enabling their interpretive tradition's dominance
 *   - Subordinationist Bishops: Powerless victims (powerless/trapped) — face anathema, exile, loss of office for doctrinal dissent; experience constraint as pure extraction
 *   - Non-Aligned Regional Bishops: Organized but constrained actors (organized/constrained) — experience mixed coordination and extraction; doctrinal formula enables provincial coherence but enforcement creates asymmetric costs
 *   - Theological Rationality (Universal Observer): Analytical perspective (analytical/analytical) — risks naturalizing a contingent institutional choice as logical necessity; sees homoousios as emerging from divine nature rather than ecclesiastical power
 *   - Post-Constantinian Tradition: Civilizational perspective (organized/constrained) — imagines homoousios becoming self-evidently true through rational consensus, making enforcement unnecessary (scaffold logic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.58).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.72).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Homoousios Christology (Pro-Nicene Reading)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, '5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a').
narrative_ontology:cs_kernel_codification('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', formalized).
narrative_ontology:cs_authority_grounding('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', extraction).
narrative_ontology:cs_interpretation_layer_present('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a').
narrative_ontology:cs_reading_relation('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', foundational, christ_consubstantial_father).
narrative_ontology:cs_axiom_status(christ_consubstantial_father, holdable).
narrative_ontology:cs_axiom_grounding('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', christ_consubstantial_father, deontological).
narrative_ontology:cs_axiom('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', secondary, apostolic_authority_via_councils).
narrative_ontology:cs_axiom_status(apostolic_authority_via_councils, holdable).
narrative_ontology:cs_axiom_grounding('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', apostolic_authority_via_councils, conventional).
narrative_ontology:cs_reference_frame('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', imperial_ecclesiastical_orthodoxy).
narrative_ontology:cs_drift_state('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', post_constantinian_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5f2c8e1a-9b4d-4c6f-a2e3-1b7d8f9c0e4a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_episcopal_coalition).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, alternative_christological_positions).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, non_aligned_bishops).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATIONIST BISHOP (SNARE) — Trapped by imperial decree and ecclesiastical anathema. Recanting requires public humiliation; refusing requires exile. No exit path; full extraction: loss of office, excommunication, social death. The constraint is experienced as pure coercive force.
constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: NON-ALIGNED REGIONAL BISHOPS (TANGLED ROPE) — Constrained by both doctrinal pressure and political consequences of dissent. Experience mixed coordination and extraction: the homoousios formula does coordinate christological language across provinces (genuine function), but enforcement mechanisms (council anathemas, imperial pressure) create asymmetric costs for those who resist. Exit is possible through apostasy or provincial isolation, but at high cost.
constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: IMPERIAL AUTHORITY (ROPE) — Benefits from doctrinal standardization as a governance tool. Homoousios serves coordination: unified Christian doctrine reduces religious faction as a vector of imperial instability. The emperor experiences this as a coordination mechanism — communication of a binding formula that solves collective action problems (preventing schism, stabilizing the church as an institutional partner). Low extraction experience because benefits flow reliably toward this agent.
constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: NICENE EPISCOPAL COALITION (ROPE) — Organized beneficiaries with strong exit options (can defect to anti-Nicene factions; can navigate councils to reverse decisions). The coalition experiences homoousios primarily as coordination: standardizing christological language enables theological coherence across provinces and protects their preferred interpretive tradition. The formula gives them institutional power to exclude competitors. Benefits accrue to coalition members through doctrinal authority and imperial alignment. Extraction is low relative to coordination benefit.
constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: UNIVERSALIST THEOLOGICAL TRADITION (SCAFFOLD) — Sees the homoousios decree as a temporary enforcement mechanism with an implicit sunset. The tradition's reference frame imagines a future (post-Constantine) where universal theological consensus achieved through rationally compelling doctrine replaces imperial coercion. The formula functions as a scaffold: temporarily enforced (hence suppression ≥ 0.40), but pointing toward a state where homoousios would be self-evidently true and enforcement unnecessary. Theater ratio moderate (performative council theater required now, but diminishing as consensus matures).
constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABLE DIVINE NATURE VIEW (MOUNTAIN) — From a civilizational/universal vantage, the homoousios formula reflects an immutable feature of Christian theology itself: if Christ is fully divine and fully God's son, then consubstantiality is not a contingent institutional choice but a logical necessity of monotheism. This perspective sees the constraint as emerging naturally from theological coherence, not from imperial power. However, the structural data (beneficiary declarations, measured suppression, active enforcement) contradicts this naturalization. The engine will flag this as a false summit: a contingent institutional arrangement (imperial-backed anathematization) being presented as divine necessity.
constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(homoousios_christology__pro_nicene_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The pro-Nicene reading consolidates power in the imperial-episcopal apparatus while suppressing alternative Christologies. However, extractiveness is not maximal because the formula does provide genuine coordination benefits — standardizing language across provinces addresses a real theological coordination problem. The constraint is not pure extraction (snare) because it solves a real problem; it is tangled rope because it solves that problem while simultaneously enriching and empowering the beneficiary group relative to alternatives. Suppression (0.72): High. Enforcement mechanisms are severe: anathema (ecclesiastical death sentence), exile (physical removal), confiscation of bishoprics, imperial displeasure. These are not voluntary consensus but coercive imposition. However, suppression is not absolute (not 0.90+) because alternative Christologies persist despite suppression — the constraint requires ongoing enforcement rather than becoming self-maintaining. Theater ratio (0.65): Moderate-high. The Council of Nicaea itself is performative theater: imperial ceremonial, episcopal choreography, predetermined outcomes justified through theological debate. The homoousios formula acquires authority partly through its rational content and partly through the performative authority of the council. The formula's theological interpretability is limited — it relabels the incarnation problem as 'mystery' without resolving it, suggesting theater components are significant. Post-council, the theater moderates slightly as the formula becomes institutionalized and loses novelty, though ongoing anathematization events maintain performative reinforcement. Claimed type (tangled_rope): The constraint exhibits both coordination function (unified Christological language enabling ecclesiastical coherence) and enforcement function (anathema, exile, imperial pressure suppressing alternatives). Neither dominates entirely, requiring both beneficiary/victim structure and enforcement_active flag.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of perspectival classification from a single set of base properties. The imperial authority sees Rope (coordination mechanism enabling stable ecclesiastical governance). The Nicene episcopal coalition sees Rope (doctrinal unification protecting their interpretive tradition and institutional power). The non-aligned bishops see Tangled Rope (genuine coordination benefit mixed with enforcement costs). The subordinationist bishops see Snare (pure coercive extraction with no exit). The universalist tradition sees Scaffold (temporary enforcement pointing toward voluntary consensus). The analytical observer risks seeing Mountain (homoousios as logical necessity of Christian monotheism) — a false summit. The perspectival divergence reveals that the constraint's structure is not about theological truth but about institutional power: who benefits from standardization, who bears the cost of suppression, and whose theological preferences become law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow. Imperial Authority: beneficiary + arbitrage exit → d ≈ 0.00-0.15 (full beneficiary, can easily switch policies if advantageous). Nicene Coalition: beneficiary + organized power → d ≈ 0.10-0.25 (strong beneficiary, but organizational constraints reduce perfect exit flexibility). Non-aligned Bishops: mixed (some benefit from Nicene consolidation, some bear costs) + constrained exit → d ≈ 0.45-0.55 (near-symmetric position with constrained exit). Subordinationist Bishops: victim + trapped exit → d ≈ 0.90-0.98 (near-total target, no exit options). The engine applies the sigmoid f(d) to convert these directionality values into experienced extractiveness chi = ε × f(d) × σ(S). Beneficiaries experience low or negative chi; victims experience high chi. The institutional beneficiary (imperial authority) with arbitrage exit experiences chi ≈ ε × (-0.12) × 1.1 ≈ -0.08 (negative extractiveness — the constraint subsidizes them). The trapped victim experiences chi ≈ ε × 1.42 × 1.1 ≈ 0.90 (high effective extraction). This explains the perspectival gap: the constraint's classification ranges from Rope (imperial beneficiary) to Snare (trapped victim) while maintaining a single ε value.
 *
 * MANDATROPHY ANALYSIS:
 *   The pro-Nicene reading resolves mandatrophy by foregrounding the commitment system structure: the homoousios kernel is a contested codified doctrine where legitimacy grounds itself in a lineage (apostolic succession, council authority, patristic interpretation). The Nicene reading stakes its authority on the claim that homoousios represents the true apostolic teaching, enforced by episcopal hierarchy aligned with imperial power. This is a classic commitment system constraint: a stabilized text (the Nicene formula) interpreted through an authority structure (the councils and imperial enforcement) that benefits identifiable institutional actors (the imperial apparatus and Nicene coalition) while suppressing alternatives through anathema. The mandatrophy is resolved not by claiming one type is 'correct' but by showing that the constraint's classification depends on whether you ask: (1) Does it coordinate theological language? (YES — Rope aspects). (2) Does it suppress alternatives through coercion? (YES — Snare aspects). (3) Is it enforced by institutional power? (YES — requires suppression score ~0.72). (4) Does it benefit identifiable actors? (YES — beneficiary/victim asymmetry). The tangled_rope classification captures both functions. The false summit (mountain) is a rhetorical move: naturalizing the contingent institutional arrangement as logical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoousios_philosophical_necessity,
    'Is homoousios a logically necessary conclusion from monotheism + incarnational Christology, or a contingent institutional choice that became naturalized as necessity?',
    'Historical analysis of pre-Nicene Christologies: Are subordinationist, adoptationist, and modalist readings logically inconsistent with Christian monotheism, or are they coherent alternative theologies that were suppressed for political reasons? Philosophical reconstruction of which premises force homoousios uniquely.',
    'If logically necessary: mountain classification confirmed; the constraint emerges naturally. If contingent: false summit confirmed; the naturalization is the extraction mechanism. The distinction determines whether enforcement is justified (logical compulsion) or coercive (institutional preference).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoousios_philosophical_necessity, conceptual, 'Whether homoousios is logically necessary or institutionally contingent').

omega_variable(
    council_coercion_versus_consensus,
    'Did the Council of Nicaea reflect genuine theological consensus that pre-existed the council, or did imperial pressure and episcopal intimidation CREATE consensus where genuine disagreement existed?',
    'Close reading of council minutes and pre-council bishop correspondence: track which bishops changed positions and under what pressure; analyze whether Arian and semi-Arian positions had genuine theological support or were strawman positions constructed to justify anathema.',
    'If genuine consensus: suppression metric should be lower (~0.40); the constraint coordinates emergent agreement. If created by coercion: suppression metric justified (~0.72); the constraint imposes uniformity through force. This affects whether the constraint is primarily extraction (snare) or mixed coordination-extraction (tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(council_coercion_versus_consensus, empirical, 'Whether Nicene consensus was pre-existing or created by coercion').

omega_variable(
    theological_interpretability_gap,
    'Does the homoousios formula actually resolve the theological problem it claims to address (how Christ is both fully divine and not identical to the Father), or does it merely relabel the problem as ''mystery''?',
    'Examine post-Nicene Christological development: Did homoousios enable resolution of Christological disputes, or did it create new problems (Nestorian/Monophysite splits) that required further councils and further doctrinal refinement? If it enabled clarification, it functions as coordination. If it obscured the problem, theater_ratio should be higher.',
    'If homoousios clarifies: lower theater_ratio, lower extractiveness (genuine coordinate achievement). If it obscures through rhetorical authority: higher theater_ratio, higher extractiveness (extraction through linguistic authority rather than comprehension).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_interpretability_gap, empirical, 'Whether homoousios resolves or relabels the Christological problem').

omega_variable(
    pro_nicene_reading_identity,
    'This constraint is one reading of the homoousios_christology kernel. What distinguishes the pro-Nicene reading from its Arian and semi-Arian sibling readings at the structural level?',
    'The pro-Nicene reading grounds legitimacy in imperial-ecclesiastical authority (extraction grounding); it forecloses Arian readings by claiming that subordinationism contradicts monotheism itself; it coexists with semi-Arianism through the interpretive gap (how much subordination is tolerable?). The sibling readings have different authority groundings and different forclosure/coexistence structures.',
    'This omega documents the committer frame: you are generating one reading, and the alternative readings are OTHER constraint stories with DIFFERENT ε values and DIFFERENT beneficiary/victim structures. The contrast between readings reveals what each reading stabilizes and what it suppresses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pro_nicene_reading_identity, conceptual, 'Structural identity of pro-Nicene reading vs sibling readings').

omega_variable(
    post_constantinian_sustainability,
    'Without imperial enforcement, would homoousios have remained the dominant Christological framework, or was it dependent on state power for maintenance?',
    'Historical analysis of post-Constantinian periods: track which Christological positions gain ground when imperial enforcement weakens (4th-5th century Western schisms, non-Chalcedonian Eastern churches). Examine whether anti-Nicene Christianity persists in regions with weaker imperial authority.',
    'If homoousios requires ongoing imperial enforcement: the constraint is extraction-dependent; suppression is essential to its survival; scaffold perspective is overly optimistic about sunset. If homoousios survives through internalized ecclesial consensus: the constraint transitions from snare toward rope; the extraction wanes as consensus matures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_constantinian_sustainability, empirical, 'Whether homoousios is sustainable without imperial enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_nicene_theater_pre_council, homoousios_christology__pro_nicene_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(homo_nicene_theater_post_council, homoousios_christology__pro_nicene_reading, theater_ratio, 1, 0.68).
narrative_ontology:measurement(homo_nicene_theater_stabilization, homoousios_christology__pro_nicene_reading, theater_ratio, 3, 0.65).

% Extraction over time
narrative_ontology:measurement(homo_nicene_extract_pre_council, homoousios_christology__pro_nicene_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(homo_nicene_extract_post_council, homoousios_christology__pro_nicene_reading, base_extractiveness, 1, 0.62).
narrative_ontology:measurement(homo_nicene_extract_stabilization, homoousios_christology__pro_nicene_reading, base_extractiveness, 3, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(homo_nicene_suppress_pre_council, homoousios_christology__pro_nicene_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(homo_nicene_suppress_post_council, homoousios_christology__pro_nicene_reading, suppression_requirement, 1, 0.85).
narrative_ontology:measurement(homo_nicene_suppress_stabilization, homoousios_christology__pro_nicene_reading, suppression_requirement, 3, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, homoousios_christology__arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, homoousios_christology__semi_arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, council_of_nicaea_legitimacy).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, imperial_ecclesiastical_alignment).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three constraint stories: pro-Nicene (ε≈0.58, tangled_rope), Arian (ε≈0.35, rope), and semi-Arian (ε≈0.48, tangled_rope). Each reading instantiates the same theological kernel differently and produces different classifications. This pro-Nicene reading grounds legitimacy in authority lineage + imperial enforcement; the Arian reading grounds it in biblical literalism; the semi-Arian reading grounds it in rational restraint. The ε differences reflect different enforcement requirements: pro-Nicene requires high suppression (council anathema, exile); Arian requires less (it claims to be removing false institutional additions); semi-Arian requires moderate suppression (managing the ambiguity boundary). All three affect a common set of downstream constraints (imperial ecclesiastical alignment, conciliar authority legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__pro_nicene_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
