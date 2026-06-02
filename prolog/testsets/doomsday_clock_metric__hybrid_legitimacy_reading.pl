% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Metric: Hybrid Legitimacy Reading (Scientific Judgment + Normative Stakes Entanglement)
 *   domain: existential_risk/science_communication/normative_epistemology
 *
 * SUMMARY:
 *   The Doomsday Clock embodies a foundational ambiguity in existential risk
 *   governance: it is simultaneously a scientific judgment about global
 *   catastrophic risk and a normative call for existential urgency. The
 *   Bulletin of Atomic Scientists sets the Clock annually by combining
 *   empirical claims (nuclear arsenals, biological capacity for pandemics, AI
 *   safety trajectories) with explicit value judgments (how much risk is
 *   unacceptable, which tail scenarios deserve focal attention). This
 *   constraint exhibits the signature of tangled rope — genuine coordination
 *   of existential risk discourse with embedded asymmetric extraction. The
 *   hybrid legitimacy reading treats the deliberate ambiguity itself as the
 *   source of the Bulletin's authority: the institution can defend decisions
 *   on scientific grounds when challenged on objectivity, and on normative
 *   grounds when challenged on scope. This ambiguity is maintained and is not
 *   treated as a defect to be resolved. The constraint generates tension
 *   between the public's need for methodological transparency and the
 *   Bulletin's structural dependence on methodological ambiguity. The rising
 *   theater_ratio (0.35 → 0.65) over two decades reflects increasing
 *   decoupling between the Clock's media function (threat communication) and
 *   its epistemic function (risk measurement). The suppression_requirement
 *   rising over the same period reflects that maintaining the hybrid status
 *   requires suppressing alternative frameworks for existential risk
 *   assessment.
 *
 * KEY AGENTS:
 *   - Bulletin of Atomic Scientists: Institutional beneficiary (institutional/arbitrage) — maintains authority through deliberate ambiguity; able to defend decisions as either scientific or normative as needed
 *   - Public Epistemic Commons: Primary victim (powerless/trapped) — cannot verify which Clock adjustments were scientific vs normative; absorbs existential risk framing without access to deliberation structure
 *   - Alternative Risk Frameworks: Secondary victim (moderate/constrained) — (Ord, long-termism, climate tipping-points) face visibility disadvantage; Clock occupies focal-point slot; constrained by pre-empted legitimacy
 *   - Existential Risk Research Domain: Mixed (powerful/mobile) — benefits from Clock's legitimacy for risk-seriousness in mainstream discourse; constrained by non-transparency of methodology
 *   - Media and Policy Consumption: Degraded function (institutional/arbitrage) — treats Clock as communication theater; institutional inertia maintains despite low decision-impact
 *   - Open-Source Risk Quantification: Organized counter-force (organized/constrained) — building alternative focal points with transparent methodology; sunset pressure on Clock's exclusive authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.52).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.48).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Metric: Hybrid Legitimacy Reading (Scientific Judgment + Normative Stakes Entanglement)").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "existential_risk/science_communication/normative_epistemology").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e').
narrative_ontology:cs_kernel_codification('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', formalized).
narrative_ontology:cs_authority_grounding('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', extraction).
narrative_ontology:cs_interpretation_layer_present('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e').
narrative_ontology:cs_reading_relation('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', foundational, deliberate_ambiguity_as_legitimacy).
narrative_ontology:cs_axiom_status(deliberate_ambiguity_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', deliberate_ambiguity_as_legitimacy, instrumental).
narrative_ontology:cs_axiom('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', secondary, existential_risk_measurement_inherently_valuational).
narrative_ontology:cs_axiom_status(existential_risk_measurement_inherently_valuational, holdable).
narrative_ontology:cs_axiom_grounding('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', existential_risk_measurement_inherently_valuational, deontological).
narrative_ontology:cs_reference_frame('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', bulletin_scientific_authority_through_judgment_synthesis).
narrative_ontology:cs_drift_state('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', contemporary_alt_risk_platform_maturation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('58aed9d4-81e5-45fa-ae7a-cf40f5fbfb9e', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_discourse_authority).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, public_epistemic_commons).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, alternative_risk_framings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PUBLIC EPISTEMIC COMMONS (SNARE) — Powerless to exit. The Doomsday Clock's metaphorical status (is it a scientific instrument or a communication tool?) is deliberately unresolved. This ambiguity prevents public scrutiny of the judgment process. Citizens cannot verify which decisions were scientific vs. normative. Maximum extraction: the public absorbs existential risk framing without access to the deliberation structure.
constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% ALTERNATIVE RISK ASSESSMENT FRAMEWORKS (SNARE) — Constrained exit. Other researchers producing existential risk estimates (Ord, GiveWell, long-termist modelers) face visibility disadvantage: the Doomsday Clock occupies the public-facing symbol slot. The legitimacy of the Clock's hybrid status (scientific-normative fusion) pre-empts recognition of competing frameworks. High suppression, low exit options.
constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% BULLETIN OF ATOMIC SCIENTISTS (ROPE) — Institutional beneficiary with arbitrage exit. The ambiguity between scientific judgment and normative stakes IS the source of the Bulletin's authority: they can claim scientific rigor when challenged on objectivity, and normative mandate when challenged on scope. The hybrid legitimacy is the coordination mechanism — it allows the Bulletin to coordinate existential risk discourse without being pinned down on methodology. Net beneficiary.
constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% EXISTENTIAL RISK DISCOURSE DOMAIN (TANGLED ROPE) — Powerful researchers with mobile exit (alternative venues, independent models). The Clock both enables and constrains: it provides focal point and legitimacy for taking existential risks seriously in mainstream discourse. But the hybrid status limits epistemic transparency — researchers cannot fully contest the Clock's methodology without appearing to challenge its normative mission. Genuine coordination (risk legitimacy) with embedded extraction (methodology non-transparency).
constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% MEDIA AND POLICY-MAKER CONSUMPTION (PITON) — The Doomsday Clock persists as a media and policy focal point through institutional inertia. The threat visualization (minutes to midnight) is compelling but largely decoupled from actual decision-making. Policy actors treat the Clock as communication theater — a legitimacy signal rather than a decision input. Theater ratio high; functional decision impact low.
constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% OPEN-SOURCE RISK QUANTIFICATION MOVEMENT (SCAFFOLD) — Organized actors (Metaculus, AI safety research, climate-risk quantification platforms) are building alternative focal points with transparent methodology. These platforms treat existential risk as subject to distributed scrutiny and ongoing calibration. The hybrid legitimacy of the Clock is being displaced by platforms with explicit uncertainty quantification and community deliberation. Sunset clause: alternative focal points mature over 10-15 years.
constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some entanglement of scientific judgment and normative stakes is inherent to existential risk discourse itself: we cannot measure extinction-level risk empirically (it has never happened), so any framework must fuse judgment and values. This perspective sees the hybrid status as an unavoidable feature of existential risk reasoning. ENGINE ALERT: False summit. The 'irreducible entanglement' is reified as natural law, but it is actually a choice about which entanglement to make transparent.
constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(doomsday_clock_metric__hybrid_legitimacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, TR),
    TR >= 0.70.

:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Bulletin extracts authority and resource allocation focus during the verification interval (annual announcement attention, funding dependency for existential risk research). But the extraction is not maximal (0.66+) because the hybrid status does provide genuine coordination benefit — other researchers can cite the Clock as evidence that existential risks deserve serious attention. The extraction is the cost others pay for that coordination. Suppression (0.48): Moderate. The mechanism is partly structural (alternative frameworks lack the media focal point) and partly epistemic (the hybrid status is not made explicit, so the suppression is not fully visible). Theater ratio (0.65): Moderately high. The annual Clock adjustment is substantially performative — the media theater (midnight countdown, human-interest narrative) is often decoupled from methodological discussion. The theater has increased over time as the Clock has become more a communication device and less a technical measurement tool. The theater enables the ambiguity to persist — methodological questions get drowned out by narrative. Claimed type (tangled_rope): The Bulletin provides genuine coordination (existential risks legitimized in discourse) but with asymmetric beneficiary structure (Clock holders benefit from ambiguity, alternative frameworks suppressed). Active enforcement required (true): The Bulletin's annual board meetings function as enforcement — they maintain the decision process and its ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. The Bulletin sees itself as coordinating discourse (Rope) — the hybrid status is a feature allowing legitimate risk urgency without false precision claims. The powerless public sees extraction (Snare) — they cannot verify which decisions were scientific vs normative. Alternative risk frameworks see suppression (Snare) — the Clock pre-empts their visibility. Researchers see mixed benefit and constraint (Tangled Rope) — the Clock legitimizes their field but constrains methodological transparency. Policy makers see theater (Piton) — the Clock signals urgency but does not drive decision inputs. The analytical observer risks seeing this as natural law (Mountain) — the 'irreducible entanglement of judgment and stakes' — but this is a false summit: the entanglement is a choice about which ambiguity to maintain transparent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position. The Bulletin (institutional, arbitrage) has d ≈ 0.10 — low d because beneficiary with escape options. The public epistemic commons (powerless, trapped) has d ≈ 0.95 — maximum extraction experienced. Alternative frameworks (moderate, constrained) have d ≈ 0.65 — high but not maximal because they can publish and conduct research, just at visibility disadvantage. The existential risk research domain (powerful, mobile) has d ≈ 0.55 — roughly symmetric because the Clock both enables their research visibility and constrains their methodological autonomy. These d values feed the sigmoid f(d) to produce effective extractiveness chi experienced by each agent. The perspectival gap reflects these different d values: the Bulletin sees rope (coordination, low extraction), while trapped agents see snare (maximum extraction, no alternatives).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here resolves through the kernel reading structure: the constraint is not 'which type is correct?' but 'which reading of the Clock's legitimacy is operative?' The hybrid_legitimacy reading shows that the Clock is tangled_rope (genuine coordination + asymmetric extraction). The objective_index reading would classify as rope (coordination with explicit methodology). The performative_tool reading would classify as piton (theater at high ratio, low function). All three are structurally valid readings of different aspects of the same institution. The false summit at the analytical observer level reveals that treating existential risk as having an 'irreducible' fusion of judgment and stakes naturalizes a contingent institutional choice: the choice to maintain ambiguity rather than make it transparent. Alternative designs (open-source quantified platforms, transparent threshold-setting) show that the entanglement is not inescapable. The mandatrophy is resolved by recognizing that the descriptive question ('what is the Clock?') and the prescriptive question ('should the Clock be transparent?') are entangled, and the hybrid_legitimacy reading is the reading that names this entanglement as intentional rather than accidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_status_deliberate_vs_emergent,
    'Is the ambiguity between scientific judgment and normative stakes in the Clock''s setting a deliberate design choice of the Board, or an emergent property of the constraint that the Board has not explicitly resolved?',
    'Archival analysis of Bulletin board deliberations, meeting minutes, and published methodology documents from 1945-present. Interview analysis of Board members'' stated epistemology.',
    'If deliberate: the tangled rope classification is correct (the Bulletin intentionally coordinates via ambiguity). If emergent but unexamined: the constraint is a piton (the Board has not noticed the hybrid status becomes the legitimacy source). If emergent and noticed but maintained: the snare classification becomes stronger (suppression includes epistemic closure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_status_deliberate_vs_emergent, empirical, 'Whether hybrid status is deliberate Board design or emergent unexamined property').

omega_variable(
    genuine_coordination_vs_false_consensus,
    'Does the Doomsday Clock actually coordinate existential risk discourse (rope function), or does it merely create the appearance of consensus while suppressing methodological disagreement (snare function)?',
    'Citation analysis: does the Clock''s annual adjustment drive new research programs, policy initiatives, or resource allocation? Or does it function primarily as media narrative without downstream coordination effects? Longitudinal tracking of policy implementation correlated with Clock movements.',
    'If genuine coordination: tangled rope is correct. If false consensus: snare classification strengthens, and the suppression metric increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_coordination_vs_false_consensus, empirical, 'Whether Clock provides real discourse coordination or false consensus').

omega_variable(
    bounded_rationality_legitimacy,
    'Is the public acceptance of the Clock based on understanding its hybrid status (scientific-normative fusion) as epistemically appropriate given the domain, or on lack of awareness that the fusion exists?',
    'Survey analysis: public understanding of Clock methodology and epistemology. Cognitive framing experiments with and without explicit hybrid-status disclosure. Comparison of public acceptance when Clock is presented as ''scientific instrument'' vs ''expert consensus judgment'' vs ''normative call to action.''',
    'If aware and accepting: legitimacy is genuine (rope component valid). If unaware: legitimacy is contingent on hidden structure (snare component validates). Difference in public epistemology affects suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_rationality_legitimacy, empirical, 'Whether public acceptance reflects understanding or unawareness of hybrid status').

omega_variable(
    existential_risk_measurement_inescapable_normative,
    'Is it epistemically impossible to measure existential risk without fusing scientific judgment and normative evaluation, or is this constraint-specific to the Doomsday Clock''s methodology?',
    'Comparative analysis: Metaculus existential risk forecasts, Ord''s risk model, IPCC climate tipping-point thresholds. Examine whether alternative frameworks achieve transparent separation of empirical claims from normative thresholds. Identify which elements of the fusion are unavoidable (tail-risk measurement) vs. contingent (choice of 2-minute vs 5-minute framing).',
    'If inescapable: mountain classification becomes stronger (natural law reading). If contingent: the mountain-in-mountain false summit detection is stronger (the ''irreducible entanglement'' is reified but not actually immutable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_risk_measurement_inescapable_normative, conceptual, 'Whether scientific-normative fusion is epistemically inescapable in existential risk measurement').

omega_variable(
    boundary_between_sibling_readings,
    'What structural features distinguish this hybrid-legitimacy reading from the objective-index reading and performative-tool reading in actual practice?',
    'Textual analysis of Bulletin publications, board deliberations, and media representations. Identify which claims align with ''objective index'' (the Clock measures real risk), which with ''performative tool'' (the Clock communicates urgency), and which with ''hybrid legitimacy'' (ambiguity between the two is the authority source). Map to which actors make which claims in which contexts.',
    'If readings map cleanly to different actors or historical periods: constraint family is well-decomposed. If readings overlap substantially: the three stories may be over-articulation of a single constraint, not genuinely distinct ε-values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_between_sibling_readings, empirical, 'Structural boundary between hybrid-legitimacy, objective-index, and performative-tool readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dc_hybrid_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dc_hybrid_tr_t10, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(dc_hybrid_tr_t20, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(dc_hybrid_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dc_hybrid_be_t10, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(dc_hybrid_be_t20, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dc_hybrid_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(dc_hybrid_su_t10, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(dc_hybrid_su_t20, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_focal_point_competition).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_authority_legitimacy_foundations).

% DUAL FORMULATION NOTE:
% The Doomsday Clock kernel (doomsday_clock_metric) decomposes into three distinct constraint stories: hybrid_legitimacy_reading (this file), objective_index_reading, and performative_tool_reading. Each reading has a different ε value reflecting different structural facts about how the Clock functions. They are not the same constraint viewed from different angles — they are genuinely different constraints instantiated by different claims about what the Clock legitimately is. The network relationships show how different sibling readings influence and constrain each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__hybrid_legitimacy_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
