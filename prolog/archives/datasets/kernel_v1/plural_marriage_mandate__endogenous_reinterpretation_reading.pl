% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: Prophetic Reinterpretation of Plural Marriage (1890 Manifesto — Endogenous Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration–1) represents a constitutive
 *   moment in Latter-day Saint institutional history: the church's formal
 *   suspension of the practice of plural marriage in response to federal
 *   pressure and legal jeopardy. This constraint story instantiates ONE
 *   reading of the contested kernel 'plural_marriage_mandate' — specifically,
 *   the endogenous reinterpretation reading, which holds that the Manifesto
 *   represents a legitimate prophetic reinterpretation grounded in the
 *   doctrine of continuing revelation, wherein God revealed the temporal
 *   suspension of plural marriage to preserve the church's salvific mission
 *   and institutional continuity. This reading treats the reinterpretation as
 *   doctrinally coherent: plural marriage remains eternally true in principle
 *   but is suspended in practice by divine instruction, preserving both
 *   theological integrity and institutional survival. The constraint is
 *   classified as Rope from the institutional beneficiary perspective
 *   (coordination around prophetic guidance), Snare from the fundamentalist
 *   dissenter perspective (excommunication and doctrinal foreclosure), and
 *   Tangled Rope from the transitional generation perspective (mixed
 *   coordination benefit and personal extraction cost). The measurement
 *   trajectory shows base_extractiveness rising from 0.08 (pre-Manifesto) to
 *   0.38 (post-Manifesto, stabilized), reflecting the emergence of
 *   excommunication enforcement against dissenters maintaining the original
 *   doctrine. Theater_ratio rises similarly, indicating increasing
 *   ritualization of the 'continuing revelation' mechanism over time.
 *
 * KEY AGENTS:
 *   - Church Leadership (Institutional Beneficiary): 1890 decision-makers and prophetic authority structure — institutionally positioned to interpret divine will; experience the Manifesto as legitimate prophetic guidance enabling institutional survival.
 *   - Broader Believing Membership: Members benefiting from restored institutional capacity (temples, missions, social integration) — experience the constraint as coordination around new prophetic direction; benefit from the church's return to legal and political viability.
 *   - Fundamentalist Dissenters: Members maintaining original plural marriage doctrine after 1890 — structurally powerless, trapped by excommunication, social severance, and legal vulnerability; experience the constraint as pure extraction and doctrinal coercion.
 *   - Transitional Generation: Members with existing plural families navigating both old and new doctrinal frameworks — face mixed coordination benefit (persecution reduction, institutional access) and extraction cost (family dissolution or concealment).
 *   - Federal State Apparatus: Background coercive actor (not primarily beneficiary, but structural driver enabling the reinterpretation) — creates the crisis that the Manifesto resolves.
 *   - Analytical Observer: Civilizational perspective viewing the Manifesto as inevitable submission to superior state power — risks naturalizing a contingent institutional decision as immutable federal law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.52).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "Prophetic Reinterpretation of Plural Marriage (1890 Manifesto — Endogenous Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'd5b5c995-063a-40dc-9130-843c3364fc1d').
narrative_ontology:cs_kernel_codification('d5b5c995-063a-40dc-9130-843c3364fc1d', fixed_text).
narrative_ontology:cs_authority_grounding('d5b5c995-063a-40dc-9130-843c3364fc1d', lineage).
narrative_ontology:cs_interpretation_layer_present('d5b5c995-063a-40dc-9130-843c3364fc1d').
narrative_ontology:cs_reading_relation('d5b5c995-063a-40dc-9130-843c3364fc1d', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5b5c995-063a-40dc-9130-843c3364fc1d', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('d5b5c995-063a-40dc-9130-843c3364fc1d', foundational, continuing_revelation_framework_valid).
narrative_ontology:cs_axiom_status(continuing_revelation_framework_valid, holdable).
narrative_ontology:cs_axiom_grounding('d5b5c995-063a-40dc-9130-843c3364fc1d', continuing_revelation_framework_valid, deontological).
narrative_ontology:cs_axiom('d5b5c995-063a-40dc-9130-843c3364fc1d', foundational, temporal_suspension_preserves_eternal_principle).
narrative_ontology:cs_axiom_status(temporal_suspension_preserves_eternal_principle, holdable).
narrative_ontology:cs_axiom_grounding('d5b5c995-063a-40dc-9130-843c3364fc1d', temporal_suspension_preserves_eternal_principle, conventional).
narrative_ontology:cs_reference_frame('d5b5c995-063a-40dc-9130-843c3364fc1d', prophetic_authority_framework).
narrative_ontology:cs_drift_state('d5b5c995-063a-40dc-9130-843c3364fc1d', contemporary_institutional_doctrine, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d5b5c995-063a-40dc-9130-843c3364fc1d', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institutional_survival).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, temple_access_preservation).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, missionary_work_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHURCH LEADERSHIP (ROPE) — Faces immediate coordination crisis: federal seizure of temple, missionary bans, property confiscation unless the church pivots. The Manifesto is experienced as genuine prophetic guidance enabling survival coordination. Leadership perceives the constraint as a pure coordination mechanism — align doctrine with revelation to preserve salvific mission and institutional continuity. No extraction experienced; rather, the Manifesto is the beneficiary's coordination solution.
constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: BROADER BELIEVING MEMBERSHIP (ROPE) — Benefits from institutional survival and restored missionary capacity. Temple access, endowment programs, and educational institutions become available again. Members experience the Manifesto as coordination around a new prophetic directive — the principle of continuing revelation is invoked, and the sacrifice (suspension of plural marriage practice) is framed as obedience to God's will. Moderately high power and mobile exit options (members can choose other denominations or secular life) but benefit from the constraint's coordination function — retention and temple participation.
constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: FUNDAMENTALIST PRACTITIONERS / DISSENTERS (SNARE) — From this perspective, the Manifesto is pure extraction disguised as revelation. Those who maintain the original doctrine (plural marriage as eternally divine) are excommunicated, losing community, temple access, and social identity. The constraint appears as coercive doctrinal enforcement backed by institutional authority and social pressure. Maximum suppression (exit costs include excommunication, family severance, community dissolution) and maximum experienced extraction — the dissenting reading is foreclosed and its practitioners are expelled.
constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: TRANSITIONAL GENERATION / MEMBERS WITH PLURAL FAMILIES (TANGLED ROPE) — Members already living in plural marriages face a genuine mixed constraint. The Manifesto coordinates return to legal status and reduces persecution risk (genuine coordination benefit), but requires dissolution or concealment of existing plural families (significant extraction cost). These members benefit from institutional survival but bear personal costs. Exit is constrained by family ties, community identity, and financial dependence. The constraint functions as both coordination (around new prophetic authority) and extraction (enforcement of doctrinal pivot).
constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THEOLOGICAL INSTITUTION / LONG-TERM DOCTRINE (PITON) — Over a civilizational horizon, the doctrine of continuing revelation (the mechanism that licenses the reinterpretation) becomes partially performative. The principle that God can reveal new truth is invoked; later theological developments (and later doctrinal shifts) cite the same principle. The 1890 reinterpretation establishes precedent but also creates institutional theater around 'prophetic guidance' — the mechanism itself becomes a flexibility buffer used to manage doctrinal liability rather than a direct expression of divine will. Theater_ratio high because the same 'continuing revelation' mechanism is invoked for multiple doctrinal pivots, reducing its epistemic force. The original practice suspension is functional, but the mechanism for justifying it becomes ritualized.
constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FEDERAL POWER VIEW (MOUNTAIN) — From a universal/civilizational analytical perspective, federal supremacy over internal religious practices is treated as a natural law. No religious institution can indefinitely resist state power at that scale. The Manifesto appears not as prophetic reinterpretation but as inevitable submission to physical/political law. The constraint is the irreducible power imbalance between a federal state and a subnational religious institution. This perspective naturalizes what the endogenous reading sees as a legitimate reinterpretation and what the exogenous reading sees as coercion. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plural_marriage_mandate__endogenous_reinterpretation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, TR),
    TR >= 0.70.

:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.38): Moderate. The endogenous reading claims the constraint is fundamentally a coordination mechanism (pure Rope has ε ≤ 0.05), but structural evidence indicates extraction: excommunication of dissenters and enforcement against continued practice represent real coercive asymmetry. The 0.38 reflects that the constraint FUNCTIONS as coordination for beneficiaries (the broader membership, the institutional leadership) but STRUCTURES as extraction for dissenters (excommunication is a high-cost penalty for maintaining the original reading). From the institutional perspective, extractiveness appears low (0.08 pre-Manifesto, rising to 0.22 immediately post as dissenters are identified). The value 0.38 represents the full system-level extraction: the Manifesto enables the church's survival but redistributes costs onto those who cannot align with the new reading. Suppression (0.52): Moderate-High. Alternatives to the new doctrine are actively suppressed through excommunication, social pressure, and institutional control of temple access (the most economically and spiritually valuable resource in the tradition). Members cannot easily maintain the original doctrine without losing community, family integration, and religious legitimacy. Suppression is structural but not absolute — fundamentalist communities do form outside official church structures, indicating that suppression, while severe, does not prevent exit for those willing to pay high costs. Theater Ratio (0.48): Moderate. The Manifesto invokes the doctrine of continuing revelation to legitimate the pivot. This doctrine is genuinely central to LDS theology, but its invocation here is also the mechanism by which the constraint naturalizes what is structurally an institutional capitulation. Theater rises from 0.15 to 0.48 because the 'continuing revelation' mechanism becomes increasingly stylized and ritualized as a tool for managing doctrinal flexibility. By time t=5-10, the mechanism is so standardized that the gap between the theological claim ('God revealed...') and the structural reality ('the institution adapted to survive') becomes a conventional background assumption rather than an open question.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces profound perspectival divergence from identical structural facts. The church leadership perspective sees Rope (pure coordination around prophetic guidance). The broader membership perspective sees Rope with minor mixed elements (coordination benefit outweighs constraint costs). The fundamentalist dissenters perspective sees Snare (pure extraction disguised as revelation; doctrine is foreclosed, and dissenters are expelled). The transitional generation perspective sees Tangled Rope (genuine mixed coordination and extraction). The long-view theological perspective sees Piton (the 'continuing revelation' mechanism becomes performative theater). The analytical observer perspective sees Mountain (federal supremacy as natural law). The entire presheaf — all six types simultaneously — is the constraint. No single type is 'correct'; the gap itself reveals the structural dynamics. The endogenous reading claims that the Rope classification is the deepest truth (the Manifesto genuinely coordinated around prophetic reinterpretation and preserved the church's mission). The exogenous_override reading claims the Snare classification is the deepest truth (the Manifesto was coerced federal capitulation disguised as revelation). The institutional_pragmatism reading claims the gap between these is the truth (the constraint is strategic institutional adaptation, using the 'continuing revelation' framework to legitimate survival-driven doctrinal pivot).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each perspective's structural position relative to the extraction flow. Church Leadership: d ≈ 0.05 (full beneficiary, arbitrage exit) — they control interpretation authority and benefit from institutional survival; the Manifesto is their coordination solution, not extraction imposed on them. Broader Believing Membership: d ≈ 0.35 (partial beneficiary, mobile exit) — they benefit from institutional restoration and temple access but accept the constraints of the new doctrine; exit is possible (leave for other denominations) but costly (family, community, identity). Fundamentalist Dissenters: d ≈ 0.92 (full victim, trapped exit) — they bear maximum extraction (excommunication, institutional severance, legal vulnerability); exit is structurally available (they can leave the institution) but identity-locked (their religious identity is constituted through plural marriage theology, so exit means becoming a different person). Transitional Generation: d ≈ 0.62 (victim + constrained exit) — they experience both coordination benefit (persecution reduction) and extraction cost (family policy enforcement); exit is constrained (available but very costly — family dissolution, community loss). These d values are derived from beneficiary/victim status and exit_options; they feed into f(d) to produce chi = ε × f(d) × σ(S). The analytical mountain perspective derives d ≈ 0.72 (observer position), producing f(d) ≈ 1.15 and χ ≈ 0.44 for national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is not resolvable by measurement — it is a contested kernel. The 'correct' classification depends on what actually happened: (1) Did God genuinely reveal the suspension of plural marriage to church leadership? (If yes: Rope is correct; the constraint is coordination around prophetic guidance.) (2) Did federal power force the church to capitulate while the institution manufactured a revelation narrative to preserve theological legitimacy? (If yes: Snare is correct; the constraint is pure extraction.) (3) Did the church leadership strategically adapt institutional doctrine to survive, using the 'continuing revelation' framework as the legitimation mechanism? (If yes: the constraint is Tangled Rope with significant theater; institutional_pragmatism reading applies.) These three questions cannot be resolved by examining the constraint's metrics alone. They require theological, historical, and institutional analysis. The endogenous reading answers (1): it claims the Manifesto is doctrinally coherent and structurally describable as coordination. The exogenous reading answers (2): it claims the constraint is coercion. The institutional_pragmatism reading answers (3): it claims the constraint is strategic adaptation with theater. This constraint is a canonical example of how DR classification systems function when the underlying causal structure (empirical fact about divine revelation vs. institutional strategy) is irreducibly contested. The system does not resolve the mandatrophy; it maps it. All three readings are structurally coherent. The choice between them is not a measurement question — it is a theological commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_authenticity_criterion,
    'What criterion distinguishes a genuine prophetic revelation from strategic doctrinal reframing used to legitimate institutional survival?',
    'Historical-theological analysis: comparison of the 1890 Manifesto''s revelation claim to other documented cases of claimed revelation in the tradition; examination of whether the doctrine pre-dates or post-dates the federal pressure; analysis of theological coherence within the tradition''s own framework of continuing revelation.',
    'If revelation precedes federal pressure: endogenous reading is structurally defensible (rope classification holds). If doctrine is retrofitted after pressure: constraint reclassifies to institutional_pragmatism reading (constraint structure becomes snare, with stronger extraction). If incoherent with prior revelation claims: exogenous_override reading gains structural weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_authenticity_criterion, conceptual, 'Criterion for distinguishing genuine prophecy from strategic doctrinal adaptation').

omega_variable(
    doctrine_suspension_permanence,
    'Is the suspension of plural marriage practice doctrine-level (the eternal principle is suspended) or practice-level (the principle remains eternal but is not currently implemented)?',
    'Textual analysis of official doctrinal statements, temple ritual language, and successive prophetic claims; determination of whether current church leadership teaches plural marriage as eternally true but temporally suspended, or as no longer divinely required.',
    'If doctrine retained: endogenous reading is structurally coherent (constraint is coordination around new understanding, not abandonment of principle). If doctrine abandoned: constraint reclassifies to institutional_pragmatism reading (the ''revelation'' narrative is post-hoc legitimation of actual doctrinal abandonment). This changes the class of beneficiaries from ''survivors preserving principle'' to ''institutional actors discarding principle while maintaining narrative continuity''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_suspension_permanence, empirical, 'Whether plural marriage remains eternally true but temporally suspended, or has been superseded').

omega_variable(
    continued_secret_practice_prevalence,
    'How widespread was the continued secret practice of plural marriage among church members after the Manifesto, and did church leadership tolerate, suppress, or actively enable it?',
    'Historical archive analysis: membership records, court testimony, demographic patterns, institutional tolerance levels; determination of whether continuation was underground dissent or tacitly authorized ''ecclesiastical exception''.',
    'If suppressed: the Manifesto functioned as genuine doctrinal reinterpretation and constraint on practice (rope, with real coordination function). If tacitly authorized: the constraint is theater — public compliance for federal legitimacy, private continuation for doctrinal integrity (transforms to piton). If enabled: suggests doctrine was never actually suspended, only practice rhetoric changed (reclassifies to institutional_pragmatism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continued_secret_practice_prevalence, empirical, 'Institutional tolerance and enforcement of continued plural marriage after 1890').

omega_variable(
    reading_kernel_contest,
    'What distinguishes THIS reading (endogenous reinterpretation) from its sibling readings in the plural_marriage_mandate kernel?',
    'Structural analysis per Rules 1–4: this reading treats the Manifesto as a legitimate prophetic pivot grounded in continuing revelation doctrine; sibling readings treat it as federal coercion (exogenous_override) or strategic institutional adaptation (institutional_pragmatism). The distinction hinges on the axiom ''continuing_revelation_framework_valid'' (holdable in this reading, potentially overridden in institutional_pragmatism, foreclosed in exogenous_override).',
    'If continuing revelation framework is genuine: this reading and institutional_pragmatism coexist (different parties hold different readings; framework itself is not in dispute, but whether the Manifesto is a sincere application of it is). If framework is retrospectively constructed: institutional_pragmatism reading forecloses this reading (the framework is the theater, not the substance). If coercion is the explanans: exogenous_override reading forecloses this reading (no framework can simultaneously be ''legitimate reinterpretation'' and ''federal coercion disguised as reinterpretation'').',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Structural distinction between this reading and sibling readings in the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plmnd_theater_t0_predecision, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(plmnd_theater_t1_immediate_post_manifesto, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1, 0.32).
narrative_ontology:measurement(plmnd_theater_t5_institutionalization, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(plmnd_theater_t10_settled_enforcement, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(plmnd_extractiveness_t0_predecision, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(plmnd_extractiveness_t1_immediate_post_manifesto, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1, 0.22).
narrative_ontology:measurement(plmnd_extractiveness_t5_institutionalization, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(plmnd_extractiveness_t10_settled_enforcement, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(plmnd_suppression_t0_predecision, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(plmnd_suppression_t1_immediate_post_manifesto, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1, 0.52).
narrative_ontology:measurement(plmnd_suppression_t5_institutionalization, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(plmnd_suppression_t10_settled_enforcement, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_institutional_coercion__lds_case).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel has three structurally distinct readings, each instantiated as a separate constraint story: (1) endogenous_reinterpretation_reading [this file] — treats the Manifesto as legitimate prophetic reinterpretation grounded in continuing revelation doctrine, Rope classification, ε=0.38; (2) exogenous_override_reading — treats the Manifesto as federal coercion forcing doctrinal abandonment, Snare classification, ε≈0.68-0.75; (3) institutional_pragmatism_reading — treats the Manifesto as strategic institutional adaptation using revelation narrative as theater, Tangled Rope classification, ε≈0.55-0.62. Each reading has its own beneficiary/victim set, its own perspective structure, and its own omega variables addressing the kernel contest. The three stories are linked via network.affects_constraints: each sibling affects the others because the kernel contest is irreducibly three-way. The choice between readings is not resolvable by additional data about the constraint's structure — it is determined by theological and historical interpretation of whether the revelation claim is authentic, whether coercion was the primary driver, or whether strategic adaptation explains the pattern. All three readings are structurally coherent. The presheaf over the kernel is the complete picture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__endogenous_reinterpretation_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
