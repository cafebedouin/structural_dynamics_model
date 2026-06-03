% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy_endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Marriage Commitment Legitimacy: Endogenous Reinterpretation Reading (Prophetic Revelation View)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models ONE reading of a contested kernel in
 *   religious institutional history: the question of legitimacy for
 *   institutional doctrine reversal in the face of exogenous
 *   (political/federal) pressure. The kernel is the Marriage Commitment
 *   Legitimacy itself — the enduring question of what grounds the legitimacy
 *   of marriage doctrine within the institution. The reading instantiated
 *   here is the ENDOGENOUS REINTERPRETATION reading: the claim that the
 *   doctrinal reversal (from plural marriage to monogamy) represents genuine
 *   prophetic revelation — divine guidance that originated within the
 *   religious framework itself, not merely capitulation to federal coercion.
 *   Under this reading, God commanded the reversal to preserve the Church for
 *   higher purposes; the federal pressure was a catalyst (structuring the
 *   historical moment) but not the cause (the causal origin is divine
 *   authority, communicated through the prophetic mechanism). This reading
 *   preserves theological autonomy and frames the reversal as a new covenant
 *   stage rather than doctrinal break. The constraint's structural signature
 *   (low extractiveness, moderate suppression, low theater) reflects this
 *   interpretation: the institution experiences the reversal as coordination
 *   within a divinely-sanctioned framework, not as extraction under coercion.
 *   Sibling readings (exogenous_override_reading and
 *   hybrid_pragmatic_reading) attribute different causal weights to federal
 *   coercion versus endogenous theological logic, producing higher
 *   extractiveness values and different beneficiary structures.
 *
 * KEY AGENTS:
 *   - Church Authority Structure (Institutional): Primary beneficiary from prophetic authority reading — maintains institutional legitimacy through divine guidance continuity; institutional succession (living prophets) is the operational mechanism that sustains the reading
 *   - Divine Legitimacy Framework: Beneficiary in theological/abstract sense — the constraint coordinates institutional theology around prophetic revelation as authorization mechanism
 *   - Dissenting Voices and Fundamentalist Factions (Powerless/Trapped): Victims from exogenous perspective — silenced by institutional authority; from endogenous perspective, seen as rejecting divinely-sanctioned guidance
 *   - Federal Authority (Powerful/Mobile): Catalyst rather than cause under this reading — provides external pressure that structures the historical moment but does not determine the theological content of the response
 *   - Church Members and Communities (Moderate/Constrained): Experience the reversal as coordinative within a legitimate theological framework; suppression is normalized as legitimate divine authority rather than perceived as coercion
 *   - Theological Reform Movement (Organized/Constrained): See the reversal as pragmatic institutional adaptation with implicit sunset — the prophetic apparatus is temporary scaffolding for institutional coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.35).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Marriage Commitment Legitimacy: Endogenous Reinterpretation Reading (Prophetic Revelation View)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'cc81ea9d-c3a1-4cf5-8fa8-3c585903b252').
narrative_ontology:cs_kernel_codification('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', formalized).
narrative_ontology:cs_authority_grounding('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', lineage).
narrative_ontology:cs_interpretation_layer_present('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252').
narrative_ontology:cs_reading_relation('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', foundational, prophetic_revelation_authenticity).
narrative_ontology:cs_axiom_status(prophetic_revelation_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', prophetic_revelation_authenticity, theological).
narrative_ontology:cs_axiom('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', foundational, divine_authority_causation_primacy).
narrative_ontology:cs_axiom_status(divine_authority_causation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', divine_authority_causation_primacy, theological).
narrative_ontology:cs_reference_frame('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', prophetic_institutional_authority).
narrative_ontology:cs_drift_state('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', contemporary_secular_governance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cc81ea9d-c3a1-4cf5-8fa8-3c585903b252', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_institutional_continuity).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_legitimacy_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHURCH AUTHORITY STRUCTURE — The Manifesto is not capitulation but divine instruction. The church experiences the reversal as coordination: God commands the doctrinal shift to preserve the institutional body for higher purposes. Prophetic authority (through institutional succession) legitimates the change. This is pure coordination — the constraint solves a theological coherence problem through divinely-sanctioned reframing. No extraction: the institutional actor is both origin and beneficiary of the reinterpretation.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 2: FEDERAL AUTHORITY — From the federal vantage, the constraint is mixed. Federal pressure created conditions for institutional capitulation (extraction — the church subordinates doctrine to political survival). But the church's reframing (prophetic revelation justifying the reversal) is also a coordination mechanism that stabilizes the church's institutional position without explicit federal coercion. The federal agent benefits from doctrinal compliance (extraction vector) but the church's theological autonomy is nominally preserved (coordination component). Effective extraction is moderate because the legitimacy claim (divine authority) absorbs some of the coercive pressure and converts it into voluntary theological reorientation.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: CHURCH MEMBERS AND COMMUNITIES — Members experience the reversal as coordination within the divine framework. The Manifesto establishes new theological coherence: monogamy is now understood as a higher-order stage of covenant (New Covenant refinement). Members face constraints (biographical time, regional scope) but perceive the reversal as legitimate reorientation, not extraction. The constraint is purely coordinative from this perspective — it establishes shared understanding of institutional doctrine across membership boundaries. Suppression is present (doctrinal authority prevents questioning the revelation claim) but is normalized as legitimate divine guidance rather than perceived as coercion.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DISSENTING VOICES — Those who reject the prophetic revelation claim see the constraint as pure extraction. The Manifesto forces doctrinal conformity under institutional authority; dissenters are silenced or expelled. From this perspective, the 'divine revelation' frame is a legitimation strategy masking federal capitulation. The constraint is a snare: it extracts compliance (monogamy enforcement), suppresses alternative interpretations (fundamentalist readings), and denies exit (schism is institutional death). Theater is moderate (the theological framing has genuine work to do in stabilizing the institution, not purely ornamental). Extraction is high because dissenters cannot voice alternative readings without institutional penalty.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: THEOLOGICAL REFORM MOVEMENT — Organized reformers see the Manifesto as a temporary institutional stabilization mechanism with an implicit sunset. The prophetic revelation frame is pragmatic — it allows doctrinal evolution while preserving authority legitimacy. This perspective perceives the constraint as a scaffold: it coordinates institutional survival during a crisis period (federal pressure + internal theological strain) but anticipates the eventual integration of the 'new covenant' into conventional theological reasoning, after which the prophetic apparatus becomes unnecessary. Low extractiveness because the reform coalition sees the reinterpretation as transitory and functional (preserving the institution for future theological development).
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL HISTORIAN — From a civilizational retrospective, the endogenous reinterpretation reading is a performative legitimation strategy layered over institutional inertia. The Manifesto articulates theological coherence (real function: stabilizing internal doctrine), but the deeper mechanism is institutional survival. The prophetic revelation claim persists through institutional continuity even after its historical origins are forgotten — the reading becomes a formal aspect of authority structure maintained through tradition. Theater is high (the theological work is genuine coordination, but the claim's predictive/epistemic content is minimal once the institutional context changes). Extractiveness is low because civilization-scale actors (historians, theologians) see the reversal as adaptive, not exploitative.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate, reflecting the endogenous reading's claim that the reversal is coordinative rather than extractive. The low value derives from the reading's assertion that (1) divine authority is the beneficiary, (2) prophetic legitimacy originates within the religious framework itself, not through capitulation to external coercion, and (3) the reversal solves a genuine theological coherence problem (integrating monogamy as a covenant stage rather than doctrinal break). The extractiveness is not zero because (a) institutional actors do benefit from the reversal (doctrinal authority is consolidated through prophetic claims), (b) dissenting voices are suppressed (they cannot argue that the reversal is false without institutional penalty), and (c) the reading itself cannot be empirically falsified (divine guidance claims are not subject to external verification, creating an asymmetric epistemic structure). Suppression (0.35): Moderate. Dissenters face institutional penalties for rejecting the revelation claim, but the suppression is framed as legitimate exercise of prophetic authority rather than coercive force. Alternative theological readings are foreclosed institutionally (you cannot maintain full institutional standing while denying the revelation) but are not legally or physically suppressed. Theater (0.42): Moderate-low. The prophetic revelation claim does genuine theological work — it stabilizes internal doctrine and reconciles contradictions in marriage theology. The theater is not minimal (the claim's predictive content is limited; we cannot test whether God actually commanded the reversal) but it is not high (the theological coherence function is genuine, not purely ornamental).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint because the readings split sharply on causal attribution. The institutional beneficiary (church authority) sees coordination within a divine framework (Rope). Dissenting powerless agents see pure coercion (Snare). The federal authority sees mixed extraction and coordination (Tangled Rope). The theological reform movement sees a temporary scaffold. The institutional historian sees a performative legitimation strategy layered over institutional necessity (Piton). The perspectival variance is not measurement error — it reflects genuine structural ambiguity: federal pressure created the historical conditions for doctrinal reversal, but the causal mechanisms (whether the reversal was driven by external coercion or endogenous theological logic) are not empirically distinguishable from observable behavior. The endogenous reading claims divine guidance as the true cause; the exogenous reading claims federal pressure; the hybrid reading claims both working together. No perspective can be directly refuted by the other because they agree on the observable outcome (monogamy mandate) and disagree on unobservable causal mechanisms (divine intention, institutional necessity, theological coherence).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this reading is low because the beneficiary is identified as divine authority and institutional theological legitimacy — claims that originate within the religious framework itself rather than through subordination to external agents. From the institutional perspective (the primary beneficiary), d ≈ 0.15-0.20: the institution benefits from the reversal through consolidated prophetic authority, but this benefit is framed as coordinated alignment with divine will rather than extraction from external sources. Federal authority has higher d (≈0.55-0.65) because from the federal vantage, the institutional doctrinal shift represents compliance with political pressure — but this directionality is not primary in the endogenous reading (federal pressure is catalyst, not cause). Dissenters have the highest d (≈0.85-0.90) because they are the targets of institutional suppression and bear the costs of doctrinal conformity enforcement. The endogenous reading suppresses the directionality between federal authority and church (treating it as catalyst rather than primary extractor), which is the core analytical move distinguishing this reading from the exogenous override reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve a traditional mandatrophy (mixed classification requiring meta-level analysis) because the claimed_type (rope) is stable across the beneficiary and analytical perspectives. The mandatrophy present here is at the kernel level: the three readings produce structurally incommensurable constraints (rope, snare/tangled_rope, tangled_rope) that cannot be unified into a single classification. The endogenous reading avoids mandatrophy by identifying divine authority as beneficiary and prophetic legitimacy as the coordinative mechanism — this preserves rope classification. The exogenous and hybrid readings face mandatrophy because they must account for both genuine coordination function (the reversal does stabilize internal theology and resolve doctrinal tensions) and asymmetric extraction (federal pressure forces institutional capitulation). This constraint does not resolve the kernel-level mandatrophy — it instantiates one interpretation that escapes it. The sibling readings inherit the mandatrophy because they must balance coordinative and extractive elements with roughly equal weight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_revelation_authenticity,
    'Is the prophetic revelation claim genuine divine guidance or a post-hoc theological legitimation of institutional necessity?',
    'Historical analysis of the revelation''s timing relative to federal pressure; examination of theological precedent for prophetic doctrine reversal; comparison with other institutional crises and theological responses; assessment of whether the prophetic mechanism could have generated the reversal without external coercion.',
    'If genuine divine guidance: the endogenous reading is correct, extractiveness is low (0.15-0.25), the constraint is pure coordination from the beneficiary perspective. If post-hoc legitimation: the exogenous override reading is structurally accurate, extractiveness is high (0.55-0.70), the constraint is snare or tangled_rope from more perspectives. If hybrid (genuine theological work enabled by external pressure): the hybrid_pragmatic reading is correct, extractiveness is moderate (0.35-0.45).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_revelation_authenticity, conceptual, 'Whether the prophetic revelation is endogenous (divine guidance originating within the religious framework) or exogenous (institutional response to external coercion justified post-hoc through theology)').

omega_variable(
    theological_continuity_preservation,
    'Does the reframing of monogamy as a ''new covenant stage'' actually preserve the core theological commitments of the marriage institution, or does it represent a fundamental doctrinal break masked by continuity language?',
    'Detailed comparison of pre-Manifesto marriage theology (plural marriage as covenant fulfillment) with post-Manifesto theology (monogamy as covenant refinement); analysis of whether the new covenant framing is internally consistent with foundational theological principles; assessment of what theological commitments had to be abandoned or reinterpreted to accommodate the reversal.',
    'If continuity is genuine: the endogenous reinterpretation reading is structurally sound, beneficiaries include the theological integrity of the institution, suppression can be lower (40-50%, legitimate authority asserting theological coherence). If continuity is illusory: the reframing is a break presented as continuity (extraction mechanism), suppression is higher (50-70%, forced conformity to contradictory doctrine), the constraint shifts toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_continuity_preservation, conceptual, 'Whether the monogamy reframing preserves core theological commitments or represents fundamental doctrinal break masked as continuity').

omega_variable(
    institutional_vs_coercive_causation,
    'What proportion of the doctrinal reversal was driven by endogenous institutional logic (divine guidance, theological coherence) versus exogenous coercion (federal pressure, threat of institutional dissolution)?',
    'Counterfactual analysis: would the reversal have occurred in the absence of federal pressure? Historical examination of institutional communications, internal debates, and decision-making processes; comparison with parallel theological developments in institutions not facing federal coercion; analysis of timing (did the reversal happen at the precise moment maximum federal pressure was applied, or was there theological and institutional preparation independent of political events?).',
    'If primarily endogenous (>70% institutional logic): extractiveness remains low (0.20-0.30), beneficiaries are primarily the divine legitimacy framework and institutional theology. If primarily exogenous (>70% federal pressure): extractiveness is higher (0.50-0.65), federal authority is primary beneficiary (through doctrinal compliance), endogenous reading is a cover story. If genuinely mixed (40-60% each): the hybrid_pragmatic reading is correct, extractiveness is moderate (0.35-0.45).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_coercive_causation, empirical, 'Attribution of doctrinal reversal to endogenous theological logic versus exogenous political coercion').

omega_variable(
    divine_authority_beneficiary_legitimacy,
    'When the endogenous reading identifies ''divine authority'' as beneficiary, is this naming a genuine structural role (divine legitimacy framework as beneficiary of institutional coherence) or performing theological language that obscures institutional agency?',
    'Analytical deconstruction: divine authority operates through human institutional structures (prophetic officers, church councils, theological authorities). The beneficiary is operationally the institutional actors who wield divine authority claims. Does identifying ''divine authority'' as beneficiary clarify the constraint''s structure, or does it mystify institutional agency? Comparison with other institutional beneficiary declarations: when naming institutional beneficiaries, we name the human agents (shareholders, executives, board members), not abstract principles. Does theological framing require different practice?',
    'If divine authority naming is legitimate: the endogenous reading preserves theological categories without reducing them to institutional instrumentality. If it obscures institutional agency: the beneficiary should be reparsed as ''prophetic institutional actors'' and ''doctrinal authority officers,'' shifting the reading toward the exogenous or hybrid interpretations. This omega documents the committer frame tension between theological and structural language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_authority_beneficiary_legitimacy, conceptual, 'Whether ''divine authority'' as beneficiary names a genuine structural role or performs theological language that obscures institutional agency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcl_endo_theater_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mcl_endo_theater_t2, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 2, 0.4).
narrative_ontology:measurement(mcl_endo_theater_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 5, 0.42).

% Extraction over time
narrative_ontology:measurement(mcl_endo_extract_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mcl_endo_extract_t2, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(mcl_endo_extract_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of a three-way kernel contest over marriage doctrine legitimacy. The endogenous_reinterpretation_reading attributes doctrinal reversal to divine authority and prophetic revelation (low extractiveness, pure coordination). The sibling readings (exogenous_override and hybrid_pragmatic) attribute different causal weights to federal coercion, producing higher extractiveness values. Each reading is a complete constraint story with its own ε, beneficiary structure, and perspectives. They do not measure the same constraint from different angles — they disagree about causal mechanisms (divine guidance vs. federal coercion) that cannot be empirically distinguished from observable behavior. The three stories are linked via network.affects_constraints because they describe alternative interpretations of the same historical kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
