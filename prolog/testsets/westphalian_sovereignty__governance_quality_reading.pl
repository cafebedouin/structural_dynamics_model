% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__governance_quality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__governance_quality_reading, []).

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
 *   constraint_id: westphalian_sovereignty__governance_quality_reading
 *   human_readable: Governance-Quality Westphalian Sovereignty: Legitimacy Scaled to Domestic Governance Quality
 *   domain: international_law/political_philosophy/international_relations
 *
 * SUMMARY:
 *   The governance-quality reading of Westphalian sovereignty proposes that legitimate
 *   intervention in another state's internal affairs is scaled by the target
 *   regime's demonstrated governance quality. This reading decouples
 *   legitimacy from legality — a state with poor governance metrics can be
 *   assigned lower sovereignty protection even if unilateral intervention
 *   would violate the UN Charter and Article 2(4). The reading privileges
 *   democracy and liberal human rights governance as markers of legitimacy,
 *   implicitly granting the liberal democratic coalition (US, EU, allied
 *   states) a structural right to judge and potentially act against regimes
 *   that fail the governance-quality test. Unlike the absolutist reading
 *   (sovereignty is binary and equal) or the R2P reading (humanitarian
 *   catastrophe overrides sovereignty), the governance-quality reading creates a
 *   continuous spectrum where sovereignty itself is diminished — not
 *   suspended, but structurally weakened — when governance metrics decline.
 *   The constraint operates as tangled rope: it provides genuine coordination
 *   function (prevents interventions against well-governed states) while
 *   extracting asymmetrically (reduces protection for regimes the coalition
 *   judges as insufficiently democratic). The measurement trajectory shows
 *   increasing extractiveness and theater over time, consistent with the
 *   constraint becoming more visible as an active principle (Iraq 2003
 *   onward; Libya 2011; Syria 2013–present) rather than an implicit norm.
 *
 * KEY AGENTS:
 *   - Liberal Democratic Coalition: Institutional beneficiary (institutional/arbitrage) — US, EU, Allied democracies who define governance-quality criteria and retain intervention prerogative
 *   - Authoritarian Regimes: Primary victim (powerless/trapped) — structurally vulnerable to intervention framing; no exit from graduated legitimacy judgment
 *   - Non-Aligned Movement / Global South: Secondary victim (organized/constrained) — face asymmetric scrutiny; constrained by coalition voting power in intervention decisions
 *   - Human Rights Advocacy Network: Organized actor (organized/constrained) — legitimate intervention advocates; see graduated legitimacy as temporary tool with sunset (open-science-like normative evolution toward universal governance standards)
 *   - UN Security Council: Institutional degraded actor (institutional/arbitrage) — performatively applies governance-quality framing; actually operates under geostrategic interest (piton perspective)
 *   - Domestic Sovereignty Principle: Abstract collective victim (powerless/trapped) — the principle that states govern themselves without external judgment is undermined; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__governance_quality_reading, 0.58).
domain_priors:suppression_score(westphalian_sovereignty__governance_quality_reading, 0.52).
domain_priors:theater_ratio(westphalian_sovereignty__governance_quality_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__governance_quality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__governance_quality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__governance_quality_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__governance_quality_reading, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__governance_quality_reading, "Governance-Quality Westphalian Sovereignty: Legitimacy Scaled to Domestic Governance Quality").
narrative_ontology:topic_domain(westphalian_sovereignty__governance_quality_reading, "international_law/political_philosophy/international_relations").

domain_priors:requires_active_enforcement(westphalian_sovereignty__governance_quality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__governance_quality_reading, 'b07129d6-2ed3-4825-bad1-20431230422b').
narrative_ontology:cs_kernel_codification('b07129d6-2ed3-4825-bad1-20431230422b', formalized).
narrative_ontology:cs_authority_grounding('b07129d6-2ed3-4825-bad1-20431230422b', extraction).
narrative_ontology:cs_interpretation_layer_present('b07129d6-2ed3-4825-bad1-20431230422b').
narrative_ontology:cs_reading_relation('b07129d6-2ed3-4825-bad1-20431230422b', westphalian_sovereignty__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b07129d6-2ed3-4825-bad1-20431230422b', westphalian_sovereignty__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('b07129d6-2ed3-4825-bad1-20431230422b', foundational, governance_quality_correlates_sovereignty_legitimacy).
narrative_ontology:cs_axiom_status(governance_quality_correlates_sovereignty_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b07129d6-2ed3-4825-bad1-20431230422b', governance_quality_correlates_sovereignty_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('b07129d6-2ed3-4825-bad1-20431230422b', foundational, liberal_democracy_as_governance_gold_standard).
narrative_ontology:cs_axiom_status(liberal_democracy_as_governance_gold_standard, holdable).
narrative_ontology:cs_axiom_grounding('b07129d6-2ed3-4825-bad1-20431230422b', liberal_democracy_as_governance_gold_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('b07129d6-2ed3-4825-bad1-20431230422b', liberal_democratic_governance_legitimacy).
narrative_ontology:cs_drift_state('b07129d6-2ed3-4825-bad1-20431230422b', post_cold_war_unipolarity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b07129d6-2ed3-4825-bad1-20431230422b', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__governance_quality_reading, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__governance_quality_reading, liberal_democratic_coalition).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__governance_quality_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_victim(westphalian_sovereignty__governance_quality_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalian_sovereignty__governance_quality_reading, non_aligned_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__governance_quality_reading, domestic_sovereignty_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AUTHORITARIAN REGIME (SNARE) — Cannot exit the graduated legitimacy judgment; permanently vulnerable to intervention framing based on governance-quality metrics they are structurally incentivized to fail. No alternative framing available; no coalition support for counter-legitimacy claims. Trapped by the constraint itself — the constraint is specifically designed to reduce their sovereignty if governance metrics decline.
constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ALIGNED MOVEMENT / GLOBAL SOUTH (TANGLED ROPE) — Constrained by asymmetric voting power in intervention coalitions and dependency on liberal-democratic approval for legitimacy claims. Some genuine coordination function exists (shared interest in norm stability), but the constraint extracts asymmetrically: Global South regimes face higher scrutiny thresholds for intervention legitimacy than Northern democracies. Agency exists but is subordinate to coalition dynamics.
constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LIBERAL DEMOCRATIC COALITION (ROPE) — Benefits from the graduated legitimacy frame: their own governance quality is presumed higher, granting them intervention prerogative. Experiences the constraint as coordination: norm-setting that enables collective action against regimes deemed illegitimate. Net beneficiary with exit option (can exit via changing domestic governance quality standards or abandoning intervention altogether).
constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN RIGHTS ADVOCACY NETWORK (SCAFFOLD) — Organized agents (NGOs, treaty bodies, ICC) see the constraint as a temporary coordination tool with a sunset: as global governance quality improves and sovereignty becomes less dependent on regime type, the graduated framework becomes unnecessary. Low theater (advocacy is reasonably transparent about governance metrics). Constrained because advocacy organizations depend on coalition political will but see a path to norm evolution.
constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UN SECURITY COUNCIL VETO HOLDERS (PITON) — The graduated legitimacy framework is performatively invoked but functionally degraded: P5 members declare intervention decisions based on 'governance quality' framing while actually operating under geostrategic interest. The ritual of legitimacy assessment persists through institutional inertia (UN charter maintenance) despite low correlation between stated governance criteria and actual authorization patterns. Theater persists because alternatives haven't fully replaced the institutional form.
constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some differentiation of sovereignty based on regime performance is inherent to legitimate authority: no rational international system would grant equal intervention protection to genocidal regimes and functioning democracies. This perspective naturalizes governance-quality-as-fate. However, the structural data reveals false summit risk: the 'inherent' quality differentiation is actually constructed by a specific coalition with interest in asymmetric intervention prerogative.
constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__governance_quality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalian_sovereignty__governance_quality_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__governance_quality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westphalian_sovereignty__governance_quality_reading, TR),
    TR >= 0.70.

:- end_tests(westphalian_sovereignty__governance_quality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The graduated legitimacy framework creates asymmetric intervention prerogative: democracies in the coalition retain sovereignty protection while non-democracies experience sovereignty erosion. The extraction is not total (legitimate humanitarian crises remain a constraint on intervention even under this reading) but is substantial. The trajectory from 0.35 to 0.58 reflects how the principle has become increasingly operative post-2000, visible in Iraq, Libya, Syria intervention debates. Suppression (0.52): Moderate. Authoritarian regimes cannot easily challenge the governance-quality criteria (criteria are set by coalition with greater institutional power, media reach, and coordination capacity), but they retain some agency: they can appeal to Charter sovereignty, construct counter-narratives about Western hypocrisy, or build alternative coalitions (hence not maximal suppression like a snare). Theater ratio (0.61): Moderate-high. Governance-quality assessment involves genuine evaluation (some regimes objectively perform better on metrics like rule of law, human rights protection) but also substantial performative element: criteria are selectively applied, threshold shifts with coalition interest, and the legitimacy framing disguises geopolitical interest as principle. The measurement trajectory shows theater increasing as the principle becomes explicitly invoked — earlier iterations were more implicit, hence lower theater.
 *
 * PERSPECTIVAL GAP:
 *   This reading demonstrates how the same kernel (Westphalian sovereignty) can generate radically different constraint classifications depending on observer position. The absolutist reading (see sibling constraint westphalian_sovereignty__absolutist_reading) sees sovereignty as binary and equal — producing Rope or Mountain from all perspectives. The R2P reading (see sibling constraint westphalian_sovereignty__r2p_reading) makes humanitarian catastrophe the override principle — producing different victim sets (populations in danger, not regimes) and different legitimacy criteria. This governance-quality reading creates a continuous spectrum of sovereignty based on governance quality — producing Snare for authoritarian regimes, Rope for democracies, Tangled Rope for non-aligned states, and risk of false summits for analytical observers. The perspectival gap reveals that the 'right' reading of sovereignty depends entirely on where you sit: if you benefit from graduated legitimacy, you see it as principle; if you are vulnerable to it, you see it as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The liberal democratic coalition has low directionality value (d ≈ 0.15–0.25): they are net beneficiaries (positive arbitrage exit option) who experience low effective extraction. Authoritarian regime victims have high directionality (d ≈ 0.85–0.95): they are trapped by the framework itself and experience extraction at the maximum. Non-aligned organized actors occupy middle ground (d ≈ 0.55–0.65): they have some coalition bargaining power (organized exit option) but limited capacity to exit or reframe the principle. The constraint's effective extractiveness chi scales with spatial scope (global context amplifies the coalition's power via σ(S) = 1.2) and with the beneficiary's power level (institutional actors with arbitrage capacity produce low f(d), suppressing their experienced extraction while high-power differentials increase chi for victims). This asymmetry is the structural signature of tangled rope: genuine coordination function (prevents some interventions) exists alongside highly asymmetric extraction (reduces sovereignty asymmetrically based on regime type).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is the core tension in this reading: graduated legitimacy claims to be a principle (universal governance-quality standard applied equally to all) but functions as an extraction mechanism (applied asymmetrically to benefit liberal democracies and harm authoritarian regimes). Resolution requires determining whether: (a) the principle is genuinely universal — governance quality is objectively measurable and consistently applied — or (b) the principle is construction — metrics are coalition-determined and threshold shifts with coalition interest. If (a): the constraint is Rope (genuine coordination based on legitimate principle). If (b): the constraint is Snare (principle framing disguises coalition extraction). The measurements and omegas support (b): extractiveness rises over time as the principle becomes more explicitly invoked; theater increases as coalition interest in operational intervention framing grows; suppression requirement increases as targeted regimes have fewer counter-legitimacy options. The constraint functions as Tangled Rope because some genuine coordination exists (preventing interventions against well-governed states) while the extraction is real and asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_quality_metric_malleability,
    'Who defines ''governance quality'' and by what metrics? Can the definition be manipulated by the beneficiary coalition to expand or contract the victim set?',
    'Historical analysis of governance-quality assessments applied to intervention-target candidates vs. countries in the beneficiary coalition; correlation between metric definitions and geopolitical outcomes; comparison of criteria applied across regime types (do democracies face equivalent scrutiny?).',
    'If metrics are objective and stable: graduated legitimacy is a genuine principle (Rope from more perspectives). If metrics are coalition-determined and shifting: graduated legitimacy is an extraction mechanism disguised as principle (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_quality_metric_malleability, empirical, 'Whether governance-quality metrics are objective/stable or coalition-determined/shifting').

omega_variable(
    legitimacy_decoupling_from_legality,
    'Does the graduated legitimacy framework actually justify interventions that violate Charter Article 2(4) (prohibition on force) and Chapter VII authorization procedures?',
    'Case study analysis of post-2000 interventions: which used graduated legitimacy as justification for non-authorization? Did courts or UN bodies retroactively accept the governance-quality framing as sufficient legitimacy despite procedural illegality?',
    'If legality and legitimacy remain coupled: framework is rhetorical gloss on existing law (Piton). If decoupling occurs: graduated legitimacy is creating a parallel legality (Snare for targets; Rope for beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_decoupling_from_legality, empirical, 'Whether graduated legitimacy decouples from legal authorization requirements').

omega_variable(
    threshold_volatility_and_selective_application,
    'Has the governance-quality threshold for intervention legitimacy remained constant, or has it shifted to include/exclude specific regimes based on coalition geostrategic interest?',
    'Diachronic analysis of governance assessments: apply 2010 criteria to 2020 regime classifications; apply 2020 criteria retrospectively to 2010 cases; identify whether threshold-shift correlates with coalition interest shifts (e.g., ''We need to redefine governance quality to justify this intervention'').',
    'If threshold is stable and consistently applied: graduated legitimacy functions as principle (Rope). If threshold volatility correlates with coalition interest: extraction mechanism (Tangled Rope or Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_volatility_and_selective_application, empirical, 'Whether governance-quality threshold shifts to accommodate coalition interest').

omega_variable(
    counter_coalition_formation,
    'Can non-aligned or authoritarian states establish an alternative graduated legitimacy principle (e.g., ''intervention legitimacy based on respect for Charter sovereignty'') that would constrain liberal-democratic intervention?',
    'Analysis of counter-legitimacy claims in GA resolutions, regional organizations (AU, ASEAN, Arab League); assessment of whether alternative frameworks gain institutional traction; observation of whether counter-principle invocation changes intervention outcomes.',
    'If counter-coalition unable to establish principle: liberal democratic gradient is effectively monopolistic (high suppression, high extraction). If counter-principle gains traction: constraint becomes genuinely balanced (lower asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_coalition_formation, empirical, 'Whether alternative legitimacy principles can constrain liberal-democratic intervention').

omega_variable(
    kernel_reading_underdetermination,
    'Is this governance-quality reading of Westphalian sovereignty the only coherent interpretation of the kernel, or does the absolutist reading (sovereignty is binary, indivisible, equal for all recognized states) represent an equally defensible reading of the same foundational commitment to state legitimacy?',
    'Genealogical analysis of Westphalian treaty language and subsequent 400-year legal tradition; identification of whether absolutism or gradation is the *original* reading vs. later innovation; assessment of whether both readings remain live in contemporary state practice and legal doctrine.',
    'If governance-quality reading is the only coherent interpretation: this constraint''s legitimacy is structural (its axioms are holdable). If absolutism remains live: the kernel supports both readings equally, making this reading a *choice* rather than a logical necessity — the choice benefits the liberal coalition specifically, raising questions about whether the choice is justified by principle or interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether governance-quality reading is the only coherent interpretation of Westphalian kernel or whether absolutist reading remains equally defensible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__governance_quality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsov_grad_tr_t0, westphalian_sovereignty__governance_quality_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(wsov_grad_tr_t10, westphalian_sovereignty__governance_quality_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(wsov_grad_tr_t20, westphalian_sovereignty__governance_quality_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(wsov_grad_be_t0, westphalian_sovereignty__governance_quality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wsov_grad_be_t10, westphalian_sovereignty__governance_quality_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(wsov_grad_be_t20, westphalian_sovereignty__governance_quality_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wsov_grad_su_t0, westphalian_sovereignty__governance_quality_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(wsov_grad_su_t10, westphalian_sovereignty__governance_quality_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(wsov_grad_su_t20, westphalian_sovereignty__governance_quality_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__governance_quality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__governance_quality_reading, westphalian_sovereignty__absolutist_reading).
narrative_ontology:affects_constraint(westphalian_sovereignty__governance_quality_reading, westphalian_sovereignty__r2p_reading).
narrative_ontology:affects_constraint(westphalian_sovereignty__governance_quality_reading, humanitarian_intervention_prerogative).
narrative_ontology:affects_constraint(westphalian_sovereignty__governance_quality_reading, democratic_peace_hypothesis).

% DUAL FORMULATION NOTE:
% The three Westphalian readings (governance-quality, absolutist, R2P) are structurally distinct constraints with different ε values derived from the same foundational kernel. Each reading generates a different constraint family with its own beneficiary/victim sets, effective extraction mechanisms, and classification profiles. The governance-quality reading (this file) has ε ≈ 0.58; the absolutist reading has ε ≈ 0.15 (Rope, minimal extraction); the R2P reading has ε ≈ 0.42 (Tangled Rope, humanitarian override logic). They are linked via network.affects_constraints because each reading constrains the legitimacy space for the others: adopting the governance-quality reading forecloses some absolutist interpretations; the R2P reading influences both by establishing humanitarian catastrophe as override; the absolutist reading coexists with both by denying that either principle should override state equality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__governance_quality_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
