% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The abolitionist reading of the NPT Article IV/VI pairing interprets
 *   Article VI's disarmament mandate as binding, and treats Article IV's
 *   permission for civilian nuclear technology as illegitimate to the extent
 *   that such programs enable dual-use proliferation. This reading derives
 *   authority from humanitarian law (weapons of mass destruction are
 *   categorically prohibited in warfare) and from the TPNW precedent
 *   (complete prohibition of nuclear weapons achieved through alternative
 *   treaty). Under this reading, the NPT is not a bargain that non-nuclear
 *   states should accept—it is a structural trap that legitimizes weapons
 *   while extracting a commitment against proliferation from the majority of
 *   signatories. The reading coexists with the nonproliferation reading
 *   (which treats Article IV/VI as a workable compromise balancing security
 *   with development) and influences but does not foreclose the grand-bargain
 *   reading (which treats the compromise as a valid multi-generational
 *   commitment with redefinable terms). The extractiveness trajectory
 *   (0.52→0.68) shows accumulating extraction as nuclear weapon states have
 *   managed disarmament ambiguity while consolidating technical advantage and
 *   leverage over non-nuclear states through dual-use control regimes. The
 *   theater ratio (0.35→0.58) reflects rising performative content in NPT
 *   review conferences as disarmament language becomes decoupled from any
 *   measurable reduction trajectory. The suppression requirement (0.58→0.72)
 *   indicates intensifying institutional suppression of alternatives (TPNW is
 *   isolated diplomatically; non-nuclear state coalition-building faces
 *   nuclear-power opposition) and technical suppression (verification regimes
 *   that prevent non-aligned states from developing enrichment/reprocessing
 *   capacity).
 *
 * KEY AGENTS:
 *   - Non-nuclear states: Primary victims (powerless/trapped) — surrendered proliferation pathway in exchange for disarmament promise; trapped by verification regime and sanctions threat
 *   - Nuclear abolition movement: Secondary organized victim (organized/constrained) — excluded from treaty revision machinery; TPNW provides alternative but lacks enforcement; faces diplomatic isolation
 *   - Nuclear weapon states: Primary beneficiary (institutional/arbitrage) — exercise dual authority (Article IV legitimation + Article VI ambiguity) with no enforcement mechanism; maintain strategic advantage
 *   - IAEA: Mixed institutional role (institutional/constrained) — genuine verification function alongside institutional dependence on treaty continuation; incentive to suppress alternatives
 *   - Review conference consensus machinery: Performative institution (institutional/arbitrage) — persists through diplomatic protocol despite absent disarmament agreement
 *   - Humanitarian law regime: Normative framework (analytical/analytical) — TPNW instantiates humanitarian law precedent; creates structural pressure on NPT legitimacy through alternative authority grounding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'a9b28440-34f5-4c7c-9f96-d8236c155d7e').
narrative_ontology:cs_kernel_codification('a9b28440-34f5-4c7c-9f96-d8236c155d7e', fixed_text).
narrative_ontology:cs_authority_grounding('a9b28440-34f5-4c7c-9f96-d8236c155d7e', extraction).
narrative_ontology:cs_interpretation_layer_present('a9b28440-34f5-4c7c-9f96-d8236c155d7e').
narrative_ontology:cs_reading_relation('a9b28440-34f5-4c7c-9f96-d8236c155d7e', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('a9b28440-34f5-4c7c-9f96-d8236c155d7e', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('a9b28440-34f5-4c7c-9f96-d8236c155d7e', foundational, weapons_of_mass_destruction_categorical_prohibition).
narrative_ontology:cs_axiom_status(weapons_of_mass_destruction_categorical_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('a9b28440-34f5-4c7c-9f96-d8236c155d7e', weapons_of_mass_destruction_categorical_prohibition, deontological).
narrative_ontology:cs_axiom('a9b28440-34f5-4c7c-9f96-d8236c155d7e', foundational, dual_use_technology_incompatible_with_prohibition).
narrative_ontology:cs_axiom_status(dual_use_technology_incompatible_with_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('a9b28440-34f5-4c7c-9f96-d8236c155d7e', dual_use_technology_incompatible_with_prohibition, empirically_contingent).
narrative_ontology:cs_axiom('a9b28440-34f5-4c7c-9f96-d8236c155d7e', secondary, tpnw_represents_superior_legitimacy_basis).
narrative_ontology:cs_axiom_status(tpnw_represents_superior_legitimacy_basis, holdable).
narrative_ontology:cs_axiom_grounding('a9b28440-34f5-4c7c-9f96-d8236c155d7e', tpnw_represents_superior_legitimacy_basis, conventional).
narrative_ontology:cs_reference_frame('a9b28440-34f5-4c7c-9f96-d8236c155d7e', humanitarian_law_weapons_prohibition).
narrative_ontology:cs_drift_state('a9b28440-34f5-4c7c-9f96-d8236c155d7e', contemporary_nuclear_modernization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9b28440-34f5-4c7c-9f96-d8236c155d7e', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, global_prohibition_advocates).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, humanitarian_harm_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATES (SNARE) — Trapped by NPT structure that permits nuclear weapons in permanent members while restricting peaceful nuclear development. No exit without treaty breach; suppression via verification regime and sanctions threat. High experienced extraction: surrendered proliferation pathway in exchange for disarmament promise never fulfilled.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NUCLEAR ABOLITION MOVEMENT (SNARE) — Organized but structurally excluded from treaty revision. TPNW provides alternative framework but lacks enforcement; NPT's dual standard suppresses abolition pathway through diplomatic isolation and resource constraints on non-aligned states. Moderate power but high extraction through institutional gatekeeping.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR WEAPON STATES (ROPE) — Experience Article IV/VI pairing as pure coordination mechanism: treaty legitimizes possession, Article IV permits civilian programs that provide dual-use cover, Article VI disarmament language provides diplomatic cover while possession continues. Net beneficiary through institutional arbitrage.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IAEA AND VERIFICATION REGIME (TANGLED ROPE) — Genuine coordination function (preventing proliferation escalation) alongside asymmetric extraction (institutional legitimacy depends on treaty continuation; disarmament acceleration would undermine IAEA authority over peaceful programs). Enforcement active; beneficiary in treaty continuation, victim if framework collapses.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENSUS MACHINERY (PITON) — The Article IV/VI compromise text reflects 1968 consensus that is no longer functional; theaters of compliance (review conferences, working groups) persist through institutional inertia despite absent agreement on disarmament timeline. Performative rather than operative.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — Risk naturalizing the NPT compromise as an immutable feature of great-power relations: disarmament is structurally incompatible with sovereign autonomy; dual-use technology is inherently uncontrollable; nuclear deterrence is inevitable. However, this naturalizes what the abolitionist reading identifies as contingent institutional design choices.
constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_article_iv_vi_pairing__abolitionist, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, TR),
    TR >= 0.70.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts commitment from non-nuclear states (surrender of proliferation pathway, submission to verification) in exchange for disarmament that does not occur. The extraction is asymmetric: nuclear powers retain weapons and strategic advantage; non-nuclear states gain only normalization of non-possession. The trajectory (0.52→0.68) shows accumulating extraction as disarmament becomes explicitly unachievable within NPT timescales while dual-use control regimes intensify. Suppression (0.72): High. Multiple suppression mechanisms: (1) institutional suppression of TPNW through diplomatic opposition and resource constraints; (2) technical suppression through verification regimes that prevent enrichment/reprocessing capability in non-aligned states; (3) normative suppression through claims that disarmament is 'structurally impossible' (naturalizing what is institutional choice). Theater ratio (0.58): Moderate-high. NPT review conferences generate extensive performative activity (working groups, consensus texts, political commitments) with minimal outcome on disarmament timelines. The rise (0.35→0.58) reflects escalating gap between rhetoric and practice as nuclear arsenals modernize while formal disarmament negotiations stall. Claimed type (snare): The abolitionist reading classifies the constraint as pure extraction because the underlying assumption is that Article IV legitimation of dual-use programs is fundamentally incompatible with the disarmament mandate of Article VI—there is no coordination benefit that offsets the extraction, only institutional contradiction that suppresses the abolition pathway.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading produces a perspectival gap with the nonproliferation and grand-bargain readings. Nonproliferation reading (not this constraint): the NPT/IAEA regime successfully prevents proliferation escalation, Article IV/VI pairing works, disarmament progress is incremental but real. Abolitionist reading (this constraint): NPT legitimizes weapons through dual-use loophole, disarmament language is performative cover, TPNW represents superior framework based on humanitarian law. Grand-bargain reading (not this constraint): NPT is a binding multi-generational commitment; disarmament accelerates over time as conditions permit; Article IV and VI are compatible because verification mechanisms constrain proliferation risk. The gap is located in the interpretation of what constitutes legitimate treaty design: abolitionist reading treats dual-use technology permission as inherently delegitimizing; nonproliferation reading treats it as manageable through verification; grand-bargain reading treats it as acceptable provided disarmament occurs eventually.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the constraint. Non-nuclear states (powerless/trapped) experience maximum extraction: they surrender proliferation capability and accept inspection regimes in exchange for a disarmament commitment that is not binding and is explicitly unachievable by any stated timeline. This produces d≈0.95 (full target). The abolition movement (organized/constrained) experiences high extraction through exclusion from treaty revision: they face diplomatic isolation, resource constraints, and institutional gatekeeping despite representing a coherent alternative framework. This produces d≈0.75. Nuclear weapon states (institutional/arbitrage) experience the constraint as coordination with negative d: they benefit from Article IV legitimation and Article VI ambiguity, exercise strategic optionality, and face no enforcement cost. This produces d≈0.10 (beneficiary with escape routes). IAEA (institutional/constrained) occupies a contradictory position: it has genuine verification function (positive) but depends on treaty continuation for institutional legitimacy (negative). This produces d≈0.50 (mixed). The analytical observer at the civilizational context risks false-summiting the constraint as a mountain—naturalizing dual-use technology and disarmament incompatibility as laws of state relations rather than recognizing them as contingent institutional design that could be altered through treaty revision.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading avoids mandatrophy collapse by maintaining a coherent alternative framework (TPNW) grounded in humanitarian law rather than trying to rescue Article IV/VI pairing as a coordination mechanism. The abolitionist reading's strength is precisely that it does NOT try to call the constraint a Tangled Rope—it acknowledges that disarmament is the core commitment, Article IV's dual-use permission undermines disarmament, and therefore the NPT is extractive rather than coordinative. This avoids the trap of calling something coordination when extraction is occurring. The TPNW reading allows the abolitionist framework to sustain both a prohibition norm and enforcement capacity, whereas the NPT framework offers prohibition language without enforcement (Article VI has no verification mechanism, no breach consequences, no timeline). The mandatrophy resolution is therefore: if humanitarian law override is normatively binding, the abolitionist reading is correct (snare classification stands). If humanitarian law operates only as aspirational guidance, the NPT framework may be Tangled Rope or even Rope, but the abolitionist reading still provides a more honest assessment of the extraction that IS occurring under Article IV/VI pairing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_use_verification_threshold,
    'Is complete verification of dual-use proliferation risk technically feasible, or does it require impossible confidence levels?',
    'Technical assessment of verification protocols (IAEA enhanced inspection, environmental sampling, supply-chain tracking); comparison to medical isotope, research reactor, and enrichment facility detection rates over 50+ years',
    'If feasible: Article IV and VI are reconcilable through technical verification (supports nonproliferation reading). If infeasible: dual-use pathway is inherent to civilian programs, Article IV legitimizes proliferation risk (supports abolitionist reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_use_verification_threshold, empirical, 'Technical feasibility of dual-use proliferation verification').

omega_variable(
    disarmament_structural_necessity,
    'Is complete nuclear disarmament structurally incompatible with autonomous state security doctrine, or is this a contingent strategic choice by nuclear powers?',
    'Comparative institutional analysis: comparison of security models (mutual assured destruction vs collective security frameworks vs non-aligned defense alliances); case studies of states that abandoned weapons programs (South Africa, Libya, Ukraine, Belarus) — what conditions enabled reversal?',
    'If structurally necessary: mountain classification correct — nuclear weapons are unavoidable law of state relations. If contingent: abolitionist reading correct — disarmament is choice, not constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_structural_necessity, empirical, 'Whether disarmament is structural inevitability or contingent choice').

omega_variable(
    tpnw_enforcement_legitimacy,
    'Does TPNW represent equivalent or superior legitimacy to NPT for nuclear prohibition, or is it an incomplete parallel framework lacking enforcement capacity?',
    'Analysis of TPNW ratification trajectory, entry-into-force mechanisms, enforcement provisions (International Court of Justice jurisdiction), and state participation patterns; comparison to NPT verification infrastructure and negotiating history',
    'If equivalent/superior: abolitionist reading via TPNW is operationally viable. If incomplete: NPT remains the operative framework despite its dual standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_enforcement_legitimacy, conceptual, 'Comparative legitimacy of TPNW vs NPT for prohibition framework').

omega_variable(
    article_vi_disarmament_metric,
    'What constitutes ''good faith'' disarmament under Article VI? Is there a quantitative threshold (warhead reduction %, delivery-system limitation) that would satisfy the reading, or is the reading''s demand categorical (zero)?',
    'Abolitionist doctrine analysis; comparison to TPNW complete prohibition language vs NPT step-down language; assessment of whether any warhead reduction trajectory would resolve the abolitionist constraint or whether only categorical abolition suffices',
    'If threshold-based: abolitionist reading is a Tangled Rope incentivizing strategic disarmament. If categorical: abolitionist reading is a Snare with no exit short of complete reversal of deterrence doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_disarmament_metric, conceptual, 'Quantitative vs categorical interpretation of Article VI disarmament mandate').

omega_variable(
    humanitarian_law_override_scope,
    'Does humanitarian law override treaty language about dual-use technology, or does it operate at a different normative level (aspirational but not binding)?',
    'International humanitarian law analysis (IHL precedent, Protocol Additional I/II integration); case law from International Court of Justice (nuclear weapons opinions); comparison to other arms-control contexts where IHL constraints explicit (biological weapons, chemical weapons, landmines)',
    'If binding override: humanitarian law constraint invalidates Article IV permissiveness (supports abolitionist reading). If aspirational: treaty language controls over IHL general principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_law_override_scope, conceptual, 'Whether humanitarian law creates binding constraints on treaty interpretations').

omega_variable(
    reading_specification_ambiguity,
    'Is the abolitionist reading a claim about NPT''s CURRENT binding force (Article VI already mandates abolition), or a normative/prescriptive claim about what NPT OUGHT to require (article IV is illegitimate under humanitarian law)?',
    'Textual analysis of Article VI language (''pursue negotiations in good faith''); case law from International Court of Justice advisory opinions on nuclear weapons and Article VI interpretation; state practice in treaty review conferences',
    'If descriptive (current binding): NPT is already a snare for non-nuclear states. If prescriptive: abolitionist reading is aspirational political constraint requiring TPNW or treaty amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specification_ambiguity, conceptual, 'Whether abolitionist reading is descriptive or prescriptive of NPT binding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_abol_tr_t0, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0, 0.35).
narrative_ontology:measurement(npt_abol_tr_t10, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 10, 0.5).
narrative_ontology:measurement(npt_abol_tr_t20, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(npt_abol_be_t0, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(npt_abol_be_t10, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(npt_abol_be_t20, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt_abol_su_t0, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(npt_abol_su_t10, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(npt_abol_su_t20, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_legal_status_and_enforcement).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_norms_conflict_with_deterrence).

% DUAL FORMULATION NOTE:
% The abolitionist, nonproliferation, and grand-bargain readings of the NPT Article IV/VI pairing are three distinct constraint stories derived from one kernel. Each reading has its own epsilon value, classification type, and perspectives. They are linked via network.affects_constraints because interpretation of one reading influences legitimacy and feasibility of the others. The abolitionist reading (this story) has epsilon=0.68 (high extraction); the nonproliferation reading would have lower epsilon (treats dual-use verification as feasible); the grand-bargain reading would have even lower epsilon (treats disarmament timeline as acceptable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
