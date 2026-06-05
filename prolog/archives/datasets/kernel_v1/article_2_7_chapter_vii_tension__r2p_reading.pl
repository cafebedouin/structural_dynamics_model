% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Reading: Sovereignty Conditional on Population Protection
 *   domain: international_law/humanitarian_intervention/security
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P) reading of the Article 2(7)/Chapter
 *   VII tension instantiates a contemporary legal doctrine that makes state
 *   sovereignty CONDITIONAL on the state's capacity and willingness to
 *   protect its own population from systematic atrocity. This reading emerged
 *   in the 2000s in response to perceived failures of non-intervention
 *   doctrine during genocides in Rwanda and Bosnia. The R2P doctrine asserts
 *   that when a state manifestly fails or is unwilling to protect populations
 *   from mass atrocity, the international community acquires a responsibility
 *   to intervene — first through diplomatic and humanitarian means,
 *   escalating to enforcement action if necessary. The constraint is a
 *   TANGLED ROPE: it provides genuine coordination function (establishing
 *   shared international criteria for when intervention is legitimate) while
 *   simultaneously extracting from states that prioritize sovereignty
 *   discretion. The R2P reading coexists with the SOVEREIGNTY-FIRST reading
 *   (Article 2(7) non-intervention as foundational, intervention permitted
 *   only for interstate aggression or explicit consent). These readings
 *   represent genuinely contested positions in contemporary international
 *   law, not a settled hierarchy. The R2P reading does not foreclose the
 *   sovereignty-first reading — both remain live positions held by different
 *   state coalitions, different regional organizations, and different phases
 *   of international practice.
 *
 * KEY AGENTS:
 *   - Persecuted populations and groups facing systematic atrocity (powerless/trapped) — primary beneficiaries of R2P enforcement; structurally prevented from protecting themselves
 *   - Perpetrating states engaging in atrocity (institutional/constrained) — experience R2P as loss of sovereignty discretion; constrained but not eliminated (can moderate conduct below threshold)
 *   - Sovereignty-prioritizing states (non-perpetrators, institutional/constrained) — bear extraction in form of reduced non-intervention discretion; experience suppression of their exit option to remain neutral
 *   - Intervening coalitions (organized actors, UNSC P5, regional organizations) (organized/mobile) — benefit from legitimate intervention authority; gain political standing as norm-enforcers
 *   - International accountability regime (ICC, fact-finding missions) (organized/arbitrage) — gains legitimacy from R2P but sees role as evolving toward prosecution rather than emergency intervention
 *   - Non-intervention principle (institutional/arbitrage) — vestigial norm displaced by R2P but invoked in debate; persists through inertia
 *   - Analytical observer (analytical/analytical) — risks conflating normative aspiration (universal human rights) with legal construction (contested doctrine)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.58).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.48).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Reading: Sovereignty Conditional on Population Protection").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/humanitarian_intervention/security").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, 'bb6c374e-1c83-4023-b32a-22f16ce5b5da').
narrative_ontology:cs_kernel_codification('bb6c374e-1c83-4023-b32a-22f16ce5b5da', formalized).
narrative_ontology:cs_authority_grounding('bb6c374e-1c83-4023-b32a-22f16ce5b5da', lineage).
narrative_ontology:cs_interpretation_layer_present('bb6c374e-1c83-4023-b32a-22f16ce5b5da').
narrative_ontology:cs_reading_relation('bb6c374e-1c83-4023-b32a-22f16ce5b5da', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('bb6c374e-1c83-4023-b32a-22f16ce5b5da', foundational, sovereignty_conditional_on_protection_capacity).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection_capacity, holdable).
narrative_ontology:cs_axiom_grounding('bb6c374e-1c83-4023-b32a-22f16ce5b5da', sovereignty_conditional_on_protection_capacity, deontological).
narrative_ontology:cs_axiom('bb6c374e-1c83-4023-b32a-22f16ce5b5da', foundational, atrocity_as_international_peace_threat).
narrative_ontology:cs_axiom_status(atrocity_as_international_peace_threat, holdable).
narrative_ontology:cs_axiom_grounding('bb6c374e-1c83-4023-b32a-22f16ce5b5da', atrocity_as_international_peace_threat, instrumental).
narrative_ontology:cs_reference_frame('bb6c374e-1c83-4023-b32a-22f16ce5b5da', protection_trigger_framework).
narrative_ontology:cs_drift_state('bb6c374e-1c83-4023-b32a-22f16ce5b5da', contemporary_hybrid_enforcement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb6c374e-1c83-4023-b32a-22f16ce5b5da', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, international_accountability_norm).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, state_sovereignty_discretion).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, non_intervention_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED POPULATION (ROPE) — Persecuted groups experience R2P as a coordinating mechanism that legitimizes rescue. Their structural position is trapment, but the constraint functions to align international community behavior toward protection rather than extraction. The coordination function (organizing external intervention) dominates; extraction costs (loss of state sovereignty as protection against intervention) fall on the perpetrating state, not on victims. This population genuinely benefits from the R2P norm despite power asymmetry.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PERPETRATING STATE (TANGLED ROPE) — A state committing systematic atrocities experiences R2P as a hybrid: the norm provides coordination function (establishing what conduct triggers intervention) but also imposes asymmetric extraction (loss of sovereignty discretion when atrocities exceed a threshold). The state is constrained — it cannot exit the international system, but can moderate behavior to remain below intervention threshold. Active enforcement by international coalitions is present. The constraint coordinates international responses to atrocity while extracting sovereignty from the perpetrating state.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-INTERVENING STATE PRIORITIZING SOVEREIGNTY (SNARE) — States committed to unfettered sovereignty see R2P as pure extraction: a norm that legitimizes intervention in their domestic affairs, reduces their discretionary power, and creates precedent for unilateral or coalition-based coercion. These states bear extraction without meaningful coordination benefit. The norm functions to suppress their exit options (they cannot claim neutrality without cost to legitimacy). High suppression because the R2P norm is backed by the Security Council enforcement machinery and growing customary law status. No coordination function from this state's perspective — only loss of discretion.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERVENING COALITION (TANGLED ROPE) — Regional organizations, UNSC permanent members, and humanitarian coalitions experience R2P as mixed: the norm provides coordination function (establishing shared criteria for collective action on atrocities) and enables extraction of political benefit (humanitarian intervention raises legitimacy, contains regional conflict, positions interveners as norm-enforcers). The coalition has significant agency and exit options (can decline to intervene, can shape implementation). The constraint both coordinates their response AND grants them political advantages through enforcement role. Enforcement is active; beneficiaries (interveners) and victims (perpetrating state, non-intervening states) are clear.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL ACCOUNTABILITY REGIME (SCAFFOLD) — The ICC, UN fact-finding missions, and accountability mechanisms experience R2P as temporary support for transitional justice. The norm has a built-in sunset: as accountability mechanisms mature and deter atrocities, the need for emergency intervention declines. The regime benefits from R2P's legitimacy boost but sees its own role as evolving toward prosecution of atrocities rather than emergency response. Low theater ratio — accountability mechanisms are functionally oriented toward evidence gathering and prosecutorial standards. The sunset clause is implicit: if deterrence succeeds, intervention becomes unnecessary.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-INTERVENTION PRINCIPLE AS VESTIGIAL CONSTRAINT (PITON) — The original Article 2(7) non-intervention norm persists in treaty text and theoretical discourse but has been substantially displaced by R2P customary law. The principle is maintained through institutional inertia (still invoked in debates) and theoretical appeal (appealed to by sovereignty-first advocates) but no longer functions as the organizing principle of international intervention. Theater ratio ≥0.70: states invoke non-intervention without behavioral consequence when atrocities are severe. The constraint is degraded but not abandoned.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective anchored in human rights law and jus cogens norms, systematic atrocity triggering intervention appears as an immutable legal principle: human dignity and bodily integrity are inalienable, and intervention to prevent mass death is obligatory rather than discretionary. This perspective treats R2P as a natural law principle grounded in universal humanity. However, the false summit detector will identify this as naturalization of a political reading: R2P is contested (sovereignty-first reading coexists), recently codified (2005), and depends on enforcement by specific actors. This perspective risks conflating normative aspiration with structural law.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__r2p_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, TR),
    TR >= 0.70.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The R2P reading legitimizes intervention that violates the classical non-intervention principle, imposing constraints on state sovereignty. However, extractiveness is NOT maximal (0.72+) because the constraint includes substantial coordination function: it establishes shared criteria for intervention, channels coercion through institutionalized mechanisms (UNSC, regional bodies), and primarily targets perpetrating states rather than all sovereigns. Most states benefit from the protection norm when they are not perpetrators, and the norm is selectively enforced. Theater ratio (0.35): Low-moderate. R2P enforcement has been functionally oriented toward actual intervention (not mere rhetoric) in Syria (no intervention despite threshold breach — theater here), Libya (intervention occurred), South Sudan (limited intervention), Myanmar (no enforcement). The declining theater ratio over time (0.55 → 0.35) reflects growing institutional maturity: accountability mechanisms and armed interventions follow more consistent protocols rather than ad-hoc justifications. Suppression (0.48): Moderate. The R2P reading suppresses alternative framings of state behavior by establishing a new category ('atrocity triggering intervention') that overrides classical sovereignty discretion. However, suppression is not total (sovereignty-first reading remains defended by major states) and enforcement varies by case. The rising suppression over time (0.32 → 0.48) reflects growing customary law status and institutional adoption — the constraint's normative force has increased since 2001, making alternatives less viable.
 *
 * PERSPECTIVAL GAP:
 *   The R2P reading produces maximum perspectival divergence. Persecuted populations classify the constraint as ROPE (coordination for rescue); perpetrating states classify it as TANGLED ROPE (mixed coordination of international response with extraction of sovereignty); sovereignty-first states classify it as SNARE (pure extraction of non-intervention discretion); intervening coalitions classify it as TANGLED ROPE (mixed coordination of shared intervention criteria with extraction of political legitimacy for interveners); accountability regimes see SCAFFOLD (temporary emergency response, sunset as prosecution mechanisms mature); the non-intervention principle persists as PITON (degraded but not abandoned); the analytical observer risks seeing MOUNTAIN (universal human rights principle) but the false summit detector will flag this as naturalization of a contingent, contested reading. This divergence is STRUCTURAL, not observational — it reflects genuine differences in how the constraint functions for different agents, not measurement ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   The R2P reading's directionality derives from asymmetric beneficiary and victim relationships. Persecuted populations benefit absolutely (d ≈ 0.05 as beneficiaries with trapped exit); perpetrating states bear extraction (d ≈ 0.80 as victims of sovereignty loss); sovereignty-first states bear extraction (d ≈ 0.72 as victims of non-intervention principle erosion); intervening coalitions benefit (d ≈ 0.30 as organizational beneficiaries with mobile exit). The constraint's effective extractiveness chi scales upward for victims (sovereignty-first states experience higher chi due to f(d) sigmoid applied to d ≈ 0.72) and scales downward for beneficiaries (persecuted populations experience lower chi despite powerless power level, because f(d) applied to d ≈ 0.05 produces negative chi — the constraint subsidizes rather than extracts from them). Institutional beneficiaries (intervening coalitions) with arbitrage exit derive d ≈ 0.20, producing low chi per the formula χ = ε × f(d) × σ(S). Global scope amplifies chi via σ(S) ≈ 1.2, making the constraint's extraction more visible across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for R2P is resolved by showing that the constraint's classification depends fundamentally on which agent perspective is taken. From the persecuted population's perspective, R2P is ROPE (coordination mechanism for protection). From the sovereignty-first state's perspective, R2P is SNARE (pure extraction of discretion). From the perpetrating state's perspective, R2P is TANGLED ROPE (mixed). From the intervening coalition's perspective, R2P is TANGLED ROPE (mixed with political benefits). From the accountability regime's perspective, R2P is SCAFFOLD (temporary until prosecution mechanisms substitute). These are NOT inconsistent classifications of a single fact — they are consistent observations of how the constraint FUNCTIONS DIFFERENTLY for agents in different structural positions. The R2P reading's mandatrophy resolves not by selecting one type but by recognizing that the presheaf over all perspectives IS the complete description of the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_definition,
    'What scale and type of atrocity triggers the R2P intervention threshold? Is the boundary at genocide only, crimes against humanity generally, or systematic human rights violations?',
    'Analysis of UNSC intervention decisions and Security Council practice patterns; examination of which atrocities have triggered intervention versus which have not despite similar scale',
    'If threshold is narrow (genocide only): R2P applies to fewer cases, reduces extraction from non-intervening states, approaches pure coordination for true extreme cases. If threshold is broad (systematic HR violations): R2P extraction is higher, applies to more state conduct, shifts toward snare for sovereignty-first observers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_definition, empirical, 'Definition and consistency of atrocity threshold triggering R2P').

omega_variable(
    sovereignty_reading_foreclosure,
    'Does the R2P reading logically foreclose the sovereignty-first reading within a single international legal framework, or do both coexist as live positions held by different state coalitions?',
    'Analysis of UNSC voting patterns and state position statements; examination of whether any state simultaneously endorses both readings or whether positions are mutually exclusive by coalition',
    'If foreclosure: R2P is the dominant reading and sovereignty-first is becoming marginalized (supports higher extractiveness for sovereignty-first observers, legitimizes R2P enforcement). If coexistence: both readings remain live, reducing the normative force of either (supports lower extractiveness, indicates hybrid ambiguity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_reading_foreclosure, conceptual, 'Whether R2P forecloses or coexists with sovereignty-first reading').

omega_variable(
    intervener_selective_enforcement,
    'Is R2P applied consistently across comparable atrocities, or do intervening states selectively enforce based on geopolitical interest (selective enforcement → pure extraction from perspective of non-intervened-in states)?',
    'Comparative analysis of atrocities meeting R2P threshold but not triggering intervention versus atrocities that did trigger intervention; correlation with intervener strategic interests',
    'If consistent enforcement: R2P is genuine coordination norm. If selective: the norm is a facade for power politics, and extraction from non-intervened-in states is pure (they bear sovereignty loss while others escape). Selective enforcement raises theater ratio and changes snare classification for non-intervening states toward absolute snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervener_selective_enforcement, empirical, 'Consistency of R2P intervention across comparable atrocities').

omega_variable(
    alternative_remedy_substitution,
    'Could international accountability mechanisms (prosecution, sanctions, asset freezes) substitute for kinetic intervention without requiring sovereignty violation, or is military intervention uniquely necessary to stop ongoing atrocity?',
    'Historical analysis of atrocities halted by prosecution threats vs sanctions vs kinetic intervention; examination of counterfactuals where alternative remedies were unavailable',
    'If substitution possible: R2P is mislabeled extraction (could achieve protection goals without sovereignty loss). If kinetic intervention is necessary: R2P extraction is justified by necessity. Affects whether tangled_rope classification is accurate or whether constraint should degrade toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_remedy_substitution, empirical, 'Whether accountability mechanisms can substitute for kinetic R2P intervention').

omega_variable(
    kernel_reading_vs_natural_law_ambiguity,
    'Is this constraint ONE READING of the contested Article 2(7)/Chapter VII tension (a legal interpretation among others), or is it a discovery of natural law principle binding on all sovereigns (immutable human rights)?',
    'Analysis of R2P''s status in international law: is it binding customary law (supports natural law reading) or contested norm with competing interpretations (supports kernel reading)? Examination of whether states fundamentally disagree on R2P or merely on its application.',
    'If kernel reading: R2P is one interpretation that coexists with sovereignty-first reading; the constraint has higher epsilon because it competes with another reading for legitimacy. If natural law: R2P is universal and binding; the constraint has lower epsilon because it reflects discovered principle, not contested construction. The false summit detector will flag the natural law perspective as precisely this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_natural_law_ambiguity, conceptual, 'Whether R2P is a contingent reading or a universal natural law principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r2p_theater_2001, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(r2p_theater_2007, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(r2p_theater_2013, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(r2p_theater_2021, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(r2p_extractiveness_2001, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(r2p_extractiveness_2007, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(r2p_extractiveness_2013, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(r2p_extractiveness_2021, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(r2p_suppression_2001, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(r2p_suppression_2007, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(r2p_suppression_2013, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(r2p_suppression_2021, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, unsc_authorization_requirement).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, preventive_vs_reactive_intervention).

% DUAL FORMULATION NOTE:
% The Article 2(7)/Chapter VII tension decomposes into two structurally distinct constraints with different epsilon values and incompatible readings: the R2P reading (this constraint, eps≈0.58, legitimizes intervention conditional on atrocity) and the sovereignty-first reading (sibling constraint, eps≈0.68, treats sovereignty as foundational except for interstate aggression). These are not observations of the same constraint from different angles — they are two different constraints that compete for authority in the same legal space. Both readings use the same charter text (Article 2(7) and Chapter VII) but interpret the relationship between the principles differently. The R2P reading makes protection conditional on sovereignty; the sovereignty-first reading makes sovereignty foundational. The tension between these readings is the generative structure of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
