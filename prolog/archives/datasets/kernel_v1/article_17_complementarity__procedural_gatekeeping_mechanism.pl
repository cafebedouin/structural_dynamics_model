% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__procedural_gatekeeping_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__procedural_gatekeeping_mechanism, []).

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
 *   constraint_id: article_17_complementarity__procedural_gatekeeping_mechanism
 *   human_readable: ICC Article 17 Complementarity as Procedural Gatekeeping
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes the complementarity principle:
 *   the International Criminal Court can intervene in a case only when a
 *   state is 'unwilling or unable' to genuinely investigate or prosecute.
 *   This reading instantiates Article 17 as a procedural gatekeeping
 *   mechanism that functions as both a coordination device (protecting state
 *   sovereignty, enabling consensual international cooperation) and an
 *   extraction mechanism (permitting powerful states to shield their own
 *   nationals from accountability while maintaining the legitimacy of the
 *   international justice system). The constraint exhibits structural
 *   hallmarks of tangled rope across multiple perspectives: genuine
 *   coordination function (states would never have consented to the ICC
 *   without gatekeeping), genuine asymmetric extraction (weak states and
 *   their victims bear disproportionate burden of gatekeeping while powerful
 *   states benefit from de facto immunity), and active enforcement (Pre-Trial
 *   Chambers must actively interpret and apply Article 17 to admit or reject
 *   cases). The rising theater ratio (0.52 → 0.68 over the interval) reflects
 *   increasing performative invocation of 'complementarity' as states develop
 *   elaborate justifications for inaction that satisfy the gatekeeping test
 *   nominally while circumventing its justice function substantively.
 *
 * KEY AGENTS:
 *   - Victims in weak states (powerless/trapped): Primary victims — cannot access ICC if domestic courts are inadequate or captured; bear full extraction cost
 *   - Weak states with unstable governance (moderate/constrained): Secondary beneficiaries of sovereignty protection but primary victims of extraction because gatekeeping shields corrupt elites and prevents external accountability mechanisms
 *   - Powerful states with functioning courts (institutional/arbitrage): Net beneficiaries — gatekeeping protects their sovereignty while their court systems are presumed adequate, providing them de facto immunity from ICC prosecution
 *   - ICC institution (institutional/constrained): Trapped between legitimacy function (preserving state consent through gatekeeping) and justice function (prevented from intervening in many high-impact cases by the same gatekeeping rule)
 *   - Complementarity doctrine as normalized principle (institutional/arbitrage): Persists through institutional inertia and codification; increasingly performative as states develop sophisticated gatekeeping justifications while blocking justice
 *   - Analytical observer (analytical/analytical): Risks naturalizing the Rome Statute's contingent design choice (gatekeeping) as a necessary feature of international law itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__procedural_gatekeeping_mechanism, 0.58).
domain_priors:suppression_score(article_17_complementarity__procedural_gatekeeping_mechanism, 0.62).
domain_priors:theater_ratio(article_17_complementarity__procedural_gatekeeping_mechanism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__procedural_gatekeeping_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__procedural_gatekeeping_mechanism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__procedural_gatekeeping_mechanism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__procedural_gatekeeping_mechanism, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__procedural_gatekeeping_mechanism, "ICC Article 17 Complementarity as Procedural Gatekeeping").
narrative_ontology:topic_domain(article_17_complementarity__procedural_gatekeeping_mechanism, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__procedural_gatekeeping_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__procedural_gatekeeping_mechanism, '5734e59b-e5be-4241-9f2d-2865fbebb277').
narrative_ontology:cs_kernel_codification('5734e59b-e5be-4241-9f2d-2865fbebb277', formalized).
narrative_ontology:cs_authority_grounding('5734e59b-e5be-4241-9f2d-2865fbebb277', extraction).
narrative_ontology:cs_interpretation_layer_present('5734e59b-e5be-4241-9f2d-2865fbebb277').
narrative_ontology:cs_axiom('5734e59b-e5be-4241-9f2d-2865fbebb277', foundational, state_sovereignty_nonsubordination).
narrative_ontology:cs_axiom_status(state_sovereignty_nonsubordination, holdable).
narrative_ontology:cs_axiom_grounding('5734e59b-e5be-4241-9f2d-2865fbebb277', state_sovereignty_nonsubordination, deontological).
narrative_ontology:cs_axiom('5734e59b-e5be-4241-9f2d-2865fbebb277', foundational, gatekeeping_as_power_asymmetry_enabler).
narrative_ontology:cs_axiom_status(gatekeeping_as_power_asymmetry_enabler, holdable).
narrative_ontology:cs_axiom_grounding('5734e59b-e5be-4241-9f2d-2865fbebb277', gatekeeping_as_power_asymmetry_enabler, empirically_contingent).
narrative_ontology:cs_reference_frame('5734e59b-e5be-4241-9f2d-2865fbebb277', state_consent_based_international_justice).
narrative_ontology:cs_drift_state('5734e59b-e5be-4241-9f2d-2865fbebb277', contemporary_structural_impunity_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5734e59b-e5be-4241-9f2d-2865fbebb277', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__procedural_gatekeeping_mechanism, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__procedural_gatekeeping_mechanism, powerful_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__procedural_gatekeeping_mechanism, icc_institutional_legitimacy).
narrative_ontology:constraint_victim(article_17_complementarity__procedural_gatekeeping_mechanism, weak_states).
narrative_ontology:constraint_victim(article_17_complementarity__procedural_gatekeeping_mechanism, marginalized_victims_in_weak_states).
narrative_ontology:constraint_victim(article_17_complementarity__procedural_gatekeeping_mechanism, justice_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIM IN WEAK STATE (SNARE) — A person harmed by mass atrocity in a state with weak judicial capacity faces complete extraction. Article 17 gatekeeping prevents ICC intervention precisely when domestic capacity is lowest. No exit: cannot access international justice if domestic courts are captured, corrupt, or inadequate. Bears full cost of the complementarity rule.
constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WEAK STATE GOVERNMENT (TANGLED ROPE) — Benefits from the coordination function of Article 17: protects state sovereignty and prevents external intervention, which genuinely enables some states to build capacity without foreign domination. Also bears extraction: the gatekeeping mechanism can be weaponized by captured elites to shield themselves from accountability. Moderate extraction, real coordination component — cannot exit without risking foreign pressure, but benefits from the principle even as exploited by corrupt actors within the state.
constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POWERFUL STATE WITH FUNCTIONING COURTS (ROPE) — Experiences Article 17 as pure coordination. The gatekeeping mechanism protects their own sovereignty: their courts are presumed adequate, so they face minimal ICC scrutiny. The rule coordinates state behavior toward internal accountability without triggering external enforcement. Net beneficiary — experiences extraction flow INTO this actor (protection), not away.
constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ICC AS INSTITUTION (TANGLED ROPE) — Experiences Article 17 as both coordination and extraction. Coordination function: preserves state consent to ICC jurisdiction; states would withdraw if ICC ignored domestic processes. Extraction function: the gatekeeping mechanism prevents the ICC from fulfilling its mandate to prevent impunity, leaving the institution as a symbolic actor constrained by rules that serve powerful states. The ICC is caught between its legitimacy (coordinating state cooperation) and its justice mission (blocked by gatekeeping).
constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLEMENTARITY DOCTRINE AS ESTABLISHED NORM (PITON) — The Article 17 gatekeeping mechanism persists as an accepted principle in international law despite evidence that it often shields impunity rather than enabling accountability. The doctrine has become performative: states and the ICC invoke 'complementarity' as a legitimating ritual while knowing it frequently prevents justice. Maintained through institutional inertia (Rome Statute codification) despite degraded function (many 'domestic investigations' are pretexts). Theater ratio is high because the rule's stated justification (protecting state sovereignty, enabling capacity-building) diverges from its actual use (permitting selective impunity).
constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Article 17 reflects an inviolable principle: state sovereignty cannot be overridden by international bodies except through state consent. The gatekeeping mechanism is seen as an immutable logical requirement of international law itself — you cannot have both sovereign equality AND supranational enforcement without consent. This perspective risks naturalizing a contingent institutional choice (the Rome Statute's design) as a law of international relations. The engine's false summit detector will identify this as a candidates for reclassification.
constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__procedural_gatekeeping_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_17_complementarity__procedural_gatekeeping_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__procedural_gatekeeping_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_17_complementarity__procedural_gatekeeping_mechanism, TR),
    TR >= 0.70.

:- end_tests(article_17_complementarity__procedural_gatekeeping_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The procedural gatekeeping mechanism extracts significant value to powerful states (protection from ICC scrutiny) while imposing costs on weak states and their victims (blocked access to international justice). The extraction is substantial but not total because: (1) some weak states genuinely benefit from sovereignty protection against intervention, (2) the ICC occasionally overcomes gatekeeping barriers (e.g., proprio motu investigations), and (3) the rule's stated justification (enabling capacity-building through coordination) has some legitimate basis even if frequently exploited. Suppression (0.62): Moderate-high. Significant barriers prevent victims and weak states from circumventing gatekeeping: the rule is codified in the Rome Statute (high procedural weight), Pre-Trial Chambers have broad discretion in applying 'unwilling or unable' (opaque standard), states control their own judicial capacity reporting (information asymmetry), and weak states face reputational/political costs of challenging powerful states' gatekeeping justifications. Theater ratio (0.68): High and rising. The gatekeeping mechanism has become increasingly performative over the 10-year measurement window as: (1) states develop elaborate justifications for 'genuine willingness to investigate' that satisfy the letter while circumventing the spirit of the rule, (2) the ICC's legitimate interest in preserving state consent creates incentives to accept gatekeeping assertions uncritically, (3) the distinction between genuine capacity-building and pretextual gatekeeping has become difficult to verify, and (4) the rule now functions as much as a symbolic affirmation of state sovereignty as a substantive justice mechanism.
 *
 * PERSPECTIVAL GAP:
 *   Perspectives range from snare (victim's experience of complete extraction with no exit) to rope (powerful state's experience of pure coordination and protection). The gap reveals that the same rule produces radically different structural realities depending on whether the agent has: (1) a functioning court system (powerful states → rope, treated as adequate), (2) an unstable court system exploited by elites (weak states → tangled rope, trapped between benefits and extraction), or (3) no access to courts at all (victims → snare, complete extraction). The ICC institution experiences tangled rope at the generational level: the gatekeeping mechanism that preserves state consent (coordination function) simultaneously prevents the ICC from fulfilling its mandate (extraction function). The mountain perspective risks naturalizing this as inherent to international law — that you 'cannot have' both sovereignty and supranational justice — when in fact it reflects a contingent institutional choice embedded in the Rome Statute's design.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value is derived from their structural position relative to the gatekeeping extraction flow. Victims in weak states are trapped with no exit options and are targets of the mechanism's blocking function: d ≈ 0.95 (maximum target). Weak states are moderate actors with constrained exits and mixed beneficiary/victim status (they benefit from sovereignty protection but are exploited by elites): d ≈ 0.55 (slight victim bias). Powerful states are institutional beneficiaries with arbitrage options (they can shop between domestic and international venues): d ≈ 0.15 (beneficiary, low extraction). The ICC institution is institutional but constrained by its own codified rules, making it neither full beneficiary nor full victim: d ≈ 0.50 (symmetric). The high suppression value (0.62) reflects that meaningful exit options for victims are severely limited — they cannot change the Rome Statute, cannot compel their states to prosecute, and cannot appeal directly to the ICC. The rising theater ratio indicates that suppression is increasingly maintained through performance and procedural complexity rather than through overt barriers, suggesting internalization and normalization of gatekeeping acceptance.
 *
 * MANDATROPHY ANALYSIS:
 *   Article 17 complementarity resolves the mandatrophy through explicit recognition of its tangled-rope structure. The rule simultaneously coordinates and extracts: it preserves state consent (necessary for ICC legitimacy and continued membership) while permitting selective impunity (undermining the justice mandate). The tension is not resolvable through pure coordination (rope) because the gatekeeping mechanism is explicitly asymmetric — it protects powerful states while constraining weak ones. The tension is not pure extraction (snare) because the coordination function is genuine and states would not have consented to the ICC without gatekeeping. The mandatrophy is resolved by accepting that international law contains irreducible structural contradictions: the Rome Statute attempts to be both a state-consent-based regime (requiring gatekeeping) and a universal justice regime (requiring supranational authority). Article 17 is where these contradictions become structurally visible. The rising theater ratio (0.52 → 0.68) indicates that the contradiction is increasingly hidden behind performative invocation of complementarity rather than openly negotiated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_capacity_measurement_ambiguity,
    'What constitutes ''genuine willingness or ability'' to investigate/prosecute under Article 17(1)(a)? Is a state''s assertion sufficient, or must external parties verify capacity?',
    'Comparative analysis of ICC Pre-Trial Chamber admissibility decisions; examination of states'' track records of prosecution and whether ICC has ever imposed capacity requirements beyond state self-reporting',
    'If assertion alone suffices: gatekeeping is nearly absolute, complementarity becomes de facto immunity. If external verification required: gatekeeping is moderate, but creates sovereignty friction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_capacity_measurement_ambiguity, conceptual, 'Whether domestic capacity can be verified or only asserted by the state').

omega_variable(
    impunity_as_feature_not_bug,
    'Is the gatekeeping mechanism intentionally designed to permit selective impunity as a sovereignty protection, or is impunity an unintended side effect of well-intentioned cooperation principles?',
    'Historical analysis of Rome Statute drafting debates; interviews with negotiators and ICC architects; examination of whether threshold for ''unwillingness'' has been deliberately set high by states',
    'If intentional: complementarity is a snare for victims and a rope for states — structural feature, not bug. If unintended: it remains a tangled rope but with different remediation logic (lower theater, capability gaps addressable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impunity_as_feature_not_bug, conceptual, 'Whether gatekeeping is intentionally designed to permit selective impunity').

omega_variable(
    counter_majoritarian_extraction,
    'Does Article 17 gatekeeping constitute a form of counter-majoritarian extraction, where a stable coalition of powerful states uses the rule to block accountability they would oppose?',
    'Network analysis of admissibility decisions correlated with state power (GDP, military capacity, UN Security Council status); statistical test of whether powerful states'' cases are disproportionately held inadmissible under Article 17',
    'If yes: extractiveness is higher than 0.58, classification approaches snare for global system level. If no: gatekeeping is more evenhanded, closer to pure tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_extraction, empirical, 'Whether powerful states systematically benefit from Article 17 gatekeeping').

omega_variable(
    kernel_reading_tension,
    'This constraint instantiates the Article 17 kernel through a ''procedural gatekeeping mechanism'' reading. What are the alternative readings of Article 17, and how do they relate to this one?',
    'Identify sibling readings (e.g., ''complementarity_as_capacity_building'', ''complementarity_as_state_consent_requirement''); map structural differences; determine whether readings coexist, foreclose, or influence each other',
    'This reading emphasizes the extraction mechanism inherent in gatekeeping; sibling readings emphasize coordination functions (capacity-building) or legitimacy principles (state consent). The relationship between readings determines whether Article 17 is coherent or contradictory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_tension, conceptual, 'Relationship between this procedural gatekeeping reading and sibling readings of Article 17').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__procedural_gatekeeping_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a17_gate_tr_t0, article_17_complementarity__procedural_gatekeeping_mechanism, theater_ratio, 0, 0.52).
narrative_ontology:measurement(a17_gate_tr_t5, article_17_complementarity__procedural_gatekeeping_mechanism, theater_ratio, 5, 0.61).
narrative_ontology:measurement(a17_gate_tr_t10, article_17_complementarity__procedural_gatekeeping_mechanism, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(a17_gate_be_t0, article_17_complementarity__procedural_gatekeeping_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(a17_gate_be_t5, article_17_complementarity__procedural_gatekeeping_mechanism, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(a17_gate_be_t10, article_17_complementarity__procedural_gatekeeping_mechanism, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(a17_gate_su_t0, article_17_complementarity__procedural_gatekeeping_mechanism, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(a17_gate_su_t5, article_17_complementarity__procedural_gatekeeping_mechanism, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(a17_gate_su_t10, article_17_complementarity__procedural_gatekeeping_mechanism, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__procedural_gatekeeping_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__procedural_gatekeeping_mechanism, icc_proprio_motu_trigger_limitation).
narrative_ontology:affects_constraint(article_17_complementarity__procedural_gatekeeping_mechanism, security_council_referral_asymmetry).
narrative_ontology:affects_constraint(article_17_complementarity__procedural_gatekeeping_mechanism, state_withdrawal_threat).

% DUAL FORMULATION NOTE:
% Article 17 complementarity is a kernel constraint that decomposes into multiple readings with different ε values and structural properties. The procedural_gatekeeping_mechanism reading (this file, ε=0.58) emphasizes extraction dynamics. Sibling readings would model the same Rome Statute text with different analytical focus: capacity_building reading might have lower ε (complementarity as coordination), sovereignty reading might have zero ε (complementarity as natural law). These are not observable-dependent variations of a single constraint — they are genuinely distinct structural readings of an ambiguous kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__procedural_gatekeeping_mechanism, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
