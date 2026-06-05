% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: War Winnability: Rhetorical Taboo / Operational Continuity (Post-1945)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the rhetorical_contraction reading of the
 *   war_winnability_post_1945 kernel. The core claim is that winnability
 *   underwent a dual-layer contraction post-1945: the public rhetorical space
 *   treats it as categorically unthinkable (MAD doctrine, mutual assured
 *   destruction, deterrence through invulnerability), while the operational
 *   planning space maintains explicit contingency plans for damage
 *   limitation, selective targeting, and constrained victory conditions. This
 *   is not a case of simple inconsistency — it is a structural constraint
 *   that extracts value for strategic planners by enabling operational
 *   flexibility without public accountability. The beneficiary is the
 *   strategic planning apparatus (military command, nuclear war planners,
 *   executive war cabinet), which maintains the ability to plan for victory
 *   conditions while the rhetorical taboo shields them from democratic
 *   debate. The victim is democratic oversight and the civilian epistemic
 *   commons, which cannot access or challenge the operational assumptions
 *   underlying nuclear strategy. The winnability taboo functions as a
 *   coordination mechanism for the strategic apparatus (allowing them to
 *   maintain deterrence credibility while preserving operational options)
 *   while simultaneously functioning as an extraction mechanism for the
 *   democratic public (denying them information and agency over existential
 *   risk).
 *
 * KEY AGENTS:
 *   - Strategic Planning Apparatus: Primary beneficiary (institutional/arbitrage) — maintains operational planning flexibility while rhetorical taboo prevents public scrutiny
 *   - Democratic Electorate: Primary victim (powerless/trapped) — bears existential risk without access to operational war plans or meaningful consent for nuclear strategy
 *   - Executive Authority / War Cabinet: Secondary beneficiary (institutional/arbitrage) — coordinates nuclear decision-making while taboo shields from political pressure
 *   - Military/Nuclear Command: Complex agent (powerful/constrained) — required to maintain victory-contingent plans while publicly affirming unthinkability; experiences strain between roles
 *   - Allied Civilian Populations: Secondary victim (powerless/trapped) — subject to extended deterrence without visibility into strategic calculations
 *   - Legislative Oversight Bodies: Organized agent (organized/constrained) — formally coordinate policy but lack access to operational details
 *   - Cold War Doctrine Institution: Institutional actor (institutional/arbitrage) — maintains performative rhetorical layer (theater ratio 0.81)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent political choice as thermodynamic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.58).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "War Winnability: Rhetorical Taboo / Operational Continuity (Post-1945)").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '5d6f65a8-222d-4888-8aff-3b7fd345e98c').
narrative_ontology:cs_kernel_codification('5d6f65a8-222d-4888-8aff-3b7fd345e98c', fixed_text).
narrative_ontology:cs_authority_grounding('5d6f65a8-222d-4888-8aff-3b7fd345e98c', extraction).
narrative_ontology:cs_interpretation_layer_present('5d6f65a8-222d-4888-8aff-3b7fd345e98c').
narrative_ontology:cs_reading_relation('5d6f65a8-222d-4888-8aff-3b7fd345e98c', war_winnability_post_1945__deterrence_unthinkable, influences).
narrative_ontology:cs_reading_relation('5d6f65a8-222d-4888-8aff-3b7fd345e98c', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_axiom('5d6f65a8-222d-4888-8aff-3b7fd345e98c', foundational, winnability_publicly_incoherent).
narrative_ontology:cs_axiom_status(winnability_publicly_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('5d6f65a8-222d-4888-8aff-3b7fd345e98c', winnability_publicly_incoherent, deontological).
narrative_ontology:cs_axiom('5d6f65a8-222d-4888-8aff-3b7fd345e98c', foundational, operational_planning_requires_contingency).
narrative_ontology:cs_axiom_status(operational_planning_requires_contingency, holdable).
narrative_ontology:cs_axiom_grounding('5d6f65a8-222d-4888-8aff-3b7fd345e98c', operational_planning_requires_contingency, empirically_contingent).
narrative_ontology:cs_reference_frame('5d6f65a8-222d-4888-8aff-3b7fd345e98c', rhetorical_taboo_with_operational_discretion).
narrative_ontology:cs_drift_state('5d6f65a8-222d-4888-8aff-3b7fd345e98c', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d6f65a8-222d-4888-8aff-3b7fd345e98c', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planning_apparatus).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, civilian_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEMOCRATIC ELECTORATE (SNARE) — Trapped within a rhetorical frame that declares winnability unthinkable while classified war plans assume constrained victory conditions. The electorate cannot exit the constraint or access the operational plans that contradict public doctrine. Maximum extraction: deprived of meaningful consent for nuclear strategy while bearing the existential risk. No alternative framing available in public discourse.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED CIVILIAN POPULATIONS (SNARE) — Trapped in extended deterrence commitments whose actual operational content is hidden behind rhetorical taboo. Cannot exit the security architecture or access war plans that ostensibly protect them. Subjected to nuclear risk without visibility into the strategic calculations governing that risk.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LEGISLATIVE OVERSIGHT BODIES (TANGLED ROPE) — Organized but constrained. Formally coordinate nuclear policy (coordination function) but lack access to operational war plans and targeting doctrine. Can demand transparency or refuse budget allocations (constrained exit), but classification and national security privilege restrict their actual leverage. Experience both coordinating function and extraction via information asymmetry.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STRATEGIC PLANNING APPARATUS (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: rhetorical taboo on winnability allows planners to maintain operational flexibility without public debate. The taboo is a coordination mechanism that solves their structural problem: how to plan for contingencies without triggering political pressure to abandon nuclear posture. Arbitrage exit: can shift between rhetorical public positions and classified operational postures without accountability. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXECUTIVE AUTHORITY (ROPE) — Secondary beneficiary. Coordinates nuclear decision-making between civilian and military hierarchies using the winnability taboo as a rhetorical shield. The taboo enables binding commitments to deterrence while preserving operational options. Arbitrage exit: can reframe public doctrine without changing military plans. Experiences the constraint as coordination problem solved.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MILITARY/NUCLEAR COMMAND (TANGLED ROPE) — Constrained by political requirement to affirm that nuclear war is unwinnable, but operationally required to maintain plans for damage limitation and victory conditions. Must coordinate with civilian authority (coordination function) while maintaining strategic flexibility (extraction via planning discretion). High tension between public doctrine and operational doctrine creates structural strain. Constrained exit: cannot publicly challenge the winnability taboo without triggering civilian-military crisis.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: COLD WAR DOCTRINE INSTITUTION (PITON) — The rhetorical taboo itself has become substantially performative. The declarative commitment to MAD (Mutual Assured Destruction) and the operational reality of counterforce planning coexist as two theater layers of the same institution. Theater ratio is high (0.81) because public affirmation of the taboo serves primarily to signal commitment without constraining operational planning. The doctrine persists through institutional inertia and because alternatives (explicit winnability doctrine) would trigger political instability. Core function (deterrence through ambiguity about retaliatory capacity) has atrophied; rhetorical performance (affirming unthinkability) maintains the institution.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational perspective, the rhetorical contraction appears structurally inevitable: once nuclear weapons reach destructive thresholds above societal recovery capacity, any rational actor's operational doctrine must treat total victory as impossible. The taboo is not contingent — it is a thermodynamic consequence of weapons physics. Winnability cannot be preserved operationally once destructive capacity exceeds regeneration. However, this perspective risks naturalizing what is actually a contingent political choice: the United States, USSR, and their allies could have chosen countervailing doctrine instead (see sibling reading). The 'inevitability' framing obscures that the rhetorical taboo is maintained through active suppression of countervailing planning discussion.
constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_winnability_post_1945__rhetorical_contraction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, TR),
    TR >= 0.70.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The strategic planning apparatus extracts significant value from the winnability taboo — it enables them to maintain operational flexibility (explicit contingency plans for victory conditions) while avoiding political pressure to either commit to deterrence-only doctrine or abandon nuclear posture entirely. The extraction is not maximal because countervailing planners must coordinate with civilian authority and cannot publicly articulate winning conditions. Suppression (0.68): Moderate-high. The taboo is actively maintained through classification of war plans, career consequences for officials who articulate winnability, and rhetorical framing that treats winnability as incoherent rather than merely undesirable. Declassified documents show that winnability discourse was systematically suppressed in public debate while operationally planned. Theater ratio (0.81): High and rising. The public affirmation of the winnability taboo is substantially performative — the declarative commitment to MAD serves to signal deterrence commitment without constraining operational planning. Theater ratio has risen from 0.42 in 1945 (when winnability was still openly debated in strategy circles) to 0.81 by 1990 (when the taboo had become nearly absolute in public discourse while operational counterforce planning remained classified).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival divergence. The strategic planning apparatus experiences the taboo as Rope (pure coordination: the rhetorical frame solves the problem of maintaining deterrence credibility while preserving options). The democratic electorate experiences it as Snare (pure extraction: they cannot exit the nuclear architecture or access information to consent). The analytical observer from a civilizational perspective risks seeing it as Mountain (inevitable consequence of weapons physics), which obscures that the rhetorical taboo is a contingent political choice. The military command experiences it as Tangled Rope (required to coordinate with civilian authority while maintaining operational flexibility). The legislative bodies experience it as Tangled Rope (can formally coordinate policy but lack decision-relevant information). The institutional piton perspective recognizes that the taboo has become performative — its core function (preventing escalation through ambiguity) has been displaced by its rhetorical performance (affirming unthinkability). The key diagnostic signal: if the taboo were genuinely about the physical impossibility of winnability, all perspectives would classify it as Mountain. Instead, the divergence across contexts reveals that the taboo is a political mechanism, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from the agent's structural relationship to the winnability constraint. Strategic planners are beneficiaries with high discretion (arbitrage-level exit): they can shift rhetorical positions without changing operational plans. Derived d ≈ 0.15 (beneficiary + arbitrage exit → low/negative f(d)). Democratic electorates are victims with no exit options (trapped): they cannot abandon nuclear deterrence or access war plans. Derived d ≈ 0.95 (victim + trapped → high f(d) ≈ 1.42). Military command are constrained beneficiaries (powerful agents facing political constraints on discourse): they benefit from operational flexibility but cannot exercise full arbitrage because public articulation of winnability would trigger civilian crisis. Derived d ≈ 0.48 (benefits from planning discretion + constrained exit on discourse). Legislative bodies are organized victims (can coordinate formally but lack information): d ≈ 0.65 (organized + constrained → moderate f(d)). The chi formula χ = ε × f(d) × σ(S) produces high effective extraction for powerless/trapped agents at global scope, moderate extraction for constrained institutional actors, and low extraction for beneficiaries with arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING MANDATROPHY: This reading resolves the mandatrophy by claiming that winnability is operationally thinkable (classified plans assume damage-limitation and constrained victory conditions) while rhetorically unthinkable (public doctrine treats it as incoherent). The benefit of this reading over alternatives: it explains the actual structure of deterrence policy (why planners maintain war plans they claim cannot succeed). The cost: it implies that democratic oversight has failed — the public cannot access the operational assumptions underlying their existential risk. The mandatrophy is NOT 'which type is correct' but rather 'how do we account for the dual-layer contraction?' This reading shows that the constraint exhibits genuine Tangled Rope properties (coordination function + asymmetric extraction) at the institutional level and Snare properties (pure extraction) at the democratic level. The reading is mandatrophy-coherent because it identifies why the same structural phenomenon produces different classifications across power levels: the taboo solves a coordination problem for the powerful (benefits them without requiring explicit negotiation) while extracting from the powerless (prevents them from exercising agency over existential decisions).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_enforcement_mechanism,
    'Is the winnability taboo maintained through genuine epistemic consensus (nuclear war is unwinnable), institutional discretion (planners choose not to articulate winnability), or active suppression (discourse that suggests winnability is actively punished)?',
    'Historical analysis of declassified planning documents; tracking of career consequences for officials who publicly articulate winnability conditions; comparison of classified war plans against public declaratory policy',
    'If consensus: the constraint is legitimate coordination (Rope). If discretion: moderate extraction masked by taboo (Tangled Rope). If active suppression: high extraction disguised as consensus (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taboo_enforcement_mechanism, empirical, 'Mechanism maintaining the winnability taboo').

omega_variable(
    operational_plan_content_divergence,
    'What is the actual operational content of current nuclear war plans? Do they contain explicit victory conditions or damage-limitation targets that constitute a de facto winnability doctrine?',
    'Declassification of post-Cold War nuclear war plans (SIOP); FOIA releases of strategic planning documents; official testimonies on targeting doctrine; comparison with published academic strategic literature',
    'If plans assume unwinnable deterrence: the taboo is genuine and extraction is moderate. If plans contain victory/damage-limitation conditions: the taboo is rhetorical camouflage, and extraction is severe (beneficiaries maintain operational flexibility while denying public debate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_plan_content_divergence, empirical, 'Whether classified war plans contain de facto winnability conditions').

omega_variable(
    countervailing_doctrine_foreclosure,
    'Is the winnability taboo logically compatible with countervailing nuclear strategy, or does it foreclose the possibility of explicit damage-limitation planning?',
    'Comparison of rhetorical frames: can one coherently hold ''nuclear war is unwinnable'' AND ''we plan to limit damage through selective counterforce targeting''? Are these coexistent in current doctrine or mutually exclusive?',
    'If coexistent: the taboo and countervailing planning are compatible, and the sibling ''countervailing_thinkable'' reading can coexist with this one. If mutually exclusive: this reading forecloses the countervailing reading within a single decision framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(countervailing_doctrine_foreclosure, conceptual, 'Whether winnability taboo logically forecloses countervailing strategy').

omega_variable(
    democratic_oversight_restoration_cost,
    'If the winnability taboo were lifted and war plans declassified, what would be the political cost to the strategic planning apparatus?',
    'Counterfactual analysis: polling on public acceptability of explicit victory planning; comparative case studies (e.g., public reaction to France''s declared force de frappe, India''s no-first-use doctrine changes); cost-benefit analysis of transparency vs. operational flexibility loss',
    'If cost is high: the taboo has significant extraction value (Snare tendency). If cost is low: the taboo is performative rather than extractive (Piton tendency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_oversight_restoration_cost, preference, 'Political cost of lifting winnability taboo').

omega_variable(
    rhetoric_operationality_drift_direction,
    'Is the gap between rhetorical taboo and operational planning widening or narrowing over time?',
    'Trend analysis: comparing declassified war plans across decades; tracking the frequency and visibility of winnability discourse in strategic literature; monitoring shifts in official policy statements',
    'If widening: extraction is accelerating (the taboo is being maintained at increasing cost to democratic oversight). If narrowing: the readings may be converging (operational planning is conforming to rhetoric, or rhetoric is shifting toward operational realism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetoric_operationality_drift_direction, empirical, 'Whether gap between rhetoric and operations is widening or narrowing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(winnability_rhet_theater_1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(winnability_rhet_theater_1965, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 20, 0.71).
narrative_ontology:measurement(winnability_rhet_theater_1990, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 45, 0.81).

% Extraction over time
narrative_ontology:measurement(winnability_rhet_extract_1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(winnability_rhet_extract_1965, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(winnability_rhet_extract_1990, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 45, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(winnability_rhet_suppress_1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(winnability_rhet_suppress_1965, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(winnability_rhet_suppress_1990, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, nuclear_strategy_counterforce_targeting).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, extended_deterrence_credibility_gap).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_asymmetry_nuclear).

% DUAL FORMULATION NOTE:
% The winnability kernel decomposes into three structurally distinct constraints with different ε values: (1) deterrence_unthinkable (ε ≈ 0.15, Mountain, pure natural law framing) — winnability is physically impossible; (2) countervailing_thinkable (ε ≈ 0.42, Tangled Rope) — winnability is operationally constrained but achievable; (3) rhetorical_contraction (ε ≈ 0.58, Tangled Rope) — winnability is planned operationally but unsayable publicly. Each reading gets its own constraint story. They are linked via network.affects_constraints because the rhetorical taboo (this constraint) depends on the institutional ability to maintain countervailing planning (downstream sibling) while publicly affirming deterrence doctrine (upstream frame from deterrence_unthinkable). The ε values differ because they measure different observables: what is physically inevitable vs. what is operationally planned vs. what is publicly expressible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, institutional, 0.22).
constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
