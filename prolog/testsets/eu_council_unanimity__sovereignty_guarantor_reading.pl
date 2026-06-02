% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity as Sovereignty Protection (Sovereignty Guarantor Reading)
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   This constraint models the EU Council unanimity requirement through the
 *   lens of the sovereignty-guarantor reading: unanimity rule protects small
 *   states from majoritarian coercion by requiring explicit consent to all
 *   collective decisions that implicate state sovereignty. In this reading,
 *   veto is a legitimate rights-exercise, not an extraction mechanism. The
 *   constraint exhibits a perspectival gradient: from the small state's view,
 *   unanimity is pure coordination (rope); from the collective action body's
 *   view, it is mixed coordination with extraction costs (tangled rope); from
 *   blocked citizens' view, it becomes pure extraction (snare); from the
 *   analytical civilizational view, it risks appearing as natural law
 *   (mountain, revealed as false summit). This is one of three structurally
 *   distinct readings of the same kernel (EU Council unanimity). The
 *   veto-trap reading models the same rule as an asymmetric extraction
 *   mechanism where some states block decisions to extract side benefits. The
 *   diplomatic-capital reading models unanimity as a coordination mechanism
 *   that creates perverse incentives for side-bargaining and diplomatic
 *   capital accumulation. All three readings operate on the same
 *   institutional mechanism but with different ε values, different
 *   beneficiary/victim structures, and different classifications. The
 *   sovereignty-guarantor reading claims moderate extractiveness (ε=0.32)
 *   because coordination costs exist but systematic asymmetric extraction
 *   does not — veto is the mechanism being coordinated, not the target of
 *   extraction.
 *
 * KEY AGENTS:
 *   - Small States: Primary beneficiary (powerful/mobile) — unanimity rule protects their sovereignty from majoritarian override; veto is their legitimate right-exercise
 *   - Large States in Majority Coalition: Mixed (institutional/constrained) — benefit from EU coordination; bear extraction costs when small states veto decisions large-state coalitions want
 *   - EU Collective Action Capacity: Victim (organized/constrained) — blocked by unanimity requirement when decisions would serve collective interest; requires ongoing negotiation overhead
 *   - Citizens Across EU: Victim (powerless/trapped) — when state-level veto blocks policies (climate action, pandemic response) that would benefit citizen welfare; no exit from blocked jurisdictions
 *   - Treaty Principle of Sovereign Equality: Beneficiary (institutional/arbitrage) — unanimity rule is the institutional embodiment of the principle; protected by the constraint structure
 *   - Analytical Observer: Positions the constraint as natural law boundary (analytical/analytical) — risks naturalizing contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.32).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.28).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity as Sovereignty Protection (Sovereignty Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d').
narrative_ontology:cs_kernel_codification('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', formalized).
narrative_ontology:cs_authority_grounding('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', lineage).
narrative_ontology:cs_interpretation_layer_present('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d').
narrative_ontology:cs_reading_relation('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', foundational, veto_as_sovereignty_defense).
narrative_ontology:cs_axiom_status(veto_as_sovereignty_defense, holdable).
narrative_ontology:cs_axiom_grounding('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', veto_as_sovereignty_defense, deontological).
narrative_ontology:cs_axiom('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', foundational, sovereignty_equality_principle).
narrative_ontology:cs_axiom_status(sovereignty_equality_principle, holdable).
narrative_ontology:cs_axiom_grounding('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', sovereignty_equality_principle, deontological).
narrative_ontology:cs_reference_frame('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', sovereign_equality_treaty_foundation).
narrative_ontology:cs_drift_state('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', contemporary_climate_fiscal_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a75ac97-2575-4dec-9b1c-7ab3a0a78f5d', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, sovereign_equality_principle).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, collective_action_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL STATE (ROPE) — Unanimity rule functions as genuine coordination mechanism for protecting state sovereignty from majoritarian override. Veto is a legitimate rights-exercise, not extraction. The constraint enables the small state to participate in collective decisions without surrendering sovereignty to larger states. Low extraction — the veto power is the thing being coordinated, not exploited.
constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: EU COORDINATION CAPACITY (TANGLED ROPE) — Genuinely must coordinate action across diverse sovereigns (coordination function present); simultaneously bears extraction costs when unanimity requirement blocks decisions that would benefit the collective. Mixed: some vetoes are legitimate sovereignty defense; others represent hold-up or veto-bargaining for side benefits. The constraint enables sovereignty protection AND creates asymmetric extraction when veto power is weaponized.
constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE STATE IN MAJORITY (TANGLED ROPE) — Coordinates with other large states while constrained by unanimity requirement. Benefits from participating in collective framework; bears extraction costs when small states veto decisions that large-state coalitions want. The large state has higher exit costs (cannot abandon EU framework without severe economic/diplomatic consequences) than small states in some decision domains, creating asymmetric vulnerability to veto.
constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FOUNDING TREATY PRINCIPLE (ROPE) — Unanimity as instantiation of sovereign equality principle sees the constraint as pure coordination: the rule itself IS the mechanism for respecting state equality in collective decisions. No extraction — the rule is the coordination function. From the treaty's normative frame, unanimity is how collective decisions get legitimacy (all states consent) rather than how they get blocked.
constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: CITIZENS ACROSS EU (SNARE) — From the perspective of citizens needing coordinated action (climate policy, pandemic response, border management), unanimity rule becomes a snare: individual state vetoes block decisions that would benefit the collective, and citizens have no exit from the jurisdictions imposing those vetoes. Pure extraction from the citizen's view — sovereignty protection for states becomes sovereignty capture blocking citizen welfare improvements. The blocking coalition pays no cost; the blocked majority pays full cost.
constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, unanimity appears as a natural boundary protection: any collective agreement that requires state A to surrender sovereignty to majority decision-making without A's explicit consent is, by definition, a sovereignty violation. Unanimity is the logical wall protecting this boundary. However, the base properties contradict this — structural beneficiaries and victims exist, revealing the mountain as a false summit: the 'natural boundary' naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_council_unanimity__sovereignty_guarantor_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate, reflecting the reading's core claim that veto is legitimate sovereignty defense rather than systematic extraction. The value is constrained by the upward trajectory in measurements (0.18 → 0.32 over 20 years), indicating that extraction costs have risen as the EU accumulated more collective-action domains and more vetoes accumulated. However, the trajectory does not approach snare territory (ε > 0.46) because the reading does not treat vetoes as extraction — the increase reflects growing coordination overhead, not growing asymmetric extraction. The trajectory would be steeper if modeling the veto-trap reading (ε would reach 0.55+ by year 20). Suppression (0.28): Low-moderate. Small states face some barriers to exiting the unanimity protection (EU membership itself is hard to exit; treaty change requires unanimous approval), but suppression is not high because the unanimity rule itself is the thing being protected — there is no alternative institutional arrangement being suppressed. Suppression would be much higher in the veto-trap reading (where blocking becomes coercive). Theater ratio (0.42): Moderate, and rising (0.30 → 0.42). The trend reflects that as unanimity vetoes became more frequent and more consequential for EU decision-making (climate, defense, fiscal integration), the performative element increased — debates about sovereignty increasingly frame positional bargaining and side-benefit extraction as principled defenses of national interest. The theater increase tracks the reading's divergence from pure coordination (rope-like) toward mixed coordination-extraction (tangled-rope-like).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a systematic perspectival gradient across power levels and exit options. Small states with arbitrage options (can exit EU if needed, though costly) experience the constraint as pure coordination (rope) — the unanimity rule is precisely what gives them meaningful agency in collective decisions. Large states constrained by EU framework (exit is very costly) experience mixed coordination-extraction (tangled rope) — they benefit from collective decisions when they can form supermajorities but bear extraction costs when small-state vetoes block their preferred policies. Citizens powerless and trapped within blocked jurisdictions experience pure extraction (snare) — sovereign state vetoes prevent decisions that would benefit their welfare, and they have no mechanism to override the veto or exit the jurisdiction. The founding treaty principle sees pure coordination (rope) — unanimity IS the mechanism that coordinates sovereign equality. The analytical observer risks seeing natural law (mountain) — the sovereignty boundary appears immutable. This gradient shows that the constraint's type depends entirely on the observer's structural position, not on objective features of the rule. The 'same' unanimity requirement is simultaneously coordination, extraction, and natural boundary depending on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the veto mechanism. Small states are beneficiaries with arbitrage options (can exit EU, though costly) — they experience low or negative d, experiencing the constraint as beneficial protection rather than extraction. The treaty principle of sovereign equality is the beneficiary-in-principle (arbitrage position) — d ≈ 0.15. Large states in majority coalitions are mixed: they benefit from EU collective action but are victims of veto-blocking — d ≈ 0.55, experiencing moderate extraction. The EU's collective action capacity is pure victim (trapped, no exit from the unanimity requirement without treaty change) — d ≈ 0.95, experiencing maximum extraction. Citizens are powerless victims with no exit (trapped) — d ≈ 0.98. The analytical observer's canonical d for analytical power is 0.73. These directionality values explain why perspectives produce different classifications despite the same base extractiveness: the sigmoid f(d) maps different d values to different effective extraction experiences. Small state (d ≈ 0.15) maps to rope (low chi); large state (d ≈ 0.55) maps to tangled rope (moderate chi); citizen (d ≈ 0.98) maps to snare (high chi). The constraint structure is stable; the experienced extraction varies by position.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by clearly distinguishing the coordination function (protection of sovereignty through unanimous consent requirement) from any extraction mechanism (veto as strategic blocking for side benefits). The tangled-rope classification acknowledges both: unanimity genuinely coordinates the protection of sovereign equality (no false symmetry here) AND creates measurable extraction costs for the collective action body and for citizens blocked by vetoes. The reading's moderate ε (0.32) reflects that extraction costs exist but are not treated as the primary function of the rule. If the veto-trap reading were instantiated instead, ε would rise to 0.55+ because the extraction mechanism would be primary. The mandatrophy is resolved by recognizing that the same rule has multiple true descriptions depending on which structural properties you weight — sovereignty protection OR coordination overhead OR strategic blocking. This reading chooses the sovereignty-protection description, yielding the moderate ε and tangled-rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_legitimacy_threshold,
    'At what frequency or scope of veto use does the sovereignty protection reading collapse into the veto-trap reading?',
    'Empirical tracking: count vetoes per state per year; classify as sovereignty defense vs. hold-up bargaining based on ex-post justification; identify inflection point where veto becomes seen as blocking rather than protecting',
    'If threshold is high (vetoes frequent): sovereignty reading holds — veto is normal exercise. If threshold is low (rare vetoes legitimate): even moderate use collapses into veto-trap framing. This is the reading_relations hinge point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_legitimacy_threshold, empirical, 'Frequency threshold distinguishing sovereignty defense from veto-trap').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the blocking of collective action by unanimity requirement a legitimate sovereignty exercise or an asymmetric extraction mechanism?',
    'Welfare analysis: identify decisions where unanimity-blocked policy would have benefited majority; track whether small-state beneficiaries from blocking capture benefits or merely prevent externalities; measure whether blocking states offer alternative proposals or simply obstruct',
    'If blocking is sovereignty defense: ε remains moderate (0.32), tangled_rope holds. If blocking is systematic hold-up for side benefits: ε rises to 0.50+, snare emerges at institutional perspective. The reading would shift from sovereignty_guarantor to veto_trap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether blocking represents sovereignty defense or extractive hold-up').

omega_variable(
    sibling_reading_coherence,
    'Can the sovereignty-guarantor and veto-trap readings coexist in the same institutional framework, or does one reading''s core premise logically foreclose the other?',
    'Logical analysis: test whether ''unanimity protects sovereignty'' and ''unanimity enables veto-trap extraction'' can both be true of the same mechanism. Answer is YES coexist — both describe real properties of the same rule. The readings differ in framing (how the mechanism is justified) not in the underlying structure.',
    'Determines reading_relations value: ''coexists_with'' (likely) or ''forecloses'' (unlikely). If coexistence is coherent, both readings remain live positions held by different parties in the EU institutional debate. If foreclosure is detected, one reading''s legitimacy claim undermines the other''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coherence, conceptual, 'Logical relationship between sovereignty-guarantor and veto-trap readings').

omega_variable(
    unanimity_as_contingent_choice,
    'Is unanimity requirement a natural law protecting state sovereignty, or a contingent institutional design choice that could be replaced by alternative mechanisms (qualified majority voting with safeguards)?',
    'Historical analysis: trace the treaty design rationale — was unanimity chosen as necessary protection or as pragmatic political settlement? Counterfactual analysis: what alternative decision rules would protect sovereignty while enabling more collective action? Design comparisons: do other federal systems use unanimity or alternatives?',
    'If natural law: mountain classification holds (all perspectives should agree). If contingent: mountain is a false summit, revealed as naturalization of institutional choice. The engine will detect false summit via FSM; this omega documents why.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_as_contingent_choice, conceptual, 'Whether unanimity is natural law or contingent institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eucoun_sg_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(eucoun_sg_tr_t10, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(eucoun_sg_tr_t20, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(eucoun_sg_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eucoun_sg_be_t10, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(eucoun_sg_be_t20, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 20, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% The EU Council unanimity requirement is modeled as three structurally distinct constraint stories, each with a different ε, beneficiary/victim structure, and classification. This story (sovereignty-guarantor reading) claims moderate ε (0.32) and tangled-rope type. The veto-trap reading claims higher ε (≈0.55) and snare type. The diplomatic-capital reading claims moderate ε (≈0.48) and tangled-rope with high theater. All three describe the same institutional mechanism but from different normative frames. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
