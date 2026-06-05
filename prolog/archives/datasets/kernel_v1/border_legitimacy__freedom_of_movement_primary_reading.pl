% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_primary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_primary_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_primary_reading
 *   human_readable: Border Enforcement as Constraint on Individual Freedom of Movement
 *   domain: political_philosophy/migration_policy/constitutional_law
 *
 * SUMMARY:
 *   The border legitimacy kernel is a contested commitment about whether
 *   state authority to control territorial entry is foundational to political
 *   legitimacy or is itself an illegitimate constraint on human liberty. This
 *   constraint story instantiates the FREEDOM_OF_MOVEMENT primary reading:
 *   the claim that individual liberty to move across space is a foundational
 *   right, and state borders that coercively prevent migration are imposing
 *   extractive constraints without legitimate justification absent compelling
 *   harm prevention. Under this reading, border enforcement is a snare — a
 *   pure extraction mechanism that suppresses the foundational human right to
 *   freedom of movement and benefits incumbent citizen coalitions and state
 *   border authorities while victimizing excluded migrants and the principle
 *   of human liberty itself. The measurement trajectory shows intensifying
 *   extraction and suppression (0.55 → 0.72 extractiveness, 0.70 → 0.82
 *   suppression) over the 1945-2020 interval as border enforcement
 *   infrastructure matured and became more sophisticated, while theater ratio
 *   increased (0.35 → 0.55) reflecting growing performative apparatus (visa
 *   protocols, citizenship ceremonies, border rituals) maintaining legitimacy
 *   narrative as actual enforcement became more visible and morally
 *   contested.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victims (powerless/trapped) — face legal prohibition on entry backed by armed enforcement; cannot appeal to alternative legitimate authority
 *   - Human Liberty Principle: Abstract collective victim (powerless/trapped) — suppressed recognition as foundational; systematically discounted in border legitimacy discourse
 *   - State Border Authority: Primary beneficiary (institutional/arbitrage) — maintains monopoly on territorial gatekeeping; captures regulatory authority and enforcement power
 *   - Incumbent Citizen Coalitions: Secondary beneficiaries (moderate/constrained) — extract benefits of public goods provision and resource scarcity protection; also bear some coordination costs
 *   - International Sovereignty Doctrine: Institutional actor (institutional/arbitrage) — maintains legitimacy narrative through institutional inertia despite increasing moral contestation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing border regimes as inevitable rather than contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_primary_reading, 0.68).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_primary_reading, 0.82).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_primary_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_primary_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_primary_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_primary_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_primary_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_primary_reading, "Border Enforcement as Constraint on Individual Freedom of Movement").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_primary_reading, "political_philosophy/migration_policy/constitutional_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_primary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_primary_reading, '25509a8d-004a-4c77-af79-122ce35d2b18').
narrative_ontology:cs_kernel_codification('25509a8d-004a-4c77-af79-122ce35d2b18', distributed).
narrative_ontology:cs_authority_grounding('25509a8d-004a-4c77-af79-122ce35d2b18', extraction).
narrative_ontology:cs_interpretation_layer_present('25509a8d-004a-4c77-af79-122ce35d2b18').
narrative_ontology:cs_reading_relation('25509a8d-004a-4c77-af79-122ce35d2b18', border_legitimacy__sovereignty_primary_reading, coexists_with).
narrative_ontology:cs_reading_relation('25509a8d-004a-4c77-af79-122ce35d2b18', border_legitimacy__economic_utility_reading, influences).
narrative_ontology:cs_axiom('25509a8d-004a-4c77-af79-122ce35d2b18', foundational, individual_freedom_movement_foundational).
narrative_ontology:cs_axiom_status(individual_freedom_movement_foundational, holdable).
narrative_ontology:cs_axiom_grounding('25509a8d-004a-4c77-af79-122ce35d2b18', individual_freedom_movement_foundational, deontological).
narrative_ontology:cs_axiom('25509a8d-004a-4c77-af79-122ce35d2b18', foundational, border_constraints_require_compelling_justification).
narrative_ontology:cs_axiom_status(border_constraints_require_compelling_justification, holdable).
narrative_ontology:cs_axiom_grounding('25509a8d-004a-4c77-af79-122ce35d2b18', border_constraints_require_compelling_justification, deontological).
narrative_ontology:cs_reference_frame('25509a8d-004a-4c77-af79-122ce35d2b18', universal_human_liberty_framework).
narrative_ontology:cs_drift_state('25509a8d-004a-4c77-af79-122ce35d2b18', contemporary_2020s, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25509a8d-004a-4c77-af79-122ce35d2b18', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_primary_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_primary_reading, state_border_gatekeepers).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_primary_reading, incumbent_citizen_coalitions).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_primary_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_primary_reading, human_liberty_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces absolute legal prohibition on entry backed by armed enforcement. No exit from the constraint; cannot appeal to alternative legitimate authority. Extraction is maximal: individual liberty is coercively overridden with no compensatory benefit. No coordination function exists from this agent's perspective — the constraint is pure extraction.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMAN LIBERTY PRINCIPLE / ABSTRACT COLLECTIVE (SNARE) — The principle of individual freedom of movement as a foundational human right is collectively victimized. Border enforcement systematically suppresses recognition of this principle as legitimate. No organized advocate; no exit option; total suppression. The constraint extracts from the very concept that legitimizes individual agency.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: STATE BORDER AUTHORITY (ROPE) — From the state's institutional perspective, border enforcement coordinates collective security, resource management, and community membership. Experiences the constraint as solving coordination problems: managing population flows, preventing resource collapse, maintaining social contract membership criteria. Net beneficiary with maximum exit options (can revise border policy at will). Does not perceive extraction because the constraint aligns with institutional goals.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT CITIZEN COALITION (TANGLED ROPE) — Citizens within the border experience genuine coordination benefits (public goods provision, mutual protection, shared liability) alongside extraction of migrant exclusion. The constraint provides coordination for insiders while extracting from outsiders. Constrained exit: renouncing citizenship is costly but possible. Mixed perception: beneficiaries of coordination who also extract from excluded migrants.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL SOVEREIGNTY DOCTRINE (PITON) — The doctrine that states have inherent right to control borders is now increasingly performative. Weakened by contradiction with international human rights law, economic interdependence, climate migration inevitability, and digital borderlessness. The doctrine persists through institutional inertia and mutual recognition agreements rather than functional necessity. Theater is elevated: border legitimacy rituals (treaties, visa protocols, administrative procedures) maintain appearance of consensual order while enforcement becomes increasingly visible and contested.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STATE NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, state borders and population management may appear as immutable structural requirements: finite resources require allocation, collective action requires boundaries, and some form of gatekeeping is inevitable to any organized community. This perspective risks naturalizing the state border system as a law of politics or economics. However, the structural data contradicts the mountain classification — the beneficiary declarations and suppression measurements reveal this as false summit naturalization: border regimes are constructed institutions with identifiable beneficiaries, not natural laws.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_primary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_primary_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_primary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_primary_reading, TR),
    TR >= 0.70.

:- end_tests(border_legitimacy__freedom_of_movement_primary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Under this reading, border enforcement extracts individual liberty from excluded migrants without providing them any compensatory benefit. The extraction rises from 0.55 (1945) to 0.72 (2020) as enforcement technology and state capacity increased. The value is not at the snare ceiling (0.80+) because this reading acknowledges some legitimate coordination functions within borders (public goods provision, resource management) — the extraction is maximal from the perspective of excluded migrants but partial when viewed as embedded in state coordination. However, the primary snare classification treats the border constraint as predominantly extraction with minimal coordination function for the victims. Suppression (0.82): Very high. Border enforcement involves direct physical prohibition (armed border patrol, detention, deportation), legal prohibition (visa restrictions, citizenship requirements), and epistemic prohibition (exclusion from rights discourse). Alternatives to exit are nonexistent for powerless migrants — they cannot negotiate, appeal to higher authority, or organize collective resistance effective at changing border policy. The suppression has intensified (0.70 → 0.82) as border technology advanced. Theater ratio (0.55): Moderate-high and increasing. Border legitimacy is increasingly defended through performative mechanisms (international agreements, humanitarian protocols, visa processing theatrics, citizenship ceremonies) even as actual enforcement becomes more visibly coercive. The rise (0.35 → 0.55) reflects the growing gap between legitimation narrative and enforcement reality — as coercion became harder to justify morally, administrative and diplomatic theater increased to maintain appearance of consensual order.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the deepest perspectival divergence in the corpus. A single structural arrangement (border enforcement) is simultaneously Snare (extractive), Rope (coordinative), Tangled Rope (mixed), Piton (performative), and Mountain (natural law) depending on the observer's structural relationship. The gap reveals that 'border legitimacy' is not a single constraint but a presheaf of constraint readings over different observational positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position relative to the constraint. Excluded migrants occupy full-target position (d ≈ 0.95): powerless, trapped, no exit options, victims of constraint — maximum experienced extractiveness. State authorities occupy full-beneficiary position (d ≈ 0.05): institutional power, arbitrage exit options, beneficiaries — negative experienced extractiveness (constraint subsidizes them). Incumbent citizens occupy mixed position (d ≈ 0.60): moderate power, constrained exit, both beneficiary and victim of the constraint — moderate experienced extractiveness. These d values feed the chi formula: χ = ε × f(d) × σ(S), where f(d) is the sigmoid directionality function. For excluded migrants at global scope: chi is maximized. For state authorities: chi is inverted (negative or minimal). For analytical observer at universal scope: chi reflects abstract extraction from the liberty principle itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel frame: the apparent contradiction between 'border constraints are natural/inevitable' (mountain perspective) and 'border constraints are extractive arrangements' (snare perspective) is actually a contradiction between two readings of the border legitimacy kernel, not a contradiction between measurement approaches. The false summit detector will identify the mountain perspective as naturalization of a contingent institutional arrangement. The snare classification is the terminal classification under the freedom-of-movement reading. Under the sovereignty reading, the classification would be rope or tangled_rope. Under the economic-utility reading, the classification would depend on empirical net benefit. No single classification resolves the mandatrophy — the reading choice determines the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liberty_vs_collective_flourishing_foundation,
    'Is individual freedom of movement a foundational right that overrides collective resource constraints, or is it constrained by collective sustainability requirements?',
    'Normative philosophy; cross-jurisdictional comparative analysis of societies with open borders vs. closed borders and correlation with human flourishing metrics (wellbeing, equality, liberty, sustainability)',
    'If liberty is foundational: border constraints are inherently illegitimate unless justified by compelling harm prevention. If collective flourishing is foundational: borders are legitimate coordination mechanisms. This is the core axiom divergence distinguishing this reading from the sovereignty and economic utility readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liberty_vs_collective_flourishing_foundation, conceptual, 'Whether individual liberty is foundational or constrained by collective requirements').

omega_variable(
    empirical_border_necessity,
    'Do open migration scenarios produce resource collapse, social instability, or public goods degradation sufficient to justify coercive border enforcement?',
    'Controlled policy experiments; comparative case analysis (EU open borders, internal migration within federal states, historical open-border periods); climate migration projections under constraint vs. unconstrained scenarios',
    'If no empirical harm from open migration: suppression values should decrease, and snare classification strengthens (extraction without coordination benefit). If empirical harm is substantial: tangled_rope classification is more defensible (extraction serves real coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_border_necessity, empirical, 'Whether empirical evidence supports necessity of coercive border enforcement').

omega_variable(
    committer_kernel_underdetermination,
    'Is the border legitimacy kernel itself underdetermined across these three readings, or do the readings represent genuine logical contradictions that force a choice?',
    'Formal analysis of axioms: identify whether (a) each reading''s foundational claim logically forecloses the others (forcing selection via consistency), or (b) the readings can coexist within different frameworks or at different scales (genuine pluralism)',
    'If genuine logical contradiction: one reading must be rejected and the constraint receives a single terminal classification. If coexistence is possible: the presheaf over the kernel is the answer, and the engine should emit all three readings as live positions. This determines whether the kernel''s treatment requires consensus or permits persistent disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_underdetermination, conceptual, 'Whether border legitimacy kernel readings are logically contradictory or genuinely coexistent').

omega_variable(
    state_gatekeeper_identification,
    'Are state border gatekeepers (''border_state_gatekeepers'' beneficiary group) well-defined as a structural actor, or is gatekeeper agency distributed across voters, politicians, bureaucrats, and military such that no single coherent beneficiary exists?',
    'Institutional analysis of border control apparatus; attribution of decision authority and benefit distribution; identification of whether concentrated elite benefit (snare signature) or distributed coalition benefit (tangled_rope signature) characterizes border enforcement',
    'If gatekeepers are concentrated: snare classification is robust and extractiveness is validly high. If gatekeeper authority is distributed: chi calculations must account for multiple institutional layers, and classification may shift to tangled_rope with lower effective extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_gatekeeper_identification, empirical, 'Definition and concentration of border gatekeeper agency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_primary_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_freedom_theater_1945, border_legitimacy__freedom_of_movement_primary_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(border_freedom_theater_1980, border_legitimacy__freedom_of_movement_primary_reading, theater_ratio, 35, 0.45).
narrative_ontology:measurement(border_freedom_theater_2020, border_legitimacy__freedom_of_movement_primary_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(border_freedom_extractiveness_1945, border_legitimacy__freedom_of_movement_primary_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(border_freedom_extractiveness_1980, border_legitimacy__freedom_of_movement_primary_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(border_freedom_extractiveness_2020, border_legitimacy__freedom_of_movement_primary_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(border_freedom_suppression_1945, border_legitimacy__freedom_of_movement_primary_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(border_freedom_suppression_1980, border_legitimacy__freedom_of_movement_primary_reading, suppression_requirement, 35, 0.8).
narrative_ontology:measurement(border_freedom_suppression_2020, border_legitimacy__freedom_of_movement_primary_reading, suppression_requirement, 75, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_primary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_primary_reading, border_legitimacy__sovereignty_primary_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_primary_reading, border_legitimacy__economic_utility_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_primary_reading, climate_migration_capacity).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_primary_reading, welfare_state_sustainability).

% DUAL FORMULATION NOTE:
% The border legitimacy kernel has three structurally distinct constraint instantiations: freedom_of_movement_primary_reading (epsilon=0.68, Snare — focuses on individual liberty extraction), sovereignty_primary_reading (epsilon=0.35, Rope — focuses on state coordination function), economic_utility_reading (epsilon=0.45, Tangled Rope — focuses on net welfare distribution). These are NOT different measurements of the same constraint; they are different readings of the contested kernel. Each has its own epsilon, its own beneficiary/victim declarations, and its own classification. They are linked via network.affects_constraints and via shared kernel_id in cs_structure blocks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
