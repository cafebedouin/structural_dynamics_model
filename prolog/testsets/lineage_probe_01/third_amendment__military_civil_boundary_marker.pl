% ============================================================================
% CONSTRAINT STORY: third_amendment__military_civil_boundary_marker
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_third_amendment__military_civil_boundary_marker, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: third_amendment__military_civil_boundary_marker
 *   human_readable: Third Amendment: Military-Civil Boundary as Deep Rule
 *   domain: constitutional_doctrine/civil_military_relations
 *
 * SUMMARY:
 *   The Third Amendment — 'No Soldier shall, in time of peace, be quartered
 *   in any house, without the consent of the Owner, nor in time of war, but
 *   in a manner to be prescribed by law' — appears in constitutional
 *   scholarship as the least-litigated amendment, yet this reading interprets
 *   it as marking a deep structural principle: military power is subordinate
 *   to civil society, instantiated through the inviolable household boundary.
 *   The household functions as synecdoche for all civilian space that must be
 *   kept free from military occupation. This reading instantiates the
 *   constraint as a mountain — a foundational axiom of constitutional
 *   democracy that cannot be negotiated or overcome, even in emergencies (war
 *   is regulated but not exempted). The beneficiary is civil supremacy
 *   doctrine itself; the victim is any militarized administration that seeks
 *   to extract the low-cost logistics of billeting soldiers in civilian
 *   homes. The constraint embeds absolute suppression (quartering is not
 *   permitted, negotiable, or subject to cost-benefit analysis) and minimal
 *   extractiveness (the military bears the cost of constructing barracks;
 *   civil society extracts nothing from the constraint). This reading
 *   contests with two sibling readings: the dormant-by-success reading (the
 *   amendment's force is measured by the absence of quartering cases, not by
 *   active principle) and the privacy-penumbra reading (the amendment's work
 *   is done through implication in Griswold, not through direct application).
 *   This story generates the military-civil-boundary reading alone, without
 *   hedging across siblings.
 *
 * KEY AGENTS:
 *   - Civil Supremacy Doctrine: Primary beneficiary (institutional/analytical) — the constitutional principle that military power remains subordinate to civil authority; benefits from the amendment's marking of this boundary as non-negotiable
 *   - Occupied Householder: Primary victim (powerless/trapped) — compelled by statute to quarter troops; the amendment protects them with absolute prohibition
 *   - Militarized Administration: Secondary victim (organized/constrained) — seeks efficient troop logistics through billeting; faces absolute suppression of this option
 *   - Constitutional Order: Institutional boundary-maintainer (institutional/analytical) — the amendment is self-imposed constraint on state power; the state cannot dissolve it through subsequent legislation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the amendment as embedding a foundational principle about the structure of civil-military relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(third_amendment__military_civil_boundary_marker, 0.08).
domain_priors:suppression_score(third_amendment__military_civil_boundary_marker, 0.02).
domain_priors:theater_ratio(third_amendment__military_civil_boundary_marker, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, extractiveness, 0.08).
narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(third_amendment__military_civil_boundary_marker, mountain).
narrative_ontology:human_readable(third_amendment__military_civil_boundary_marker, "Third Amendment: Military-Civil Boundary as Deep Rule").
narrative_ontology:topic_domain(third_amendment__military_civil_boundary_marker, "constitutional_doctrine/civil_military_relations").

domain_priors:emerges_naturally(third_amendment__military_civil_boundary_marker).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(third_amendment__military_civil_boundary_marker, '2f245a99-7ed0-4caf-9ef2-24f5a77a5d83').
narrative_ontology:cs_kernel_codification('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', fixed_text).
narrative_ontology:cs_authority_grounding('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', lineage).
narrative_ontology:cs_interpretation_layer_present('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83').
narrative_ontology:cs_reading_relation('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', third_amendment__dormant_by_success_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', third_amendment__privacy_penumbra_contribution, coexists_with).
narrative_ontology:cs_axiom('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', foundational, military_power_structurally_subordinate_to_civil).
narrative_ontology:cs_axiom_status(military_power_structurally_subordinate_to_civil, holdable).
narrative_ontology:cs_axiom_grounding('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', military_power_structurally_subordinate_to_civil, deontological).
narrative_ontology:cs_axiom('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', foundational, household_inviolable_from_military_occupation).
narrative_ontology:cs_axiom_status(household_inviolable_from_military_occupation, holdable).
narrative_ontology:cs_axiom_grounding('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', household_inviolable_from_military_occupation, conventional).
narrative_ontology:cs_reference_frame('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', civil_supremacy_constitutional_order).
narrative_ontology:cs_drift_state('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f245a99-7ed0-4caf-9ef2-24f5a77a5d83', '').
narrative_ontology:cs_kernel_id(third_amendment__military_civil_boundary_marker, third_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(third_amendment__military_civil_boundary_marker, civil_supremacy_doctrine).
narrative_ontology:constraint_victim(third_amendment__military_civil_boundary_marker, militarized_administration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OCCUPIED HOUSEHOLDER (MOUNTAIN) — A civilian compelled to quarter soldiers faces a prohibition so absolute it admits no negotiation, no cost-benefit analysis, no exit. The rule is immutable within the constitutional order: housing is inviolable. The householder cannot exit this constraint; it exists to prevent their exit from freedom.
constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CIVIL STATE (MOUNTAIN) — The constitutional order has bound itself: the state cannot requisition private households for military quarters. This is not a policy choice or incentive structure. It is a structural limit on state power — a boundary that civil society has marked as non-negotiable. The state's power is reduced in a way that cannot be overcome by subsequent legislation.
constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MILITARY ADMINISTRATION (SNARE) — From the standpoint of an administration seeking to quarter troops efficiently, the Third Amendment is extractive: it blocks a low-cost logistics solution and forces expensive barracks construction. The constraint suppresses the military's preferred option absolutely. Yet suppression alone is not sufficient — the constraint also forecloses any alternative framing (private housing as 'temporary resource sharing,' quartering as 'civic duty'). The military must absorb the cost; civilian refusal is legally protected, not a negotiation.
constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, snare,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — The deep rule is visible: military power subordinate to civil society via an inviolable household boundary. This is a structural feature of constitutional democracy, not a contingent policy. The amendment embeds a principle about what cannot be negotiated even in emergencies. Accessed from the civilizational, universal scope, the constraint appears as a foundational axiom of the constitutional order.
constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(third_amendment__military_civil_boundary_marker_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(third_amendment__military_civil_boundary_marker, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, ExtMetricName, E),
    domain_priors:suppression_score(third_amendment__military_civil_boundary_marker, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(third_amendment__military_civil_boundary_marker),
    narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(third_amendment__military_civil_boundary_marker, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(third_amendment__military_civil_boundary_marker_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint imposes a cost on the military (barracks construction) but extracts nothing from civil society. Civil society does not pay to maintain the boundary; the boundary exists as a prohibition, not as a coordination mechanism requiring ongoing compliance costs from civilians. The military's cost is not extraction from civil society but rather a structural feature of the constitutional order. Suppression (0.02): Minimal. The constraint is expressed as a clear rule ('No Soldier shall... be quartered'). There is no ambiguity about what is forbidden. No suppression machinery is needed — the rule is self-executing through the clarity of the boundary. Theater ratio (0.15): Minimal. The constraint has negligible performative content. Either soldiers are quartered in private homes (violating the rule) or they are not (complying with it). There is no room for ritual compliance or strategic ambiguity. The measurement of compliance is straightforward: count soldiers in private homes (should be zero in peacetime). The minimal theater reflects that the constraint operates as a structural prohibition, not as a policy framework requiring interpretation or enforcement discretion. These metrics support the mountain classification: low extractiveness, low suppression, low theater, with accessibility_collapse (0.92) and resistance (0.08) indicating that the constraint is perceived as immutable and nearly impossible to overcome even if one wished to.
 *
 * PERSPECTIVAL GAP:
 *   The occupied householder sees an absolute protection (mountain): their home cannot be conscripted. The civil state sees a structural limit it has bound itself with (mountain): it has committed to bearing the cost of military housing. The military administration sees an absolute extraction in the opposite direction (snare): it cannot use the lowest-cost logistics option and must absorb the cost of barracks. The analytical observer sees the deep rule (mountain): civil power maintains supremacy by marking the household as a boundary military occupation cannot cross. All perspectives that attend to the deep principle converge on mountain. The snare perspective (military administration) is the dissenting view, but even the snare classification reflects the constraint's absolute force — it is extractive precisely because it is immutable. The perspectival gap is minimal for this constraint; the mountain classification is robust across power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the constraint. The beneficiary (civil supremacy doctrine) receives the benefit of having a clear, inviolable boundary established in constitutional form; d ≈ 0.05 (full beneficiary). The military administration is the victim; it bears the cost of expensive barracks construction and loses the logistics option of billeting; d ≈ 0.95 (full target). The occupied householder is a victim in principle but protected by the constraint, so their position is structurally trapped but the constraint works in their favor; d ≈ 0.05 (the constraint extracts nothing from them). Directionality reflects the asymmetry: beneficiaries are doctrine and civil order; victims are any administration seeking militarized extraction. No overrides are needed — the structural derivation is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face mandatrophy. The mountain classification is fully warranted and stable across all perspectives. No agent perceives the constraint as coordination (Rope) that might be confused with extraction (Snare). The snare perspective from the military is a dissenting view about whether the cost is fair, not a claim that the constraint is a coordination mechanism. The constraint has no extractive benefit-sharing dimension — it is pure prohibition. No mandatrophy resolution is needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_dormancy_vs_force,
    'Is the Third Amendment''s force constitutive (it structures the possibility of civil-military separation) or merely conditional (it would enforce only if quartering were attempted)?',
    'Distinguish constitutive force from enforcement triggers: the amendment''s statement of the boundary itself (civil supremacy doctrine) versus the enforcement mechanism (prohibition on quartering). Historical analysis of constitutional drafting intent and post-ratification doctrine on military-civil boundaries.',
    'If constitutive: the reading frames the amendment as marking a deep structural principle (this reading). If merely conditional: the dormant reading (sibling) becomes plausible — the amendment''s force is its non-activation, its success measured by the absence of quartering cases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dormancy_vs_force, conceptual, 'Whether Third Amendment force is constitutive or conditional on quartering attempts').

omega_variable(
    household_synecdoche_scope,
    'How far does the ''household as synecdoche for governance'' interpretation extend? Does it constrain only military billeting or implicate broader military authority over domestic spaces?',
    'Textual analysis of ''house'' scope in the amendment; historical judicial construction in contexts beyond direct quartering (e.g., military police jurisdiction, eminent domain for bases, domestic surveillance); comparison with Fourth Amendment home protection doctrine.',
    'If narrow: the constraint is about logistics of quartering only (Rope or Tangled Rope under militarized emergency). If broad: the constraint marks a deep principle about military-free zones in civil space (Mountain, as this reading holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(household_synecdoche_scope, conceptual, 'Scope of household as synecdoche: logistics vs. deep governance principle').

omega_variable(
    false_summit_beneficiary_presence,
    'Civil supremacy doctrine is declared as beneficiary. Is this a genuine natural law (the boundary is inherent to constitutional democracy) or a constructed constraint (the boundary exists because beneficiaries successfully imposed it and maintain it through doctrine)?',
    'Comparative constitutional analysis: do all democracies derive this boundary from natural law, or do some derive it contingently? Historical analysis of constitutional debates: was the boundary discovered or invented? Post-amendment evolution: is the boundary stable because of doctrine, or because no one has successfully challenged it?',
    'If natural law: the mountain classification is warranted. If constructed: the engine''s false-summit detector will flag this constraint as a naturalized institutional arrangement rather than an immutable principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_presence, conceptual, 'Whether military-civil boundary is natural law or constructed doctrine').

omega_variable(
    amendment_text_vs_doctrine_gap,
    'The amendment text is minimal (''No Soldier shall, in time of peace, be quartered in any house''). How much of the ''deep rule'' reading derives from the text itself versus from post-ratification doctrine claiming a broader principle?',
    'Strict textual analysis: what the amendment literally says versus what jurists and legal theorists have claimed it means. Historical doctrine evolution: trace how courts and commentators extended quartering prohibition to the synecdoche interpretation.',
    'If doctrine-heavy: the reading is vulnerable to the charge that it projects back onto the text a meaning the drafters may not have intended. If text-grounded: the reading is more robust. This affects the confidence level for the axiom status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_text_vs_doctrine_gap, empirical, 'Gap between amendment text and doctrine''s deep-rule interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(third_amendment__military_civil_boundary_marker, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ta3_mcbm_tr_t0, third_amendment__military_civil_boundary_marker, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ta3_mcbm_tr_t125, third_amendment__military_civil_boundary_marker, theater_ratio, 125, 0.15).
narrative_ontology:measurement(ta3_mcbm_tr_t250, third_amendment__military_civil_boundary_marker, theater_ratio, 250, 0.15).

% Extraction over time
narrative_ontology:measurement(ta3_mcbm_be_t0, third_amendment__military_civil_boundary_marker, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ta3_mcbm_be_t125, third_amendment__military_civil_boundary_marker, base_extractiveness, 125, 0.08).
narrative_ontology:measurement(ta3_mcbm_be_t250, third_amendment__military_civil_boundary_marker, base_extractiveness, 250, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(third_amendment__military_civil_boundary_marker, enforcement_mechanism).
narrative_ontology:affects_constraint(third_amendment__military_civil_boundary_marker, third_amendment__dormant_by_success_reading).
narrative_ontology:affects_constraint(third_amendment__military_civil_boundary_marker, third_amendment__privacy_penumbra_contribution).

% DUAL FORMULATION NOTE:
% The Third Amendment kernel has been decomposed into three distinct constraint stories, each representing a different reading of the amendment's significance. This story (military_civil_boundary_marker) emphasizes the deep structural principle; the sibling stories emphasize dormancy (success measured by absence) and penumbral contribution (work done through implication). Each story has its own ε, perspectives, and beneficiary/victim declarations. They are linked as a constraint family through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
