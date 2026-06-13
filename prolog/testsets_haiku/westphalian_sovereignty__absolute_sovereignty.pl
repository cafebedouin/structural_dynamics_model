% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Westphalian Sovereignty Doctrine
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The absolute sovereignty reading of Westphalian doctrine establishes that
 *   states possess unconditional authority over domestic affairs and that
 *   external interference — whether military, legal, or coercive — is
 *   categorically illegitimate under international law. This reading treats
 *   sovereignty as an irreducible right: states cannot be subject to external
 *   judgment or intervention based on internal governance. However, the
 *   constraint exhibits high extractiveness and substantial suppression
 *   because the doctrine operates asymmetrically (powerful states routinely
 *   violate it; weak states cannot) and serves to shield authoritarian
 *   regimes from accountability. Domestic populations under systematic
 *   repression become victims of a constraint that privileges state authority
 *   over human rights enforcement. The absolute sovereignty reading competes
 *   with two sibling readings: conditional sovereignty (which grounds
 *   intervention rights in human rights violations) and graduated sovereignty
 *   (which permits intervention or authority scaling based on state capacity
 *   and legitimacy). This story models the absolute reading alone, as a clean
 *   ε-invariant constraint; sibling readings are separate constraint stories
 *   in the family.
 *
 * KEY AGENTS:
 *   - Authoritarian regimes: Primary beneficiaries; invoke absolute sovereignty to shield repression from international legal challenge and intervention. They both benefit from and actively enforce this reading.
 *   - Liberal democratic states: Structural payers; constrained by the same non-interference principle they depend on for their own protection. They face moral compromise when confronting atrocities they cannot legally address.
 *   - Domestic populations under repression: Victims; lack recourse to international legal protection or intervention due to the sovereignty shield.
 *   - International legal establishment: Agenda-setters; operationalize and legitimize the doctrine through UN structures, treaty frameworks, and legal precedent.
 *   - Humanitarian organizations and diaspora communities: Payers; limited in advocacy effectiveness and intervention capability by the constraint.
 *   - Regional powers: Beneficiaries; use the doctrine to shield themselves and their client states from external pressure or intervention.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Westphalian Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '94ef335e-23fa-43c1-8a72-71ef5394c158').
narrative_ontology:cs_kernel_codification('94ef335e-23fa-43c1-8a72-71ef5394c158', formalized).
narrative_ontology:cs_authority_grounding('94ef335e-23fa-43c1-8a72-71ef5394c158', extraction).
narrative_ontology:cs_interpretation_layer_present('94ef335e-23fa-43c1-8a72-71ef5394c158').
narrative_ontology:cs_reading_relation('94ef335e-23fa-43c1-8a72-71ef5394c158', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('94ef335e-23fa-43c1-8a72-71ef5394c158', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('94ef335e-23fa-43c1-8a72-71ef5394c158', foundational, sovereignty_unconditional_immunity).
narrative_ontology:cs_axiom_status(sovereignty_unconditional_immunity, holdable).
narrative_ontology:cs_axiom_grounding('94ef335e-23fa-43c1-8a72-71ef5394c158', sovereignty_unconditional_immunity, deontological).
narrative_ontology:cs_axiom('94ef335e-23fa-43c1-8a72-71ef5394c158', foundational, non_interference_absolute_principle).
narrative_ontology:cs_axiom_status(non_interference_absolute_principle, holdable).
narrative_ontology:cs_axiom_grounding('94ef335e-23fa-43c1-8a72-71ef5394c158', non_interference_absolute_principle, conventional).
narrative_ontology:cs_reference_frame('94ef335e-23fa-43c1-8a72-71ef5394c158', absolute_sovereignty_framework).
narrative_ontology:cs_drift_state('94ef335e-23fa-43c1-8a72-71ef5394c158', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('94ef335e-23fa-43c1-8a72-71ef5394c158', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, state_system_as_such).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, cross_border_diaspora_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.52) reflects the constraint's core asymmetry: it provides unconditional protection to state sovereignty regardless of internal legitimacy, which authoritarian regimes exploit to shield repression. The doctrine solves a genuine coordination problem (preventing ideological intervention), but the measured extraction represents the disproportionate benefit to illegitimate states and the cost to victims lacking recourse. Suppression is high (0.71) because maintaining the constraint requires active enforcement: states must prevent humanitarian intervention, block international legal processes (through veto, withdrawal from treaties, or non-compliance), and delegitimize external pressure as 'interference.' Theater is moderate-high (0.42) because significant effort goes into performing the principle's universality and neutrality while selectively enforcing it (powerful states breach it regularly; weak states cannot). The temporal measurement series documents the constraint's evolution: initially serving its coordination function well (1648-1815), but increasingly extractive as the doctrine became weaponized to shield mass atrocities (1945 onward, correlating with genocide documentation and human rights documentation systems). The suppression requirement rises as victims and advocates press for conditional intervention rights, requiring stronger defensive effort from regime-beneficiaries. Theater rises as states perform non-interference while violating it (NATO intervention in Kosovo, China's non-interference in Myanmar's genocide, etc.), making the constraint increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The authoritarian regime seat and the victim seat experience radically different constraint types from the same rule. From the regime's position: absolute sovereignty is genuine coordination that protects their legitimacy, which they depend on for internal control. From the victim's position: absolute sovereignty is pure extraction—a shield that prevents anyone from helping. The international legal establishment and liberal democratic states occupy intermediate positions: they benefit from sovereignty protection but experience the constraint as limiting their ability to act on their stated human rights commitments, creating internal cognitive dissonance that manifests as inconsistent enforcement. The engine will compute different types per seat based on these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes: d ≈ 0.1 (beneficiary end; they collect immunity and control the constraint's application). Liberal democracies: d ≈ 0.4-0.5 (near-symmetric; they depend on the principle for their own protection but bear costs from constrained intervention capacity). Domestic populations: d ≈ 0.95 (victim end; they are trapped, powerless, with zero voice in the framework that denies them protection). Regional powers: d ≈ 0.15 (beneficiary end; they use it to shield their sphere). Humanitarian organizations: d ≈ 0.65 (payer end; they pay in constrained advocacy and effectiveness). The derivation chain produces this directionality from beneficiary/victim declarations and exit options: regimes are declared beneficiaries with high power and arbitrage exit (they can choose to invoke or ignore the doctrine); populations are victims with powerless status and trapped exit (they have no choice). The overrides are not necessary; the derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is central to this constraint's classification. The founding problem—preventing ideological intervention and religious war—is contested as to whether it remains 'live.' If the founding problem is dead (systematic atrocities are now a greater threat than ideological intervention), the constraint becomes a zombie: it persists because regimes depend on it, not because anyone is fixing the original problem. The measured extractiveness (0.52) and rising theater (0.08 to 0.42 over 376 years) support the zombie diagnosis: the constraint is performing its neutrality while being applied asymmetrically. The tangled-rope classification captures both the real coordination function (non-interference did solve something genuine) and the extractive operation (it now shields atrocities). If the founding problem is dead and the constraint persists mainly because authoritarian regimes and the powerful states that tolerate them benefit from it, this is a mandatrophy case. The engine should flag the (founding_problem_status=dead, disappearance_verdict=world_rearranges) mismatch for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (preventing ideological/religious intervention) been solved, or has it been replaced by a different problem (preventing atrocities) such that the constraint''s mandate is obsolete?',
    'Historical assessment of intervention motives over time (are post-WWII interventions driven by ideological expansion or by humanitarian/human rights concerns?), and counterfactual analysis: if states could intervene on humanitarian grounds, would ideological wars increase? If not, the founding problem is dead.',
    'If the founding problem is dead and the constraint persists only because regimes depend on it, this is mandatrophy: the constraint shifts from rope toward piton-like performance. The classification would remain tangled_rope (asymmetric extraction + real coordination) but the omega documents that the coordination function has atrophied relative to the extraction function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Whether the founding problem (preventing ideological intervention) remains live or has become obsolete.').

omega_variable(
    symmetry_vs_asymmetric_application,
    'Is the measured extraction (0.52) a property of the absolute sovereignty principle itself, or of its asymmetric application (powerful states routinely violate it without consequences; weak states cannot)?',
    'Test whether symmetric application would reduce extractiveness: if all states faced equal enforcement of non-interference, would the principle become genuine rope? This requires counterfactual (what if major powers faced intervention threats for sovereignty violations?). Alternatively: examine whether weaker states experience lower extraction when they apply absolute sovereignty symmetrically among themselves.',
    'If the extraction is structural (inherent to the principle), the constraint is inherently tangled_rope favoring powerful actors. If the extraction is entirely from asymmetric application, a hypothetical symmetric enforcement regime might be genuine rope, and current extractiveness reflects power asymmetry rather than the principle itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_vs_asymmetric_application, empirical, 'Whether extraction is structural to absolute sovereignty or flows from asymmetric enforcement.').

omega_variable(
    coordination_vs_cover_story,
    'Is non-interference a genuine coordination solution that states actually want and depend on, or is it a cover story that powerful states use to shield their actions while weak states must comply?',
    'Examine state behavior in closed diplomatic channels versus public statements. Do states want non-interference for themselves (seeking immunity for their actions) or do they want it universally? If powerful states consistently violate it and weak states protest, the principle is a cover story. If all states genuinely maintain non-interference practices, the principle is real coordination.',
    'If non-interference is genuine coordination, the constraint is structurally tangled_rope (real coordination + some extraction). If it is primarily a cover story, it edges toward snare (the coordination function is performative; the real function is shielding powerful states from accountability). The engine will measure this through behavior analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_cover_story, empirical, 'Whether absolute sovereignty is genuine coordination or primarily a cover story for power asymmetry.').

omega_variable(
    universality_vs_selective_enforcement,
    'Is absolute sovereignty universally applicable (all states equally protected) or selectively enforced (powerful states routinely violate it with impunity, weak states cannot)?',
    'Empirical audit: track all major military interventions, sanctions, and coercive measures since 1945. Classify each by intervening state''s power and target state''s power. If power asymmetry predicts enforcement patterns (powerful states intervene freely; weak states intervene and face consequences), the principle is selectively enforced. If enforcement is uniform regardless of power, the principle is universal.',
    'Selective enforcement amplifies the extractiveness and reduces the principle''s legitimacy as genuine coordination. It supports reclassification toward snare (coercive, benefiting powerful states) or piton (performative, maintained by powerful states while they violate it). A universal enforcement pattern would support tangled_rope classification (real coordination with legitimate asymmetries) or even rope (genuine mutual non-interference).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_vs_selective_enforcement, empirical, 'Whether absolute sovereignty is applied uniformly or selectively enforced based on state power.').

omega_variable(
    victims_boundary_ambiguity,
    'Who counts as a victim of this constraint? Only domestic populations under repressive regimes, or also humanitarian advocates, diaspora communities, and liberal democracies constrained in their values-based foreign policy?',
    'Boundary definition from evidence: whose material welfare or freedom of action is restricted by this constraint? The direct victims are clearly populations under repression. Secondary victims include diaspora communities separated from aid/intervention. Tertiary payers include liberal democracies forced to tolerate atrocities they oppose. The boundary of who is harmed affects the measured extractiveness and the constraint''s classification.',
    'If victims include only domestic populations, extractiveness measures their trapped condition. If victims expand to include advocates and diaspora, extractiveness rises (more people pay costs). If liberal democracies are victims of their own constraint, the payer set expands dramatically, potentially raising extractiveness further. The author declares beneficiaries/victims; the engine computes directionality from that. This omega documents the boundary uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victims_boundary_ambiguity, conceptual, 'The scope of who is victimized by the constraint: direct victims, secondary victims, or inclusive of systemic constraints on would-be interveners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1648, 0.08).
narrative_ontology:measurement(west_tr_t1815, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1815, 0.12).
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(west_tr_t1975, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(west_tr_t2000, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1648, 0.35).
narrative_ontology:measurement(west_be_t1815, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1815, 0.38).
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.48).
narrative_ontology:measurement(west_be_t1975, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1975, 0.51).
narrative_ontology:measurement(west_be_t2000, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1648, 0.45).
narrative_ontology:measurement(west_su_t1815, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1815, 0.5).
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.62).
narrative_ontology:measurement(west_su_t1975, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement(west_su_t2000, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__absolute_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, responsibility_to_protect_principle).

% DUAL FORMULATION NOTE:
% The westphalian_sovereignty kernel decomposes into three structurally distinct constraint stories: absolute_sovereignty (this file, no conditions on state authority), conditional_sovereignty (intervention legitimate when human rights thresholds violated), and graduated_sovereignty (sovereignty scope varies with state legitimacy and capacity). Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different classifications. All three coexist in international law and practice, held by different state factions and legal traditions. They are linked via network.affects_constraints to enable contamination analysis: if one reading's legitimacy erodes (e.g., conditional_sovereignty gains acceptance), it exerts pressure on this reading's persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
