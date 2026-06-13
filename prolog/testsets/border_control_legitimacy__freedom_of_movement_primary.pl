% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Closure as Violation of Freedom of Movement
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint captures the freedom-of-movement-primary reading of the
 *   contested border_control_legitimacy kernel: the claim that freedom of
 *   movement is a fundamental human right and that territorial sovereignty
 *   does NOT entail absolute border closure authority. Under this reading,
 *   state capacity to exclude non-citizens is delegitimized. Displaced
 *   persons, asylum seekers, economic migrants, and stateless individuals are
 *   identified as victims trapped by border enforcement machinery. The
 *   constraint appears as a snare from this reading's perspective: borders
 *   extract exclusion from movement-seeking populations through legal
 *   prohibition, enforcement, and legitimized state violence, with no genuine
 *   coordination function justifying the extraction. The measurement series
 *   track the constraint's intensification over 50 time units as enforcement
 *   machinery hardens (suppression rising) and states increase rhetorical
 *   justification (theater rising) while the core extraction (denial of
 *   movement) plateaus at high levels. This reading structurally contests
 *   sovereignty_primary (which grounds legitimacy in absolute state
 *   discretion) and coexists with jurisdictional_sovereignty (which attempts
 *   to balance state authority with protection obligations).
 *
 * KEY AGENTS:
 *   - territorial_state_apparatus (institutional, agenda_setter): Administers border closure; justifies it as necessary to sovereignty and security; maintains enforcement machinery through law, detention, deportation.
 *   - displaced_persons_seeking_work (powerless, payer, trapped): Seek employment but face absolute legal prohibition; bear extraction through exclusion and immobility.
 *   - asylum_seekers_fleeing_persecution (powerless, payer, identity_locked): Flee violence but face borders; locked in their asylum-seeking identity by the persecution they fled; extraction is risked survival.
 *   - economic_migrants (powerless, payer, trapped): Seek wage improvements but are legally prohibited from crossing to higher-wage labor markets; extraction is prevented productivity.
 *   - stateless_individuals (powerless, payer, excluded, trapped): Possess no citizenship; global borders trap them in legal void with no rights anywhere; they are excluded from border-setting conversations by definition.
 *   - separated_families (powerless, payer, identity_locked): Divided by borders; reunion requires state permission; identity as family members overridden by non-citizen status.
 *   - international_human_rights_bodies (institutional, observer, analytical): Produce normative arguments that freedom of movement is fundamental; hold the freedom_of_movement_primary reading; minimal enforcement capacity.
 *   - national_security_advocates (institutional, excluded, analytical): Argue border control is necessary for security; structurally excluded from freedom_of_movement_primary framing; would dispute the axiom itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.81).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.87).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Closure as Violation of Freedom of Movement").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '821a5ee5-bbbc-427d-8626-e869b7457e08').
narrative_ontology:cs_kernel_codification('821a5ee5-bbbc-427d-8626-e869b7457e08', distributed).
narrative_ontology:cs_authority_grounding('821a5ee5-bbbc-427d-8626-e869b7457e08', diffuse_epistemic).
narrative_ontology:cs_reading_relation('821a5ee5-bbbc-427d-8626-e869b7457e08', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('821a5ee5-bbbc-427d-8626-e869b7457e08', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('821a5ee5-bbbc-427d-8626-e869b7457e08', foundational, freedom_of_movement_fundamental_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('821a5ee5-bbbc-427d-8626-e869b7457e08', freedom_of_movement_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('821a5ee5-bbbc-427d-8626-e869b7457e08', foundational, territorial_sovereignty_does_not_entail_closure_authority).
narrative_ontology:cs_axiom_status(territorial_sovereignty_does_not_entail_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('821a5ee5-bbbc-427d-8626-e869b7457e08', territorial_sovereignty_does_not_entail_closure_authority, deontological).
narrative_ontology:cs_reference_frame('821a5ee5-bbbc-427d-8626-e869b7457e08', universal_human_rights_framework).
narrative_ontology:cs_drift_state('821a5ee5-bbbc-427d-8626-e869b7457e08', contemporary_border_enforcement_intensification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('821a5ee5-bbbc-427d-8626-e869b7457e08', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_persons_seeking_work).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers_fleeing_persecution).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, economic_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, stateless_individuals).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, separated_families).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the constraint denies displaced persons access to labor markets, family, and safety without state permission—the extraction is fundamental, not marginal. Suppression is higher still (0.87) because the constraint's persistence depends on active enforcement: legal frameworks, detention, deportation, militarized borders. Alternatives (illegal border crossing, visa overstay, family separation) exist but are dangerous and legally prohibited, leaving no safe exit. Theater_ratio is elevated (0.62) because states increasingly justify borders through security narratives and economic arguments, which this reading frames as cover stories for exclusion—the performance is expanding as international challenge to border legitimacy intensifies. Accessibility_collapse is moderate-low (0.41) because alternatives to borders remain conceptually and organizationally salient (labor mobility programs, refugee resettlement, open-border experiments); they are legally suppressed, not intellectually suppressed. Resistance is high (0.78) because asylum-seeking, migrant, and human rights communities actively contest border legitimacy; this reading amplifies that resistance by framing border closure as rights violation rather than policy choice. The measurement series show extraction plateauing once enforced (rising sharply 0-24, then flat 24-50), suppression continuously intensifying (rising throughout, as enforcement machinery is hardened in response to migration pressure), and theater rising (states investing in legitimacy rhetoric as their authority is challenged). This profile is consistent with snare: genuine extraction for identified victims, active enforcement to sustain it, increasing rhetorical performance to defend legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is fundamental: from the territorial state apparatus's position, border control is a coordination function (organizing who belongs, who may access resources, protecting citizens). From the displaced person's position, the same structure operates as extractive snare (denied movement, denied labor, denied family). The state sees a functional ordering; the payer sees oppression. The engine computes this divergence from structural data: the state sits at d~0.0 (benefits from control, organized power, analytical exit); the displaced person sits at d~1.0 (targeted for extraction, powerless, trapped or identity-locked exit). These are not two perspectives on one truth; they are incommensurable positions in a contested kernel. The state's position is coded in sovereignty_primary; this position is coded in freedom_of_movement_primary. The divergence is not a gap to be bridged—it is the phenomenon itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial state apparatus: d near 0.0 (full beneficiary). The state controls borders, extracts compliance, and uses closure to organize its territory. It has institutional power, unlimited time horizon (generational permanence of the state), and analytical exit (the state can redraw borders conceptually; it is not subject to the constraint it administers). Benefits flow to the state in the form of control, legitimacy claims, and resource prioritization for citizens. Displaced persons: d = 1.0 (full targets). They are explicitly identified as victims in base_properties.victims. They bear extraction through prohibition, enforcement, and consequences (deportation, family separation, forced immobility). Power is powerless; exit options are trapped or identity_locked (asylum seekers cannot exit their asylum-seeking identity; economic migrants cannot exit their labor-seeking identity). The constraint is specifically designed to target them. Asylum seekers specifically: d = 1.0 + identity_locked multiplier (unable to exit their persecution status, doubly bound). Their very identity (fleeing person, persecution victim) is what makes them migration-vulnerable; they cannot un-flee. Stateless individuals: d = 1.0 + global scope amplification (every state closes borders to them; there is no alternative jurisdiction). They are trapped by the logic of the state system itself—each state closure reinforces their statelessness. No directionality overrides are needed; the structural data derives the true d values cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution (founding_problem_status x disappearance_verdict mismatch): Under sovereignty_primary, the founding problem is 'states need border closure for security and social cohesion' and that problem is asserted as live. Under freedom_of_movement_primary, the founding problem is reframed as 'human beings lack equal access to opportunity and safety due to territorial exclusion' and that problem is asserted as contested (human rights bodies say it is live; states say it is not live). The disappearance_verdict is world_rearranges (borders are not natural law; if they vanished, states would be forced to compete via rights/economics). The mismatch: sovereignty_primary asserts founding_problem_status=live but under this reading that problem (state security necessity) is itself contested, not inherent. Freedom_of_movement_primary inverts the problem and asserts status=contested, reflecting genuine disagreement. This constraint does NOT show mandatrophy (the constraint persisting past the death of its founding problem) because the founding problem is explicitly contested—states claim it remains live for security reasons; human rights frameworks deny it. The constraint persists because it is actively defended by states with institutional power, not because anyone has forgotten why it was built. Mandatrophy would only apply if states had abandoned the security justification while maintaining the closure machinery out of inertia; here the justification is actively reasserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_axiom_foreclosure,
    'Can a single coherent legal and political framework hold both the freedom-of-movement-primary axiom (freedom of movement is a fundamental human right) AND the sovereignty-primary axiom (border closure is constitutive of statehood) without logical contradiction?',
    'Comparative constitutional law analysis: examine whether any state has successfully grounded legitimacy in both freedom of movement AND absolute border closure authority in the same constitutional order. Examine whether the axioms must be held by different factions (foreclosure across factions) or whether they truly coexist within one framework (coexistence acknowledged as internal tension).',
    'If the axioms truly foreclose each other, the relation shifts from coexists_with to forecloses, and the constraint should be reformulated as a pure negation of sovereignty_primary. If they coexist in tension, the current coexists_with relation stands but requires higher confidence in the theoretical distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_axiom_foreclosure, conceptual, 'Whether the two foundational axioms of competing kernel readings logically foreclose or coexist.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.87) structural (legal barriers, enforcement machinery, impossibility of exit) or internalized (migrants have internalized the belief that borders are legitimate and unchangeable)?',
    'Ethnographic study of displaced populations in border regions and diaspora contexts: track whether suppression persists after exposure to alternative framings (human rights arguments, evidence of successful movements of people). If suppression is partly internalized, it should decay when the frame breaks; if purely structural, it persists despite frame change.',
    'If suppression is substantially internalized, the constraint''s effective extraction is higher than the structural measure suggests — the payer carries suppression with them and may not recognize themselves as payers. This would support deeper reformulation of the constraint from snare to entrenched psychological capture. If purely structural, the high suppression reflects law and enforcement architecture only, and legal change alone could dissolve it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism: structural (law/enforcement) vs. internalized (belief internalization).').

omega_variable(
    fundamental_right_vs_policy_preference,
    'Is the freedom-of-movement axiom grounded in a deontological human right (people have inherent dignity that entails movement) or in instrumental policy preference (movement produces better outcomes)?',
    'Philosophical argumentation and precedent analysis: examine the grounding structure of freedom-of-movement claims in human rights documents (UDHR, ICCPR, regional conventions). Determine whether the grounding is deontological (inherent right) or instrumental (serves flourishing). This affects whether challenges to the axiom''s empirical evidence can refute it.',
    'Deontological grounding is foreclosed-resistant: empirical arguments (e.g., security concerns, welfare distribution problems) cannot refute an inherent right claim. Instrumental grounding is vulnerable to empirical refutation: if evidence shows that open borders produce measurable harms exceeding benefits, the axiom weakens. Clarifying the grounding type informs which evidence matters for the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamental_right_vs_policy_preference, conceptual, 'Epistemic grounding of the freedom-of-movement axiom.').

omega_variable(
    theater_ratio_performance_vs_enforcement,
    'Does the rising theater_ratio (0.48 to 0.62 over the interval) indicate increased performative justification (states rhetorically defending borders while enforcement capacity remains stable) or increased performative enforcement (states performing closure through theater while actual exclusion weakens)?',
    'Time-series analysis of state border rhetoric vs. enforcement capacity: track political speech justifying borders, count actual deportations/detention, measure border crossing rates, examine enforcement budget. Determine whether rhetoric is compensating for erosion of state capacity or whether both rhetoric and capacity are rising together.',
    'If performance compensates for eroding capacity, the theater_ratio rise indicates the constraint is becoming piton-like (maintained by theater, not by structural effectiveness). If both rhetoric and capacity rise, the theater increase reflects higher stakes and amplified legitimacy struggle, keeping the snare classification. The piton possibility would shift prognosis and suggest the constraint is more vulnerable to collapse than current metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_performance_vs_enforcement, empirical, 'Interpretation of rising theater_ratio: rhetorical compensation vs. amplified enforcement.').

omega_variable(
    committer_kernel_contest_readings,
    'This constraint is one reading (freedom_of_movement_primary) of the kernel border_control_legitimacy. The sibling readings (sovereignty_primary and jurisdictional_sovereignty) represent competing framings of state authority and its legitimacy. What is the structural nature of the contest?',
    'Genealogical and discourse analysis: trace the historical development of border control doctrine in international law, state practice, and human rights law. Identify the moments where each reading emerged as a coherent position and document the competing authority structures grounding each (state sovereignty doctrine vs. human rights universalism vs. jurisdictional balancing). Map where each reading is institutionalized (which states, which courts, which international bodies).',
    'This omega documents the kernel contest itself as an irreducible structural fact about this constraint. Clarifying the contest structure informs which evidence and arguments pertain to which reading, prevents conflation of readings, and grounds the cs_structure.reading_relations declarations. The contest is not a flaw in the analysis; it is the phenomenon the constraint captures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_contest_readings, conceptual, 'The kernel contest structure: three readings of border control legitimacy with incommensurable authority grounds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 8, 0.52).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 16, 0.56).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 24, 0.59).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 32, 0.61).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 40, 0.62).
narrative_ontology:measurement(bord_tr_t50, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 8, 0.74).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 32, 0.81).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(bord_be_t50, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.79).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 8, 0.81).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 16, 0.83).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 32, 0.86).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(bord_su_t50, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 50, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__freedom_of_movement_primary, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_control_legitimacy kernel. It asserts that freedom of movement is a fundamental human right and that territorial sovereignty does NOT entail border closure authority. Sibling readings instantiate alternative authority grounds: sovereignty_primary grounds legitimacy in state discretion; jurisdictional_sovereignty attempts to balance state authority with protection obligations. The three readings are linked via network.affects_constraints and coexist as live positions in international law and political philosophy. Each reading instantiates a different ε_value and victim/beneficiary structure. Collectively, the three constraints map the border_control_legitimacy kernel's contested structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
