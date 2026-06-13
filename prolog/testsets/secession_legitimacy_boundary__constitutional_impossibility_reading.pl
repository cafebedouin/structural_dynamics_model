% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the constitutional_impossibility_reading of
 *   a contested kernel: the secession_legitimacy_boundary. The reading
 *   asserts that the Constitution establishes federal supremacy and
 *   territorial indivisibility as supreme law. Unilateral secession is not
 *   merely impermissible as policy — it is categorically illegitimate as a
 *   legal claim. Only constitutional amendment, controlled by processes the
 *   federal framework itself defines, can alter this boundary. This reading
 *   frames the Constitution as the discovered source of federal legitimacy,
 *   not as a constructed instrument that benefits federal authority. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as a mountain
 *   (natural principle of federalism) while the authored metrics describe
 *   measurable extraction and suppression — the engine will detect whether
 *   this reading credibly sustains the mountain classification or whether
 *   rising extractiveness signals false summitry.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda-setter; administers the constitutional rule; claims it is supremacy doctrine, not extractive constraint.
 *   - separatist_provincial_movement: Payer; constrained by the rule; identity-locked (self-determination is fused with separatist identity); exit requires constitutional amendment.
 *   - non_separatist_provincial_residents: Beneficiary; protected by the rule from unilateral majority secession; depend on federal constitutional protection.
 *   - constitutional_court: Observer; interprets the Constitution and adjudicates whether the rule holds; authority derives from the Constitution itself.
 *   - international_community: Observer; sees the rule through international law and precedent; generally recognizes state borders and reinforces the federalism reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.62).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political/constitutional").

domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '5781e8f6-09a7-45dd-bb78-1faa5a80e6ca').
narrative_ontology:cs_kernel_codification('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', formalized).
narrative_ontology:cs_authority_grounding('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', lineage).
narrative_ontology:cs_interpretation_layer_present('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca').
narrative_ontology:cs_reading_relation('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', foundational, constitutional_supremacy_doctrine).
narrative_ontology:cs_axiom_status(constitutional_supremacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', constitutional_supremacy_doctrine, deontological).
narrative_ontology:cs_axiom('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', secondary, territorial_indivisibility_principle).
narrative_ontology:cs_axiom_status(territorial_indivisibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', territorial_indivisibility_principle, instrumental).
narrative_ontology:cs_reference_frame('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', federal_constitutional_order).
narrative_ontology:cs_drift_state('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', contemporary_separatist_contestation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5781e8f6-09a7-45dd-bb78-1faa5a80e6ca', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_authority_structure).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.62 reflects the constraint's dual nature: it is claimed as natural principle (low extraction) but operates to preserve federal authority against legitimacy challenges (non-trivial extraction). Suppression of 0.71 is high because enforcing the rule against organized separatist movements requires sustained constitutional and legal action — court decisions affirming the rule, amendment processes that set the bar impossibly high, criminal prohibition of separatist advocacy in some regimes. Theater ratio of 0.28 indicates modest performative content: the constitutional text is genuinely enforced, but constitutional reaffirmation rituals (court decisions rehearsing the supremacy doctrine) are part of the enforcement machinery. Accessibility collapse of 0.87 is consistent with a mountain: once the constitutional text is understood as supreme, the alternatives (unilateral secession as a legitimate claim) collapse nearly entirely — the only remaining exit is amendment. Resistance of 0.58 is moderate: natural laws meet little resistance, but constructed doctrines always meet some; separatist movements actively contest the rule, which is why resistance is non-trivial. The measurement series show extractiveness rising from 0.48 to 0.62 over the interval, suggesting the rule is increasingly used to defend federal authority rather than to preserve a naturally emergent principle of federalism — this trajectory is precisely what FSM detects.
 *
 * PERSPECTIVAL GAP:
 *   The federal-government seat and the separatist-movement seat experience fundamentally different constraints. From the federal position, the rule is a discovered principle that defines the federation's structure — the federal government is not extracting but administering supremacy doctrine. From the separatist position, the same rule is an imposed boundary that denies the legitimacy of self-determination — the constraint operates as coercive suppression of a rival claim. The non-separatist residents sit between: they benefit from the rule's protection against unilateral majority action, but they experience it as coordination benefit, not extraction. The engine will compute per-seat classifications from the stakeholder positions and structural data; the divergence between the federal (low d, beneficiary framing) and separatist (high d, payer/target framing) positions is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government: d near 0.0 (full beneficiary; administers the rule, collects the coordination benefit of territorial unity). Separatist movement: d near 1.0 (full target; identity-locked exit; the rule denies its core legitimacy claim). Non-separatist residents: d near 0.5 (symmetric; genuine coordination benefit balanced by some constraint on provincial autonomy). The identity-lock on the separatist movement drives its high d: exit from the constraint means existential recategorization — the movement cannot leave the federation without ceasing to be a self-determination movement. This is not a choice to exit; it is a redefinition of identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live but contested: federal authority attests that the founding problem (territorial cohesion and unified coordination) is still present and the rule is still necessary; separatist movements and comparative analysts attest the founding problem is solved (provinces are capable of independent governance) or wrongly framed (the problem is federal overreach, not dissolution). The rising extractiveness over the interval (0.48 → 0.62) is a key signal: if the rule were addressing a genuine natural principle, extractiveness should be stable. The upward drift suggests the rule is increasingly deployed to defend federal authority against legitimacy challenges, which indicates the constraint is performing less like discovered natural law and more like constructed doctrine. This is exactly the condition that triggers FSM evaluation: a mountain with rising extractiveness and a declared beneficiary (federal_authority_structure) signals a false summit candidate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the constitutional rule against unilateral secession a discovered natural principle of federalism (a mountain), or a constructed doctrine that benefits the federal authority structure and happens to be written into the Constitution?',
    'Comparative analysis of federal systems that lack explicit anti-secession clauses: do they organically produce the same rule, or do they permit secession when provincial majorities vote for it? Examination of whether pre-constitutional federal theory identified anti-secession as a necessary principle, or whether it emerged as a post-hoc defense of territorial integrity.',
    'If the rule is discovered natural law, the classification holds as mountain; if it is constructed doctrine benefiting federal authority, FSM would reclassify as tangled_rope (coordination + extraction). The beneficiary declaration (federal_authority_structure) is designed to test this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the constitutional impossibility is natural to federalism or a constructed doctrine.').

omega_variable(
    reading_contest_foreclosure,
    'Does the constitutional_impossibility_reading logically foreclose the popular_sovereignty_reading and grievance_threshold_reading, or can they coexist as competing frameworks held by different parties?',
    'Examine whether a single legal/constitutional framework could simultaneously hold (a) constitutional supremacy bars unilateral secession AND (b) provincial referendum results self-legitimize secession. If holding both requires rejecting the Constitution''s authority, they foreclose each other. If holding both requires only assigning different weight to competing principles, they coexist.',
    'If they foreclose each other, this reading is a competing absolute; if they coexist, the contest is between factions with different axioms, not between logically incompatible systems. Affects the reading_relations array: forecloses vs. coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Logical relationship between this reading and sibling readings.').

omega_variable(
    suppression_as_enforcement_or_coercion,
    'Is the measured suppression (0.71) the structural cost of enforcing a constitutional rule, or does it represent coercive suppression of a legitimate self-determination claim?',
    'Distinguish enforcement cost from coercion: enforcement is the machinery required to sustain a rule (court proceedings, constitutional amendment barriers, legal prohibition); coercion is the measure applied to suppress resistance to an unjust rule. Examine whether separatist movements experience the suppression as procedural obstacle (constitutional amendment process) or as active oppression (criminal prosecution, military occupation, denial of democratic voice).',
    'If suppression is procedural enforcement cost, it is consistent with the mountain reading (structures that are natural produce resistance but do not require active coercion). If suppression is active coercion of a legitimacy claim, the constraint drifts toward snare. The measurement of 0.71 is high either way; the ambiguity is about the category, not the magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_as_enforcement_or_coercion, empirical, 'Whether suppression measures procedural enforcement or coercive denial of legitimacy.').

omega_variable(
    federal_authority_as_beneficiary,
    'Does naming ''federal_authority_structure'' as a beneficiary commit us to the claim that the constraint exists because it serves federal interests, or does it merely document that federal authority is preserved by the rule?',
    'If the constraint is genuinely a discovered natural principle (mountain), then federal authority is preserved incidentally, not beneficially — the federal structure benefits, but not because the rule was created to benefit it. The FSM check gates on beneficiary presence alone (not on motive). Document what the constraint''s actual genealogy reveals: was it adopted to preserve federal authority, or adopted as a principle of federalism and federal authority persists as a consequence?',
    'This omega is diagnostic for FSM: if beneficiary declaration triggers FSM, the constraint is reclassified as tangled_rope. The reclassification signals that a ''natural law'' framing masks constructed doctrine. The measurement series (extractiveness drifting upward over time) supports the reclassification: if the rule were natural, extractiveness would be stable; rising extractiveness suggests the rule is being increasingly used to defend federal authority against legitimacy challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_authority_as_beneficiary, conceptual, 'Whether beneficiary presence indicates the rule is constructed or merely that federal authority is preserved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.18).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four constraint stories, one for each reading. All readings share the same domain (secession legitimacy) but differ in which authority framework is treated as supreme and which actors hold legitimate claims. The constitutional_impossibility_reading treats the Constitution as supreme; grievance_threshold treats structural justice as a competing supremacy; popular_sovereignty treats provincial referendum as self-legitimating; treaty_primacy treats treaty authority as prior to both federal and provincial. Each reading is a distinct constraint with its own ε, beneficiary/victim structure, and type. The four stories are linked via network.affects_constraints so contamination and coupling analysis can track how legitimacy contests at the kernel level cascade to downstream policy constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
