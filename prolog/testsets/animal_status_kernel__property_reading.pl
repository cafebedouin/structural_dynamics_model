% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animal Property Status (Property-Rights Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   The property-rights reading of animal moral status asserts that animals
 *   are property; that moral considerability derives exclusively from
 *   ownership rights; and that economic value is the only relevant metric for
 *   evaluating animal use. Under this reading, an owner's use of an animal is
 *   justified by property law alone, constrained only by rules that protect
 *   the owner's property value (such as anti-cruelty statutes interpreted
 *   narrowly to prevent owner loss). Animals have no independent moral
 *   standing, no intrinsic interests, and no claim against use. The reading
 *   is CLAIMED as a mountain—a natural and foundational fact of morality and
 *   law—while the authored metrics describe high extractiveness and rising
 *   theater ratio, indicating that significant enforcement activity maintains
 *   the reading against philosophical and regulatory challenge. The
 *   claim/metric divergence is intentional per kernel-reading rules: the
 *   reading asserts naturalness; the metrics describe what actual operation
 *   requires.
 *
 * KEY AGENTS:
 *   - property_owners: institutional power, unrestricted use rights, direct economic benefit from animal property status
 *   - economic_beneficiaries: institutional power, profit margins protected by property exclusivity, motive to defend reading
 *   - animal_advocates: organized power, structurally excluded, would advance competing reading if permitted
 *   - consumers: powerful market participants whose material benefit depends on property-rights stability
 *   - analytical observer: sees the reading's structural assumptions and how it forecloses alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.88).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.79).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, mountain).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal Property Status (Property-Rights Reading)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:emerges_naturally(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, 'b5354922-58f5-4b42-b88b-f2df1ad54765').
narrative_ontology:cs_kernel_codification('b5354922-58f5-4b42-b88b-f2df1ad54765', fixed_text).
narrative_ontology:cs_authority_grounding('b5354922-58f5-4b42-b88b-f2df1ad54765', lineage).
narrative_ontology:cs_interpretation_layer_present('b5354922-58f5-4b42-b88b-f2df1ad54765').
narrative_ontology:cs_reading_relation('b5354922-58f5-4b42-b88b-f2df1ad54765', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('b5354922-58f5-4b42-b88b-f2df1ad54765', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('b5354922-58f5-4b42-b88b-f2df1ad54765', foundational, animals_are_property).
narrative_ontology:cs_axiom_status(animals_are_property, holdable).
narrative_ontology:cs_axiom_grounding('b5354922-58f5-4b42-b88b-f2df1ad54765', animals_are_property, deontological).
narrative_ontology:cs_axiom('b5354922-58f5-4b42-b88b-f2df1ad54765', foundational, economic_value_only_relevant).
narrative_ontology:cs_axiom_status(economic_value_only_relevant, holdable).
narrative_ontology:cs_axiom_grounding('b5354922-58f5-4b42-b88b-f2df1ad54765', economic_value_only_relevant, conventional).
narrative_ontology:cs_reference_frame('b5354922-58f5-4b42-b88b-f2df1ad54765', property_law_supremacy).
narrative_ontology:cs_drift_state('b5354922-58f5-4b42-b88b-f2df1ad54765', contemporary_animal_ethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b5354922-58f5-4b42-b88b-f2df1ad54765', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, economic_beneficiaries_of_animal_use).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status_kernel__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status_kernel__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status_kernel__property_reading),
    narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88 at interval end) is high because the reading imposes no countervailing moral constraint on animal use. Once animals are property and moral considerability is indexed to ownership, owners face no independent ethical barrier to any use. Measurement trajectory shows rising extractiveness over the interval (0.81→0.88), suggesting that as alternative readings gain philosophical and regulatory traction, the property-rights reading must do more enforcement work to maintain itself—the constraint does not become more extractive in real operation, but the effort to suppress countervailing readings intensifies. Theater ratio rises from 0.28 to 0.42, indicating growing proportion of definitional and rhetorical work (defending property-law primacy, insisting economic value is the only legitimate metric) relative to substantive practical enforcement. Suppression requirement rises 0.68→0.79, showing that defending the reading requires sustained effort against organized animal-advocacy movements and welfare-regulation campaigns. Accessibility collapse (0.72) is high because once one accepts property-law supremacy and economic monism, the reading offers a complete, internally coherent moral and legal framework—alternatives are cognitively difficult to hold within it. Resistance (0.68) is substantial, not low, because this reading faces persistent philosophical challenge (the growth of animal ethics as a discipline), regulatory pressure (welfare statutes), and social movement opposition (animal rights and liberation movements)—the reading is not a genuine natural law but a contested framework being actively defended.
 *
 * PERSPECTIVAL GAP:
 *   The property-owner seat and the analytical observer seat should compute radically differently. From the property-owner seat, the reading is genuinely natural: animals are property just as land and equipment are; use is justified by ownership; economics is the only intelligible metric. From the observer seat, the reading is a contingent legal and moral choice that benefits owners and harmed by animals (if we credit them any interests); it forecloses alternatives and requires enforcement. The engine computes per-seat classification from power/exit/beneficiary-victim data. Owners are beneficiaries with high power and arbitrage-exit (they set the rules, can adjust use, can relocate); they should compute as deeply immune to the constraint. Animals, if they appear as a seat at all (they don't here, per the property reading), would be trapped, identity-locked (cannot escape species membership), and victimized (all use is permitted)—they would compute as fully targeted. The constraint-as-written excludes animals from the victim-set per the property reading's logic: the reading does not recognize animals as moral patients, so it does not recognize them as victims. This exclusion is the reading's structural signature and is why its extractiveness is so high: there is no competing victim interest to balance against owner extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the property-rights reading: property_owners have d → 0.0 (full beneficiaries; they control the framework, extract value, set terms). economic_beneficiaries_of_animal_use have d → 0.1-0.2 (beneficiaries; they profit from property access, but do not administer it). consumers_of_animal_products have d → 0.25-0.35 (symmetric to moderately beneficiary; they gain material benefit but have modest power over use terms). animal_advocates have d → not directly computed because they are excluded and thus outside the structural frame of the reading. analytical_observer has d → 0.5 (symmetric; neither benefits from nor is targeted by the constraint, observes both). The property reading assigns no victim group, which is its defining structural claim: the reading does not recognize animals as having interests that could be victimized. If alternative readings (welfare, abolitionist) were instantiated as separate constraint stories, they would assign animals as victims and would compute high d for animals and low d for owners. The difference in victim assignment is what makes the readings structurally distinct constraints, not two perspectives on one constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The property-rights reading is NOT mandatrophy—its founding problem (establishing clear, enforceable property rights in animals to enable economic planning) is still live, still motivates the reading, and still justifies enforcement. The reading faces philosophical and regulatory contestation, which explains the rising theater ratio and suppression requirement, but this contestation is evidence of active defense, not atrophy. The reading would only be mandatrophy if it persisted despite being abandoned as justified—if owners and advocates no longer believed animals should be property but property law remained in place out of institutional inertia. Currently, the reading is actively vindicated by legal systems, maintained by powerful beneficiaries, and defended against competing readings. Its rising theater ratio indicates that as challenges grow, more rhetorical work is required to sustain the claim of naturalness, but the underlying extraction and enforcement remain substantive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_constructed_choice,
    'Is the property-rights reading a natural law (animals are objectively property, moral consideration objectively derives from ownership) or a constructed legal and moral choice that benefits certain parties?',
    'Genealogical analysis: when and why did property status become the dominant framework for animals? Were there historical alternative frameworks (treating animals as subjects of moral concern independent of use)? Cross-cultural and temporal comparison: do all legal systems treat animals as property, or only some? If only some, what varies with jurisdiction, philosophy, and power structure?',
    'If the property-rights reading is natural, it should compute as mountain across all seats and should have near-zero resistance. If it is a constructed choice, it is a false summit (mountain that benefits identifiable parties): the engine''s false-summit detection would flag it and reclassify to tangled_rope or snare depending on beneficiary concentration and suppression level. The rising theater ratio and suppression requirement suggest the reading is increasingly dependent on enforcement work, which points toward constructed-choice interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_vs_constructed_choice, empirical, 'Whether animals-as-property is a natural law or a contingent institutional choice.').

omega_variable(
    animal_moral_patienthood,
    'Do animals have intrinsic interests (capacity for suffering, preference satisfaction, flourishing) that exist independent of owner preferences? If so, do those interests generate moral considerability independent of ownership?',
    'Empirical animal science: evidence of sentience, pain perception, emotional and social complexity, preference expression. Philosophical analysis: what conditions generate moral considerability? If sentience generates it, and animals are sentient, then they generate moral considerability—which the property-rights reading denies. If economic utility is the ONLY relevant value (as the reading asserts), does this follow from any defensible premise, or is it stipulated to protect property rights?',
    'If animals have intrinsic interests and sentience grounds moral considerability, the property-rights reading is false (animals are moral patients, not just property) and the victim-set should include animals. The constraint would reclassify to snare (animals trapped, victimized, no countervailing moral constraint on use) or tangled_rope (if welfare constraints are recognized, coordination + extraction). The whole structure of the reading collapses if animal interests are real and morally relevant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_moral_patienthood, empirical, 'Whether animals possess intrinsic interests that ground moral considerability.').

omega_variable(
    suppression_mechanism_attrition,
    'Is the rising suppression_requirement (0.68→0.79 over the interval) evidence of enforcement machinery hardening, or evidence of attrition in enforcement capacity relative to rising challenge?',
    'Track the specific enforcement actions: are property-rights doctrines being MORE aggressively articulated, defended, and litigated over time (hardening)? Or is enforcement capacity staying constant while challenges multiply (relative attrition)? Measure: legal resources devoted to defending property doctrines against welfare and rights challenges; judicial outcomes in property vs. welfare cases; legislative activity protecting vs. constraining property rights.',
    'Hardening suggests the reading is being actively defended and remains institutionally vital. Attrition suggests the reading is losing its grip despite legal formalism, pointing toward future reclassification as inert piton (maintained by institutional inertia, not by active vindication).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_attrition, empirical, 'Whether rising suppression is evidence of enforcement hardening or capacity attrition.').

omega_variable(
    economic_monism_defensibility,
    'Can the claim that economic value is the ONLY relevant value in evaluating animal use be defended philosophically, or is it stipulated to protect property interests?',
    'Philosophical analysis and peer engagement: can a coherent moral theory be built on the claim that sentience, suffering, autonomy, and flourishing are entirely irrelevant to moral status unless they have market price? If not—if economic monism is indefensible as a general principle but asserted in this domain anyway—then it is a stipulation designed to benefit property owners, not a foundational truth.',
    'If economic monism is indefensible, the reading is a false summit masquerading as a principle: it is a constructed choice benefiting property owners, rationalized by an incoherent moral principle. This supports reclassification to tangled_rope or snare and supports the FSM (false-summit-mountain) detection firing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_monism_defensibility, conceptual, 'Whether economic monism is a defensible principle or a stipulation protecting property interests.').

omega_variable(
    identity_fusion_between_reading_and_legal_system,
    'Is the property-rights reading deeply fused with the legal system''s institutional identity (such that rejecting the reading would require institutional self-transformation) or is it one component the legal system could modify without existential threat?',
    'Institutional history: did property-rights-in-animals enter the law as a foundational principle or as one rule among others? Can legal systems modify animal property doctrine (as some jurisdictions have done) without the system collapsing? Or does property law''s entire framework depend on animals being property?',
    'If fused (animals-as-property is constitutive of legal systems), the reading is identity-locked for institutional agents; they cannot exit without institutional dissolution. If modular (the reading is one component), then exit is possible and the reading is more vulnerable to displacement. High identity-fusion suggests the reading will persist despite challenges (piton or sustained tangled-rope); low fusion suggests it could be dislodged more readily (the constraint is more fragile than its institutional entrenchment suggests).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_between_reading_and_legal_system, conceptual, 'Whether the property-rights reading is fused with legal-system identity or modular.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(anim_tr_t15, animal_status_kernel__property_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__property_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(anim_tr_t75, animal_status_kernel__property_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__property_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.81).
narrative_ontology:measurement(anim_be_t15, animal_status_kernel__property_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__property_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.87).
narrative_ontology:measurement(anim_be_t75, animal_status_kernel__property_reading, base_extractiveness, 75, 0.88).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__property_reading, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(anim_su_t15, animal_status_kernel__property_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__property_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement(anim_su_t75, animal_status_kernel__property_reading, suppression_requirement, 75, 0.79).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__property_reading, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.18).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status_kernel has three readings instantiated as separate constraints: property_reading (this story, asserting property law supremacy and economic monism), welfare_reading (animals are sentient, use constrained by suffering minimization), and abolitionist_reading (animals are moral persons, property status is the injustice). Each reading has its own ε, beneficiary/victim structure, enforcement mechanisms, and theater dynamics. They are not three perspectives on one constraint; they are three structurally distinct constraints sharing a common kernel (the moral and legal status of animals). The property_reading forecloses the abolitionist_reading within its own framework; it coexists with welfare_reading across institutional seats; it influences both by establishing property-law supremacy as the default institutional frame. Constraint family network should link all three; the engine will detect patterns of cross-reading coupling and compute foreclosure/coexistence/influence relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__property_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
