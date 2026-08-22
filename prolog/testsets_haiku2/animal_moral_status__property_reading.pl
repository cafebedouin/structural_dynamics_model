% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animals as Property: Legal and Philosophical Classification
 *   domain: applied_ethics/legal_philosophy/animal_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the property reading of the
 *   animal_moral_status kernel: the position that animals are
 *   property/resources with no independent moral standing, and their
 *   interests are categorically subordinate to human interests by definition.
 *   This is NOT a descriptive claim about how all humans treat animals or how
 *   all legal systems classify them. It is a normative-classificatory
 *   reading: a specific philosophical and legal commitment about what moral
 *   standing IS (non-existent for animals) and what follows from that premise
 *   (property rights are unconstrained by the animal's own interests). The
 *   property reading is one of three structurally distinct constraints
 *   derived from the contested kernel; the sibling readings (welfare_reading,
 *   abolitionist_reading) instantiate different premises about moral standing
 *   and produce different victim/beneficiary structures, different ε values,
 *   and different constraint types.
 *
 * KEY AGENTS:
 *   - Property owners: hold legal title and use-rights; structured as beneficiaries because the constraint establishes their entitlement unconditionally.
 *   - Resource users: agricultural, research, sport, labor industries; beneficiaries because the constraint permits their use-practices without requiring justification via the animal's experiential state.
 *   - Legal/philosophical tradition: the institutional apparatus that codifies and transmits the property classification.
 *   - Welfare advocates: excluded from constraint-setting but present in the discourse; their moral claims are filtered through property-law permissibility rather than recognized as independent standing.
 *   - Abolitionist advocates: excluded from constraint-setting; their claim (animals have rights) is structurally incoherent to the property reading.
 *   - Animals: not present in the constraint story as agents, because under this reading they have no independent moral standing and thus no stake in the structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.15).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.22).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property: Legal and Philosophical Classification").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/legal_philosophy/animal_studies").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '3d6a5f92-1ea9-4dfc-a2da-547c616aee46').
narrative_ontology:cs_kernel_codification('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', formalized).
narrative_ontology:cs_authority_grounding('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', lineage).
narrative_ontology:cs_interpretation_layer_present('3d6a5f92-1ea9-4dfc-a2da-547c616aee46').
narrative_ontology:cs_reading_relation('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', animals_lack_independent_moral_standing, deontological).
narrative_ontology:cs_axiom('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', secondary, property_rights_unconstrained_by_animal_interests).
narrative_ontology:cs_axiom_status(property_rights_unconstrained_by_animal_interests, holdable).
narrative_ontology:cs_axiom_grounding('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', property_rights_unconstrained_by_animal_interests, deontological).
narrative_ontology:cs_reference_frame('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', classical_property_law_animal_classification).
narrative_ontology:cs_drift_state('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', contemporary_rising_animal_welfare_and_rights_advocacy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d6a5f92-1ea9-4dfc-a2da-547c616aee46', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, resource_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals and the right to use them according to property law. This reading establishes their use-right as a categorical property entitlement grounded in the animal's status as a non-sentient resource. They benefit from the constraint by having enforceable ownership and use claims without subordination to the animal's experiential state.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, property_owners, beneficiary,
    powerful, generational, arbitrage, universal).

% Use animals for food production, research, sport, labor, and other purposes. Under this reading, their use-right is grounded in the categorization of animals as resources, not subjects of moral concern. No constraint on the purpose or method of use follows from the animal's own interests.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, resource_users, beneficiary,
    organized, biographical, arbitrage, universal).

% Codifies and transmits the property classification through law, contract, educational institutions, and philosophical canon. Maintains the constraint by treating the property/resource categorization as foundational to the rule of law and property rights architecture.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_and_philosophical_tradition, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(animal_moral_status__property_reading, legal_and_philosophical_tradition).

% Challenge the property classification and argue for moral consideration of animal suffering. They are structurally excluded from the constraint-setting process: their claims are heard only through the filter of property-law permissibility (cruelty laws), not through an independent moral standing claim for the animal itself.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_welfare_advocates, excluded,
    moderate, biographical, constrained, global).

% Reject the property status as a violation in principle and argue for categorical abolition of animal use. Their position is not merely regulated within this framework—it is structurally incoherent to it: property rights and rights-bearing status are mutually exclusive in the same legal system.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_advocates, excluded,
    moderate, biographical, constrained, global).

% Analyze the conceptual foundations and consistency of the property classification. They take no direct stake in ownership or use but examine the logical structure and implications of the reading itself.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, philosophers_and_ethicists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, property_owners).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal category (property/resource status) that allows predictable ownership transfer, use claims, and inheritance across all contexts where animals exist within legal systems. Solves the coordination problem of allocating use-rights without requiring case-by-case moral assessment of each animal's sentience or interests.
% TRANSFER_FUNCTION: Transfers the right to use, profit from, and dispose of animals from the animal itself (which has no moral standing under this reading) to human property-holders. The transfer is categorical and unconditional on the animal's experiential state or preferences.
% ABSENT_VOICES: Animals themselves (who cannot participate in legal or philosophical discourse) and abolitionist advocates (whose core premise—that animals have rights—contradicts the reading's foundational claim). Welfare advocates are present but only in a constrained form: their concerns are heard as advice on efficient property management, not as independent moral claims.
% DISAPPEARANCE_RATIONALE: If this constraint vanished—if animals were suddenly recognized as rights-bearing individuals with independent moral standing—the legal architecture of property rights would remain intact, but the categorization would shift. The world would not rearrange because of the constraint itself; rather, the constraint represents a reading of a deeper question (what moral status animals have) that exists independent of legal structure. The constraint is a classificatory answer, not a functional mechanism that others depend on for coordination.
% FOUNDING_PROBLEM: How should use-rights over animals be allocated in legal systems that require stable property categories? The property reading answers: by treating animals as non-sentient resources, eliminating the need to assess each animal's moral interests in determining who may use it.
% FOUNDING_PROBLEM_CORROBORATION: Property owners and most legal systems attest the problem is live and the reading is valid—stable property classification is necessary for commerce and law. Welfare and abolitionist advocates attest the founding problem is mis-specified: the real problem is how to respect animal moral standing while organizing human societies, not how to deny it. Independent philosophical analysis from outside the property-owner class documents that the 'problem' this reading solves is a problem only if one first accepts the premise that animals have no independent moral status—which is exactly what is at stake.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_unchanged).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The property reading is authored as a mountain because it claims categorical irreducibility: the classification 'animals are property/resources with no independent moral standing' is presented as following from the nature of property law and moral personhood itself, not as a policy choice. The measured extractiveness (0.15 at interval end) is LOW because once you accept the mountain premise (animals lack moral standing), extraction is nearly invisible: property owners are exercising their categorical right, not extracting value at anyone's expense. The suppression metric (0.22) is modest because the constraint persists through legal codification and institutional transmission rather than through active coercion of conscious resisters—the excluded voices (welfare and abolitionist advocates) face legal barriers and loss of voice, not ongoing individual suppression. Theater ratio (0.08) is very low because the constraint has minimal performative dimension: the property classification is asserted directly and maintained through routine institutional practice. Accessibility collapse (0.92) is HIGH because once one accepts the reading's foundational premise, alternatives (treating animals as moral patients, abolishing use, recognizing welfare as independent moral claim) become conceptually unavailable—not closed by external barriers, but by the logical structure of the reading itself. This is the hallmark of a claimed mountain: the alternatives collapse because the premise is asserted as irreducible, not because an enforcement apparatus prevents exit.
 *
 * PERSPECTIVAL GAP:
 *   The property-owner and resource-user seats should compute as beneficiaries with near-zero extraction from their own position (they are the premise-holders). Welfare and abolitionist advocates, if they were authored as full stakeholders rather than excluded, would compute as targets of high extraction (the constraint denies their claims any standing and channels their concerns through a filtered conduit). The engine computes this divergence from the structural data—the beneficiary/victim declarations and exit options. Philosophers and ethicists, in the observer seat, should compute as neutral: they have no stake in the outcome, only in the logical coherence of the reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are property owners and resource users—the constraint establishes their entitlement unconditionally. No victims are authored under this reading because the property reading denies that animals can be victims (they lack moral standing). Welfare and abolitionist advocates are excluded, not victims: they are shut out from the constraint-setting process, but the constraint does not extract from them—it simply denies them voice. The directionality is nearly zero for beneficiaries (d near 0.0) because they do not bear costs; it approaches 1.0 for the excluded advocates (they pay the cost of being shut out), but they are not in the victim set because victimization presupposes moral standing, which the reading denies them.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is whether this constraint's founding problem (allocating use-rights predictably) is still live or has become obsolete. The reading asserts it is live: stable property classification is necessary for commerce and law. Welfare and abolitionist advocates assert it is dead or mis-specified: the real problem is respecting animal moral standing, which this constraint prevents. This is a FOUNDATIONAL READING DISAGREEMENT (R5), not a case of an old constraint persisting out of inertia. The constraint persists because the property reading retains institutional power and legal codification, not because the problem it solves is undisputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_classification,
    'Is the property classification of animals a natural law (following irreducibly from the nature of property rights and moral personhood) or a constructed constraint that benefits human property owners?',
    'Historical and comparative analysis: if legal systems with different moral premises (recognizing animal rights, placing moral weight on sentience) can maintain functional property law and commerce, the property classification is constructed, not natural. If all functional legal systems require the property classification, it is closer to natural law.',
    'If natural law: the constraint is a genuine mountain and extraction is invisible. If constructed: the beneficiary presence (property owners) triggers false summit evaluation, reclassifying to tangled rope or snare depending on suppression dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_classification, conceptual, 'Whether the property/resource classification of animals is a natural necessity or a constructed choice that benefits identifiable parties.').

omega_variable(
    moral_status_premise_contestability,
    'Can the premise ''animals have no independent moral standing'' be refuted by empirical evidence (animal sentience, pain response, social complexity) or is it a foundational philosophical commitment immune to empirical revision?',
    'Examination of whether the property reading''s defenders treat sentience evidence as relevant to the moral-standing question or as irrelevant by definitional fiat. If the former, the premise is empirically contingent; if the latter, it is a foundational axiom.',
    'If empirically contingent, disagreement with welfare and abolitionist readings is resolvable in principle through empirical investigation. If foundational, the readings are incommensurable—they operate on different axioms and cannot be reconciled by evidence alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_premise_contestability, empirical, 'Whether the property reading''s core premise about animal moral standing is empirically falsifiable or philosophically foundational.').

omega_variable(
    institutional_stability_of_property_classification,
    'How much of the constraint''s persistence is due to its institutional codification and how much is due to the logical force of the property reading''s own argument?',
    'Comparative institutional analysis: if jurisdictions that explicitly reject the property premise (granting animals moral standing) maintain stable legal systems and commerce, institutional codification is separable from logical necessity. If they face systemic friction, the logical force of the reading is substantive.',
    'If separable: the constraint is more piton-like than mountain-like—it persists through institutional inertia despite being contestable. If inseparable: the reading''s logical coherence is genuine and the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_stability_of_property_classification, empirical, 'Whether the constraint''s stability depends on the reading''s logical force or on institutional path-dependence.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of welfare and abolitionist advocates structural (legal barriers, institutional exclusion) or internalized (the advocates have fused their identity with the property-reading framework, making exit psychologically difficult)?',
    'Post-institutional-change observation: if advocates maintain their positions after legal and institutional barriers are removed, suppression is partly internalized. If they shift positions when barriers fall, suppression was primarily structural.',
    'If structural: the constraint''s suppression is extrinsic and could be reduced by removing legal barriers. If internalized: the constraint carries deeper identity-lock dynamics and would persist even if legal barriers were removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of alternative readings is structural (external barriers) or internalized (identity fusion, cognitive capture).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__property_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement_basis(anim_tr_t8, observed).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__property_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement_basis(anim_tr_t16, observed).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__property_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement_basis(anim_tr_t24, observed).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__property_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement_basis(anim_tr_t32, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__property_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement_basis(anim_be_t8, observed).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__property_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement_basis(anim_be_t16, observed).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__property_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement_basis(anim_be_t24, observed).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__property_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement_basis(anim_be_t32, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__property_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement_basis(anim_su_t8, observed).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__property_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement_basis(anim_su_t16, observed).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__property_reading, suppression_requirement, 24, 0.23).
narrative_ontology:measurement_basis(anim_su_t24, observed).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__property_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement_basis(anim_su_t32, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__property_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_moral_status__property_reading, 0.12).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel decomposes into three structurally distinct constraints: property_reading (this file), welfare_reading, and abolitionist_reading. Each instantiates a different answer to the question 'What is the moral status of non-human animals?' and each produces different ε values, beneficiary/victim structures, and constraint types. The decomposition follows ε-invariance (DP-001): the property reading claims negligible extraction (animals have no moral standing, so no extraction occurs) while the abolitionist reading claims high extraction (property status itself is the violation). These cannot be the same constraint measured differently; they are different constraints derived from different readings of a shared kernel. The property reading codifies and institutionalizes the classification that the other readings reject. See the kernel_context field in commentary for the full reading-relations structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
