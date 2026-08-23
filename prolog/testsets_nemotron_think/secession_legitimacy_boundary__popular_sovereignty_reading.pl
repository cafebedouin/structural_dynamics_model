% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Secession Legitimacy Principle
 *   domain: political/federalism/resource
 *
 * SUMMARY:
 *   This constraint story captures the popular sovereignty reading of the
 *   secession legitimacy boundary kernel: the claim that a democratic
 *   majority within existing provincial boundaries holds ultimate sovereignty
 *   and that a referendum result is self-legitimating, requiring no further
 *   consent from the federal authority, other provinces, or minority
 *   populations. The reading functions as both a coordination mechanism
 *   (providing a clear, procedurally definite exit path) and an extraction
 *   mechanism (transferring sovereignty, territory, and resource control from
 *   the federal order and from minority/treaty-holding populations to the
 *   provincial majority). The constraint requires active enforcement —
 *   referendum legislation, clarity laws, international recognition
 *   campaigns, and often security apparatus to manage the transition. The
 *   claimed type is tangled_rope because the coordination function
 *   (democratic exit) is genuine but inextricably bound to asymmetric
 *   extraction (federal authority overridden, minorities and treaty holders
 *   excluded).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.72).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.75).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Secession Legitimacy Principle").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/federalism/resource").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '93127201-c8cb-4077-88a6-296d7d22f2a7').
narrative_ontology:cs_kernel_codification('93127201-c8cb-4077-88a6-296d7d22f2a7', fixed_text).
narrative_ontology:cs_authority_grounding('93127201-c8cb-4077-88a6-296d7d22f2a7', lineage).
narrative_ontology:cs_interpretation_layer_present('93127201-c8cb-4077-88a6-296d7d22f2a7').
narrative_ontology:cs_reading_relation('93127201-c8cb-4077-88a6-296d7d22f2a7', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('93127201-c8cb-4077-88a6-296d7d22f2a7', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('93127201-c8cb-4077-88a6-296d7d22f2a7', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('93127201-c8cb-4077-88a6-296d7d22f2a7', foundational, provincial_majority_sovereignty_absolute).
narrative_ontology:cs_axiom_status(provincial_majority_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('93127201-c8cb-4077-88a6-296d7d22f2a7', provincial_majority_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('93127201-c8cb-4077-88a6-296d7d22f2a7', foundational, referendum_self_legitimating).
narrative_ontology:cs_axiom_status(referendum_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('93127201-c8cb-4077-88a6-296d7d22f2a7', referendum_self_legitimating, conventional).
narrative_ontology:cs_reference_frame('93127201-c8cb-4077-88a6-296d7d22f2a7', westphalian_provincial_sovereignty).
narrative_ontology:cs_drift_state('93127201-c8cb-4077-88a6-296d7d22f2a7', contemporary_self_determination_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('93127201-c8cb-4077-88a6-296d7d22f2a7', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majorities).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, external_secession_supporters).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, democratic_self_determination_principle).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, majoritarian_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilizes referendum campaigns, frames the provincial boundary as the legitimate democratic unit, and claims the right to unilaterally trigger secession. Benefits by converting demographic majority into sovereign authority. Exit from the constraint means accepting federal constitutional order — constrained by identity investment in the secessionist project.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority, agenda_setter,
    organized, generational, constrained, regional).

% Loses territorial integrity, resource revenue, and constitutional authority when the principle is activated. Retains agenda-setting power through control of referendum rules, clarity legislation, and international recognition gatekeeping. Exit means accepting provincial departure — constrained by state survival imperatives and domestic political cost.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, agenda_setter).

% Linguistic, ethnic, or political minorities within the seceding province who face status loss, property uncertainty, or forced assimilation under the new sovereign. Have no effective voice in the referendum franchise (often defined by provincial residency) and no exit that preserves their community in place. Trapped by geography and identity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities, payer,
    powerless, biographical, trapped, local).

% Hold treaty rights with the federal Crown that predate provincial boundaries. The popular sovereignty reading treats the provincial majority as the sole legitimating demos, rendering treaty consent structurally irrelevant. Identity-locked because treaty relationship constitutes their political existence — exit from the treaty framework is existential, not optional.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, payer,
    organized, generational, identity_locked, regional).

% Foreign states or non-state actors who gain strategic advantage, resource access, or ideological validation from provincial secession. Provide diplomatic, financial, or military support to the secessionist majority. Arbitrage-grade exit — they engage instrumentally and withdraw when costs exceed returns.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, external_secession_supporters, beneficiary,
    powerful, biographical, arbitrage, global).

% States and organizations that confer or withhold recognition of the new entity. Their practice shapes whether the referendum's self-legitimating claim produces actual sovereignty. Analytical seat — they evaluate the claim against precedent (Kosovo, South Sudan, Crimea, Catalonia) without being bound by it.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_community, observer,
    institutional, generational, analytical, global).

% Adjudicate the legal status of referendum legislation, clarity requirements, and the constitutional amending formula. Can block, delay, or shape the referendum's execution. Their rulings determine whether the popular sovereignty claim operates inside or outside the constitutional order.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unilateral democratic mechanism for a provincial population to exit a federation without requiring negotiated consent from the federal center or other provinces — replaces bargaining with a threshold vote.
% TRANSFER_FUNCTION: Transfers sovereign authority over territory, resources, and population from the federal order to the provincial majority; transfers political status and constitutional protections from minorities and treaty holders to the new sovereign majority.
% ABSENT_VOICES: Minorities within the province (linguistic, ethnic, political) who are outvoted by design; indigenous treaty holders whose consent is structurally excluded by the 'provincial majority' demos definition; future generations in both the seceding and remnant units who inherit the rearranged borders and liabilities without a vote.
% DISAPPEARANCE_RATIONALE: If the principle that a provincial referendum is self-legitimating vanished, secession would revert to requiring constitutional amendment negotiated among federal and provincial governments (per the constitutional_impossibility_reading) or meeting a grievance threshold (per the grievance_threshold_reading). The federal center would regain veto power; treaty holders would regain structural veto; the unilateral exit path would close.
% FOUNDING_PROBLEM: How to make democratic self-determination operationally real without subjecting it to the veto of the very authority from which a people seeks to separate — the problem of legitimate exit from a union that claims indivisibility.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists outside secessionist movements (e.g., Buchanan, Moore, Norman) acknowledge the founding problem as real but argue the popular sovereignty reading's solution creates worse pathologies (minority subordination, boundary instability). Federalist scholars (e.g., Watts, Tierney) attest the problem is substantially managed by existing amendment formulas and that the popular sovereignty reading manufactures a crisis to justify extraction. No neutral arbiter corroborates the reading's claim that the problem remains live in its preferred form.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers full sovereign authority and resource control from the federal center and from non-consenting populations to the provincial majority, with no compensation mechanism. Suppression (0.75) is high because the constraint's operation depends on structurally excluding treaty holders and minorities from the legitimating demos, and on overriding the federal constitutional order — both maintained by active legal and political enforcement. Theater ratio (0.45) reflects that the referendum process has genuine coordination function (it really does settle the question for the majority) but a growing share of the machinery (clarity legislation, campaign spending, international lobbying) serves to legitimate a predetermined outcome rather than test the will. Accessibility collapse (0.82) is very high because accepting the principle that provincial boundaries define the legitimate demos makes negotiated alternatives (confederation, asymmetric federalism, treaty renegotiation) structurally incoherent — the frame itself excludes them. Resistance (0.68) is substantial because federal governments, courts, and international actors actively contest the principle's legal validity and practical consequences.
 *
 * PERSPECTIVAL GAP:
 *   From the secessionist majority's seat, the constraint appears as rope — a genuine coordination solution to the problem of legitimate exit. From the federal government's seat, it appears as snare — an extraction mechanism that strips sovereignty without consent. From minorities' and treaty holders' seats, it appears as snare with identity-locked suppression — they are not just excluded from benefit but structurally erased from the legitimating demos. The engine computes this divergence from the declared roles, power, and exit options; the claimed_type (tangled_rope) acknowledges the coordination function is real for the majority while extraction is real for the payers.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial secessionist majority is the structural beneficiary (d ~ 0.15): it collects sovereign authority and resource control, controls the referendum timing and question, and faces constrained but real exit (it can abandon the project). The federal government is a payer with agenda-setting capacity (d ~ 0.75): it bears the loss of territory and authority but retains procedural gatekeeping (clarity legislation, recognition). Provincial minorities are trapped payers (d ~ 0.95): they bear status loss and forced assimilation with no effective voice or exit. Indigenous treaty holders are identity-locked payers (d ~ 0.90): their constitutional relationship is with the Crown, not the province, and the constraint treats their consent as irrelevant — exit from the treaty framework is existential. External supporters are arbitrage beneficiaries (d ~ 0.10): they extract strategic value with minimal cost and full exit freedom. International community and courts are analytical observers (d ~ 0.50 symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making self-determination real against a veto-holding center) was live in the decolonization era and remains live for stateless nations. But in developed federations with robust minority protections and amendment formulas, the popular sovereignty reading often persists as a mobilization tool for regional elites seeking resource control or political autonomy beyond what negotiation yields — the mandate has atrophied into a lever for asymmetric extraction. The constraint is not a piton because the secessionist majority actively maintains and invests in it (high theater_ratio growth, active enforcement); it is not a scaffold because no sunset or transition is declared. The mandatrophy is unresolved: the coordination function (democratic exit) is real but the extraction function (majority over minorities, province over federation) has become the dominant operational logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the popular_sovereignty_reading a distinct constraint from its sibling readings of the secession_legitimacy_boundary kernel, or a measurement variant of a single constraint?',
    'Apply the ε-invariance test: if the extractiveness, beneficiary/victim structure, and enforcement requirements differ across readings, they are distinct constraints. This reading''s ε (0.72) and victim set (federal, minorities, treaty holders) differ structurally from the constitutional_impossibility_reading (ε ≈ 0.1, victims: secessionist majorities) and treaty_primacy_reading (ε ≈ 0.6, victims: provincial majorities, federal government).',
    'If distinct, each reading gets its own classification and temporal trajectory. If unified, the kernel would require a single ε — which would force averaging over structurally incompatible arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are distinct constraints or one constraint measured differently.').

omega_variable(
    minority_rights_vs_majority_will,
    'Does the constraint''s coordination function (democratic exit) structurally require the exclusion of minorities and treaty holders, or is that exclusion a contingent political choice that could be decoupled?',
    'Examine historical cases where secession referendums included minority veto provisions or treaty holder consent requirements (e.g., Bougainville, Scotland 2014 franchise debates). If coordination survives with inclusive demos, exclusion is contingent; if coordination fails, exclusion is structural to this reading''s logic.',
    'If exclusion is structural, the constraint is inherently extractive toward minorities/treaty holders (tangled_rope or snare). If contingent, a reformed version could approach rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_rights_vs_majority_will, empirical, 'Whether minority/treaty-holder exclusion is structurally necessary to the reading''s coordination function.').

omega_variable(
    referendum_fairness_vs_manipulation,
    'Is the referendum process a fair test of the provincial will, or is it structurally biased by the agenda-setter''s control of timing, question wording, franchise definition, and clarity thresholds?',
    'Compare referendum design across cases (Quebec 1995, Scotland 2014, Catalonia 2017, Bougainville 2019) for systematic agenda-setter advantage. Measure correlation between incumbent control of referendum rules and outcome.',
    'If systematically biased, the theater_ratio is understated — the coordination function is theater for a predetermined extraction. If fair, the coordination function is genuine and the extraction is a side effect of majority rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_fairness_vs_manipulation, empirical, 'Whether the referendum mechanism is a fair coordination instrument or a rigged extraction tool.').

omega_variable(
    natural_law_vs_constructed_principle,
    'Is the principle ''provincial majority referendum is self-legitimating'' a genuine natural law of democratic legitimacy, or a constructed political principle that serves identifiable beneficiaries?',
    'Trace the principle''s genealogy: from Wilsonian self-determination (applied to colonies, not sub-state units) through decolonization jurisprudence to its selective application in Quebec, Scotland, Catalonia, Kosovo. Identify whether the ''provincial boundary as demos'' criterion emerges from democratic theory or from the political interests of sub-state majorities.',
    'If constructed, the constraint is a false summit candidate (claimed mountain, actual tangled_rope/snare) — FSM signature would trigger. If natural law, the high extractiveness metrics would require re-interpretation (e.g., extraction is the price of justice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_principle, conceptual, 'Whether the principle''s natural-law framing conceals a constructed beneficiary structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_tr_t16, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_tr_t32, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_tr_t48, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_tr_t64, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 64, 0.43).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_tr_t80, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_be_t16, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_be_t32, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_be_t48, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_be_t64, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 64, 0.7).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_be_t80, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_su_t16, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_su_t32, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_su_t48, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 48, 0.68).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_su_t64, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 64, 0.72).
narrative_ontology:measurement(secession_legitimacy_boundary__popular_sovereignty_reading_su_t80, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 80, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the secession_legitimacy_boundary constraint family. The popular_sovereignty_reading has substantially higher ε (0.72 vs ~0.1-0.6) because it legitimates unilateral extraction from federal authority and minority populations. The constitutional_impossibility_reading has near-zero ε (mountain-like) because it imposes a procedural barrier without transferring sovereignty. The grievance_threshold_reading has intermediate ε (coordination function activated only by injustice). The treaty_primacy_reading has high ε but different victim set (provincial majorities, federal government). All four are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, institutional, 0.75).
constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, organized, 0.9).
constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, powerless, 0.95).
constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, powerful, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
