% ============================================================================
% CONSTRAINT STORY: instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_instrumentalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: instrumentalist_reading
 *   human_readable: Instrumentalist Reading: Positional Disagreement Resolved Through Generative Falsifier Tooling
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the instrumentalist reading of the kernel
 *   'positional disagreement as evidence': disagreement between standpoints
 *   becomes tractable not because of any epistemic principle about whose
 *   position deserves weight, but simply because a cheap generative tool now
 *   exists that can mass-produce candidate falsifiers and
 *   alternative-position samples, which a classification/bookkeeping layer
 *   then sorts into a shared evidentiary ledger. On this reading, the
 *   operative resource is generation-and-curation slack, not standpoint,
 *   procedure, or convergence-under-argument. The beneficiary set is
 *   therefore whoever holds compute, engineering time, and survivable
 *   public-error tolerance to run the loop repeatedly — not whoever holds
 *   marginalized standpoint (the sibling standpoint_reading's beneficiary
 *   set) and not whoever participates in a fair deliberative procedure (the
 *   sibling proceduralist_reading's concern). This exposes a genuinely new
 *   extraction path invisible to the other three readings: curated menus (the
 *   model's willingness to generate certain positions fluently and not
 *   others) and model agreeableness (the model's tendency to produce
 *   classifiable, well-formed candidates for well-resourced prompters) become
 *   sites of epistemic gatekeeping that have no analogue in
 *   standpoint-theoretic or procedural accounts of the same disagreement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(instrumentalist_reading, 0.58).
domain_priors:suppression_score(instrumentalist_reading, 0.47).
domain_priors:theater_ratio(instrumentalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(instrumentalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(instrumentalist_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(instrumentalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(instrumentalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(instrumentalist_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(instrumentalist_reading, "Instrumentalist Reading: Positional Disagreement Resolved Through Generative Falsifier Tooling").
narrative_ontology:topic_domain(instrumentalist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(instrumentalist_reading, '80825da5-b2f8-4ae4-a06e-824149e94bbc').
narrative_ontology:cs_kernel_codification('80825da5-b2f8-4ae4-a06e-824149e94bbc', distributed).
narrative_ontology:cs_authority_grounding('80825da5-b2f8-4ae4-a06e-824149e94bbc', practice).
narrative_ontology:cs_interpretation_layer_present('80825da5-b2f8-4ae4-a06e-824149e94bbc').
narrative_ontology:cs_reading_relation('80825da5-b2f8-4ae4-a06e-824149e94bbc', instrumentalist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('80825da5-b2f8-4ae4-a06e-824149e94bbc', instrumentalist_reading__pragmatist_reading, influences).
narrative_ontology:cs_reading_relation('80825da5-b2f8-4ae4-a06e-824149e94bbc', instrumentalist_reading__proceduralist_reading, coexists_with).
narrative_ontology:cs_axiom('80825da5-b2f8-4ae4-a06e-824149e94bbc', foundational, tractability_realized_through_generation_capacity).
narrative_ontology:cs_axiom_status(tractability_realized_through_generation_capacity, holdable).
narrative_ontology:cs_axiom_grounding('80825da5-b2f8-4ae4-a06e-824149e94bbc', tractability_realized_through_generation_capacity, instrumental).
narrative_ontology:cs_axiom('80825da5-b2f8-4ae4-a06e-824149e94bbc', foundational, classification_bookkeeping_constitutes_evidence).
narrative_ontology:cs_axiom_status(classification_bookkeeping_constitutes_evidence, holdable).
narrative_ontology:cs_axiom_grounding('80825da5-b2f8-4ae4-a06e-824149e94bbc', classification_bookkeeping_constitutes_evidence, conventional).
narrative_ontology:cs_reference_frame('80825da5-b2f8-4ae4-a06e-824149e94bbc', manual_adversarial_argument_baseline).
narrative_ontology:cs_drift_state('80825da5-b2f8-4ae4-a06e-824149e94bbc', post_cheap_generation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80825da5-b2f8-4ae4-a06e-824149e94bbc', '').
narrative_ontology:cs_kernel_id(instrumentalist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(instrumentalist_reading, tooling_operators).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, well_resourced_research_labs).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, model_curation_teams).
narrative_ontology:constraint_victim(instrumentalist_reading, unslacked_domain_experts).
narrative_ontology:constraint_victim(instrumentalist_reading, communities_without_generation_access).
narrative_ontology:constraint_victim(instrumentalist_reading, positions_outside_model_menu).
narrative_ontology:constraint_vindicates(instrumentalist_reading, disagreement_tractability_via_generation).
narrative_ontology:constraint_vindicates(instrumentalist_reading, classification_bookkeeping_suffices_for_evidence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate and maintain the generative pipeline that produces candidate falsifiers and alternative-position samples at scale. They decide what counts as a well-formed candidate, what gets classified into the evidence bookkeeping system, and what generation parameters (temperature, prompt scaffolding, filtering) shape the sample distribution. Their control over the tool is the mechanism by which 'positional disagreement becomes tractable' at all.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, tooling_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Have the compute budget, engineering time, and institutional cover to run the generate-classify-bookkeep loop repeatedly, iterate on prompts, and survive publicly being wrong about a generated falsifier. They convert cheap generation into publishable tractability claims and accumulate a track record of 'resolved' disagreements that were actually curated.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, well_resourced_research_labs, beneficiary,
    organized, biographical, mobile, global).

% Decide which alternative-position samples the model will readily generate versus resist or refuse (via RLHF, safety layers, or dataset composition). This shapes the menu of 'tractable' disagreements before any classification occurs — a position the model won't fluently generate is functionally excluded from the evidence base regardless of its substantive merit.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, model_curation_teams, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(instrumentalist_reading, model_curation_teams, agenda_setter).

% Possess genuine positional knowledge but lack the time, tooling access, or institutional buffer to run iterative generation loops, curate outputs, or absorb the reputational risk of a bad public falsifier claim. Their disagreement, however substantively grounded, does not enter the tractable-evidence pipeline because they cannot pay the generation-and-curation cost, not because their standpoint is discounted.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, unslacked_domain_experts, payer,
    moderate, biographical, constrained, national).

% Lack compute, connectivity, or institutional relationships with tooling operators entirely. Their positions can only enter the evidentiary record if a well-resourced actor chooses to generate on their behalf, making their epistemic standing wholly dependent on external slack they do not control.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, communities_without_generation_access, payer,
    powerless, biographical, trapped, regional).

% Substantive alternative positions that the model's training distribution or safety curation renders difficult or impossible to generate fluently. They are not represented by any human seat directly; they simply fail to appear as candidate falsifiers, so the bookkeeping system never registers a disagreement about them at all — the strongest form of exclusion, since it does not even register as a contested claim.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, positions_outside_model_menu, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(instrumentalist_reading, positions_outside_model_menu).

% Journals, funding panels, and disciplinary societies evaluating whether generation-and-classification pipelines constitute legitimate evidentiary method or merely relabel access-to-tooling as epistemic virtue. They can audit sample provenance and curation decisions but generally lack visibility into model-level suppression of unfavorable candidates.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, epistemic_standards_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(instrumentalist_reading, tooling_operators).
narrative_ontology:fixing_cost_class(instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuinely useful mechanism for converting previously untestable positional disagreements into structured, checkable candidate claims — generation at scale lets parties produce and screen falsifiers faster than manual adversarial argument could, and the classification/bookkeeping layer gives a shared ledger of what has been tried and rejected.
% TRANSFER_FUNCTION: Moves epistemic authority and the practical capacity to 'resolve' disputes from whoever holds the substantive positional knowledge to whoever holds generation slack (compute, engineering time, institutional risk-tolerance) and curation control over the model's output menu — from unslacked knowers to well-tooled operators.
% ABSENT_VOICES: Domain experts without generation access, and — more radically — entire positions the model resists producing fluently, never enter the bookkeeping ledger at all; they would object that 'tractability' has been quietly redefined as 'generatable and curatable,' but they are structurally outside the room where candidates are classified.
% DISAPPEARANCE_RATIONALE: If the generative tooling and its classification/bookkeeping apparatus vanished, positional disputes would revert to slower, more symmetric adversarial argument and manual literature review; the current asymmetric advantage held by tooling operators and well-resourced labs would collapse, and disagreements currently treated as 'resolved via falsifier generation' would need re-litigation through methods that do not privilege generation slack.
% FOUNDING_PROBLEM: Positional disagreements (across standpoints, methodologies, or contested framings) were often treated as intractable because no cheap mechanism existed to generate and test candidate falsifiers or alternative-position samples at the scale needed to make progress observable.
% FOUNDING_PROBLEM_CORROBORATION: Tooling operators and well-resourced labs attest the founding problem is being actively and successfully solved, citing throughput gains in generating and screening candidates. Independent epistemic-standards reviewers and unslacked domain experts outside the beneficiary set attest that the tractability gain is largely an artifact of who can afford to run and curate the loop, not a genuine resolution of the underlying disagreement — the problem has been reframed rather than solved, and no reviewer entirely outside the tooling-operator/lab nexus has corroborated the strong 'tractability' claim independently.
narrative_ontology:disappearance_verdict(instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(instrumentalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(instrumentalist_reading, 0.58, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-rising 0.58: the tooling genuinely accelerates falsifier discovery (real coordination value) but the same mechanism systematically channels epistemic credit toward whoever can afford to run and curate the pipeline, and that channeling compounds as tooling operators build reputational capital from repeated 'resolutions.' Suppression (0.47) is lower than extraction because there is no direct coercive barrier stopping unslacked experts from generating candidates themselves — the barrier is resource asymmetry and model curation, which is real but less totalizing than legal or physical suppression. Theater ratio rises to 0.40 because a growing share of 'resolution' activity is publication and citation of generated-falsifier results that substitute for, rather than settle, the underlying positional dispute — a Goodhart-style substitution of classification volume for genuine convergence.
 *
 * PERSPECTIVAL GAP:
 *   From the tooling-operator seat, this looks like a rope: a genuine coordination breakthrough that makes previously intractable disputes checkable. From the unslacked-expert seat, the same structure looks like a tangled rope shading toward snare: a coordination story (falsifier generation is useful!) providing cover for a new resource-gated gatekeeping mechanism that reallocates epistemic authority toward whoever holds tooling slack. The engine should compute this divergence directly from the beneficiary/victim and exit-option declarations rather than from either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Tooling operators and model curation teams sit near the full-beneficiary end: they set the generation parameters, control what the model will fluently produce, and capture the reputational and institutional credit for 'resolving' disagreements. Well-resourced labs are beneficiaries by proxy — they don't control the model but have the arbitrage-grade mobility to switch tools, afford compute, and survive being publicly wrong. Unslacked domain experts and access-poor communities are targets: the same instrumentalist logic that makes disagreement 'tractable' for the well-tooled makes their positional knowledge invisible for want of the resource, not for want of merit. The excluded non-agent seat (positions outside the model menu) captures the most severe case — a position that literally cannot be generated is never classified as a live disagreement at all, so it cannot even be counted among the losers of the ledger.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (converting intractable positional disagreement into checkable claims) remains genuinely live in some domains, which is why this is authored as contested rather than resolved mandatrophy — the tooling does real work. But the classification/bookkeeping apparatus has begun to do additional, unacknowledged work: converting 'who has slack to run the loop' into 'who counts as having produced evidence,' which is a different function than the one the tool was originally justified by. Treating this purely as coordination (rope) would erase the newly created extraction path; treating it purely as extraction (snare) would erase the genuine tractability gain the tool provides for well-resourced and under-resourced actors alike when access is actually shared. Tangled rope is the structurally honest classification precisely because both functions are simultaneously present and mutually dependent — the coordination function is what makes the extraction path effective, since only a tool that is genuinely useful for generating falsifiers can also be gatekept.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tractability_vs_relabeling,
    'Does the generative-classification loop genuinely make previously intractable positional disagreement resolvable, or does it relabel ''resolvable by whoever has tooling slack'' as ''tractable'' while leaving the underlying disagreement as unresolved as before?',
    'Compare disagreements ''resolved'' via this pipeline against matched disagreements resolved through slower manual adversarial methods: track whether the generated-falsifier resolutions hold up under independent re-litigation by parties without tooling access, or whether they are quietly re-opened once slack-asymmetry is corrected for.',
    'If resolutions hold under independent re-litigation, the instrumentalist mechanism is closer to genuine rope-like coordination gain; if they systematically fail to hold, the tangled_rope classification understates the extraction and a snare reading becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tractability_vs_relabeling, empirical, 'Whether generation-based tractability is genuine resolution or resource-gated relabeling.').

omega_variable(
    model_menu_completeness,
    'How much of the space of substantively defensible alternative positions is the underlying generative model actually capable of producing fluently, versus suppressing through training distribution or safety curation?',
    'Systematic red-teaming across ideologically and methodologically diverse prompts, audited by a body independent of the model''s own curation team, measuring refusal rates and fluency degradation for positions outside the model''s dominant training distribution.',
    'A large uncovered region would mean the ''positions_outside_model_menu'' victim class is substantial and the instrumentalist mechanism''s evidentiary ledger is systematically incomplete in ways invisible to its own bookkeeping; a small uncovered region would narrow this concern considerably.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(model_menu_completeness, empirical, 'Extent of model-level suppression of non-dominant alternative positions.').

omega_variable(
    kernel_framing_choice,
    'Is locating the kernel''s realization in ''model classification and bookkeeping capacity'' (this reading) the correct framing, or should the kernel instead be read as realized through the underlying epistemic principle the tool merely accelerates (closer to the pragmatist_reading)?',
    'Examine cases where the tool is entirely absent but the same positional disagreement becomes tractable through slower manual methods: if tractability tracks tool-availability specifically (not just practical convergence generally), the instrumentalist framing is supported; if tractability tracks convergence-through-use regardless of tool presence, the pragmatist framing better explains the same cases.',
    'If the pragmatist framing is more accurate, much of what this story attributes to tooling-specific extraction (curated menus, model agreeableness) would need to be re-attributed to a more general practical-convergence dynamic, weakening the case for a distinct instrumentalist constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the instrumentalist framing correctly isolates a tool-specific mechanism distinct from general pragmatist convergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(instrumentalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, instrumentalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inst_tr_t4, instrumentalist_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(inst_tr_t8, instrumentalist_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(inst_tr_t12, instrumentalist_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(inst_tr_t16, instrumentalist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(inst_tr_t20, instrumentalist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(inst_tr_t24, instrumentalist_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, instrumentalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(inst_be_t4, instrumentalist_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(inst_be_t8, instrumentalist_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(inst_be_t12, instrumentalist_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(inst_be_t16, instrumentalist_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(inst_be_t20, instrumentalist_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(inst_be_t24, instrumentalist_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, instrumentalist_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(inst_su_t4, instrumentalist_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(inst_su_t8, instrumentalist_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(inst_su_t12, instrumentalist_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(inst_su_t16, instrumentalist_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(inst_su_t20, instrumentalist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(inst_su_t24, instrumentalist_reading, suppression_requirement, 24, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(instrumentalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(instrumentalist_reading, 0.05).
narrative_ontology:affects_constraint(instrumentalist_reading, standpoint_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, pragmatist_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, proceduralist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel positional_disagreement_as_evidence. All four readings share the same underlying kernel commitment (that positional disagreement can become tractable evidence) but locate the realization mechanism differently: standpoint_reading in marginalized epistemic position, pragmatist_reading in practical convergence through use, proceduralist_reading in fair deliberative procedure, and this instrumentalist_reading in cheap generative tooling plus classification/bookkeeping capacity. Each reading has a distinct beneficiary/victim structure and a distinct extraction path; this reading's extraction path (curated model menus, model agreeableness toward well-resourced prompters) has no analogue in the other three. Link all four via affects_constraints; do not merge their epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
