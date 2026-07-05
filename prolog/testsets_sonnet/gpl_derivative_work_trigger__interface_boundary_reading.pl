% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: API-Boundary Reading of the GPL Derivative-Work Trigger
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   The GPL's derivative-work trigger has never been given a single settled
 *   reading: does linking two pieces of software, even loosely through a
 *   documented API, create a combined derivative work subject to copyleft, or
 *   does a clean interface boundary keep the pieces legally separate
 *   regardless of how tightly they cooperate at runtime? This story
 *   instantiates ONLY the interface-boundary reading: clean, stable API
 *   boundaries constitute non-derivative aggregation even under tight
 *   coupling. Under this reading, a proprietary module that calls a GPL
 *   program exclusively through a documented interface is not itself a
 *   derivative work, and its source need not be disclosed. This reading has
 *   become the practical operating assumption for large plugin ecosystems
 *   (databases, content management systems, IDEs) even though the FSF and
 *   allied commentators maintain that substantial technical coupling can
 *   still make the combination a derivative work in substance. The sibling
 *   readings (broad_copyleft_reading, narrow_linking_permissive_reading) are
 *   separate constraint stories with their own epsilon and their own
 *   beneficiary/victim structure — this story does not average across them or
 *   hedge its epsilon to accommodate them.
 *
 * KEY AGENTS:
 *   - ecosystem_integrators: Primary beneficiary (organized/mobile) — build commercial value on the boundary reading
 *   - commercial_plugin_vendors: Primary beneficiary (moderate/constrained) — business model depends on the reading holding
 *   - gpl_project_maintainers: Agenda-setter (organized/constrained) — administers which reading their project adopts in practice
 *   - downstream_users_expecting_full_source: Primary target (powerless/trapped) — loses access to source they believed was guaranteed
 *   - copyleft_maintainers_seeking_reciprocity: Secondary target (moderate/constrained) — contributed labor under a reciprocity expectation this reading narrows
 *   - free_software_foundation_and_allied_advocates: Excluded voice (organized/analytical) — argues the reading is wrong but has no binding authority
 *   - courts_and_licensing_counsel: Analytical observer (institutional/analytical) — adjudicates case by case without settling the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.38).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "API-Boundary Reading of the GPL Derivative-Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '9ad36869-a08a-4d1a-a9dc-b07e04a47071').
narrative_ontology:cs_kernel_codification('9ad36869-a08a-4d1a-a9dc-b07e04a47071', fixed_text).
narrative_ontology:cs_authority_grounding('9ad36869-a08a-4d1a-a9dc-b07e04a47071', practice).
narrative_ontology:cs_interpretation_layer_present('9ad36869-a08a-4d1a-a9dc-b07e04a47071').
narrative_ontology:cs_reading_relation('9ad36869-a08a-4d1a-a9dc-b07e04a47071', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ad36869-a08a-4d1a-a9dc-b07e04a47071', gpl_derivative_work_trigger__narrow_linking_permissive_reading, influences).
narrative_ontology:cs_axiom('9ad36869-a08a-4d1a-a9dc-b07e04a47071', foundational, formal_interface_separability_is_legally_dispositive).
narrative_ontology:cs_axiom_status(formal_interface_separability_is_legally_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('9ad36869-a08a-4d1a-a9dc-b07e04a47071', formal_interface_separability_is_legally_dispositive, conventional).
narrative_ontology:cs_axiom('9ad36869-a08a-4d1a-a9dc-b07e04a47071', secondary, runtime_coupling_tightness_is_legally_irrelevant_if_api_is_documented).
narrative_ontology:cs_axiom_status(runtime_coupling_tightness_is_legally_irrelevant_if_api_is_documented, holdable).
narrative_ontology:cs_axiom_grounding('9ad36869-a08a-4d1a-a9dc-b07e04a47071', runtime_coupling_tightness_is_legally_irrelevant_if_api_is_documented, instrumental).
narrative_ontology:cs_reference_frame('9ad36869-a08a-4d1a-a9dc-b07e04a47071', gpl_v2_drafting_era_linking_ambiguity).
narrative_ontology:cs_drift_state('9ad36869-a08a-4d1a-a9dc-b07e04a47071', contemporary_plugin_ecosystem_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ad36869-a08a-4d1a-a9dc-b07e04a47071', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, commercial_plugin_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_module_authors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, downstream_users_expecting_full_source).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, copyleft_maintainers_seeking_reciprocity).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, modular_architecture_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, clean_room_interface_separability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial products that call GPL-licensed components strictly through documented, stable APIs. They rely on the interface-boundary reading to combine proprietary code with copyleft infrastructure without triggering a disclosure obligation on their own codebase. Their exit option is architectural: they can always redesign around a permissively-licensed alternative if the boundary reading were rejected, but at real transition cost.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    organized, biographical, mobile, global).

% Sell closed-source plugins that run against a GPL host application's public plugin API. Their business model depends entirely on the boundary reading holding; if the broad reading prevailed they would either have to open-source their product or exit the ecosystem, and switching host platforms is costly and slow.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, commercial_plugin_vendors, beneficiary,
    moderate, biographical, constrained, global).

% Wrote the license and the code, and in practice set the interpretive stance their project publicly adopts (whether the plugin API counts as a derivative-work boundary or not). Adopting the interface-boundary reading widens their user base and commercial adoption but narrows the copyleft's practical reach; adopting the broad reading maximizes reciprocity but risks driving commercial integrators to permissively-licensed rivals. They administer the ambiguity because no court has definitively resolved it for their specific architecture.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_project_maintainers, agenda_setter,
    organized, generational, constrained, global).

% Use the combined software stack believing the GPL's reciprocity guarantee extends to everything they run, including the proprietary plugins bundled with it. Under the interface-boundary reading they receive source for the GPL core but not for the closed modules that shape the actual behavior of the product they use — a gap they typically discover only when they want to modify or audit the software and find critical pieces are unavailable.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, downstream_users_expecting_full_source, payer,
    powerless, biographical, trapped, global).

% Contributed labor to the GPL codebase on the understanding that derivative works built on it would remain open, viewing this as the reciprocal exchange that justified donating their work for free. The interface-boundary reading lets commercial actors capture value from their contributions via tightly-coupled but 'separate' proprietary modules, without returning source, undermining the reciprocity they contributed under.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, copyleft_maintainers_seeking_reciprocity, payer,
    moderate, generational, constrained, global).

% Argue publicly for the broad copyleft reading and consider API-boundary combinations to often still be derivative works in substance, but have no binding authority over how any given project's maintainers or a court characterizes a specific integration. Their objections are visible in FAQs and legal commentary but do not bind the ecosystems that adopt the interface-boundary reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, free_software_foundation_and_allied_advocates, excluded,
    organized, civilizational, analytical, global).

% Adjudicate derivative-work disputes case by case, applying copyright's abstraction-filtration-comparison and merger doctrines to specific technical architectures. Their rulings are jurisdiction-specific and fact-bound, which is precisely why no single kernel reading has become universally settled law.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts_and_licensing_counsel, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets an open-source project and a commercial ecosystem coordinate around a shared, stable interface: the GPL core stays under strong copyleft while third parties build value-added modules against a documented contract, without either side needing to renegotiate licensing terms for every integration.
% TRANSFER_FUNCTION: Moves the practical benefit of reciprocal source disclosure away from downstream users and copyleft-committed contributors, and toward parties able to structure their code as a formally separate, API-coupled module rather than a modification of the GPL codebase itself.
% ABSENT_VOICES: The Free Software Foundation and allied advocates argue publicly that tight API coupling is often substantively a derivative work regardless of formal separation, but they have no binding authority over how a specific project's maintainers characterize their own architecture, and are not present in the commercial integration decisions this reading enables.
% DISAPPEARANCE_RATIONALE: If the interface-boundary reading were rejected wholesale in favor of the broad reading, the entire plugin-vendor and ecosystem-integrator business model built on GPL-licensed hosts would need to either open-source or migrate to permissively-licensed platforms; conversely if courts affirmed it categorically, copyleft projects would lose meaningful leverage over commercial derivatives built at the API layer. Either resolution reorganizes which projects commercial actors build against.
% FOUNDING_PROBLEM: GPL projects needed a workable line between 'building on this code' (triggering reciprocity) and 'merely interoperating with this code' (not triggering it), because a maximally broad derivative-work standard would deter any commercial adoption and starve the ecosystem, while a maximally narrow standard would let trivial wrapper tricks evade copyleft entirely.
% FOUNDING_PROBLEM_CORROBORATION: Commercial integrators and plugin vendors attest the API-boundary line is a workable and necessary compromise that keeps GPL projects commercially viable. Independent of that benefiting group, courts applying abstraction-filtration-comparison doctrine and academic copyright scholars analyzing technical coupling both describe the line as genuinely unsettled rather than resolved — no consensus corroboration exists outside the parties who benefit from the reading holding.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).
:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) rather than severe: the reading does enable real value capture by commercial actors who avoid disclosure obligations they might otherwise owe, but it also enables a genuine coordination function (letting a copyleft core interoperate with a broader commercial ecosystem at all). Suppression is moderate (0.38) — the reading is maintained by project-level interpretive practice and industry norm rather than heavy coercive enforcement; a determined litigant could still challenge a specific integration in court. Accessibility collapse is middling (0.4): alternative readings remain visibly contested in FAQs, legal commentary, and occasional litigation, so the interface-boundary reading has not fully foreclosed the field the way a settled legal rule would. Resistance is real (0.55) because the FSF and copyleft-committed contributors actively contest the reading in public discourse even though they cannot force a different outcome on any given project.
 *
 * PERSPECTIVAL GAP:
 *   From the ecosystem-integrator seat, this looks like a well-functioning scaffold: modular licensing coexistence that lets commercial and open-source development cooperate. From the seat of a user who assumed 'GPL' meant the whole running system was inspectable and modifiable, the same structure looks like an extraction mechanism that quietly narrows the license's promise at exactly the technical boundary most convenient for commercial capture. The engine computes both seats from the same structural data; the divergence is real and is not resolved by picking one seat's framing as authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators and commercial plugin vendors sit near the beneficiary end: the reading is precisely what lets them retain proprietary value while consuming GPL infrastructure, and their exit options (redesign around a different architecture, or in the vendor case, migrate platforms) are real but costly, keeping them short of pure arbitrage. GPL project maintainers are the agenda-setters: they administer which reading their specific project's API practically instantiates, and benefit from wider commercial adoption in exchange for narrower practical reciprocity. Downstream users are the clearest targets — powerless, trapped in the sense that they cannot renegotiate the license after the fact, and they bear the cost of an expectation (full-stack source) that the reading does not honor. Copyleft-committed contributors are secondary targets: their labor was donated under an implicit reciprocity norm this reading narrows for commercial derivatives built at the API layer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing a workable line between building-on and merely-interoperating-with GPL code — was live when the GPL was first drafted and arguably remains live today: no purely textual test resolves every architecture, and courts still decide these questions fact-by-fact. This keeps the reading from being a simple mandatrophy case (a dead founding problem propping up a live arrangement); the problem the API-boundary line answers has not disappeared, so the scaffold classification (transitional accommodation, not permanent settlement) fits better than declaring the reading either fully vindicated or a pure capture device.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    api_boundary_ambiguity_committer_structure,
    'Is the interface-boundary reading the operative reading of the gpl_derivative_work_trigger kernel, or does the broad_copyleft_reading (any linking creates a derivative work) or the narrow_linking_permissive_reading (only direct modification triggers obligations) better describe how courts and communities actually enforce the GPL for a given architecture?',
    'This is the kernel-committer structure itself: no single case resolves it, because different jurisdictions, different projects, and different technical architectures instantiate different readings. A sibling reading would change which agents count as beneficiaries versus victims — under the broad reading, commercial plugin vendors become victims (facing forced disclosure) rather than beneficiaries; under the narrow reading, even the GPL project''s own derivative modifications might escape reciprocity, harming copyleft maintainers further.',
    'Adopting the broad reading instead would reclassify commercial_plugin_vendors and ecosystem_integrators as targets rather than beneficiaries, and would likely raise this story''s epsilon substantially since the coordination function currently attributed to the API boundary would be read as extraction cover. Adopting the narrow reading would lower epsilon further and shift more victims toward copyleft_maintainers_seeking_reciprocity, since even less would trigger disclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_boundary_ambiguity_committer_structure, conceptual, 'This story is one committer reading of a genuinely contested kernel; the sibling readings are separate constraint stories with different epsilon and different victim sets, not measurement variants of this one.').

omega_variable(
    technical_coupling_threshold,
    'At what degree of runtime coupling (shared memory, in-process function calls, versus network/IPC boundaries) does an ''API boundary'' stop being clean enough to sustain the non-derivative-aggregation reading, even within this reading''s own framework?',
    'Case-by-case judicial application of abstraction-filtration-comparison analysis to specific technical architectures; a body of consistent rulings across jurisdictions would sharpen or collapse the boundary this reading depends on.',
    'If courts converge on treating tight in-process coupling as derivative regardless of API formalism, this reading''s practical scope narrows sharply even without abandoning its core premise, shrinking the beneficiary set to only genuinely loosely-coupled integrations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technical_coupling_threshold, empirical, 'Whether the reading''s own internal boundary (what counts as ''clean'' coupling) is stable or will erode under judicial scrutiny.').

omega_variable(
    user_expectation_versus_license_text,
    'Should the relevant standard for ''derivative work'' be the GPL''s own text and legislative-style intent, or the reasonable expectation of downstream users who understood the license as a full-stack source guarantee?',
    'Survey or documented evidence of user understanding of GPL guarantees at time of adoption, weighed against textual and drafting-history analysis of the license itself; a values question ultimately, not fully resolvable by evidence alone.',
    'If user expectation is weighted heavily, the interface-boundary reading looks more clearly extractive relative to what was promised; if license text and drafter intent control, the reading looks more like a legitimate, foreseen accommodation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_expectation_versus_license_text, preference, 'Whether the normative baseline for judging this reading''s fairness should be textual license interpretation or downstream user expectation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 25, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_derivative_work_trigger__interface_boundary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__interface_boundary_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gpl_derivative_work_trigger kernel. broad_copyleft_reading treats any linking as derivative-work-triggering (higher epsilon, plugin vendors and integrators become victims). narrow_linking_permissive_reading treats only direct code modification as triggering (lower epsilon, fewer victims among copyleft contributors). This story (interface_boundary_reading) sits structurally between them: it permits a real coordination function (modular mixed-licensing ecosystems) while shifting cost onto users and contributors who expected broader reciprocity. Each reading is authored with its own stable epsilon per the epsilon-invariance principle; they are not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
