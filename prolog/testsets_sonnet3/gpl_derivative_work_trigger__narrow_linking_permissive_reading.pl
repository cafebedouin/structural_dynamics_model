% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow-Linking Reading of GPL Derivative-Work Trigger (Aggregation Not Derivation)
 *   domain: software_licensing/copyright_law
 *
 * SUMMARY:
 *   This story instantiates the narrow-linking-permissive reading of the GPL
 *   derivative-work trigger kernel: the position that linking against GPL
 *   code is aggregation, not derivation, and that only actual modification of
 *   GPL source code triggers the license's copyleft obligations. This reading
 *   has gained substantial practical traction through case law favoring
 *   functional/interface-boundary reasoning in copyright generally, and
 *   through the commercial software industry's active promotion of a
 *   bright-line 'you only owe what you touched' standard. It stands beside
 *   two sibling readings — the broad copyleft reading (any linkage, even
 *   dynamic, creates a derivative work) and the interface boundary reading
 *   (clean API separation is dispositive regardless of coupling tightness) —
 *   each of which is a separate constraint story with its own ε and its own
 *   stakeholder structure. This story's ε is authored solely for the narrow
 *   reading's own operation: how much it extracts from those governed by it,
 *   on its own terms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.58).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.35).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow-Linking Reading of GPL Derivative-Work Trigger (Aggregation Not Derivation)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'c8e65c7e-8106-489d-be47-fd5be9a645be').
narrative_ontology:cs_kernel_codification('c8e65c7e-8106-489d-be47-fd5be9a645be', fixed_text).
narrative_ontology:cs_authority_grounding('c8e65c7e-8106-489d-be47-fd5be9a645be', lineage).
narrative_ontology:cs_interpretation_layer_present('c8e65c7e-8106-489d-be47-fd5be9a645be').
narrative_ontology:cs_reading_relation('c8e65c7e-8106-489d-be47-fd5be9a645be', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('c8e65c7e-8106-489d-be47-fd5be9a645be', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('c8e65c7e-8106-489d-be47-fd5be9a645be', foundational, modification_is_the_sole_derivation_trigger).
narrative_ontology:cs_axiom_status(modification_is_the_sole_derivation_trigger, holdable).
narrative_ontology:cs_axiom_grounding('c8e65c7e-8106-489d-be47-fd5be9a645be', modification_is_the_sole_derivation_trigger, conventional).
narrative_ontology:cs_axiom('c8e65c7e-8106-489d-be47-fd5be9a645be', secondary, linking_is_functional_composition_not_authorship).
narrative_ontology:cs_axiom_status(linking_is_functional_composition_not_authorship, holdable).
narrative_ontology:cs_axiom_grounding('c8e65c7e-8106-489d-be47-fd5be9a645be', linking_is_functional_composition_not_authorship, instrumental).
narrative_ontology:cs_reference_frame('c8e65c7e-8106-489d-be47-fd5be9a645be', fsf_broad_propagation_intent).
narrative_ontology:cs_drift_state('c8e65c7e-8106-489d-be47-fd5be9a645be', contemporary_commercial_linking_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8e65c7e-8106-489d-be47-fd5be9a645be', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_linux_distributors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, embedded_device_manufacturers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, downstream_users_of_linked_binaries).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation_movement_goals).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, competing_open_source_reimplementers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship closed-source modules that link against GPL-licensed libraries at the binary level, relying on the narrow reading to argue that linking is mere aggregation rather than derivation. This lets them capture the functional benefit of GPL code (a kernel driver framework, a compression library, a GUI toolkit) without disclosing their own source, so long as they do not modify the GPL code itself.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Package and certify software stacks that combine GPL kernels/libraries with proprietary drivers and applications, and lobby courts and standards bodies toward the narrow reading because their entire commercial model depends on the wall between GPL-licensed infrastructure and the closed layers built on top of it. They actively litigate and publish legal guidance defending this boundary.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_linux_distributors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_linux_distributors, agenda_setter).

% Ship firmware combining a GPL kernel with proprietary hardware-abstraction and application code, relying on the narrow reading to avoid disclosing their own IP. They benefit from the free engineering labor embedded in the GPL codebase while withholding their own contributions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, embedded_device_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Receive a binary that incorporates GPL-covered code but cannot obtain the source of the proprietary components linked against it, because the narrow reading treats that linkage as non-triggering aggregation. They lose the ability to audit, modify, repair, or redistribute the software as the GPL's authors intended, even though GPL code is doing real work inside the product they hold.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, downstream_users_of_linked_binaries, payer,
    powerless, biographical, trapped, global).

% The propagation goal the GPL was drafted to serve — that software linked to free code becomes free in turn — is structurally frustrated wherever the narrow reading governs, because the obligation trigger contracts to modification-only and leaves the far larger surface of linkage untouched. Listed for completeness as the doctrinal objective at stake, not as an acting party.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation_movement_goals, payer,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation_movement_goals).

% Maintain fully free alternatives to the linked proprietary modules and depend on copyleft propagation to keep the field of comparison fair — every proprietary module that escapes disclosure under the narrow reading is a competitor they cannot inspect, learn from, or fork, while their own code remains fully open.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, competing_open_source_reimplementers, payer,
    moderate, biographical, constrained, global).

% Drafted the GPL to trigger disclosure on any derivative work including tight linkage, and has argued for decades (through the LGPL's existence as a deliberate carve-out, and through public guidance) that ordinary dynamic linking to a GPL library does create a derivative work. Under the narrow reading, the FSF's own drafting intent is treated as non-authoritative — its interpretation is heard in commentary and amicus briefs but has not prevailed as binding law in the jurisdictions where this reading holds.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, excluded,
    organized, civilizational, analytical, global).

% Adjudicate what counts as a derivative work under copyright law generally, and have in several jurisdictions (notably U.S. case law on functional linking, APIs, and merger doctrine) leaned toward treating mere linkage without source modification as non-derivative aggregation. Their rulings are the actual enforcement mechanism that makes this reading operative rather than merely rhetorical.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, courts_and_ip_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable line for when a distributor's disclosure obligation is triggered — code you wrote and modified must be shared, code you merely call across a linkage boundary need not be. This lets commercial vendors build products atop GPL infrastructure with predictable legal exposure, avoiding the paralysis of an unbounded 'any derivative work however coupled' standard.
% TRANSFER_FUNCTION: Moves the practical benefit of GPL-licensed engineering labor (kernels, libraries, toolkits, drivers written and maintained largely by volunteer and foundation-funded contributors) to vendors who link against it, without moving back the reciprocal source-disclosure obligation the license was drafted to require of anyone building on that labor.
% ABSENT_VOICES: The Free Software Foundation and the broader copyleft-authoring community, who wrote the license text with an admittedly broader trigger in mind (evidenced by the existence of the LGPL as a deliberate weaker-obligation alternative), are heard in briefs and public commentary but are not the adjudicating authority; downstream users who cannot obtain source have essentially no forum at all.
% DISAPPEARANCE_RATIONALE: If the narrow reading were overturned in favor of the broad copyleft reading, commercial vendors and embedded manufacturers who currently ship proprietary code linked against GPL libraries would face sudden disclosure obligations or would need to re-architect around LGPL, permissively-licensed, or in-house alternatives — a substantial re-engineering and licensing-strategy shock across the commercial software industry.
% FOUNDING_PROBLEM: Software authors wanted a mechanism that would let code remain free and have that freedom propagate to anything meaningfully built on it, while courts and industry needed a workable boundary distinguishing 'building on' from 'merely combining with' so that ordinary software composition (an application calling a system library) would not become legally radioactive.
% FOUNDING_PROBLEM_CORROBORATION: Commercial distributors and their counsel attest the problem this reading solves (predictable boundaries for combining licensed and proprietary code) is very much live and well-served by the narrow trigger. The FSF and copyleft-oriented legal scholars attest, from outside the beneficiary set, that the founding problem the GPL was actually drafted to solve — propagating freedom to linked code — is being frustrated rather than resolved by this reading, citing the LGPL's existence as textual evidence the GPL's own drafters intended a broader default trigger than courts applying the narrow reading have enforced.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but not extreme: the narrow reading does capture real value — free engineering labor from GPL-covered code flows to proprietary vendors without reciprocal disclosure — but the coordination function (a workable, administrable boundary for software composition) is genuine and not merely cover. Suppression (0.35) is moderate: the mechanism is legal-doctrinal rather than coercive-enforcement; affected parties (the FSF, downstream users) can and do contest it through litigation, licensing choice (LGPL adoption), and public argument, though the practical asymmetry in litigation resources is real. Accessibility collapse (0.40) reflects that alternatives remain genuinely available — developers can choose LGPL, choose permissive licenses, or choose to avoid the linkage pattern — this is not a closed trap. Resistance (0.55) is meaningfully high because the FSF, copyleft scholars, and reimplementer communities actively contest the reading in courts, in license drafting (subsequent GPLv3 language attempting to close perceived loopholes), and in public discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (vendors, distributors, manufacturers), the narrow reading looks like sensible legal clarity enabling commercial software composition — a rope. From the payer seats (downstream users, reimplementers), and from the FSF's excluded vantage, the same structure looks like a wall erected to defeat the license's actual purpose — closer to extraction requiring active judicial maintenance. The engine computes these divergent seat classifications from the declared power/exit/beneficiary structure; this story does not adjudicate which seat is correct, only which structural facts each seat sits within.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary module vendors, commercial distributors, and embedded manufacturers are declared beneficiaries: the narrow reading directly subsidizes their business model by letting them capture GPL code's functional value while withholding reciprocal disclosure — low d, benefiting from the constraint's operation. Downstream users of linked binaries and competing open-source reimplementers are declared victims: they bear the cost of lost source-availability and lost competitive transparency respectively — high d, targets of the arrangement's effective extraction. The FSF is excluded rather than victimized directly in a rent-transfer sense — its stake is doctrinal-propagative (its founding goal being frustrated), which is why free_software_foundation_movement_goals is listed as a non-agent payer (the frustrated objective) distinct from the FSF-as-institution (excluded stakeholder, drafting authority not currently prevailing).
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading is not a pure extraction mechanism dressed as coordination — the founding_problem status is genuinely contested rather than simply dead, because both the coordination need (predictable composition boundaries) and the frustrated propagation goal remain live simultaneously. Classifying this as tangled_rope rather than snare respects that the coordination function is real (software composition needs SOME workable boundary) while still naming the asymmetric extraction (vendors capture value without reciprocal disclosure) that requires active enforcement (ongoing litigation and doctrinal maintenance) to hold. A pure snare label would erase the genuine coordination value narrow linking rules provide to the broader software ecosystem; a pure rope label would erase the documented frustration of the GPL's own drafting intent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_mechanism_as_derivation_proxy,
    'Is the narrow reading''s modification-only trigger a principled application of derivative-work doctrine to software, or a doctrinal accident produced by courts analogizing linking to non-software contexts (e.g., book compilations) where the analogy does not hold?',
    'Comparative analysis of case law reasoning across jurisdictions, and technical expert testimony on whether dynamic linking creates the kind of interdependence that traditional derivative-work doctrine was meant to capture.',
    'If the modification-only trigger is doctrinally accidental rather than principled, this reading''s coordination-function claim weakens substantially and the story would sit closer to snare than tangled_rope; if principled, the coordination claim is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_mechanism_as_derivation_proxy, conceptual, 'Whether the narrow trigger reflects sound derivative-work doctrine or a mismatched analogy.').

omega_variable(
    kernel_reading_divergence_location,
    'Where exactly does this reading''s departure from the broad_copyleft_reading and interface_boundary_reading readings live — is it a disagreement about what ''linking'' technically does (a factual/technical question about binary coupling), or a disagreement about what ''derivative work'' means as a legal term of art (a doctrinal question), or both?',
    'Decompose court opinions and FSF guidance to isolate whether disputants agree on the technical facts of linkage and disagree only on legal characterization, or dispute the technical facts themselves (e.g., whether dynamic vs static linking differ enough to matter).',
    'If the dispute is purely doctrinal (facts agreed, legal characterization contested), the three sibling readings are best modeled as genuinely coexisting interpretive frameworks with no technical resolution possible. If partly factual, resolving the technical question could collapse the interface_boundary_reading and narrow_linking_permissive_reading toward each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_location, conceptual, 'Whether the kernel readings diverge on technical fact or legal characterization.').

omega_variable(
    propagation_goal_frustration_measurement,
    'How much actual disclosure that would otherwise have occurred under the broad reading is foreclosed by the narrow reading, in practice, across the commercial software ecosystem?',
    'Empirical survey of commercial products that link against GPL libraries without disclosing proprietary source, cross-referenced against what would be required if the broad_copyleft_reading prevailed.',
    'A large foreclosed-disclosure volume would support a higher extractiveness score and strengthen the victim-side claims; a small volume would suggest the narrow reading''s practical bite is modest despite its doctrinal significance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(propagation_goal_frustration_measurement, empirical, 'The empirical scale of disclosure frustrated by this reading''s prevalence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.31).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.1).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the GPL derivative-work trigger' per the ε-invariance principle: broad_copyleft_reading (any linkage triggers disclosure, high ε for vendors relying on non-disclosure), interface_boundary_reading (clean API boundary is dispositive regardless of coupling, intermediate ε), and this narrow_linking_permissive_reading (only modification triggers disclosure, moderate ε as authored here). Each reading is adjudicated by different courts/parties and produces a different beneficiary/victim structure; they are linked here rather than merged because measuring 'the GPL trigger' by different observables (what counts as linking vs. what counts as modification vs. what counts as a clean boundary) yields materially different ε values — the signature of needing separate constraints, not one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
