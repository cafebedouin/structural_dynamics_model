% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: Broad Copyleft (Linking-as-Derivation) Reading of the GPL Derivative Work Trigger
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This story instantiates the broad copyleft reading of the GPL
 *   derivative-work kernel: the position, associated with the Free Software
 *   Foundation and much defensive corporate compliance practice, that linking
 *   against GPL-licensed code — including dynamic linking at runtime — forms
 *   a single derivative work under copyright law, triggering the obligation
 *   to disclose corresponding source for the combined work. This is one of
 *   three structurally distinct readings of the same kernel (the
 *   linking-and-derivation boundary question); the
 *   narrow_linking_permissive_reading and interface_boundary_reading are
 *   separate constraints with their own ε values, not alternative
 *   measurements of this one. Under this reading, the primary coordination
 *   function (pulling derivative improvements back into the commons) is real,
 *   but so is the extraction it imposes on vendors and integrators who face
 *   compliance costs, forced disclosure, or costly avoidance engineering —
 *   hence the classification as rope rather than mountain: it is a genuine,
 *   actively-maintained coordination mechanism with real but bounded
 *   extraction, not a natural law.
 *
 * KEY AGENTS:
 *   - gpl_licensed_project_maintainers: primary beneficiary (organized/arbitrage) — gains commons growth and leverage
 *   - free_software_foundation: agenda_setter (institutional/analytical) — authors and enforces the broad interpretation
 *   - proprietary_software_vendors: primary target (powerful/constrained) — bears compliance or avoidance cost
 *   - embedded_systems_integrators: secondary target (moderate/trapped) — least able to re-architect after the fact
 *   - downstream_source_recipients: beneficiary (powerless/constrained) — gains source access contingent on vendor compliance
 *   - narrow_reading_adjudicators: excluded (institutional/analytical) — structurally absent from most disputes due to settlement pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.38).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.55).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "Broad Copyleft (Linking-as-Derivation) Reading of the GPL Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, 'f9184cd1-a0c0-4869-aeca-f4f3768249dd').
narrative_ontology:cs_kernel_codification('f9184cd1-a0c0-4869-aeca-f4f3768249dd', distributed).
narrative_ontology:cs_authority_grounding('f9184cd1-a0c0-4869-aeca-f4f3768249dd', practice).
narrative_ontology:cs_interpretation_layer_present('f9184cd1-a0c0-4869-aeca-f4f3768249dd').
narrative_ontology:cs_reading_relation('f9184cd1-a0c0-4869-aeca-f4f3768249dd', gpl_derivative_work_trigger__narrow_linking_permissive_reading, forecloses).
narrative_ontology:cs_reading_relation('f9184cd1-a0c0-4869-aeca-f4f3768249dd', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('f9184cd1-a0c0-4869-aeca-f4f3768249dd', foundational, linkage_constitutes_single_combined_work).
narrative_ontology:cs_axiom_status(linkage_constitutes_single_combined_work, holdable).
narrative_ontology:cs_axiom_grounding('f9184cd1-a0c0-4869-aeca-f4f3768249dd', linkage_constitutes_single_combined_work, conventional).
narrative_ontology:cs_axiom('f9184cd1-a0c0-4869-aeca-f4f3768249dd', secondary, runtime_coupling_irrelevant_to_derivation_test).
narrative_ontology:cs_axiom_status(runtime_coupling_irrelevant_to_derivation_test, holdable).
narrative_ontology:cs_axiom_grounding('f9184cd1-a0c0-4869-aeca-f4f3768249dd', runtime_coupling_irrelevant_to_derivation_test, instrumental).
narrative_ontology:cs_reference_frame('f9184cd1-a0c0-4869-aeca-f4f3768249dd', gpl_v2_faq_linking_doctrine).
narrative_ontology:cs_drift_state('f9184cd1-a0c0-4869-aeca-f4f3768249dd', post_docker_microservices_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9184cd1-a0c0-4869-aeca-f4f3768249dd', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_project_maintainers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_source_recipients).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_systems_integrators).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_commons_preservation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Release code under GPL expecting that anything linking against it — even dynamically — enters the copyleft orbit. This reading maximizes the reach of their license: it guarantees that improvements and adjacent code contributed by commercial users flow back as visible source, growing the commons they maintain without their having to negotiate case-by-case.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_project_maintainers, beneficiary,
    organized, generational, arbitrage, global).

% End users and downstream developers who receive a linked binary are entitled, under this reading, to the complete corresponding source for the whole combined work. They gain visibility and modification rights they would not have under a narrower reading, but only insofar as vendors actually comply rather than route around the obligation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_source_recipients, beneficiary,
    powerless, biographical, constrained, global).

% Authors the FAQ interpretations and litigation strategy that assert linking (dynamic included) creates a derivative work, and enforces this reading through public pressure campaigns, compliance audits, and litigation support against alleged violators. Sets the interpretive agenda that lower courts and compliance departments treat as the operative standard even where it has not been squarely tested.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, civilizational, analytical, global).

% Want to use GPL libraries' functionality without releasing their own proprietary code as source. Under this reading, any dynamic linkage against a GPL library is treated as forming a single derivative work, forcing a choice between disclosing proprietary source, paying for a commercial dual-license, engineering around the dependency, or accepting litigation risk. Larger vendors can afford to re-implement or license around it; smaller ones often cannot.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Ship firmware that statically or dynamically links against GPL components inside hardware products with long production runs. Re-architecting to avoid the linkage after a product line is already tooled is often not commercially viable, so they either accept the disclosure obligation retroactively, negotiate settlement with rights-holders, or absorb legal exposure they did not budget for.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_systems_integrators, payer,
    moderate, biographical, trapped, national).

% Courts and legal scholars who would apply a narrower, interface-boundary or aggregation-based test are structurally absent from the enforcement conversation in most jurisdictions because the broad reading has rarely been tested to final judgment — most disputes settle before an adjudicator with authority to narrow it ever rules, leaving the FSF's expansive interpretation as the de facto operative norm.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, narrow_reading_adjudicators, excluded,
    institutional, generational, analytical, national).

% Corporate legal departments that advise clients on GPL exposure. They watch enforcement patterns, FSF statements, and settlement terms across many companies and generally counsel clients to comply with the broad reading defensively, since litigating the narrower theory to a final ruling is expensive and rare.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, compliance_counsel, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_project_maintainers).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that anyone who builds commercial value on top of GPL-licensed code contributes their combined work's source back to the commons, preventing free-riding on copyleft software while proprietary derivatives capture the upside without reciprocal disclosure.
% TRANSFER_FUNCTION: Moves de facto control over whether proprietary code must be disclosed from the vendor's engineering choices to the licensing terms of any GPL component it links against; moves visibility and modification rights toward downstream recipients when vendors comply, and moves litigation/compliance risk onto vendors and integrators.
% ABSENT_VOICES: Courts capable of ruling narrowly on the linking question are structurally absent — most disputes settle under threat of injunction and litigation cost before reaching a judgment that could test or narrow the broad reading, so the FSF's interpretation persists largely unchallenged in binding precedent.
% DISAPPEARANCE_RATIONALE: If the broad linking-as-derivation reading were abandoned overnight in favor of a narrow aggregation test, proprietary vendors could link dynamically against GPL libraries without disclosure obligations, commercial embedded use of GPL components would expand substantially, and GPL maintainers would lose significant leverage to compel source contributions back from commercial derivatives — the compliance industry built around GPL linking risk would largely dissolve.
% FOUNDING_PROBLEM: GPL was drafted to prevent proprietary software from capturing the benefits of free software modifications without returning improvements to the commons; the broad linking reading exists to close what its proponents see as an obvious loophole where a vendor could avoid all copyleft obligations merely by keeping proprietary code in a separate file linked at runtime.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and copyleft advocates attest the loophole-closing problem remains live and cite ongoing vendor attempts to link around obligations as evidence. Independent legal scholars and several court opinions in adjacent jurisdictions (addressing similar derivative-work boundary questions in other IP contexts) have expressed skepticism that mere runtime linkage, absent any code copying or tight structural coupling, meets the copyright law definition of a derivative work — this skepticism comes from academic commentary and non-GPL case law outside the beneficiary community, not from vendors alone.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).
:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) rather than high: the reading does impose real disclosure costs on vendors, but it also delivers a genuine, well-documented coordination benefit (commons growth, reciprocity against free-riding) that a pure snare reading would lack. Suppression is higher (0.55) and has risen over the interval as FSF compliance campaigns, GPL Compliance Project litigation support, and corporate legal defensiveness have hardened the reading into a de facto operative standard even without a definitive controlling precedent squarely testing dynamic linking. Theater ratio stays low (0.15) because enforcement activity (audits, compliance letters, settlement negotiations) tracks the substantive disclosure goal rather than degrading into pure performance. Accessibility collapse is moderate (0.45): narrower legal theories remain available in principle and are actively argued by scholars and some vendors, but in practice the settlement dynamic forecloses most opportunities to test them, so alternatives have partially but not fully collapsed.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF and maintainer seats, the broad reading is coordination doing exactly what copyleft was designed to do: closing the linking loophole that would otherwise let proprietary derivatives free-ride on community-maintained code. From the vendor and integrator seats, the same reading operates as an aggressively expansive assertion of scope enforced through litigation threat and settlement pressure rather than settled law, extracting disclosure or engineering costs under a legal theory that has rarely been tested to final judgment. The engine's per-seat computation should reflect this asymmetry: agenda_setter and beneficiary seats trend toward rope, payer seats trend toward tangled_rope or worse depending on their exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Maintainers and the FSF sit near the full-beneficiary end: they collect the coordination benefit (commons growth, reciprocity) and control the interpretive agenda without bearing the disclosure cost themselves. Downstream recipients are moderate beneficiaries whose gain is conditional on vendor compliance. Proprietary vendors and embedded integrators sit toward the target end: constrained or trapped exit options mean the disclosure-or-avoidance choice is largely forced once a product has shipped with the GPL dependency baked in, which is why embedded integrators (trapped, moderate power) experience materially higher effective extraction than large vendors (constrained, powerful) who can afford to re-architect or negotiate dual licenses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing proprietary capture of copyleft improvements via a linking loophole — remains genuinely contested rather than resolved or dead: it is neither purely live (some argue mere linkage without code copying doesn't meet the copyright derivative-work test) nor purely dead (some vendors do actively engineer around GPL dependencies specifically to avoid disclosure, which the FSF reads as evidence the loophole-closing function is still needed). Classifying this as rope rather than snare avoids mislabeling a genuine, if contested, coordination mechanism as pure extraction; classifying it as rope rather than mountain avoids treating a actively-enforced legal interpretation as settled natural law. The moderate suppression and non-trivial resistance in the metrics register that this reading requires active maintenance (compliance campaigns, litigation support) rather than being self-sustaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_as_derivation_legal_status,
    'Does dynamic linking, absent any copying of GPL source code into the linking program, actually satisfy the legal definition of a derivative work under controlling copyright doctrine, or is this an aggressive interpretive extension the FSF has successfully normalized through compliance pressure rather than adjudication?',
    'A final appellate ruling squarely testing whether dynamic linking alone (no static linking, no header inclusion of substantial expressive content) creates a derivative work would resolve this; to date most disputes settle before reaching such a ruling, leaving the question open.',
    'If courts were to adopt a narrow test, this reading''s effective enforceability would collapse toward the narrow_linking_permissive_reading, sharply reducing both suppression and extractiveness; if courts affirmed the broad test, suppression would likely increase further as the interpretation gained binding authority beyond voluntary compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_as_derivation_legal_status, empirical, 'Whether the broad linking-as-derivation theory would survive a definitive judicial test.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three sibling readings of the GPL derivative-work kernel disagree — is it the underlying copyright-law test for ''derivative work,'' the technical characterization of what linking does at runtime, or the policy question of how broadly copyleft should reach to prevent free-riding?',
    'Comparative doctrinal analysis across the three readings'' briefs and FAQ statements would locate whether the dispute is legal-doctrinal (what counts as derivative under 17 U.S.C. 101), technical (what dynamic linking does structurally versus static linking), or normative (how far copyleft''s policy goals should extend regardless of the technical/legal test).',
    'If the disagreement is purely doctrinal, a single controlling precedent could resolve all three readings at once. If it is fundamentally normative (about how far copyleft policy should reach), the readings will persist as coexisting positions indefinitely regardless of any single court ruling, since courts in different jurisdictions could still reach different technical conclusions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel dispute is resolvable by doctrine, technical fact, or is irreducibly a policy disagreement about copyleft''s proper reach.').

omega_variable(
    false_summit_natural_boundary_check,
    'Is the broad reading''s claim that linking creates a single ''work'' tracking a genuine, independently discoverable fact about software architecture (i.e., dynamically linked code genuinely functions as one integrated program), or is ''derivative work'' status here substantially a constructed legal category that happens to benefit the parties who most vigorously assert and enforce it?',
    'Compare technical integration depth (shared address space, ABI coupling, data structure sharing) across cases where the broad reading has been asserted; a genuine natural-boundary account would predict enforcement tracking integration depth, while a constructed-category account would predict enforcement tracking rights-holder identity and litigation resources regardless of integration depth.',
    'If enforcement tracks integration depth, the reading is closer to a principled derivative-work boundary; if enforcement tracks rights-holder resources and settlement leverage rather than technical integration, the reading functions more as leverage than as boundary-drawing, which would push the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_boundary_check, conceptual, 'Whether the linking-derivation boundary is a discovered technical fact or a constructed category serving identifiable enforcement interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(gpl__tr_t2000, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(gpl__tr_t2007, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(gpl__tr_t2014, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(gpl__tr_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2020, 0.14).
narrative_ontology:measurement(gpl__tr_t2026, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1991, 0.22).
narrative_ontology:measurement(gpl__be_t2000, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(gpl__be_t2007, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2007, 0.32).
narrative_ontology:measurement(gpl__be_t2014, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(gpl__be_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(gpl__be_t2026, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement(gpl__su_t2000, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(gpl__su_t2007, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2007, 0.48).
narrative_ontology:measurement(gpl__su_t2014, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2014, 0.52).
narrative_ontology:measurement(gpl__su_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(gpl__su_t2026, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint, narrow_linking_permissive_reading, and interface_boundary_reading are three readings of the single gpl_derivative_work_trigger kernel. Each authors its own ε because each reading produces a structurally distinct constraint: the broad reading (this story) treats linkage itself as sufficient for derivative-work status (moderate extraction, real coordination function, rope); the narrow reading treats only direct code modification as triggering obligations (expected lower extraction, closer to pure rope); the interface_boundary reading treats clean API separation as defeating derivative-work status regardless of coupling tightness (expected to shift extraction toward vendors who fail to maintain clean boundaries, potentially tangled_rope where boundary-cleanliness becomes a contested compliance battleground). The three are linked bidirectionally via affects_constraints because litigation outcomes or FSF policy shifts affecting one reading's enforceability directly shift resource allocation and legal risk calculus for the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
