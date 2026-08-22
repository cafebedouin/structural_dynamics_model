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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Broad Copyleft Reading: Linking as Derivative Work Trigger
 *   domain: software licensing / copyright law / open source governance
 *
 * SUMMARY:
 *   This story instantiates the broad copyleft reading of the GPL
 *   derivative-work kernel: dynamic linking against GPL-licensed code is
 *   treated as sufficient to create a derivative work, triggering the source
 *   disclosure obligation regardless of whether the linking vendor modified
 *   the GPL code itself. This reading is advanced by the FSF, the copyleft
 *   maintainer community, and compliance enforcers, and is the doctrinal
 *   basis for a substantial share of historical GPL enforcement actions and
 *   pre-litigation compliance settlements. It is a genuinely contested
 *   reading of the same kernel that also produces the narrow
 *   linking-permissive reading and the interface-boundary reading as separate
 *   constraints (not represented in this file) — each has its own ε, its own
 *   beneficiary/victim structure, and its own classification. This file does
 *   not average across those readings or describe the contest internally; it
 *   authors the broad reading on its own terms.
 *
 * KEY AGENTS:
 *   - copyleft_maintainer_community: agenda_setter (organized/arbitrage) — writes and defends the broad reading
 *   - software_freedom_conservancy_and_enforcers: agenda_setter/beneficiary (organized/arbitrage) — enforces via compliance actions
 *   - gpl_commons_downstream_users: beneficiary (moderate/mobile) — gains source access when trigger applies
 *   - proprietary_software_vendors: payer (powerful/constrained) — bears disclosure obligation or avoidance cost
 *   - commercial_linking_integrators: payer (moderate/constrained) — smaller-scale version of same exposure
 *   - narrow_reading_advocates: excluded (organized/constrained) — holds a live but non-operative rival reading
 *   - courts_and_copyright_offices: observer (institutional/analytical) — adjudicates inconsistently, rarely to final precedent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.55).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "Broad Copyleft Reading: Linking as Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software licensing / copyright law / open source governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '7dda7368-3dda-4c26-ac84-5229140d65c0').
narrative_ontology:cs_kernel_codification('7dda7368-3dda-4c26-ac84-5229140d65c0', fixed_text).
narrative_ontology:cs_authority_grounding('7dda7368-3dda-4c26-ac84-5229140d65c0', lineage).
narrative_ontology:cs_interpretation_layer_present('7dda7368-3dda-4c26-ac84-5229140d65c0').
narrative_ontology:cs_reading_relation('7dda7368-3dda-4c26-ac84-5229140d65c0', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('7dda7368-3dda-4c26-ac84-5229140d65c0', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('7dda7368-3dda-4c26-ac84-5229140d65c0', foundational, functional_coupling_constitutes_derivation).
narrative_ontology:cs_axiom_status(functional_coupling_constitutes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('7dda7368-3dda-4c26-ac84-5229140d65c0', functional_coupling_constitutes_derivation, conventional).
narrative_ontology:cs_axiom('7dda7368-3dda-4c26-ac84-5229140d65c0', secondary, commons_protection_justifies_maximal_trigger_scope).
narrative_ontology:cs_axiom_status(commons_protection_justifies_maximal_trigger_scope, holdable).
narrative_ontology:cs_axiom_grounding('7dda7368-3dda-4c26-ac84-5229140d65c0', commons_protection_justifies_maximal_trigger_scope, instrumental).
narrative_ontology:cs_reference_frame('7dda7368-3dda-4c26-ac84-5229140d65c0', fsf_broad_derivative_work_doctrine).
narrative_ontology:cs_drift_state('7dda7368-3dda-4c26-ac84-5229140d65c0', post_saas_and_dynamic_linking_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7dda7368-3dda-4c26-ac84-5229140d65c0', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_commons_downstream_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_maintainer_community).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, software_freedom_conservancy_and_enforcers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_linking_integrators).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_viral_scope_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and maintains GPL-licensed libraries and enforces the license's terms through community norms, license text, and litigation support. Advocates the broad reading that dynamic linking creates a derivative work, arguing this is necessary to prevent proprietary capture of freely contributed code. Controls the license text and its interpretive tradition.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_maintainer_community, agenda_setter,
    organized, generational, arbitrage, global).

% Brings compliance actions and public pressure against vendors it believes have triggered the derivative-work condition through linking. Its institutional purpose and funding are tied to the broad reading remaining the operative enforcement posture; a narrow reading would shrink its enforcement docket.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_freedom_conservancy_and_enforcers, agenda_setter,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, software_freedom_conservancy_and_enforcers, beneficiary).

% Receive source code and modification rights whenever a vendor's product is found to link against GPL code closely enough to trigger disclosure. Gain leverage and repair rights they would not have under a narrower reading. Their benefit is contingent on enforcement actually happening.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_commons_downstream_users, beneficiary,
    moderate, generational, mobile, global).

% Build products that link, dynamically or otherwise, against GPL-licensed components for functionality gains. Under this reading, that linkage obligates them to release their own source or face infringement claims. They can avoid the obligation only by re-implementing functionality, paying for a commercially licensed alternative, or isolating GPL code behind a process boundary — all of which cost engineering time and money.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Smaller firms and contractors who integrate GPL libraries into client deliverables without the legal staff of large vendors. They face the same triggering condition but with less capacity to negotiate around it, audit their dependency graphs, or absorb litigation risk; many discover the obligation only after a compliance letter arrives.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_linking_integrators, payer,
    moderate, biographical, constrained, national).

% Legal scholars, some judges, and permissive-license advocates who argue linking is aggregation, not derivation, and that only direct modification of GPL source should trigger disclosure. Their reading is a live position in litigation and scholarship but is not the operative enforcement posture in this constraint story; they would object that the broad reading over-extends copyright's derivative-work doctrine to non-copying technical coupling.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, narrow_reading_advocates, excluded,
    organized, generational, constrained, global).

% Adjudicate specific disputes over whether a given linking arrangement crosses the derivative-work line. Their rulings are inconsistent across jurisdictions and rarely reach final appellate resolution because most disputes settle before trial, leaving the broad reading contested but uncollapsed as binding precedent.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, courts_and_copyright_offices, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a commons of freely shared, freely modifiable code by ensuring that anyone who builds functionally on top of GPL code also contributes their own source back, preventing one-way extraction from the shared pool.
% TRANSFER_FUNCTION: Moves compliance cost and disclosure obligation from the commons onto any vendor whose product is found to link against GPL code, and moves source-code access and modification rights from vendors to downstream users when the trigger is found to apply.
% ABSENT_VOICES: Advocates of the narrow linking-is-aggregation reading and of the interface-boundary reading are structurally present in scholarship and litigation but not adopted as the operative enforcement posture in this story; vendors who settled compliance disputes under threat of litigation rather than through adjudicated precedent are also absent from any authoritative resolution of the underlying legal question.
% DISAPPEARANCE_RATIONALE: If the broad linking-triggers-derivative-work reading disappeared overnight in favor of a narrower one, vendors would stop negotiating relicensing deals and stop re-implementing GPL functionality to avoid disclosure; compliance enforcement actions premised on dynamic linking would lose their legal basis; the commons would lose a significant channel through which source code currently flows back from proprietary integrators.
% FOUNDING_PROBLEM: Early free software authors needed a mechanism to prevent proprietary vendors from taking freely contributed code, building commercial products on top of it, and returning nothing to the commons — a one-way extraction problem that voluntary licensing without a strong derivative-work trigger could not solve.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and copyleft maintainer community attest the problem remains live, citing ongoing instances of unlicensed proprietary use of GPL code. Independent legal scholars outside the enforcement community (including some sympathetic to free software) attest that the specific claim — that linking alone, without copying or modification, constitutes derivation — is doctrinally unsettled and has not been squarely tested to final appellate judgment in most major jurisdictions; several note the reading persists more through settlement pressure than adjudicated law.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.42 at interval end) because the broad reading does genuinely restore code and disclosure to the commons — this is not pure rent extraction, it is a coordination mechanism with real asymmetric cost imposed on vendors who did not anticipate the trigger applying to mere linking. Suppression (0.55) reflects that vendors face real compliance/litigation pressure and limited technical workarounds (process isolation, relicensing, reimplementation) rather than free choice. Theater ratio is low (0.2) because enforcement actions, when they occur, produce real source releases or real settlements — the mechanism is not mostly performative, though a growing share of enforcement pressure operates through settlement threat rather than adjudicated doctrine, which the theater_ratio trend captures rising modestly.
 *
 * DIRECTIONALITY LOGIC:
 *   The copyleft maintainer community and enforcement bodies are structural agenda-setters and beneficiaries: they wrote the interpretive tradition and collect compliance (source releases, settlements, precedent value) when the trigger is found to apply. Downstream users benefit contingently — real gains when enforcement succeeds, but their benefit is not self-executing. Proprietary vendors and smaller integrators are the targets: their exit options are constrained (rewrite, relicense, isolate) rather than free, because the coupling was often adopted for real technical reasons before the disclosure implication was understood.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is not a mandatrophy case in the classic sense — the founding problem (one-way extraction from the commons) remains partially live, corroborated by ongoing instances of unlicensed proprietary use. But the specific doctrinal mechanism (linking-as-derivation) is contested precisely because it has never been squarely tested to final appellate judgment; its persistence rests more on settlement pressure than settled law. Classifying this as tangled_rope rather than snare or rope prevents two errors: treating the whole GPL enforcement apparatus as pure extraction (it is not — the commons-building function is real) and treating it as pure voluntary coordination (it is not — vendors face real coercive pressure and the trigger's legal boundary is unsettled, meaning enforcement sometimes operates past what doctrine clearly supports).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_derivation_doctrinal_uncertainty,
    'Does dynamic linking, absent any modification of the GPL-licensed code itself, meet the legal threshold for a derivative work under copyright law, or does it constitute non-infringing aggregation?',
    'A final appellate ruling squarely addressing linking-only integration (not settled pre-trial) in a major jurisdiction, or explicit statutory clarification of the derivative-work standard as applied to software linking.',
    'If courts affirm the broad reading, this constraint''s coordination function is validated and enforcement gains firm doctrinal footing, likely reducing suppression over time as compliance becomes predictable rather than contested. If courts affirm a narrow reading instead, this constraint''s extraction becomes retroactively unsupported by law, and much of its historical enforcement activity would be recharacterized as settlement-pressure extraction rather than legitimate rights vindication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_derivation_doctrinal_uncertainty, empirical, 'Whether linking-only integration meets the legal derivative-work threshold remains judicially untested at the appellate level in most jurisdictions.').

omega_variable(
    settlement_pressure_vs_adjudicated_right,
    'How much of this reading''s practical force comes from vendors settling under litigation-cost pressure versus from courts actually affirming the broad linking-as-derivation theory on the merits?',
    'A systematic review of GPL enforcement case outcomes distinguishing settled-before-ruling cases from cases reaching a merits decision on the linking question specifically.',
    'If most historical compliance was obtained through settlement rather than adjudicated precedent, the reading''s authority is weaker than its enforcement record suggests, and the tangled_rope classification''s extraction component would be understated by looking at settlement volume alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_pressure_vs_adjudicated_right, empirical, 'The gap between enforcement volume and adjudicated legal authority for the broad reading.').

omega_variable(
    commons_versus_control_framing,
    'Is the broad reading better understood as a commons-protection mechanism (preventing free-riding on shared code) or as a control mechanism (the FSF and enforcement bodies extending copyleft''s reach to maximize the scope of obligated code and their own enforcement relevance)?',
    'Compare enforcement priorities and settlement terms in cases with clear free-riding (near-verbatim reuse) against cases with minimal technical coupling (thin linking interfaces) — a control-maximizing pattern would show enforcement pressure applied similarly regardless of coupling depth.',
    'If enforcement pressure correlates with coupling depth (thin coupling pursued less aggressively), the commons-protection framing is supported and the tangled_rope''s coordination component is stronger than its extraction component. If enforcement pressure is uniform regardless of coupling, the control framing is supported and this reading looks more snare-like than the current metrics assume.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_versus_control_framing, conceptual, 'Whether broad-reading enforcement is calibrated to actual free-riding or maximizes scope regardless of technical coupling depth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t6, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t6, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 18, 0.39).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t6, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language label 'the GPL linking/derivative-work question.' All three readings share the kernel gpl_derivative_work_trigger but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and plausibly different classifications: broad_copyleft_reading (this file, tangled_rope, moderate extraction, real commons-protection function alongside real vendor coercion), narrow_linking_permissive_reading (expected lower extraction, closer to rope or mountain from vendor seats since compliance burden shrinks to modification-only), and interface_boundary_reading (expected to carve out a technical safe harbor, likely reducing extraction further for cleanly-bounded integrations while leaving tightly-coupled cases contested). Each sibling should be authored as its own file with its own ε per the ε-invariance principle; this file links to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
