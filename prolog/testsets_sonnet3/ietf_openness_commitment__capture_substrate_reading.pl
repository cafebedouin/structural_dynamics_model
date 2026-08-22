% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment — Capture Substrate Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This story reads the IETF's openness commitment — rough consensus,
 *   running code, open participation — as a coordination substrate that has
 *   been structurally captured: the same procedural openness that once
 *   neutralized vendor gatekeeping now functions as the medium through which
 *   resource advantage is encoded into technical outcomes. Large platform
 *   operators and browser vendors do not need to break the rules; sustained
 *   attendance, editor-pen control, and pre-ratification shipping of
 *   extensions let them win rough consensus through attrition and fait
 *   accompli, while small implementers and end users absorb the resulting
 *   interoperability gaps and lock-in. This is the capture_substrate_reading
 *   of the ietf_openness_commitment kernel — a sibling
 *   commons_stewardship_reading treats the same process as public
 *   infrastructure with negligible extraction, and a sibling
 *   legitimacy_erosion_reading treats the rough-consensus mechanism itself,
 *   rather than its capture, as the contested object. All three share the
 *   same kernel (the openness commitment) but author different epsilon,
 *   different beneficiary/victim structures, and different classifications
 *   from it.
 *
 * KEY AGENTS:
 *   - large_platform_operators: Primary beneficiary (institutional/arbitrage) — shapes specification language through sustained presence and pre-ratification extension shipping
 *   - major_browser_vendors: Co-beneficiary and agenda-setter (institutional/arbitrage) — controls editor pens and interim-meeting cadence
 *   - small_implementers: Primary target (powerless/constrained) — bears interoperability gaps closed only by adopting dominant vendors' extensions
 *   - independent_developers: Secondary target (powerless/trapped) — outlasted procedurally despite formal welcome
 *   - end_users_of_proprietary_extensions: Diffuse target (powerless/trapped) — bears lock-in costs with no standards-process seat at all
 *   - ietf_secretariat: Analytical/administering observer (institutional/analytical) — defends process legitimacy but has no lever over post-ratification vendor extension behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.52).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment — Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'e12065d2-c38e-4a28-981b-71a547ad45a5').
narrative_ontology:cs_kernel_codification('e12065d2-c38e-4a28-981b-71a547ad45a5', distributed).
narrative_ontology:cs_authority_grounding('e12065d2-c38e-4a28-981b-71a547ad45a5', practice).
narrative_ontology:cs_interpretation_layer_present('e12065d2-c38e-4a28-981b-71a547ad45a5').
narrative_ontology:cs_reading_relation('e12065d2-c38e-4a28-981b-71a547ad45a5', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('e12065d2-c38e-4a28-981b-71a547ad45a5', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('e12065d2-c38e-4a28-981b-71a547ad45a5', foundational, formal_openness_insufficient_absent_resource_parity).
narrative_ontology:cs_axiom_status(formal_openness_insufficient_absent_resource_parity, holdable).
narrative_ontology:cs_axiom_grounding('e12065d2-c38e-4a28-981b-71a547ad45a5', formal_openness_insufficient_absent_resource_parity, empirically_contingent).
narrative_ontology:cs_axiom('e12065d2-c38e-4a28-981b-71a547ad45a5', secondary, sustained_participation_capacity_functions_as_de_facto_authority).
narrative_ontology:cs_axiom_status(sustained_participation_capacity_functions_as_de_facto_authority, holdable).
narrative_ontology:cs_axiom_grounding('e12065d2-c38e-4a28-981b-71a547ad45a5', sustained_participation_capacity_functions_as_de_facto_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('e12065d2-c38e-4a28-981b-71a547ad45a5', rough_consensus_running_code_egalitarian_participation).
narrative_ontology:cs_drift_state('e12065d2-c38e-4a28-981b-71a547ad45a5', post_platform_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e12065d2-c38e-4a28-981b-71a547ad45a5', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, major_browser_vendors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, well_resourced_standards_delegations).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, independent_developers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users_of_proprietary_extensions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Send large, well-funded delegations to working groups, author the reference implementations that de facto define ambiguous spec language, and can afford to sit through years of rough-consensus process. Ships proprietary extensions ahead of ratification, then leverages market share to have those extensions retroactively treated as the practical standard. Can walk away from any single working group and still shape the outcome through implementation dominance.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary).

% Co-chair working groups, control editor pens on key drafts, and use engineering headcount to out-participate smaller stakeholders in mailing-list volume and interim meetings, where rough consensus is read from who is still in the room at the end. Their extensions become normative through deployment before the RFC is final.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, major_browser_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, major_browser_vendors, beneficiary).

% Corporate standards teams paid to attend every meeting, track every draft, and file comments at volume. Their persistence advantage lets them steer specification language toward interfaces that favor their existing infrastructure, without needing to win an up-or-down vote — attrition of opposing volunteers does the work.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, well_resourced_standards_delegations, beneficiary,
    organized, generational, mobile, global).

% Build products against the nominal open standard but discover interoperability gaps only closed by implementing the dominant vendor's undocumented or late-published extensions. Cannot staff standing delegations or attend every interim call, so their objections arrive after rough consensus has already formed around the resourced parties' preferred text. Exit means noncompliance with the de facto standard, which means losing customers.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    powerless, biographical, constrained, global).

% Volunteer or lightly-funded participants who join working groups on their own time. Their technical objections are procedurally welcome but practically outlasted: the process rewards whoever can keep showing up, and they cannot match institutional attendance over multi-year standardization cycles. Their names appear in the RFC acknowledgments; the substantive interface choices do not reflect their input.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, independent_developers, payer,
    powerless, immediate, trapped, global).

% Use products built on the extended, non-portable protocol variants without knowing the underlying specification has forked from the nominal open standard. Bear switching costs and lock-in effects created by extensions marketed as compliant with the open standard. Have no seat in the standards process at all.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users_of_proprietary_extensions, payer,
    powerless, immediate, trapped, global).

% Administers the rough-consensus process, publishes RFCs, and defends the legitimacy of the procedure itself. Has no mechanism to weigh participation by resource level rather than by presence, and no enforcement power over post-ratification extension behavior by vendors who shipped ahead of the spec.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_secretariat, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, ietf_secretariat, agenda_setter).

% Would want interoperability guarantees strong enough to prevent vendor lock-in on infrastructure equipment, but rarely have delegations in the room where interface details are actually settled. Their interests are represented, if at all, secondhand through industry associations dominated by the same large vendors.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, smaller_national_and_regional_isps, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves the problem of needing a shared, documented interface so that independently built implementations of a network protocol can interoperate without bilateral negotiation between every pair of vendors.
% TRANSFER_FUNCTION: Moves de facto standard-setting authority from the nominal open, rough-consensus process to whichever parties can sustain the longest, best-staffed presence in working groups and ship extensions fastest — and moves switching costs and compatibility risk onto implementers and users who adopt the nominal standard in good faith.
% ABSENT_VOICES: Smaller ISPs, end users, and volunteer developers rarely have a seat proportional to their stake; their objections, when raised, arrive after resourced parties have already achieved rough consensus through attrition. Public comment periods exist procedurally but do not reweight influence toward the underrepresented.
% DISAPPEARANCE_RATIONALE: The large platform operators would say the open process would be replaced by ad hoc bilateral agreements or a closed consortium model — arguably worse for openness. Small implementers and independent developers would say removing the current process's capture dynamics (without removing the process itself) would restore genuine rough consensus and they would gain a proportionate voice they currently lack. The dispute is over whether the capture is intrinsic to the process or an add-on distortion of it.
% FOUNDING_PROBLEM: Early internet protocol fragmentation threatened basic interoperability — competing vendors each building incompatible network stacks would have balkanized the network before it could achieve scale.
% FOUNDING_PROBLEM_CORROBORATION: Large platform operators and browser vendors attest the founding problem remains live and that their participation is exactly the technical contribution the process was designed to solicit. Independent researchers studying standards-body capture (documented in academic STS literature on IETF/W3C participation asymmetry) and smaller-ISP industry associations attest that the interoperability problem is largely solved at the base-protocol layer and that ongoing struggle is now chiefly over extension-layer control, which the original founding problem does not justify.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, contested).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.58 (rising from 0.34) because the coordination function is genuinely real — a documented shared interface is worth having — but is increasingly ridden by resource-based extraction: the same working-group seat is open to all in form and closed to most in practice. Suppression sits at 0.52 because exclusion here operates less through explicit coercion than through attrition economics (whoever can staff the meetings wins), which is real suppression but structurally softer than an outright exclusionary rule. Theater ratio rises to 0.44 reflecting the growing gap between the process's stated openness (public mailing lists, published minutes, comment periods) and its actual capacity to reweight influence toward underrepresented stakeholders — public participation increasingly documents decisions already settled elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   From the large platform operator seat, this reads as legitimate technical leadership rewarded by sustained investment in the commons — the engine should compute something closer to rope or scaffold from that structural position given their exit options. From the small-implementer and end-user seats, the same substrate computes as extractive: real costs, no proportionate voice, exit blocked by lock-in. The tangled_rope claim is authored because both a genuine coordination function (interoperability) and asymmetric extraction (resource-weighted capture of extension-layer decisions) are simultaneously present and load-bearing — removing either the coordination story or the extraction would misdescribe the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators and major browser vendors are coded as structural beneficiaries with arbitrage-grade exit: they can walk from any specific working group and still shape outcomes through deployed market share, so directionality sits near the full-beneficiary end even though they are formally 'just participants.' Small implementers, independent developers, and end users are coded as targets: constrained-to-trapped exit options mean they cannot avoid the extension-layer costs the substrate produces even when they recognize them. The ietf_secretariat is analytical/institutional but structurally distinct from the vendor beneficiaries — it administers the substrate without capturing its rents, which is why it is not listed as a beneficiary despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protocol fragmentation) is genuinely dead at the base-protocol layer for mature protocols, but the process's mandate has migrated to extension-layer governance without an equivalent reckoning — this is a mandatrophy-adjacent pattern where the original coordination justification is quietly reused to legitimate a different, more extractive activity (extension-layer gatekeeping) than the one it was built to solve. Classifying as tangled_rope rather than snare preserves the fact that base-layer interoperability coordination is real and worth crediting, while the victims and enforcement requirement flag the extension-layer capture as a distinct, additional extractive layer riding on top of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_intrinsic_or_incidental,
    'Is resource-weighted capture of the extension layer an intrinsic property of rough-consensus process design, or an incidental distortion introduced by scale and commercial stakes that a reformed process could correct?',
    'Comparative study of standards bodies with different participation-weighting rules (e.g., W3C''s formal member-organization structure vs. IETF''s individual-participation model) against measured extension-layer capture rates, controlling for market concentration in the covered technology.',
    'If intrinsic, the tangled_rope classification is stable and structural reform must change the coordination mechanism itself; if incidental, targeted reforms (funded travel for underrepresented implementers, weighted comment periods) could shift the constraint back toward rope without abandoning rough consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_intrinsic_or_incidental, conceptual, 'Whether capture is a design-intrinsic feature of rough consensus or a correctable distortion.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (capture_substrate_reading) of the shared ietf_openness_commitment kernel; the commons_stewardship_reading and legitimacy_erosion_reading are separate constraints instantiating the same kernel differently. Where exactly does the disagreement between readings live — in the facts about who currently benefits, in the normative weight given to procedural openness versus outcome equity, or in whether ''capture'' versus ''legitimate technical leadership'' is even a fact-of-the-matter question?',
    'Structured elicitation across the three reading-communities (documented in STS and internet-governance literature) asking each to identify which specific claims (empirical vs. normative vs. definitional) they would revise if shown counter-evidence; map disagreement to claim-type.',
    'If the disagreement is chiefly empirical (about actual influence distribution), it is resolvable by participation-weighted outcome audits. If chiefly normative (about what counts as legitimate influence), no audit resolves it and the three readings persist as genuinely coexisting framings, which is the expectation this story''s cs_structure.reading_relations encodes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating whether the kernel''s sibling readings disagree on facts, values, or definitions.').

omega_variable(
    extension_ratification_lag_measurement,
    'What is the actual time lag between a dominant vendor shipping a proprietary extension and that extension either being ratified into the formal standard or displaced by an alternative, and does that lag correlate with vendor market share at time of shipping?',
    'Empirical audit of RFC history against product-release timelines for a sample of contested extensions (e.g., HTTP/2 push, various WebRTC extensions), correlating lag and outcome with the shipping vendor''s market position.',
    'A strong correlation between market share and successful post-hoc ratification would sharpen the extractiveness measurement and support raising it further in a future revision; a weak correlation would suggest the current 0.58 authored value overstates capture relative to genuinely merit-based adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extension_ratification_lag_measurement, empirical, 'Whether market share predicts extension ratification success, which would corroborate or undercut the capture-substrate claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ietf_tr_t4, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ietf_tr_t8, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ietf_tr_t16, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ietf_be_t4, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ietf_be_t8, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(ietf_be_t16, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ietf_su_t4, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(ietf_su_t8, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(ietf_su_t16, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__capture_substrate_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating the shared ietf_openness_commitment kernel, decomposed per the epsilon-invariance principle: commons_stewardship_reading authors the same standards process as low-extraction public infrastructure (near-mountain/rope, epsilon low by that reading's lights); legitimacy_erosion_reading authors the rough-consensus mechanism's vulnerability to capture as the contested object independent of whether capture is presently occurring (a more procedural/conceptual framing, likely scaffold or piton-adjacent depending on remedial mechanisms in place); this capture_substrate_reading authors moderate-to-substantial extraction (0.58) flowing specifically from resource-asymmetric participation translating into extension-layer gatekeeping, classified as tangled_rope. All three share the founding text, institutional lineage, and procedural kernel of the IETF but diverge in beneficiary/victim structure and epsilon, and are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
