% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Copyleft as Commons-Preserving Reciprocity Mechanism
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL's copyleft clause (Section 2b of GPLv2, Section 5 of GPLv3)
 *   requires that any work 'based on the Program' be licensed as a whole
 *   under the GPL. This reading treats that obligation as an institutional
 *   technology: a mechanism that prevents the enclosure of the software
 *   commons by making reciprocity mandatory rather than voluntary. The
 *   constraint is claimed as a tangled rope because it performs genuine
 *   coordination (sustaining a global collaborative development commons)
 *   while extracting asymmetric costs from actors who would otherwise
 *   free-ride on the commons without contributing back. The engine will
 *   compute per-seat classifications from the structural data; the authored
 *   claim does not adjudicate the divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.35).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Copyleft as Commons-Preserving Reciprocity Mechanism").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '563283d6-4a41-44a5-82a4-f540eb43615e').
narrative_ontology:cs_kernel_codification('563283d6-4a41-44a5-82a4-f540eb43615e', fixed_text).
narrative_ontology:cs_authority_grounding('563283d6-4a41-44a5-82a4-f540eb43615e', lineage).
narrative_ontology:cs_interpretation_layer_present('563283d6-4a41-44a5-82a4-f540eb43615e').
narrative_ontology:cs_reading_relation('563283d6-4a41-44a5-82a4-f540eb43615e', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('563283d6-4a41-44a5-82a4-f540eb43615e', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('563283d6-4a41-44a5-82a4-f540eb43615e', foundational, reciprocity_sustains_commons).
narrative_ontology:cs_axiom_status(reciprocity_sustains_commons, holdable).
narrative_ontology:cs_axiom_grounding('563283d6-4a41-44a5-82a4-f540eb43615e', reciprocity_sustains_commons, empirically_contingent).
narrative_ontology:cs_axiom('563283d6-4a41-44a5-82a4-f540eb43615e', foundational, copyleft_prevents_enclosure).
narrative_ontology:cs_axiom_status(copyleft_prevents_enclosure, holdable).
narrative_ontology:cs_axiom_grounding('563283d6-4a41-44a5-82a4-f540eb43615e', copyleft_prevents_enclosure, empirically_contingent).
narrative_ontology:cs_reference_frame('563283d6-4a41-44a5-82a4-f540eb43615e', gnu_manifesto_founding_commitment).
narrative_ontology:cs_drift_state('563283d6-4a41-44a5-82a4-f540eb43615e', contemporary_cloud_saas_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('563283d6-4a41-44a5-82a4-f540eb43615e', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_institution).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, collective_maintainers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizing_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, reciprocity_sustains_commons).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_prevents_enclosure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The commons as a persistent institutional structure — the body of GPL-licensed code, its governance norms, and the community that maintains it. It benefits from the reciprocity obligation because every derivative work that incorporates GPL code must return its modifications to the same pool, preventing the commons from being drained into proprietary forks. It does not 'collect' rents; it persists and grows through the mandatory return flow.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_institution, beneficiary,
    institutional, generational, analytical, global).

% Developers and organizations who steward GPL projects (kernel maintainers, FSF, GNU project, major upstream projects). They benefit from the reciprocity obligation because it guarantees their labor is not captured by downstream proprietary forks. They also set the agenda: they interpret license scope, enforce compliance, and shape community norms. Their exit is identity-locked — their professional identity and reputation are fused with the commons they maintain.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, collective_maintainers, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, collective_maintainers, agenda_setter).

% End users, distributors, and organizations that deploy GPL software. They benefit from the obligation because it ensures the software they depend on remains free and modifiable, and that improvements flow back. Their exit is relatively mobile — they can switch to alternative software stacks, though switching costs exist for deeply embedded infrastructure.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users, beneficiary,
    organized, biographical, mobile, global).

% Developers or firms who want to incorporate GPL code into proprietary products without reciprocating. They bear the cost of the constraint: they must either release their derivative work under GPL (surrendering proprietary control), rewrite the GPL components from scratch (high engineering cost), or abandon the project. Their exit is constrained — they can avoid GPL code, but the ubiquity of GPL in infrastructure (Linux, GCC, core libraries) makes complete avoidance costly.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizing_developers, payer,
    moderate, biographical, constrained, global).

% Companies building proprietary software stacks who want to integrate GPL components (e.g., linking GPL libraries, embedding GPL tools). They bear the extraction cost directly: the viral clause forces a binary choice — open the whole stack or excise the GPL code. Their exit is constrained by the technical necessity of the GPL components and the cost of clean-room reimplementation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Advocates and projects using MIT, BSD, Apache licenses who argue that copyleft's mandatory reciprocity is itself a form of coercion that reduces adoption and fragments ecosystems. They would object to the characterization of GPL as a commons-preserving mechanism, viewing it instead as a barrier to maximal code reuse. They are excluded from the GPL governance conversation because they operate outside the copyleft frame.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, permissive_license_advocates, excluded,
    organized, generational, arbitrage, global).

% Legal practitioners, courts, and compliance officers who interpret and enforce the GPL's terms. They see the full structure: the reciprocity obligation as a legal mechanism, its enforcement history (BusyBox, VMware, etc.), and the strategic behavior it induces. They neither collect nor pay; they adjudicate.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, license_compliance_lawyers, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of commons maintenance: without mandatory reciprocity, rational actors would extract value from the commons (incorporate GPL code into proprietary products) without contributing back, leading to enclosure and depletion of the shared resource.
% TRANSFER_FUNCTION: Moves derivative works and modifications from exit-maximizing developers and proprietary integrators back into the commons pool, as the price of using the upstream GPL code. The transfer is not monetary but code-rights: the right to use the upstream code is conditioned on licensing the combined work under GPL.
% ABSENT_VOICES: Permissive-license advocates and maximalist proprietary vendors are structurally excluded from the GPL governance frame. They would argue that the reciprocity obligation reduces total code reuse and creates license incompatibility friction, but they do not participate in GPL compliance or stewardship.
% DISAPPEARANCE_RATIONALE: If the GPL reciprocity obligation vanished overnight, the Linux kernel, GCC, core GNU tools, and vast swathes of infrastructure could be incorporated into proprietary forks without source return. The commons would fracture: upstream maintainers would lose visibility into downstream modifications, security fixes would not flow back, and the collaborative development model would degrade toward a 'throw it over the wall' dynamic. The software ecosystem would reorganize around proprietary capture of formerly shared infrastructure.
% FOUNDING_PROBLEM: The pre-GPL software landscape of the 1980s: proprietary Unix vendors enclosing collaborative code, binary-only distribution preventing modification, and no legal mechanism to ensure improvements to shared code returned to the community. The GPL was built to solve the enclosure of collaborative software development by making reciprocity a license condition.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and GNU project attest the founding problem is live — proprietary enclosure remains the default incentive without copyleft. Major corporate Linux kernel contributors (Red Hat, Intel, Google) corroborate from outside the pure beneficiary set: they attest that the reciprocity obligation aligns their commercial incentives with upstream contribution, and that without it, kernel development would fragment into vendor forks. Permissive-license advocates contest the framing, arguing the problem was solved by social norms, not license compulsion.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.42) because the constraint imposes real costs on exit-maximizing developers and proprietary integrators (they must open their derivative works or pay reimplementation costs), but the extraction is not pure rent — it funds the commons' maintenance. Suppression is moderate (0.35): enforcement is active (compliance actions, legal pressure) but alternatives exist (permissive-licensed alternatives, clean-room reimplementation). Theater ratio is low-moderate (0.28): the compliance machinery is largely functional, though performative 'license purity' signaling exists at the margins. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives persist (BSD/MIT ecosystems thrive) and the constraint meets organized resistance from proprietary vendors and permissive advocates. The measurement series show a gradual rise in all three metrics over 35 years, tracking the GPL's expansion from a niche license to dominant infrastructure governance and the corresponding hardening of enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the commons_institution and collective_maintainers seats, the constraint is experienced as a rope (genuine coordination with minimal coercive overhead). From the exit_maximizing_developers and proprietary_integrators seats, it computes as a snare (the coordination story is cover for extraction that suppresses their preferred business model). The engine's per-seat computation will capture this divergence; the authoring task is to declare the structural data honestly.
 *
 * DIRECTIONALITY LOGIC:
 *   The commons_institution and collective_maintainers are structural beneficiaries (d near 0.0-0.2): the constraint subsidizes them by guaranteeing return flows. Downstream_users sit near symmetric (d ~0.4-0.5): they gain commons stability but bear switching costs. Exit_maximizing_developers and proprietary_integrators are targets (d near 0.8-1.0): they bear the asymmetric extraction. Permissive_license_advocates are excluded — their exit is arbitrage-grade (they operate in a different license regime). License_compliance_lawyers are analytical observers. The derivation chain from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of collaborative code) remains contested — not dead. The constraint has not atrophied into a piton: its enforcement machinery is active, its coordination function is demonstrably load-bearing (Linux kernel development model depends on it), and its sunset clause is absent by design (perpetual reciprocity is the point). However, the rising theater_ratio and extractiveness suggest creeping mandatrophy risk: as the commons matures, the marginal coordination benefit of mandatory reciprocity may decline while the extraction cost to new entrants rises. The 'contested' status on founding_problem captures this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_vs_freedom_beneficiary_ambiguity,
    'Is the primary structural beneficiary of the GPL''s reciprocity obligation the commons as an institution, or the individual end user''s freedom?',
    'Counterfactual analysis: if a license guaranteed user freedom (run, study, modify) but did NOT require derivative works to be shared back (e.g., a ''copyfarleft'' that only restricts anti-features), would the commons persist? Historical comparison with permissive-licensed ecosystems (BSD, MIT) that grant user freedom without reciprocity.',
    'If the commons is the primary beneficiary, the constraint is a tangled rope (coordination + asymmetric extraction from free-riders). If the end user is the primary beneficiary, the constraint leans toward rope (coordination for user benefit, extraction is incidental). This changes the victim/beneficiary mapping and the mandate analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_vs_freedom_beneficiary_ambiguity, conceptual, 'Whether the GPL''s reciprocity primarily serves commons persistence or user freedom.').

omega_variable(
    viral_scope_boundary,
    'Where does the GPL''s ''derivative work'' boundary lie for modern software architectures (dynamic linking, microservices, API boundaries, containerization)?',
    'Court rulings on linking (e.g., VMware vs. Christoph Hellwig, ongoing), FSF guidance evolution (GPLv3''s ''convey'' language), and community consensus on edge cases (systemd, kernel modules, SaaS).',
    'A broad viral scope increases extraction on proprietary integrators (more code captured) and suppression (fewer integration paths). A narrow scope reduces both, potentially shifting the constraint toward rope. The ambiguity is structural — the license text is ambiguous and the technical landscape has evolved beyond its drafting assumptions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(viral_scope_boundary, empirical, 'Uncertainty in the technical scope of the reciprocity obligation.').

omega_variable(
    committer_framing_kernel_reading,
    'This constraint is one reading (copyleft_as_commons_reading) of the contested kernel ''gpl_reciprocity_obligation''. How does this reading''s structural classification differ from its siblings?',
    'Generate the sibling readings as separate constraint stories and compare their computed per-seat classifications, beneficiary/victim structures, and epsilon values.',
    'If the sibling readings produce materially different classifications, the kernel is genuinely contested and the decomposition is warranted. If they converge, the kernel framing may be over-discriminating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_kernel_reading, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1989, 0.1).
narrative_ontology:measurement(gpl__tr_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(gpl__tr_t2000, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(gpl__tr_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2007, 0.22).
narrative_ontology:measurement(gpl__tr_t2014, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1989, 0.25).
narrative_ontology:measurement(gpl__be_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(gpl__be_t2000, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(gpl__be_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2007, 0.38).
narrative_ontology:measurement(gpl__be_t2014, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1989, 0.15).
narrative_ontology:measurement(gpl__su_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(gpl__su_t2000, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(gpl__su_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2007, 0.3).
narrative_ontology:measurement(gpl__su_t2014, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2014, 0.33).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.15).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, lgpl_library_exception).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, agpl_network_copyleft).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, mit_permissive_license).

% DUAL FORMULATION NOTE:
% This constraint is one member of the gpl_reciprocity_obligation kernel family. The three readings share the same license text (the kernel) but instantiate different constraints with different beneficiary/victim structures and epsilon values. The commons reading (this story) has medium extractiveness (0.42) with commons_institution as primary beneficiary. The freedom reading centers user_freedom as beneficiary with lower extractiveness. The restriction reading centers proprietary_vendor as victim with higher extractiveness. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
