% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation — Copyleft as Freedom Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL's reciprocity obligation (copyleft) requires that derivative
 *   works carry the same license, preserving user freedoms (to run, study,
 *   modify, distribute) by preventing proprietary capture of community code.
 *   This reading frames the constraint as protecting downstream users from
 *   enclosure: without copyleft, commercial actors could incorporate open
 *   code into proprietary products, stripping users of the freedoms the
 *   original authors intended. The constraint coordinates a global commons by
 *   making reciprocity the price of participation. Proprietary integrators
 *   experience this as suppression — they cannot legally combine GPL code
 *   with proprietary code without releasing their own source. The engine
 *   computes per-seat types from this structural data; the claimed_type
 *   (tangled_rope) reflects the author's structural assessment that both
 *   coordination (user freedom preservation) and asymmetric extraction (from
 *   proprietary integrators) are present and require active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.62).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation — Copyleft as Freedom Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '8916f454-bd57-432f-af5f-76624b9d46e7').
narrative_ontology:cs_kernel_codification('8916f454-bd57-432f-af5f-76624b9d46e7', formalized).
narrative_ontology:cs_authority_grounding('8916f454-bd57-432f-af5f-76624b9d46e7', lineage).
narrative_ontology:cs_interpretation_layer_present('8916f454-bd57-432f-af5f-76624b9d46e7').
narrative_ontology:cs_reading_relation('8916f454-bd57-432f-af5f-76624b9d46e7', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8916f454-bd57-432f-af5f-76624b9d46e7', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('8916f454-bd57-432f-af5f-76624b9d46e7', foundational, user_freedom_requires_reciprocity).
narrative_ontology:cs_axiom_status(user_freedom_requires_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('8916f454-bd57-432f-af5f-76624b9d46e7', user_freedom_requires_reciprocity, deontological).
narrative_ontology:cs_axiom('8916f454-bd57-432f-af5f-76624b9d46e7', secondary, proprietary_enclosure_violates_user_rights).
narrative_ontology:cs_axiom_status(proprietary_enclosure_violates_user_rights, holdable).
narrative_ontology:cs_axiom_grounding('8916f454-bd57-432f-af5f-76624b9d46e7', proprietary_enclosure_violates_user_rights, deontological).
narrative_ontology:cs_reference_frame('8916f454-bd57-432f-af5f-76624b9d46e7', stallman_gnu_manifesto_freedom_framework).
narrative_ontology:cs_drift_state('8916f454-bd57-432f-af5f-76624b9d46e7', contemporary_cloud_saas_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8916f454-bd57-432f-af5f-76624b9d46e7', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_ecosystem).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_entities_avoiding_reciprocity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_ecosystem).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_requires_reciprocity).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, user_control_over_computing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users of GPL-licensed software (individuals, organizations, governments) who receive the four freedoms: to run, study, modify, and distribute the software. They benefit from the constraint because it prevents vendors from stripping these freedoms in derivative works. Their exit is constrained — switching to proprietary alternatives costs migration effort, but the GPL ecosystem itself provides alternatives.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    organized, biographical, constrained, global).

% The collective of developers, projects, and institutions building and maintaining GPL-licensed software. They gain a protected commons where contributions cannot be enclosed, but bear coordination costs: license compliance, contribution tracking, community governance. Their exit is constrained by network effects and ideological commitment.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_ecosystem, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_ecosystem, payer).

% Commercial entities that want to incorporate GPL code into proprietary products without releasing source. The constraint forces a choice: release source (losing proprietary control), rewrite from scratch (high cost), or use alternative libraries (may not exist). Their exit is constrained by the technical necessity of the GPL code and the legal risk of non-compliance.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Companies that adopt permissive-licensed alternatives or build proprietary solutions specifically to avoid GPL obligations. They bear opportunity cost (foregoing GPL ecosystem) and development cost (reinventing functionality). Their exit is mobile — they can and do choose other technical stacks — but the constraint still extracts by narrowing their viable options.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_entities_avoiding_reciprocity, payer,
    powerful, biographical, mobile, global).

% Legal and engineering professionals who enforce GPL compliance within organizations and across the ecosystem. They administer the constraint's operational machinery: scanning, auditing, negotiating settlements. They have arbitrage-grade exit — their skills transfer across licensing regimes.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, license_compliance_officers, agenda_setter,
    institutional, biographical, arbitrage, global).

% The institutional steward of the GPL license family. Sets license terms, pursues enforcement, defines the ideological frame. Their exit is identity-locked — the FSF's institutional identity is constituted by the GPL; abandoning it would dissolve the organization's raison d'être.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_foundation, agenda_setter,
    institutional, generational, identity_locked, global).

% Academics and jurists who interpret the GPL's legal effect, boundary conditions, and interaction with copyright law. They neither collect nor pay; they map the constraint's structural properties for the broader legal system.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, legal_scholars_courts, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates and maintains a global software commons where user freedoms are structurally guaranteed by making reciprocity the condition of participation — solving the collective action problem of enclosure by ensuring no participant can defect to proprietary capture without losing access to the commons.
% TRANSFER_FUNCTION: Transfers the option-value of proprietary enclosure from commercial integrators to downstream users: proprietary integrators lose the ability to privately capture community code; users gain assured freedom to use, study, modify, and distribute all derivatives.
% ABSENT_VOICES: End users who lack technical literacy to exercise the freedoms GPL preserves — they are the nominal beneficiaries but often cannot act on the rights. Also absent: developers in jurisdictions where GPL enforceability is uncertain or untested, who bear compliance uncertainty without clear benefit.
% DISAPPEARANCE_RATIONALE: If the GPL reciprocity obligation vanished overnight, a massive body of code would become available for proprietary enclosure. Commercial actors would rapidly incorporate GPL code into closed products. The free software ecosystem would lose its structural guarantee against enclosure. Downstream users would lose assured freedoms. The software landscape would reorganize toward permissive licensing and proprietary capture within years.
% FOUNDING_PROBLEM: Early free software movement (1980s) faced accelerating enclosure: Unix vendors proprietary-izing academic code, software becoming opaque binaries, users losing control over their computing. The GPL was built to structurally prevent this by making reciprocity a license condition — a 'hack' on copyright law to force sharing.
% FOUNDING_PROBLEM_CORROBORATION: The proprietary enclosure dynamic persists: cloud providers building managed services on open code without contributing back (AWS/Elastic, MongoDB/SSPL), mobile platforms restricting user freedom (iOS), hardware vendors locking down firmware. Independent analyses from Software Freedom Conservancy, EFF, and academic literature (e.g., Weber 'Success of Open Source', Coleman 'Coding Freedom') corroborate that the enclosure pressure the GPL was built to resist remains active.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.28) because the constraint primarily coordinates — it creates a shared infrastructure of free software. Suppression is higher (0.62) because proprietary integration is actively blocked by license incompatibility and legal enforcement, not merely discouraged. Theater ratio is low (0.18) because the enforcement machinery (license compliance, community policing) is functional, not performative. Accessibility collapse is moderate (0.45): alternatives exist (permissive licenses, proprietary development) but the network effects of the GPL ecosystem create switching costs. Resistance is significant (0.55) from commercial actors who view copyleft as a barrier to business models.
 *
 * PERSPECTIVAL GAP:
 *   The copyleft_as_freedom_reading and copyleft_as_restriction_reading will compute opposite directionality profiles for the same agents. For this reading, downstream users are beneficiaries; for the restriction reading, they are victims (of restricted choice). For this reading, proprietary integrators are payers; for the restriction reading, they are beneficiaries (of freedom to integrate). The engine computes each reading's seat types independently — the kernel contest is modeled as separate constraints, not as perspectival variation within one constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users are structural beneficiaries (d near 0.0): the constraint subsidizes their freedoms by denying proprietary integrators the option to enclose the commons. Proprietary integrators are structural targets (d near 1.0): they bear the cost of reciprocity — they must either release source or forgo using GPL code. The open_source_ecosystem as a collective beneficiary sits near symmetric (d ~0.5): it gains coordination but bears maintenance burden. Commercial entities avoiding reciprocity are victims in this reading's frame — they are the ones from whom the constraint extracts the 'freedom to enclose.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary capture of user freedom) remains live — proprietary enclosure of open code continues. However, the constraint has accumulated secondary functions: it now also serves as a signaling mechanism for corporate open-source strategy, a compliance burden for companies, and a boundary marker for the free software movement. These accretions are not the founding problem but are sustained by the same enforcement machinery. The mandate has not atrophied; it has expanded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the structural disagreement between copyleft_as_freedom_reading and copyleft_as_restriction_reading locate?',
    'Trace the beneficiary/victim assignments and suppression mechanism across readings: the freedom reading assigns beneficiary=downstream_users, victim=proprietary_integrators; the restriction reading assigns beneficiary=proprietary_integrators (freedom to choose), victim=downstream_users (loss of freedom). The disagreement is structural, not evaluative.',
    'Confirms this is a genuine kernel with distinct constraint instantiations, not a single constraint with different evaluations. Each reading generates its own ε, beneficiaries, victims, and type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural locus of disagreement between sibling readings of the GPL reciprocity kernel.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the GPL''s reciprocity requirement a genuine coordination mechanism for software freedom, or is it extractive against proprietary integrators?',
    'Measure downstream user freedom preservation in GPL vs. permissive ecosystems; assess whether proprietary integration actually reduces user freedom or merely redistributes commercial value.',
    'If genuine coordination, tangled_rope is correct; if extractive cover, the constraint reclassifies toward snare for the proprietary integrator seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the coordination function (user freedom preservation) is structurally necessary or a cover for extraction from commercial actors.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (license incompatibility, legal enforcement) or does it include internalized norms (developer identity, community pressure)?',
    'Post-exit analysis: track developers who move from GPL to permissive ecosystems — does the suppression experience persist, or does it dissolve with the license choice?',
    'If partially internalized, the constraint''s effective suppression is higher than structural measure suggests; the engine''s directionality derivation would understate extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in copyleft licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1989, 0.1).
narrative_ontology:measurement(gpl__tr_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(gpl__tr_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(gpl__tr_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2007, 0.16).
narrative_ontology:measurement(gpl__tr_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1989, 0.15).
narrative_ontology:measurement(gpl__be_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1995, 0.2).
narrative_ontology:measurement(gpl__be_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(gpl__be_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2007, 0.25).
narrative_ontology:measurement(gpl__be_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2015, 0.27).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1989, 0.3).
narrative_ontology:measurement(gpl__su_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(gpl__su_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(gpl__su_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement(gpl__su_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.08).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_license_adoption).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, corporate_open_source_strategy).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'GPL reciprocity obligation' kernel. The three readings differ in ε (0.28 vs ~0.55 vs ~0.2), beneficiary/victim assignments, and claimed_type. They are linked via affects_constraints to enable contamination analysis across the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, organized, 0.15).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
