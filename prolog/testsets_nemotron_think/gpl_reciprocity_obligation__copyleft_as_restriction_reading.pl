% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation — Copyleft as Business Model Restriction
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'copyleft_as_restriction_reading'
 *   of the GPL reciprocity obligation kernel. From this reading's seat, the
 *   viral licensing requirement (GPL §2(b), GPLv3 §5) operates as a business
 *   model restriction that prohibits proprietary integration of GPL'd code.
 *   The constraint's beneficiary is proprietary_vendors — companies that
 *   avoid GPL code entirely or negotiate commercial licenses, thereby
 *   free-riding on the commons' innovation without reciprocating. The victim
 *   is commons_contributors — developers and projects who contribute to GPL'd
 *   codebases but see their work enclosed in proprietary products via SaaS
 *   loopholes, commercial relicensing, or non-compliance that goes
 *   unenforced. The restriction reading does not deny that the license
 *   creates a commons; it asserts that the commons function is secondary to
 *   the extraction pattern: proprietary vendors capture the value of the
 *   commons (ecosystem, talent pipeline, standards) while the reciprocity
 *   obligation falls asymmetrically on those least able to resist (individual
 *   contributors, small projects). The claimed type is 'snare' because the
 *   coordination story (protecting user freedom) is cover for an arrangement
 *   that enables proprietary capture.
 *
 * KEY AGENTS:
 *   - proprietary_vendors: Primary beneficiary (powerful/arbitrage) — avoids reciprocity, captures commons value
 *   - commons_contributors: Primary victim (moderate/constrained) — contributes code, bears compliance cost, sees work enclosed
 *   - gpl_enforcers: Agenda setter (institutional/generational/analytical) — FSF, Software Freedom Conservancy, courts; administers the license
 *   - corporate_open_source_offices: Secondary beneficiary/payer (organized/biographical/constrained) — manages GPL compliance as cost center, benefits from ecosystem
 *   - permissive_license_advocates: Excluded (organized/biographical/mobile) — argues restriction reading is the true nature of copyleft
 *   - end_users: Observer (powerless/biographical/trapped) — purported beneficiary of freedom preservation, structurally excluded from enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.55).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation — Copyleft as Business Model Restriction").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '2b73ba24-68f2-4dfb-8d30-e404ba22c677').
narrative_ontology:cs_kernel_codification('2b73ba24-68f2-4dfb-8d30-e404ba22c677', formalized).
narrative_ontology:cs_authority_grounding('2b73ba24-68f2-4dfb-8d30-e404ba22c677', lineage).
narrative_ontology:cs_interpretation_layer_present('2b73ba24-68f2-4dfb-8d30-e404ba22c677').
narrative_ontology:cs_reading_relation('2b73ba24-68f2-4dfb-8d30-e404ba22c677', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b73ba24-68f2-4dfb-8d30-e404ba22c677', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('2b73ba24-68f2-4dfb-8d30-e404ba22c677', foundational, proprietary_integration_prohibition_enables_capture).
narrative_ontology:cs_axiom_status(proprietary_integration_prohibition_enables_capture, holdable).
narrative_ontology:cs_axiom_grounding('2b73ba24-68f2-4dfb-8d30-e404ba22c677', proprietary_integration_prohibition_enables_capture, empirically_contingent).
narrative_ontology:cs_axiom('2b73ba24-68f2-4dfb-8d30-e404ba22c677', secondary, saas_loophole_is_structural_not_accidental).
narrative_ontology:cs_axiom_status(saas_loophole_is_structural_not_accidental, holdable).
narrative_ontology:cs_axiom_grounding('2b73ba24-68f2-4dfb-8d30-e404ba22c677', saas_loophole_is_structural_not_accidental, empirically_contingent).
narrative_ontology:cs_reference_frame('2b73ba24-68f2-4dfb-8d30-e404ba22c677', proprietary_software_freedom).
narrative_ontology:cs_drift_state('2b73ba24-68f2-4dfb-8d30-e404ba22c677', contemporary_cloud_saas_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b73ba24-68f2-4dfb-8d30-e404ba22c677', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_open_source_offices).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_open_source_offices).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_commercial_freedom).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, license_compliance_as_business_cost).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Avoid GPL code in proprietary products or negotiate commercial licenses. Capture value from the GPL ecosystem (standards, talent, interoperability) without reciprocating. Their business models are unconstrained by the viral clause because they simply don't incorporate GPL code — or they dual-license. The restriction is a competitive moat: it raises rivals' costs while they free-ride on the commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Contribute code to GPL projects (Linux, GCC, Git, etc.). Bear compliance costs: licensing discipline, contribution agreements, legal review. See their work enclosed in proprietary SaaS (via GPL loophole), dual-licensed commercial products, or non-compliant embeddings. Exit is constrained: rewriting in a permissive license forfeits the ecosystem; switching licenses requires consensus they don't control.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    moderate, biographical, constrained, global).

% FSF, Software Freedom Conservancy, SPI, courts. Administer the license: enforce compliance, educate, maintain the license text. Do not capture extraction — their funding comes from donations, not license fees. Their interest is the commons' survival. They see the restriction reading as a misreading that undermines the license's legitimacy.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcers, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcers, observer).

% Manage GPL compliance for large tech companies (Google, Microsoft, Amazon, Meta). Pay compliance costs: tooling, legal, process. Benefit from the GPL ecosystem (Linux, Kubernetes, toolchains). Push for permissive licenses internally but depend on GPL'd infrastructure. Their dual role reflects genuine tension: they are both constrained by and beneficiaries of the arrangement.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_open_source_offices, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_open_source_offices, beneficiary).

% Advocate for MIT/Apache/BSD licenses as superior to copyleft. Argue the restriction reading is the true nature of GPL: it restricts developers, not users. They are excluded from the GPL governance process (FSF controls the license) but their alternative licenses compete for mindshare. Exit is mobile — they can and do build entire ecosystems without GPL.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_license_advocates, excluded,
    organized, biographical, mobile, global).

% The purported beneficiaries of the freedom reading: users who should receive source and modification rights. In practice, they have no standing to enforce GPL (only copyright holders do), no visibility into SaaS deployments, and no practical ability to exercise freedoms on locked-down devices. They are structurally trapped: the constraint claims to serve them but gives them no leverage.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, end_users, observer,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared codebase that cannot be enclosed: anyone who distributes modified versions must share source. Solves the 'tragedy of the commons' for software by making enclosure legally risky.
% TRANSFER_FUNCTION: Moves development labor and compliance burden from proprietary vendors (who avoid GPL) to commons contributors (who build and maintain GPL'd code). Proprietary vendors capture ecosystem value (standards, talent, interoperability) without reciprocating. The SaaS loophole transfers deployment-value capture entirely to cloud vendors.
% ABSENT_VOICES: End users (the freedom reading's purported beneficiaries) have no seat at the table — no standing to enforce, no voice in license revisions. Small commons projects lack resources to enforce against large corporate violators. The restriction reading's victim (commons_contributors) is fragmented and underrepresented in governance.
% DISAPPEARANCE_RATIONALE: If the GPL reciprocity obligation vanished overnight, proprietary vendors would immediately incorporate GPL'd code into closed products without compliance costs. The commons would lose its legal shield against enclosure. Corporate open source offices would shift to permissive-license-only policies. The ecosystem would reorganize around MIT/Apache-licensed cores with proprietary extensions — a world rearrangement, not stasis.
% FOUNDING_PROBLEM: Prevent proprietary enclosure of free software: in the 1980s-90s, companies took free software, modified it, and distributed binaries without source. The GPL was built to make this legally impossible by requiring source distribution with binaries.
% FOUNDING_PROBLEM_CORROBORATION: The freedom reading (FSF, RMS) attests the problem is live — proprietary enclosure still happens via SaaS, hardware locks, and non-compliance. The restriction reading (permissive-license advocates, some corporate OSPOs) attests the problem is dead for modern deployment — the binary-distribution model is obsolete; the SaaS loophole makes the mechanism ineffective. The commons reading (Conservancy, some kernel developers) attests it's live but the mechanism is failing — enforcement is asymmetric, the commons is eroding. No external corroboration exists for any single status; the three readings' mutual existence is the evidence of contestation.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because proprietary vendors systematically capture commons-generated value (ecosystem effects, talent, standards) without reciprocating — the SaaS loophole, commercial relicensing, and enforcement asymmetry all channel value to them. Suppression (0.55) is moderate: the constraint relies on copyright law and community enforcement, not state coercion, but the legal threat is real and compliance costs are non-trivial. Theater ratio (0.25) is low-moderate: the license has genuine legal teeth and the commons exists, but a growing share of the constraint's operation (SaaS exemption, dual-licensing commercial models) serves extraction. Accessibility collapse (0.45) is moderate: permissive licenses (MIT, Apache, BSD) provide functional alternatives, but the massive installed base of GPL code creates path dependence. Resistance (0.52) is moderate: corporate avoidance of GPL is widespread (Google's 'no GPL in prod' policies, Apple's GPL purge), but the license persists.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat classifications: from proprietary_vendors (d~0.15, arbitrage exit), the constraint is a navigable cost of doing business — possibly rope or weak tangled_rope. From commons_contributors (d~0.8, constrained exit), the same constraint extracts their labor for others' gain — snare. From gpl_enforcers (d~0.3, analytical exit), it is a coordination mechanism under siege — tangled_rope. The restriction reading's structural claim is that the extraction asymmetry is the constraint's stable equilibrium, not a bug.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration (proprietary_vendors) drives d toward 0 for that seat — they avoid the constraint by not using GPL code, and capture ecosystem value. Victim declaration (commons_contributors) drives d toward 1 — they are bound by the license they chose (or inherited), bear compliance costs, and watch proprietary forks/enclosures capture value. The agenda_setter (gpl_enforcers) sits at d~0.3: they administer the constraint but do not capture its extraction; their interest is the commons' survival. Corporate open source offices are dual-role: they pay compliance costs (payer) but benefit from the GPL ecosystem (beneficiary). End users are trapped beneficiaries in theory (freedom preserved) but excluded in practice (no standing to enforce).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary enclosure of free software) is contested: the restriction reading asserts the problem is dead for the modern deployment model (SaaS, cloud), making the constraint a zombie. The freedom reading says the problem is live. The commons reading says it's live but the mechanism is failing. The mismatch (status=contested, disappearance=world_rearranges) flags mandatrophy: the constraint persists because proprietary vendors benefit from its existence as a barrier to entry for new commons-based competitors, while commons contributors lack the power to replace it with a stronger mechanism (e.g., AGPL-only ecosystems).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the GPL reciprocity obligation a single constraint with observer-dependent classification, or are the three declared readings (restriction, freedom, commons) structurally distinct constraints sharing only a label?',
    'Decompose the kernel into three constraint stories with independent ε, beneficiaries, victims, and stakeholder structures. If ε values differ substantially (as the expected structural delta suggests), the kernel is a linguistic conflation, not a single constraint.',
    'If readings are distinct constraints, the restriction reading''s high extractiveness (beneficiary=proprietary_vendors, victim=commons_contributors) stands on its own metrics. If they are one constraint, the engine would average across readings, masking the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the GPL kernel decomposes into multiple constraints per ε-invariance').

omega_variable(
    enforcement_as_extraction_vector,
    'Does active GPL enforcement (by FSF, conservancies, courts) function as coordination maintenance or as a mechanism that disproportionately burdens commons contributors while proprietary vendors free-ride on the commons without reciprocating?',
    'Track enforcement actions: who initiates, who bears legal costs, who captures the resulting code releases. Compare enforcement frequency against proprietary vendor compliance vs. commons project sustainability.',
    'If enforcement primarily extracts compliance from resource-constrained commons projects while proprietary vendors negotiate commercial licenses or avoid GPL code, the constraint operates as snare. If enforcement is symmetric, it leans tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_as_extraction_vector, empirical, 'Whether GPL enforcement symmetry matches the restriction reading''s extraction claim').

omega_variable(
    saas_loophole_as_designed_extraction,
    'Does the AGPL''s existence (and the GPL''s SaaS loophole) confirm that the viral mechanism was never designed to prevent proprietary capture in network-deployed software, making the restriction reading structurally accurate for the modern deployment model?',
    'Historical analysis of GPLv3/AGPL drafting record: was the SaaS exclusion intentional? Compare FSF statements then vs. now. Measure proprietary SaaS built on GPL/AGPL code without source release.',
    'If the loophole was intentional, the restriction reading''s claim (beneficiary=proprietary_vendors) is structurally baked into the license design, not an accident of interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_loophole_as_designed_extraction, empirical, 'Whether the SaaS loophole validates the restriction reading''s beneficiary structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_reciprocity_restriction_tr_t1991, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gpl_reciprocity_restriction_tr_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1999, 0.12).
narrative_ontology:measurement(gpl_reciprocity_restriction_tr_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2007, 0.18).
narrative_ontology:measurement(gpl_reciprocity_restriction_tr_t2015, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(gpl_reciprocity_restriction_tr_t2020, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(gpl_reciprocity_restriction_tr_t2025, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(gpl_reciprocity_restriction_be_t1991, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1991, 0.35).
narrative_ontology:measurement(gpl_reciprocity_restriction_be_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1999, 0.42).
narrative_ontology:measurement(gpl_reciprocity_restriction_be_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(gpl_reciprocity_restriction_be_t2015, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(gpl_reciprocity_restriction_be_t2020, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(gpl_reciprocity_restriction_be_t2025, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl_reciprocity_restriction_su_t1991, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1991, 0.25).
narrative_ontology:measurement(gpl_reciprocity_restriction_su_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1999, 0.35).
narrative_ontology:measurement(gpl_reciprocity_restriction_su_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2007, 0.45).
narrative_ontology:measurement(gpl_reciprocity_restriction_su_t2015, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(gpl_reciprocity_restriction_su_t2020, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement(gpl_reciprocity_restriction_su_t2025, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.08).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, agpl_network_copyleft).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_license_ecosystem).

% DUAL FORMULATION NOTE:
% This constraint family (GPL reciprocity kernel) decomposes into three readings with divergent ε and beneficiary/victim structures. The restriction reading (this story) has high extractiveness (0.68) with proprietary_vendors as beneficiary. The freedom reading would have low extractiveness with end_users as beneficiary. The commons reading would have moderate extractiveness with the commons itself as vindicated proposition. All three share the same license text but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, powerful, 0.15).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, moderate, 0.8).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, institutional, 0.3).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, organized, 0.45).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
