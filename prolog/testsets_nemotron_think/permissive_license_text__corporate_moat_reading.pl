% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Corporate Extraction Reading
 *   domain: intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the corporate_moat_reading of the
 *   permissive_license_text kernel. The reading holds that permissive
 *   licenses (MIT, BSD, Apache-2.0) function as a structural enabler of
 *   uncompensated corporate extraction: enterprises capture the maintenance
 *   labor of open source ecosystems as a free input to proprietary products
 *   and cloud services, while the permissive license's 'freedom' framing
 *   provides cover. The sibling readings are commons_coordination_reading
 *   (licenses maximize universal implementation freedom) and
 *   copyleft_counterfactual_reading (reciprocity is necessary to prevent
 *   exploitation). This reading claims snare classification: the coordination
 *   function (universal adoption) is real but the extraction is asymmetric
 *   and the persistence of the arrangement depends on suppressing the
 *   reciprocity alternative (copyleft) through normative framing and network
 *   effects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.55).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.45).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Corporate Extraction Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'd6deb5b2-6ad6-4799-a27a-f35b25e1f4d7').
narrative_ontology:cs_kernel_codification('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', formalized).
narrative_ontology:cs_authority_grounding('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', lineage).
narrative_ontology:cs_interpretation_layer_present('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7').
narrative_ontology:cs_reading_relation('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', foundational, permissive_licensing_enables_uncompensated_corporate_extraction).
narrative_ontology:cs_axiom_status(permissive_licensing_enables_uncompensated_corporate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', permissive_licensing_enables_uncompensated_corporate_extraction, empirically_contingent).
narrative_ontology:cs_axiom('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', secondary, maintainer_labor_is_expropriated_without_reciprocity).
narrative_ontology:cs_axiom_status(maintainer_labor_is_expropriated_without_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', maintainer_labor_is_expropriated_without_reciprocity, empirically_contingent).
narrative_ontology:cs_reference_frame('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', permissive_freedom_maximization).
narrative_ontology:cs_drift_state('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', corporate_cloud_adoption_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6deb5b2-6ad6-4799-a27a-f35b25e1f4d7', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, end_users).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, permissive_licensing_enables_proprietary_enclosure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Incorporate permissively-licensed open source components into proprietary products and cloud services without obligation to contribute changes back or compensate maintainers. Capture the value of maintained infrastructure (Linux, Kubernetes, PostgreSQL, Redis, etc.) as a cost-free input to commercial offerings. Choose permissive licenses strategically for internal projects to maximize downstream adoption while keeping competitive moats.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Perform the bulk of maintenance, security patching, feature development, and user support for widely-used permissively-licensed projects. Receive no mandatory compensation when corporations build proprietary derivatives worth billions. Exit options are constrained: switching to copyleft reduces adoption; demanding payment violates community norms; abandoning the project destroys reputation and career capital built on the project.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    moderate, biographical, constrained, global).

% Receive free, high-quality software infrastructure (operating systems, databases, runtimes, libraries) without direct payment. Benefit from corporate investment in hardening and scaling permissively-licensed projects. Can switch between competing proprietary offerings built on the same open core, but depend on the continued maintenance of the upstream commons.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, end_users, beneficiary,
    organized, biographical, mobile, global).

% Maintain the Open Source Definition and approve licenses (OSI); steward the Free Software Definition and GPL family (FSF). Define what counts as 'open source' and 'free software,' shaping the normative frame through which permissive licenses are understood. Their definitions treat permissive licenses as fully compliant with open source principles, which legitimizes the corporate extraction pattern.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, license_stewards, agenda_setter,
    institutional, generational, analytical, global).

% Argue that permissive licenses structurally enable exploitation and that viral reciprocity (GPL/AGPL) is necessary to protect the commons. Provide the main intellectual counter-narrative to the corporate_moat reading. Their analyses are cited in policy debates but they do not administer licenses or collect extraction.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licenses solve the coordination problem of universal adoption: by imposing near-zero legal friction, they maximize the probability that any given piece of code becomes a shared standard across corporate and community boundaries, enabling interoperability and reducing duplicated effort.
% TRANSFER_FUNCTION: Moves the value of maintenance labor (security patches, features, bug fixes, documentation, community management) from individual maintainers to enterprise corporations who capture it as proprietary product value and cloud service revenue, without reciprocal obligation.
% ABSENT_VOICES: Maintainers who burned out and left the ecosystem entirely are structurally absent — their exit is silent. Would-be contributors who chose copyleft projects instead are absent by self-selection. Small businesses that cannot afford to maintain forks but depend on the upstream are not represented in license governance.
% DISAPPEARANCE_RATIONALE: If the permissive license extraction dynamic vanished overnight (e.g., all permissively-licensed projects switched to strong copyleft), corporations would be forced to either contribute changes upstream or maintain expensive private forks. The economics of cloud infrastructure and proprietary software built on open cores would fundamentally reorganize. Maintainer compensation mechanisms (sponsorship, foundations, dual licensing) would become central rather than peripheral.
% FOUNDING_PROBLEM: Early open source (BSD, MIT, X11) needed maximum adoption to survive against proprietary Unix vendors. Near-zero friction licensing was a survival strategy: make the code impossible to ignore, impossible to exclude, so it becomes the universal substrate.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (survival against proprietary Unix) is attested as dead by the historical record: Linux and *BSD won the server market; proprietary Unix is functionally extinct. The OSI and FSF both acknowledge the historical context but maintain that the license terms remain valid for current coordination needs. No independent analyst argues the original survival threat persists.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate: corporations extract massive aggregate value but any single maintainer's loss is diffuse. Suppression (0.45) is moderate: copyleft alternatives exist and are legally available, but network effects, corporate policy bans on GPL, and the 'business-friendly' framing of permissive licenses suppress adoption of reciprocal licenses. Theater ratio (0.25) is low: the coordination function (universal standards, interoperability) is genuine, not performative. Accessibility collapse (0.4) reflects that alternatives exist but are structurally disadvantaged. Resistance (0.5) reflects maintainer burnout, license switching attempts, and funding experiments (OpenCollective, GitHub Sponsors) that have not structurally altered the dynamic.
 *
 * PERSPECTIVAL GAP:
 *   From the corporate seat, the arrangement is a rope: genuine coordination with mutual benefit (they get infrastructure, maintainers get adoption). From the maintainer seat, it is a snare: the coordination story is cover for extraction they cannot escape. The license steward seat sees a mountain: the Open Source Definition is treated as a fixed, natural categorization. The engine computes this divergence from the structural data — the claimed_type (snare) is this reading's assessment from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are structural beneficiaries (d near 0.0): they collect extraction, have arbitrage-grade exit (can fork, rewrite, or pay), and the constraint subsidizes their business models. Individual maintainers are structural targets (d near 1.0): they bear maintenance costs, have constrained exit (reputation/career locked to project), and the constraint extracts their labor. End users are near-symmetric beneficiaries (d ~ 0.3): genuine coordination benefit, diffuse indirect cost. License stewards are analytical/institutional (d ~ 0.5): they administer the kernel but do not directly extract. Copyleft advocates are analytical observers (d = 0.0 analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (survival against proprietary Unix) is dead, but the arrangement persists and has intensified extraction. This is classic mandatrophy: the mandate ('maximize adoption to survive') has been fulfilled, but the mechanism (permissive licensing) continues operating as an extraction engine for a new beneficiary class (cloud corporations) that did not exist at founding. The classification prevents mislabeling this as pure coordination (rope) by naming the asymmetric extraction and the suppression of the reciprocity alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_necessity,
    'Does the permissive license text *necessarily* enable corporate extraction, or is extraction contingent on corporate strategy and power asymmetries that could exist under any license?',
    'Counterfactual analysis: compare extraction rates for equivalent projects under permissive vs. copyleft licenses, controlling for project importance and corporate dependency. Natural experiments: projects that relicensed (e.g., MongoDB SSPL, Elastic SSPL, HashiCorp BSL) — did extraction decrease?',
    'If extraction is necessary to permissive licensing, the snare classification is structural. If contingent, the constraint may be a tangled_rope where coordination and extraction are separable and the extraction component could be mitigated without destroying the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_necessity, empirical, 'Whether permissive licensing structurally entails extraction or merely permits it.').

omega_variable(
    maintainer_counterfactual_welfare,
    'Would individual maintainers be materially better off under copyleft (GPL/AGPL) given reduced corporate adoption and contribution?',
    'Longitudinal study of maintainer compensation, burnout rates, and project sustainability metrics for matched permissive vs. copyleft projects of similar importance. Survey data on maintainer preferences and perceived agency.',
    'If maintainers are worse off under copyleft, the snare''s victim narrative is complicated — the constraint may be a tragic coordination trap rather than pure extraction. If better off, the snare classification strengthens and the case for structural remedy (license migration) strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maintainer_counterfactual_welfare, empirical, 'Whether the identified victims would actually benefit from the proposed alternative.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the corporate_moat_reading a genuine structural property of the permissive license text, or a contingent reading that projects corporate behavior onto the license?',
    'Compare the three declared readings (commons_coordination, copyleft_counterfactual, corporate_moat) against the license text''s operational semantics in court rulings, compliance practice, and community norms. Identify which readings are textually grounded vs. externally projected.',
    'If the reading is textually grounded, the constraint is a stable property of the kernel. If projected, the constraint is a property of the corporate ecosystem that uses the kernel, and the kernel itself may be neutral — changing the reading would change the constraint classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this reading reflects the kernel''s structure or the observer''s frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(permissive_license_corporate_moat_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(permissive_license_corporate_moat_tr_t6, permissive_license_text__corporate_moat_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(permissive_license_corporate_moat_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(permissive_license_corporate_moat_tr_t18, permissive_license_text__corporate_moat_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(permissive_license_corporate_moat_tr_t24, permissive_license_text__corporate_moat_reading, theater_ratio, 24, 0.25).

% Extraction over time
narrative_ontology:measurement(permissive_license_corporate_moat_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(permissive_license_corporate_moat_be_t6, permissive_license_text__corporate_moat_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(permissive_license_corporate_moat_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(permissive_license_corporate_moat_be_t18, permissive_license_text__corporate_moat_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(permissive_license_corporate_moat_be_t24, permissive_license_text__corporate_moat_reading, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(permissive_license_corporate_moat_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(permissive_license_corporate_moat_su_t6, permissive_license_text__corporate_moat_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement(permissive_license_corporate_moat_su_t12, permissive_license_text__corporate_moat_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(permissive_license_corporate_moat_su_t18, permissive_license_text__corporate_moat_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(permissive_license_corporate_moat_su_t24, permissive_license_text__corporate_moat_reading, suppression_requirement, 24, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_adoption_dynamics).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, open_source_funding_mechanisms).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, cloud_provider_open_core_strategies).

% DUAL FORMULATION NOTE:
% This constraint is one member of the permissive_license_text constraint family (kernel). The commons_coordination_reading claims mountain/rope (low extraction, genuine coordination). The copyleft_counterfactual_reading claims the kernel is structurally defective and proposes a different kernel (GPL family). This reading claims snare. The three readings share the same license text but instantiate different constraints with different ε, beneficiaries, and victims. The family linkage via affects_constraints enables contamination analysis: if copyleft adoption rises, corporate_moat extraction may decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, institutional, 0.1).
constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
