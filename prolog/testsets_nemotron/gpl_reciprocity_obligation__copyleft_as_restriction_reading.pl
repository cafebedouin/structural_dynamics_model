% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: GPL Reciprocity Obligation — Copyleft as Restriction Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'copyleft as restriction' reading
 *   of the GPL reciprocity kernel. In this reading, the viral licensing
 *   requirement is experienced primarily as a constraint on business models:
 *   it prohibits proprietary integration of GPL'd code, creating legal risk
 *   and compliance costs for commercial entities. The beneficiaries are
 *   proprietary vendors, cloud providers, and commercial fork operators who
 *   leverage the restriction narrative to steer the market toward permissive
 *   licenses, SaaS deployments, and CLA-backed proprietary forks. The victims
 *   are commons contributors, copyleft-dependent projects, and community
 *   maintainers whose labor is captured without reciprocity. The constraint
 *   is a tangled rope: it has a genuine coordination function (preventing
 *   commons enclosure) but operates with asymmetric extraction where
 *   commercial entities capture value through legal workarounds while the
 *   commons bears enforcement costs.
 *
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
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation — Copyleft as Restriction Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '41faa7ea-9045-43e3-9be8-d7b6930b94c0').
narrative_ontology:cs_kernel_codification('41faa7ea-9045-43e3-9be8-d7b6930b94c0', formalized).
narrative_ontology:cs_authority_grounding('41faa7ea-9045-43e3-9be8-d7b6930b94c0', lineage).
narrative_ontology:cs_interpretation_layer_present('41faa7ea-9045-43e3-9be8-d7b6930b94c0').
narrative_ontology:cs_reading_relation('41faa7ea-9045-43e3-9be8-d7b6930b94c0', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('41faa7ea-9045-43e3-9be8-d7b6930b94c0', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('41faa7ea-9045-43e3-9be8-d7b6930b94c0', foundational, copyleft_impedes_commercial_adoption).
narrative_ontology:cs_axiom_status(copyleft_impedes_commercial_adoption, holdable).
narrative_ontology:cs_axiom_grounding('41faa7ea-9045-43e3-9be8-d7b6930b94c0', copyleft_impedes_commercial_adoption, empirically_contingent).
narrative_ontology:cs_axiom('41faa7ea-9045-43e3-9be8-d7b6930b94c0', foundational, proprietary_integration_is_legitimate_value_capture).
narrative_ontology:cs_axiom_status(proprietary_integration_is_legitimate_value_capture, holdable).
narrative_ontology:cs_axiom_grounding('41faa7ea-9045-43e3-9be8-d7b6930b94c0', proprietary_integration_is_legitimate_value_capture, conventional).
narrative_ontology:cs_axiom('41faa7ea-9045-43e3-9be8-d7b6930b94c0', secondary, reciprocity_obligation_enables_cla_based_enclosure).
narrative_ontology:cs_axiom_status(reciprocity_obligation_enables_cla_based_enclosure, holdable).
narrative_ontology:cs_axiom_grounding('41faa7ea-9045-43e3-9be8-d7b6930b94c0', reciprocity_obligation_enables_cla_based_enclosure, empirically_contingent).
narrative_ontology:cs_reference_frame('41faa7ea-9045-43e3-9be8-d7b6930b94c0', gplv2_original_reciprocity_architecture).
narrative_ontology:cs_drift_state('41faa7ea-9045-43e3-9be8-d7b6930b94c0', contemporary_cla_saas_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('41faa7ea-9045-43e3-9be8-d7b6930b94c0', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, cloud_providers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commercial_fork_operators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_dependent_projects).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, community_maintainers).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, software_freedom_requires_permissive_licensing).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_integration_is_legitimate_business_practice).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_undermines_commercial_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell proprietary software that can incorporate permissively licensed code without reciprocity obligations. Benefit from the copyleft restriction by framing GPL'd code as legally risky, steering customers toward proprietary alternatives or permissively licensed stacks. Can choose to engage with copyleft or avoid it entirely; their business models are not structurally dependent on GPL code.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Operate managed services that often wrap GPL'd software without triggering distribution clauses (SaaS loophole). Benefit from the perception that copyleft is restrictive and legally hazardous, which drives enterprise adoption of managed services over self-hosted GPL stacks. Shape industry norms through market dominance and legal positioning.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, cloud_providers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, cloud_providers, agenda_setter).

% Maintain proprietary forks of originally copyleft projects by negotiating contributor license agreements (CLAs) or relicensing. The copyleft restriction creates a moat: only entities with legal resources and contributor agreements can legally produce proprietary variants. Capture value from community labor while denying reciprocity to the commons.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commercial_fork_operators, beneficiary,
    powerful, biographical, mobile, global).

% Contribute code, documentation, and maintenance to GPL'd projects expecting reciprocity. Their labor is captured when commercial entities produce proprietary forks via CLAs or SaaS deployments that avoid distribution triggers. Exit options are constrained: switching to permissive licenses surrenders the reciprocity guarantee; abandoning the project loses accumulated reputation and community.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    organized, biographical, constrained, global).

% Projects that structurally depend on the GPL's viral property to maintain their commons (e.g., Linux kernel, GCC, coreutils). The restriction reading undermines their foundational license logic by legitimizing the narrative that copyleft is a business obstacle. Their identity is fused with the GPL as a political and technical commitment; exit would mean abandoning the project's core governance model.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_dependent_projects, payer,
    organized, generational, identity_locked, global).

% Volunteer maintainers who enforce license compliance, review contributions, and sustain project health. Bear the enforcement cost of the reciprocity obligation while commercial beneficiaries free-ride on the maintained commons. Exit is constrained by professional identity, community ties, and the lack of alternative governance structures that protect their labor from proprietary capture.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, community_maintainers, payer,
    moderate, biographical, constrained, global).

% Corporate counsel and specialized firms that advise on GPL compliance, CLA design, and proprietary fork strategies. They administer the constraint by interpreting its boundaries, designing workarounds (SaaS deployments, dynamic linking arguments), and litigating edge cases. Collect rents from both sides: compliance services for users, workaround engineering for vendors.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, license_compliance_lawyers, agenda_setter,
    institutional, generational, arbitrage, global).

% The steward of the GPL family of licenses. Observes the constraint's operation from the authoritative interpretive position, publishing guidance, maintaining license texts, and advocating for the freedom framing. Does not directly collect extraction but legitimizes the kernel's authority.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, free_software_foundation, observer,
    institutional, civilizational, analytical, global).

% Maintains the Open Source Definition and approves licenses. Historically positioned as a pragmatic alternative to the FSF's moral framing. Excluded from the reciprocity debate's core because OSI's approval of permissive licenses and focus on 'business-friendly' licensing aligns with the restriction reading's beneficiary structure, even as it claims neutrality.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_initiative, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_initiative, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The GPL's reciprocity obligation coordinates a global commons by ensuring that improvements to copylefted code flow back to the community, preventing unilateral enclosure of collectively produced software.
% TRANSFER_FUNCTION: Moves the right to create proprietary derivatives from the commons (who produced the code) to commercial entities (who hold CLAs, operate SaaS, or have legal resources to navigate edge cases), as the price of participating in the dominant software economy.
% ABSENT_VOICES: End users who would benefit from copyleft's guarantee of software freedom but lack standing in licensing debates. Small independent developers who cannot afford CLA negotiations or compliance review. Jurisdictions with weaker copyright enforcement where the constraint's suppression mechanism fails entirely.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished overnight, the Linux kernel, GCC, and core infrastructure would face immediate proprietary forking pressure. Cloud providers would capture upstream improvements without contribution. The commons coordination mechanism would collapse, triggering a reorganization of the software economy toward proprietary silos and permissive-license dominance.
% FOUNDING_PROBLEM: Preventing the enclosure of collaboratively developed software by ensuring that any distributed derivative remains under the same terms — the 'viral' mechanism as a commons defense against proprietary capture.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and copyleft_dependent_projects attest the problem is live: proprietary forks via CLAs, SaaS loopholes, and hardware locking (TiVoization) demonstrate ongoing enclosure pressure. Proprietary_software_vendors and cloud_providers attest the problem is substantially solved or exaggerated: they argue the software commons thrives under permissive licensing and that copyleft's restrictions deter commercial investment that would benefit users. Academic research on commons sustainability (Ostrom, Benkler) and empirical studies of license migration (Vendome et al., 2017) provide external corroboration that the enclosure threat persists but takes evolved forms.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness (0.68) is high because the restriction reading enables substantial value capture by commercial entities via CLAs, SaaS loopholes, and proprietary forks, while the commons receives diminished reciprocity. Suppression (0.55) is moderate: the constraint's persistence requires active enforcement (compliance litigation, CLA governance, FSF advocacy) but alternatives (permissive licenses, BSD/MIT) are not fully suppressed — they coexist and compete. Theater ratio (0.25) reflects that the coordination function (commons defense) is real but increasingly performative as workarounds proliferate. Accessibility collapse (0.62) is moderately high: once a project adopts GPL, proprietary integration paths are legally foreclosed without CLA relicensing. Resistance (0.72) is high: commercial entities actively resist through license migration, CLA strategies, SaaS architectures, and lobbying for permissive licensing norms.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary vendor seat, the constraint is a snare (pure extraction via legal risk and compliance cost). From the commons contributor seat, it is a degraded rope (coordination function real but extraction increasing). From the copyleft-dependent project seat, it is a mountain (the reciprocity obligation is the project's constitutional identity). The engine computes this divergence from the structural data: same constraint, different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors, cloud providers, and commercial fork operators are structural beneficiaries (d near 0.0): they collect rents from the commons via proprietary forks, managed services, and the restriction narrative itself, with arbitrage-grade exit (they can avoid GPL code entirely). Commons contributors, copyleft-dependent projects, and community maintainers are targets (d near 1.0): they bear enforcement costs, lose reciprocity value, and have constrained or identity-locked exit. License compliance lawyers are agenda_setters (d ~0.3): they administer the constraint's interpretation and profit from both compliance and workaround engineering. FSF and OSI are analytical observers (d = 0.5) with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (commons enclosure) remains live but has mutated: enclosure now operates through CLAs, SaaS, and hardware locks rather than traditional proprietary forks. The restriction reading gains traction because the original viral mechanism fails against these evolved enclosure forms. The constraint persists not because it solves the current enclosure problem, but because the commons lacks an alternative coordination mechanism — a classic mandatrophy signature where the mandate (reciprocity) outlives its functional adequacy against evolved threats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restriction_vs_freedom_framing,
    'Does the ''restriction'' framing reflect an inherent property of the GPL''s reciprocity mechanism, or is it a strategic narrative advanced by beneficiaries to legitimize permissive licensing and proprietary forks?',
    'Historical analysis of the term ''viral licensing'' origin and adoption; correlation of restriction rhetoric with CLA adoption rates and license migration events; counterfactual modeling of commons sustainability under permissive vs. copyleft regimes.',
    'If the restriction framing is a strategic narrative, the constraint''s extractiveness is partly manufactured — the coordination function is real but the extraction is amplified by beneficiary-driven discourse. If inherent, the high extractiveness is structural to the viral mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restriction_vs_freedom_framing, conceptual, 'Whether the restriction reading describes the constraint''s nature or a beneficiary-advanced cover story.').

omega_variable(
    saas_loophole_extraction_boundary,
    'How much of the measured extraction flows through the SaaS deployment loophole (GPL triggers on distribution, not use), versus through CLA-based proprietary forks, versus through permissive license migration?',
    'Empirical study of commercial GPL usage: survey of cloud provider managed services, analysis of CLA-governed projects'' fork rates, license migration tracking in package ecosystems.',
    'If SaaS loophole dominates, the constraint''s suppression is miscalibrated — the viral mechanism fails against the dominant deployment model. If CLA forks dominate, extraction is mediated by legal infrastructure (enforceable). If license migration dominates, the constraint is losing its coordination function entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(saas_loophole_extraction_boundary, empirical, 'Which extraction pathway dominates in the restriction reading''s operation.').

omega_variable(
    identity_lock_vs_ideological_commitment,
    'Is the ''identity_locked'' exit status for copyleft-dependent projects a genuine structural bind (the project cannot relicense without destroying its governance model), or an ideological commitment that could be revised under pressure?',
    'Case studies of projects that migrated from GPL to permissive licenses (e.g., MongoDB SSPL, Redis BSL, Terraform BUSL) — did the project survive? Did the commons fracture? What happened to contributor retention?',
    'If identity lock is structural, the constraint''s persistence is enforced by the commons'' own governance architecture. If ideological, the constraint is vulnerable to a coordination collapse if maintainers collectively revise their commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_ideological_commitment, conceptual, 'Nature of the exit barrier for copyleft-dependent projects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tr_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tr_t1995, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tr_t2000, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tr_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2007, 0.18).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tr_t2012, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tr_t2018, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tr_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_be_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1989, 0.15).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_be_t1995, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_be_t2000, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_be_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2007, 0.45).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_be_t2012, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2012, 0.54).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_be_t2018, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_be_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_su_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1989, 0.3).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_su_t1995, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_su_t2000, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_su_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2007, 0.48).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_su_t2012, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2012, 0.52).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_su_t2018, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_restriction_reading_su_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.1).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_license_dominance).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, cla_governance_model).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, saas_deployment_loophole).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gpl_reciprocity_obligation kernel. The copyleft_as_freedom_reading and copyleft_as_commons_reading instantiate different constraints from the same kernel with different ε, beneficiary/victim structures, and claimed types. All three are linked via affects_constraints. The restriction reading has higher extractiveness (0.68) because it centers the commercial workaround pathways (CLA, SaaS, migration) as the constraint's actual operation. The freedom reading would show lower extractiveness (coordination function dominant). The commons reading would show moderate extractiveness with different victim structure (the commons as collective victim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, institutional, 0.15).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
