% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Without Reciprocity: Structural Enablement of Proprietary Extraction
 *   domain: intellectual_property/software_governance
 *
 * SUMMARY:
 *   This constraint instantiates the copyleft counterfactual reading of the
 *   permissive-license kernel: permissive licensing without reciprocal
 *   obligation (MIT, Apache 2.0, BSD) structurally enables proprietary
 *   enclosure of commons labor. Under this reading, proprietary vendors
 *   extract open-source work into closed products while the commons
 *   infrastructure that enabled the derivative starves for resources and
 *   attention. The reading asserts that GPL-style viral reciprocity is the
 *   necessary structural alternative: it forces proprietary beneficiaries to
 *   choose between reinvestment in commons, forking, or abandoning the
 *   commons foundation entirely. This reading coexists with two siblings —
 *   the commons-coordination reading (permissive licensing maximizes adoption
 *   and universal implementation freedom) and the corporate-moat reading
 *   (permissive licensing enables proprietary competitive advantage without
 *   commons feedback). The three readings share the same kernel (the
 *   permissive-license text and norm) and disagree fundamentally on what
 *   structural problem it solves and whom it benefits.
 *
 * KEY AGENTS:
 *   - commons_contributors: supply labor; victimized by extraction; identity-locked in the commons-building project
 *   - proprietary_derivative_builders: capture value from commons work; agenda-setters and beneficiaries; institutional power to maintain permissive defaults
 *   - open_source_projects: maintain foundational infrastructure; starved of reinvestment from proprietary vendors; identity-locked
 *   - open_source_foundations: gatekeepers of license norms; constrained by corporate board influence and funding; formally prefer permissive licensing
 *   - copyleft_advocates: observe and contest the extraction structure; excluded from foundation governance; propose GPL-style reciprocity as structural fix
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.71).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Without Reciprocity: Structural Enablement of Proprietary Extraction").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "intellectual_property/software_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'a72d7e31-9c50-4dd4-8d51-9de188c49da3').
narrative_ontology:cs_kernel_codification('a72d7e31-9c50-4dd4-8d51-9de188c49da3', formalized).
narrative_ontology:cs_authority_grounding('a72d7e31-9c50-4dd4-8d51-9de188c49da3', extraction).
narrative_ontology:cs_interpretation_layer_present('a72d7e31-9c50-4dd4-8d51-9de188c49da3').
narrative_ontology:cs_reading_relation('a72d7e31-9c50-4dd4-8d51-9de188c49da3', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a72d7e31-9c50-4dd4-8d51-9de188c49da3', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('a72d7e31-9c50-4dd4-8d51-9de188c49da3', foundational, proprietary_enclosure_requires_reciprocal_control).
narrative_ontology:cs_axiom_status(proprietary_enclosure_requires_reciprocal_control, holdable).
narrative_ontology:cs_axiom_grounding('a72d7e31-9c50-4dd4-8d51-9de188c49da3', proprietary_enclosure_requires_reciprocal_control, deontological).
narrative_ontology:cs_axiom('a72d7e31-9c50-4dd4-8d51-9de188c49da3', foundational, commons_sustainability_depends_on_vendor_reinvestment).
narrative_ontology:cs_axiom_status(commons_sustainability_depends_on_vendor_reinvestment, holdable).
narrative_ontology:cs_axiom_grounding('a72d7e31-9c50-4dd4-8d51-9de188c49da3', commons_sustainability_depends_on_vendor_reinvestment, empirically_contingent).
narrative_ontology:cs_reference_frame('a72d7e31-9c50-4dd4-8d51-9de188c49da3', reciprocal_commons_stewardship).
narrative_ontology:cs_drift_state('a72d7e31-9c50-4dd4-8d51-9de188c49da3', contemporary_proprietary_cloud_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a72d7e31-9c50-4dd4-8d51-9de188c49da3', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, commons_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, open_source_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, corporate_open_source_programs).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, end_users_proprietary_stack).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute code under permissive licenses believing it will remain open and benefit the wider commons. They bear the cost when proprietary actors extract their work into closed-source products without contribution or attribution beyond license compliance. Exit from this model means abandoning the commons-building identity that motivated the contribution in the first place.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commons_contributors, payer,
    organized, generational, identity_locked, global).

% Use permissively-licensed open-source code as the foundation for proprietary products and services. They extract value from commons labor while maintaining exclusive control over enhancements and lock in their users. The permissive license explicitly allows this without reciprocal contribution obligations or even acknowledgment of derivation beyond legal notice.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders, beneficiary).

% Maintain foundational open-source libraries and frameworks under permissive licenses. They receive contributions but watch proprietary vendors extract the work without reinvestment into the commons infrastructure. Their ability to sustain development depends on volunteer labor and sparse corporate sponsorship, while the value they create is concentrated in proprietary derivatives.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_projects, payer,
    moderate, generational, identity_locked, global).

% Run open-source programs as cost-centers for proprietary product lines. They maintain the legal infrastructure (license compliance, policy) that enables extraction, defend permissive licensing in standards bodies and foundations, and coordinate on non-reciprocal license adoption across vendors.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, corporate_open_source_programs, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, corporate_open_source_programs, agenda_setter).

% Benefit from feature-rich proprietary products built on commons foundations. They gain from low proprietary development cost (subsidized by commons labor) reflected as competitive pricing or feature depth. They have no direct access to the underlying commons code and no ability to modify proprietary layers.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, end_users_proprietary_stack, beneficiary,
    powerless, biographical, constrained, global).

% Argue that permissive licensing without reciprocity is structurally unjust: it allows proprietary enclosure of commons gains while commons contributors bear unsustainable labor burdens. They propose GPL-style viral reciprocity as the necessary structural alternative. Their position is excluded from licensing choice at major foundations and standards bodies.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, observer,
    organized, generational, analytical, global).

% Govern license choice and acceptance criteria for hosted projects. They have formally adopted permissive licenses as default, justified as maximizing adoption and institutional participation. This choice shapes what licenses new projects adopt and what existing projects can migrate to. They are constrained by board composition (often corporate-dominated) and funding dependencies on the proprietary vendors who benefit from permissive licensing.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Individual developers choosing licenses for personal projects face social and economic pressure toward permissive licenses (easier corporate adoption, better hiring signals, path to venture funding) while bearing no direct visibility into downstream extraction. Their voice in license governance is absent; foundation and corporate license policies are set by organizations, not by the distributed contributors.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, software_developers_individual, excluded,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing solves the problem of legal friction in multi-party software reuse: without it, every derivative work requires explicit licensing negotiation or risks infringement. The constraint coordinates on a single, minimal-friction legal standard that allows any party to build on prior work without renegotiation.
% TRANSFER_FUNCTION: The constraint moves intellectual property control and economic value from commons contributors to proprietary derivative builders: commons labor is absorbed into proprietary products without reciprocal obligation, while users and institutional control flow to the proprietary layer.
% ABSENT_VOICES: Individual software developers whose contributions are extracted but who have no seat in foundation boards or corporate licensing governance. Copyleft advocates who argue reciprocal licensing is necessary are excluded from major foundation default-license decisions. Communities in the Global South who would benefit from reciprocal commons-reinvestment structures have no formal participation in Western foundation governance.
% DISAPPEARANCE_RATIONALE: If permissive licensing (without reciprocal obligation) disappeared overnight and were replaced by GPL-style viral reciprocity, proprietary vendors would face three paths: contribute enhancements back to commons (reinvestment), fork and diverge (losing commons sync), or abandon the commons foundation entirely. The proprietary stack economy would reorganize around reciprocal models or internalized development; commons projects would receive sustained reinvestment from vendors; end-user products would face cost increases reflecting true commons labor.
% FOUNDING_PROBLEM: Early software licensing was a combative legal landscape: copyright law required explicit negotiation for every derivative; proprietary and open-source development models could not coexist without legal friction. Permissive licensing was proposed to minimize friction and enable universal reuse without renegotiation.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary vendors and open-source foundations attest the founding problem (legal friction in reuse) is solved and permissive licensing is necessary for adoption and commercial participation. Copyleft advocates and commons-focused projects attest the founding problem is partially addressed but at the cost of enabling extraction, and argue reciprocal licensing would solve friction while protecting commons reinvestment. Academic studies (Balter, Nordhaus, GPL+innovation metrics) and GPL adoption data from industries valuing sustainability (scientific computing, infrastructure) support the contested reading.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the constraint's asymmetry: proprietary builders gain exclusive control over derivative work and its profits (d near 1.0), while commons contributors receive no reciprocal obligation or reinvestment (d near 1.0 as victims). The extraction is actively maintained by foundation license-policy choices that make permissive the default and require justification to adopt reciprocal licensing — this is the requires_active_enforcement anchor. Suppression is high (0.71) because the mechanism works by legal permission, not coercion — but the permission structure systematically bars exit from the commons-building identity for contributors (identity_locked exit). Theater is moderate-rising (0.28 to 0.48) because much of the public narrative frames permissive licensing as freedom-maximizing, while the structural effect concentrates economic control. The measurement series shows extraction and suppression both rising over 40 years as proprietary cloud platforms and SaaS vendors have captured larger shares of commons-based infrastructure, while theater rises (increasing emphasis on 'inclusive' and 'permissive' framing as extraction deepens). The tangled_rope classification: genuine coordination function (friction-reduced derivative work) combined with asymmetric extraction (proprietary-layer control without commons reinvestment) maintained by institutional enforcement (foundation policy).
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary-builder seat, the constraint is win-win coordination: they gain frictionless reuse, commons contributors gain attribution and adoption, end-users gain cheaper features. From the commons-contributor and commons-project seats, the constraint is structural exploitation: they provide labor and infrastructure, proprietary vendors extract the product, and the commons starves for resources while the proprietary layer concentrates profits. From the open-source foundation seat, the constraint is policy balance: permissive licenses maximize participation and neutrality, and they believe reciprocal licensing would fragment the ecosystem and reduce adoption. The engine will compute different types from each seat because the structural relationships are genuinely different: proprietary builders compute beneficiary (d~0.1), commons contributors compute target (d~0.9), foundations compute agenda-setter (d~0.5).
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary builders are the primary beneficiaries and agenda-setters: they initiated the push for permissive licensing defaults, they coordinate on adoption across vendors, they have institutional power to maintain foundation policies, and they capture the exclusive value from derivatives (d near 0.0 — full beneficiary seat). Commons contributors are the primary victims: they bear the cost of unsustainable labor burdens as proprietary vendors scale on commons foundations without reinvestment, their exit is identity-locked (leaving the commons-building role means leaving the identity that motivated the work), and they have no institutional seat in governance (d near 1.0 — full target seat). Open-source foundations sit closer to symmetric (d~0.4-0.6): they genuinely coordinate on friction reduction and neutral hosting, but they are constrained by corporate board dominance and funding dependencies, so their governance choices reflect proprietary-vendor preferences while they lack resources to mandate reciprocal reinvestment. Copyleft advocates are observers (analytical seat, d not computed).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal friction in multi-party reuse) is contested as to whether it remains live. Proprietary vendors and foundations say it is live and permissive licensing is necessary to solve it. Copyleft advocates say the founding problem is partially solved but at the structural cost of enabling extraction; they argue the real live problem is now commons sustainability (unsustainable contributor labor, resource starvation for foundational infrastructure) and permissive licensing prevents solving it. The classification as tangled_rope is robust under this contestation: the constraint has a genuine coordination function (friction reduction) AND asymmetric extraction (proprietary control without reciprocal obligation) AND active enforcement (foundation policy maintaining permissive defaults). The mandatrophy tension arises precisely because permissive licensing solved the founding problem AND created a new structural problem (extraction without reciprocal obligation); the constraint persists because the beneficiaries (proprietary vendors) have institutional power to maintain it, while the victimized seats (commons contributors) lack enforcement power or exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_labor_sustainability,
    'Is the measured extraction (0.78) the primary driver of commons-project sustainability crisis, or are other factors (volunteer burnout, education path, funding fragmentation) more significant?',
    'Longitudinal survey of commons-project maintainers asking causal attribution of labor burden; comparison of sustainability metrics across permissive vs. reciprocal license cohorts; funding flow analysis from proprietary vendors to commons infrastructure.',
    'If proprietary extraction is the dominant factor, mandating reciprocal licensing or taxation of proprietary derivatives becomes structural necessity; if other factors dominate, extraction is symptom and suppressing it would not solve sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_labor_sustainability, empirical, 'Attribution of commons sustainability crisis to extraction vs. other structural factors.').

omega_variable(
    identity_lock_mechanism,
    'Is the measured suppression sustained by legal permission structures alone, or is it internalized through commons-contributor identity fusion (belief that commons work ''should'' be free, guilt about commercializing)?',
    'Post-exit ethnography: interview commons contributors who have left open-source work and adopted copyleft-only participation; measure whether suppression persists after structural permission barrier is removed (i.e., if they join a GPL-only project, do they still feel obligated to contribute without reciprocal return?).',
    'If identity-internalized, the effective suppression is higher than the structural permission structure suggests; commons contributors carry the suppression with them. Structural licensing change alone would not address it without identity-reframing work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether suppression is structural (legal permission) or internalized (identity-fused obligation).').

omega_variable(
    reciprocal_licensing_counterfactual,
    'Would GPL-style viral reciprocity actually solve the extraction problem, or would it simply shift the form of extraction (e.g., proprietary vendors fork GPL codebases and declare them ''internal'' to avoid reciprocal obligation)?',
    'Natural experiment from industries where copyleft licensing is dominant (scientific computing, some infrastructure tools); comparison of vendor reinvestment rates, fork-and-diverge patterns, and commons-project funding health under copyleft vs. permissive regimes.',
    'If reciprocal licensing is effective, it becomes the structural alternative to the constraint; if proprietary actors can legally escape reciprocal obligation through forking or internal-use exceptions, it merely relocates the extraction mechanism and remains a tangled_rope under different institutional pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_licensing_counterfactual, empirical, 'Whether copyleft-style reciprocity structurally solves extraction or merely relocates the mechanism.').

omega_variable(
    reading_incompatibility_on_reuse_freedom,
    'Are the commons_coordination_reading and copyleft_counterfactual_reading logically incompatible on the question of what constitutes ''freedom in reuse'' — does freedom mean maximum legal permission (permissive), or does it mean reciprocal control over derivative use (copyleft)?',
    'Normative analysis of philosophical commitments: do the two readings disagree only on empirical outcomes (which licensing regime maximizes benefits) or do they disagree on the foundational definition of freedom itself (unilateral permission vs. reciprocal control)?',
    'If the disagreement is purely empirical, the readings coexist_with and evidence will eventually resolve the dispute. If the disagreement is normative/definitional, the readings foreclose each other — one party''s freedom is the other''s constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incompatibility_on_reuse_freedom, conceptual, 'Whether the readings disagree on empirical outcomes or on foundational definitions of freedom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(perm_tr_t0, observed).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(perm_tr_t5, observed).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(perm_tr_t10, observed).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(perm_tr_t15, observed).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(perm_tr_t20, observed).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(perm_tr_t25, observed).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement_basis(perm_tr_t30, observed).
narrative_ontology:measurement(perm_tr_t40, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(perm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(perm_be_t0, observed).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(perm_be_t5, observed).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(perm_be_t10, observed).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement_basis(perm_be_t15, observed).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(perm_be_t20, observed).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(perm_be_t25, observed).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(perm_be_t30, observed).
narrative_ontology:measurement(perm_be_t40, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(perm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(perm_su_t0, observed).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(perm_su_t5, observed).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(perm_su_t10, observed).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(perm_su_t15, observed).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(perm_su_t20, observed).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(perm_su_t25, observed).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(perm_su_t30, observed).
narrative_ontology:measurement(perm_su_t40, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(perm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.18).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the permissive_license_text kernel. All three readings share the same legal text and institutional norms but instantiate different constraints because they disagree on the structural problem the kernel solves and who benefits. The commons_coordination_reading asserts permissive licensing solves friction and benefits everyone; the corporate_moat_reading asserts permissive licensing creates moats benefiting proprietary builders; the copyleft_counterfactual_reading (this story) asserts permissive licensing enables extraction and copyleft reciprocity is the necessary alternative. Siblings are linked via network.affects_constraints because a change in one reading's dominance (e.g., GPL adoption rising) would alter the structural conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
