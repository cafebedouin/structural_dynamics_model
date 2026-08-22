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
 *   human_readable: Permissive Open-Source License Without Reciprocity (Copyleft Counterfactual Reading)
 *   domain: software/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   Under the copyleft counterfactual reading, permissive open-source
 *   licenses (MIT, Apache 2.0, BSD) without reciprocity requirements
 *   structurally enable extraction: proprietary vendors incorporate code from
 *   commons maintainers without obligation to contribute improvements back or
 *   fund maintenance. The reading contrasts with sibling readings
 *   (commons_coordination_reading frames permissive licenses as
 *   friction-reducing; corporate_moat_reading frames extraction as
 *   uncompensated derivative value capture). This story instantiates the
 *   copyleft advocate's framing: permissive licensing IS the problem, and
 *   GPL-style viral licensing IS the structural solution. The constraint
 *   exists because permissive-license terms are chosen by major technology
 *   platforms and vendors, not because GPL vendors force the choice — the
 *   reading holds that this choice asymmetrically benefits vendors and harms
 *   commons.
 *
 * KEY AGENTS:
 *   - commons_maintainers: core contributors to open-source projects, bear uncompensated cost; moderate power, globally coordinated
 *   - open_source_communities: collectively maintain shared code that proprietary firms depend on; organized power, no revenue share
 *   - proprietary_derivative_builders: capture commercial value from permissive-license dependencies; powerful institutional actors, high exit options
 *   - corporate_software_vendors: use permissive-license code to reduce development cost and accelerate time-to-market; institutional power, arbitrage exit
 *   - individual_open_source_contributors: donate labor under permissive licenses; powerless, identity-locked (professional identity as open-source contributor)
 *   - copyleft_advocates: argue GPL/AGPL are necessary to prevent extraction; organized, analytical seat — the reading's own reference point
 *   - permissive_license_advocates: excluded from this reading's frame; would argue permissive licensing IS the solution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.52).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive Open-Source License Without Reciprocity (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '91880945-443a-4671-a243-42df9e432e37').
narrative_ontology:cs_kernel_codification('91880945-443a-4671-a243-42df9e432e37', distributed).
narrative_ontology:cs_authority_grounding('91880945-443a-4671-a243-42df9e432e37', practice).
narrative_ontology:cs_interpretation_layer_present('91880945-443a-4671-a243-42df9e432e37').
narrative_ontology:cs_reading_relation('91880945-443a-4671-a243-42df9e432e37', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('91880945-443a-4671-a243-42df9e432e37', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('91880945-443a-4671-a243-42df9e432e37', foundational, viral_reciprocity_structurally_necessary).
narrative_ontology:cs_axiom_status(viral_reciprocity_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('91880945-443a-4671-a243-42df9e432e37', viral_reciprocity_structurally_necessary, deontological).
narrative_ontology:cs_axiom('91880945-443a-4671-a243-42df9e432e37', foundational, commons_colonization_risk_under_permissive_licensing).
narrative_ontology:cs_axiom_status(commons_colonization_risk_under_permissive_licensing, holdable).
narrative_ontology:cs_axiom_grounding('91880945-443a-4671-a243-42df9e432e37', commons_colonization_risk_under_permissive_licensing, empirically_contingent).
narrative_ontology:cs_reference_frame('91880945-443a-4671-a243-42df9e432e37', reciprocal_commons_solidarity).
narrative_ontology:cs_drift_state('91880945-443a-4671-a243-42df9e432e37', contemporary_corporate_open_source_dependency, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91880945-443a-4671-a243-42df9e432e37', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, corporate_software_vendors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, commons_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, open_source_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, individual_open_source_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, individual_open_source_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute code to open-source projects under permissive licenses (MIT, Apache 2.0, BSD). They donate engineering labor and maintenance effort with no contractual guarantee that proprietary derivatives compensate the commons. Their exit from this arrangement means abandoning the project or switching to copyleft, a costly signal that may fragment the community.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commons_maintainers, payer,
    moderate, generational, constrained, global).

% Collectively maintain shared codebases that proprietary firms depend on. They receive no share of proprietary revenue generated from derivatives. Their only leverage is community reputation and the threat of license migration, both costly to exercise and only partially effective once adoption is widespread.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_communities, payer,
    organized, generational, constrained, global).

% Incorporate permissively-licensed code into proprietary products without reciprocal obligation to contribute improvements back or share revenue with commons maintainers. They extract commercial value, insulate their own code from disclosure, and create switching costs that lock users into proprietary ecosystems built on commons foundations.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders, beneficiary,
    powerful, generational, arbitrage, global).

% Use permissive-license dependencies to reduce development cost, accelerate time-to-market, and build proprietary moats. They monetize products that would be prohibitively expensive to build in-house, but face no contractual obligation to fund maintenance of the commons they depend on. They shape industry adoption by which licenses they endorse in their SDKs and platforms.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, corporate_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Contribute under permissive licenses because they believe in open code and community benefit. They absorb the opportunity cost of free contribution while corporations capture commercial upside. Exit means abandoning professional identity as an open-source contributor; staying means accepting structural undercompensation.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, individual_open_source_contributors, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, individual_open_source_contributors, beneficiary).

% Argue that permissive licensing without reciprocity enables corporate extraction and that GPL/AGPL-style viral licensing is necessary to prevent commons colonization. They position themselves as the structural alternative the counterfactual reading holds up.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, observer,
    organized, generational, analytical, global).

% Argue that permissive licensing maximizes freedom and adoption by removing legal friction. They are structurally excluded from this reading's frame: the counterfactual is precisely that permissive licensing IS the mechanism of extraction, not the solution to it.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, permissive_license_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, proprietary_derivative_builders).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework (permissive license terms) allowing developers to build on shared code without negotiating bilateral licenses. Solves the problem of enabling reuse and derivative work without every project requiring legal review and licensing negotiation.
% TRANSFER_FUNCTION: Transfers unpaid engineering labor and maintenance burden from proprietary companies (who absorb commons-maintained code into products) to commons maintainers (who donate it). The transfer is one-way: code flows from commons to proprietary, revenue flows only from proprietary end-users to proprietaries, not back to commons.
% ABSENT_VOICES: Permissive-license advocates (who frame licensing as freedom, not extraction) are excluded by this reading's definitional framing. Their objection would be that the reading mistakes freedom-to-use for extraction, and that the problem is not permissive licensing but corporate failure to voluntarily contribute back.
% DISAPPEARANCE_RATIONALE: If permissive-license terms were removed and replaced with mandatory reciprocal contribution or GPL-style terms, proprietary vendors would either fund commons maintenance (shifting the financial burden back), switch to truly independent code, or negotiate bilateral licenses (raising their development cost). The permissive-license framework enables a cost structure that would be unsustainable under stricter reciprocity.
% FOUNDING_PROBLEM: Early open-source licensing had high friction: GPL's reciprocal terms scared corporate adoption; no license existed that allowed commercial reuse without requiring derivative works to be open. Permissive licenses solved this by removing the reciprocity requirement, accelerating adoption and ecosystem growth.
% FOUNDING_PROBLEM_CORROBORATION: Permissive-license advocates attest the founding problem remains live: corporate deployment of open-source is vast only because permissive licensing reduced friction. Copyleft advocates and commons maintenance researchers (Nadia Eghbal's work on open-source sustainability, Icebreaker.dev surveys) attest the problem is partially solved (permissive licenses DID accelerate adoption) but created a new problem (corporations now extract without compensating). The contest is structural, not empirical — both sides agree on the facts; they disagree on whether the cost structure is justified.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures at 0.68 because permissive licensing creates a one-way transfer: proprietary vendors use commons code without compensating maintenance or contributing improvements. The metric rises from 0.45 to 0.68 over the interval as corporate dependency on open-source deepens and commons funding structures prove inadequate (Nadia Eghbal's research documents increasing supply/demand imbalance in open-source maintenance). Suppression at 0.52 reflects enforcement through legal exclusion (permissive licenses allow proprietary use without reciprocal obligation) and market structure (vendors control SDKs and endorsements, shaping which licenses get adoption). Theater at 0.41 models the performative component: corporate contribution to open-source foundations is real but modest relative to commercial dependency; it functions partly as reputation management for the extraction. The measurement series track extraction accumulation (rising extractiveness plateau at ~0.68 by year 20) and theater ratio growth (corporate 'community' messaging increases while structural compensation lags). All metrics share the same time grid (every point authored at every time interval).
 *
 * PERSPECTIVAL GAP:
 *   From the commons maintainer seat: permissive licensing is a structural mechanism that enables free-riding by wealthy corporations on infrastructure they depend on and could afford to fund. From the proprietary vendor seat: permissive licensing is a coordination solution that reduces friction and enables ecosystem growth; the problem (if any) is that some vendors don't voluntarily contribute back. The engine computes these divergences from power atom, time horizon, and exit options: the commons maintainer (moderate power, generational horizon, constrained exit) and the vendor (institutional power, same horizon, arbitrage exit) experience different effective extraction χ from the same constraint. This reading's framing asserts the maintenance cost is extraction-enabled, not inevitable; the corporate_moat_reading agrees but frames it as derivative-value capture; the commons_coordination_reading denies extraction altogether.
 *
 * DIRECTIONALITY LOGIC:
 *   Commons maintainers and open-source communities sit at the high-extraction end (d → 0.85–0.95): they bear the cost of maintenance, face identity lock (professional reputation depends on contribution), and have constrained exit (switching to proprietary code or copyleft is costly, fragmenting the community they built). Proprietary vendors sit at the beneficiary end (d → 0.05–0.15): they capture commercial value, have arbitrage exit (can leave open-source and build in-house or use alternatives), and face no reciprocal obligation. Individual contributors have asymmetric directionality: as beneficiaries of commons tooling (d → 0.4), they gain reusable infrastructure; as victims of the license structure (d → 0.75), they absorb opportunity cost without compensation. This dual-seated directionality is why individual_open_source_contributors carry secondary_role: beneficiary + payer in the same seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The copyleft counterfactual reading sidesteps mandatrophy collapse by maintaining structural clarity: permissive licensing HAS a founding coordination function (friction reduction, ecosystem growth) but that function is no longer the primary activity driving persistence. The constraint persists because vendors endorse permissive licenses as their default (shaping ecosystem adoption), not because anyone is solving the friction problem anymore. Under this reading, the founding problem (licensing friction) is substantially solved but the constraint remains as a rent-collection mechanism. This is the tangled_rope structure: genuine coordination (code reuse without negotiation) + asymmetric extraction (no reciprocal compensation) + active enforcement (vendors control SDKs and endorsements). Copyleft advocates argue this mandatrophy is evidence that permissive licensing should be replaced, not reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_contribution_counterfactual,
    'Would proprietary vendors voluntarily fund commons maintenance if permissive-license terms remained unchanged but reputational or regulatory pressure increased?',
    'Natural experiments: jurisdictions imposing licensing transparency requirements or EU Digital Markets Act–style requirements on platform gatekeepers; longitudinal tracking of corporate open-source funding relative to commercial dependency.',
    'If vendors fund adequately under reputational pressure, the extraction is not structural but behavioral (culture-fixable without license change). If funding remains inadequate despite pressure, extraction is structural and requires reciprocal licensing terms to remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_contribution_counterfactual, empirical, 'Whether extraction under permissive licensing is behavior-dependent or structure-dependent.').

omega_variable(
    commons_coordination_viability_of_permissive_licenses,
    'Is the friction-reduction coordination function of permissive licensing genuinely necessary to maintain ecosystem growth and interoperability, or could an equivalent or greater commons be built under stronger reciprocity requirements?',
    'Historical comparison of GPL-era ecosystem maturity to permissive-era growth; counterfactual analysis of which libraries and projects would exist under GPL-default assumption; measurement of adoption/network-effects differences between GPL and permissive alternatives in overlapping domains.',
    'If permissive licensing is necessary for ecosystem growth, its extraction cost is the price of coordination at scale. If viable alternatives exist under stricter licensing, permissive licensing''s friction-reduction benefit is lower than justified and its extraction cost is harder to defend.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_coordination_viability_of_permissive_licenses, conceptual, 'Whether the commons coordination function is unique to permissive licensing or whether equivalent coordination is achievable under stronger reciprocity.').

omega_variable(
    commons_identity_lock_mechanism,
    'Is the measured suppression on commons maintainers (d → 0.85–0.95) primarily structural (no viable alternative license that preserves the project) or internalized (contributors have fused their professional identity with permissive licensing ideology)?',
    'Post-license-migration analysis: when projects switch from permissive to GPL (e.g., Redis, ElasticSearch), what share of the original contributor base remains active? Does exit occur after the switch, suggesting the suppression was partly behavioral?',
    'If suppression is internalized, the constraint''s effective extraction is higher than the structural measure suggests — contributors carry the suppression with them even after exit. If structural, the constraint relies on no viable alternative licensing, which is empirically contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_identity_lock_mechanism, empirical, 'Structural vs. internalized suppression in commons maintenance under permissive licenses.').

omega_variable(
    kernel_contest_foreclosure_reading_axiom_strength,
    'Does the copyleft reading''s core axiom (viral reciprocity is structurally necessary) logically foreclose the commons_coordination_reading''s core axiom (permissive licensing maximizes freedom), or do both axioms remain holdable in different institutional contexts?',
    'Logical analysis of axiom claims under different framings: freedom (permissive axiom) vs. solidarity / non-exploitation (copyleft axiom) — are these commensurable or does each entail the other is indefensible?',
    'If axioms foreclose each other, the readings are in genuine logical contradiction (relation: forecloses). If both remain holdable, they coexist as different policy choices (relation: coexists_with). This determines the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_reading_axiom_strength, conceptual, 'Axiom reconciliability across the kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(perm_tr_t0, observed).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(perm_tr_t5, observed).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(perm_tr_t10, observed).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(perm_tr_t15, observed).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(perm_tr_t20, observed).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(perm_tr_t25, observed).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(perm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(perm_be_t0, observed).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(perm_be_t5, observed).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(perm_be_t10, observed).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(perm_be_t15, observed).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(perm_be_t20, observed).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(perm_be_t25, observed).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(perm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(perm_su_t0, observed).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(perm_su_t5, observed).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(perm_su_t10, observed).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(perm_su_t15, observed).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(perm_su_t20, observed).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(perm_su_t25, observed).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(perm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.12).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, open_source_sustainability_funding_gap).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, software_supply_chain_dependency_risk).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the permissive_license_text kernel. All three share the same referent (permissive open-source licensing as a standing commitment) but instantiate different ε values and structural relationships. commons_coordination_reading frames permissive licensing as friction-reducing and coordination-maximizing (low ε). corporate_moat_reading frames the same arrangement as uncompensated extraction but does not advocate license replacement. copyleft_counterfactual_reading (this story) frames permissive licensing as structurally extractive and argues viral reciprocity is the necessary alternative (high ε). The three readings coexist in public discourse but represent different normative commitments to the same kernel. All three are linked via network.affects_constraints to represent their epistemic interdependence: the empirical adequacy of each reading depends partly on the others' empirical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, powerful, 0.08).
constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
