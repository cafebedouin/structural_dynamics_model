% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Derivative-Work Scope Under Judicial Non-Resolution (Enforcement Vacuum Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b)'s derivative-work boundary has never been definitively
 *   settled by appellate precedent for the hardest cases (dynamic linking,
 *   plugin architectures). Rather than resolving into one dominant
 *   interpretation, the ambiguity has stabilized into a durable plurality:
 *   FSF-aligned maintainers enforce a strong reading within communities where
 *   they hold reputational and organizational leverage, while large
 *   commercial ecosystems operate on a narrow reading with little practical
 *   risk of challenge. This constraint story documents that
 *   stabilized-plurality condition itself as a structural fact with its own
 *   beneficiaries, victims, and coordination/extraction mixture — distinct
 *   from either substantive reading of what the license actually requires.
 *
 * KEY AGENTS:
 *   - fsf_aligned_maintainers: enforcement-capable within their communities, near-powerless outside them
 *   - large_platform_vendors: beneficiaries of the vacuum via scale-enabled risk absorption
 *   - dual_licensing_companies: beneficiaries who profit directly from ambiguity persisting
 *   - small_commercial_adopters: bear uncompensated transaction cost of unresolved doctrine
 *   - courts_and_legislatures: structurally absent; their non-intervention is load-bearing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.31).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Derivative-Work Scope Under Judicial Non-Resolution (Enforcement Vacuum Reading)").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '5eb16e92-750c-499e-97b2-34af7808a3d5').
narrative_ontology:cs_kernel_codification('5eb16e92-750c-499e-97b2-34af7808a3d5', distributed).
narrative_ontology:cs_authority_grounding('5eb16e92-750c-499e-97b2-34af7808a3d5', distributed).
narrative_ontology:cs_reading_relation('5eb16e92-750c-499e-97b2-34af7808a3d5', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('5eb16e92-750c-499e-97b2-34af7808a3d5', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('5eb16e92-750c-499e-97b2-34af7808a3d5', foundational, interpretive_plurality_is_operative_law).
narrative_ontology:cs_axiom_status(interpretive_plurality_is_operative_law, holdable).
narrative_ontology:cs_axiom_grounding('5eb16e92-750c-499e-97b2-34af7808a3d5', interpretive_plurality_is_operative_law, conventional).
narrative_ontology:cs_axiom('5eb16e92-750c-499e-97b2-34af7808a3d5', foundational, enforcement_capacity_determines_practical_scope).
narrative_ontology:cs_axiom_status(enforcement_capacity_determines_practical_scope, holdable).
narrative_ontology:cs_axiom_grounding('5eb16e92-750c-499e-97b2-34af7808a3d5', enforcement_capacity_determines_practical_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('5eb16e92-750c-499e-97b2-34af7808a3d5', gpl_v2_drafting_era_undertheorized_linking_boundary).
narrative_ontology:cs_drift_state('5eb16e92-750c-499e-97b2-34af7808a3d5', post_saas_and_dynamic_linking_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5eb16e92-750c-499e-97b2-34af7808a3d5', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, large_platform_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, dual_licensing_companies).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, legal_intermediary_firms).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, small_commercial_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, compliance_uncertain_startups).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, downstream_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_flexible_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_flexible_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain projects under a strong-copyleft reading and threaten or pursue enforcement action against perceived violations within communities where they have standing and reputational leverage. Their enforcement capacity is real inside FSF-aligned ecosystems but drops sharply outside them, so the same license text produces different practical constraint depending on which community is watching.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_maintainers, agenda_setter,
    organized, generational, mobile, global).

% Operate at a scale where they can fund legal teams to adopt the narrow-scope reading with confidence, absorb litigation risk if challenged, and shape industry practice toward permissive interpretations through sheer market weight. The absence of precedent lets them treat GPL scope as a negotiable business risk rather than a fixed obligation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, large_platform_vendors, beneficiary,
    institutional, civilizational, arbitrage, global).

% Sell commercial licenses as an escape from the very ambiguity they benefit from preserving: the scarier and more contested the copyleft boundary appears, the more valuable their proprietary alternative becomes. They have no incentive to see the question resolved.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, dual_licensing_companies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, dual_licensing_companies, agenda_setter).

% Provide licensing-risk audits, compliance opinions, and litigation defense whose market exists only because the derivative-work boundary is unsettled. Billable hours scale with the persistence of the ambiguity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, legal_intermediary_firms, beneficiary,
    organized, biographical, arbitrage, national).

% Must decide whether to link against GPL-covered code without the resources to commission a legal opinion or withstand an enforcement action, however unlikely. They either over-comply by avoiding useful GPL components entirely or under-comply and carry unquantified exposure that can be selectively activated by whichever interpretive community notices them.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, small_commercial_adopters, payer,
    powerless, immediate, constrained, national).

% Have already built products on architectures whose licensing status is genuinely contested (plugin boundaries, dynamic linking) and cannot cheaply re-architect. Investors and acquirers treat the ambiguity as a due-diligence risk, depressing valuation regardless of which reading is eventually correct.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, compliance_uncertain_startups, payer,
    powerless, biographical, trapped, national).

% Combine components from multiple upstream projects, each governed by communities with different enforcement postures and different de facto readings of the same license text. They inherit compounded uncertainty they did not create and cannot resolve by reading the license more carefully.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, downstream_integrators, payer,
    moderate, biographical, constrained, global).

% Deliberately exploit the interpretive plurality: they adopt whichever reading is locally convenient, betting that low enforcement capacity in their specific ecosystem makes the aggressive reading unlikely to be tested against them. They benefit from the vacuum precisely because they have the sophistication to navigate it, unlike less-resourced adopters facing the same text.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_flexible_adopters, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_flexible_adopters, payer).

% Have not been presented with, or have not resolved, a case that would settle the derivative-work boundary for software linking. Their absence from the field is not neutral — it is the structural condition that makes the enforcement-vacuum reading possible at all.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts_and_legislatures, excluded,
    institutional, civilizational, analytical, national).

% Study enforcement patterns, settlement terms, and community norms across ecosystems to map where the strong reading is actually live and where the narrow reading effectively governs, without any single authoritative doctrine to cite.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, license_compliance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Where it functions as coordination, the GPL's scope ambiguity lets diverse communities converge on shared code while each interpretive community enforces the boundary consistent with its own norms and capacity — FSF-aligned projects hold a strict line, industry-dominated ecosystems hold a loose one, and both groups get usable shared infrastructure without a single global rule being imposed on all of them.
% TRANSFER_FUNCTION: Moves compliance risk and legal-uncertainty cost from sophisticated, well-resourced adopters (who can price and navigate the ambiguity, or fund the interpretation favorable to them) onto small commercial adopters and startups who cannot afford legal opinions and must either over-comply at a competitive cost or under-comply and carry latent exposure.
% ABSENT_VOICES: Courts and legislatures that could settle the derivative-work boundary have not done so; their absence is structurally load-bearing rather than incidental. Small adopters who bear the transaction cost of the ambiguity are rarely present in the standards and community conversations where enforcement norms are set.
% DISAPPEARANCE_RATIONALE: A definitive appellate ruling settling the derivative-work boundary for dynamic linking and plugin architectures would collapse the plurality overnight: dual-licensing business models built on the ambiguity would need to reprice, legal-risk-audit demand would fall sharply, and adopters currently hedging between readings would consolidate on whichever rule won, redrawing which projects are viable to build commercially.
% FOUNDING_PROBLEM: Copyleft licensing was built to prevent proprietary capture of shared code by requiring that derivative works remain under compatible terms; the derivative-work boundary needed to be broad enough to prevent easy circumvention through trivial technical repackaging.
% FOUNDING_PROBLEM_CORROBORATION: FSF-aligned communities attest the anti-circumvention purpose is still live and requires the strong reading to hold. Independent legal scholars and several appellate-level amicus filings from outside both the FSF and industry vendor camps have noted that the original drafters' intent does not resolve the specific dynamic-linking question, and that the persistent non-litigation of key cases (rather than doctrinal settlement) is what preserves the plurality — a reading corroborated by parties with no stake in either enforcement community's success.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.38 at interval end) because the enforcement-vacuum condition itself does not directly transfer wealth the way a captured toll would — it produces diffuse compliance-cost and valuation-risk transfer onto less-resourced adopters, real but modest relative to a hard extraction mechanism. Theater ratio is authored moderate-and-rising (0.42) because a growing share of activity in this space — compliance audits, licensing risk assessments, conference panels on 'GPL compliance best practices' — has become performative reassurance rather than resolution of the underlying question; the industry has built an entire compliance-consulting apparatus around NOT resolving the ambiguity. Suppression is authored low (0.31) because no party is coercively blocked from litigating or legislating to resolve the question; the vacuum persists through inaction and asymmetric capacity, not active suppression of alternatives. Accessibility collapse is moderate (0.35): adopters can still exit into permissive-licensed alternatives or commercial licenses, so this is not a fully collapsed structure. Resistance is moderate-high (0.55): FSF-aligned enforcement actors and legal reform advocates actively push for clarification, and this pressure is a real, ongoing feature of the landscape.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF-aligned maintainer seat, the plurality looks like ongoing, legitimate defense of copyleft's coordination purpose against erosion — an agenda_setter exercising real enforcement authority within its domain. From the small commercial adopter seat, the identical structural fact (unresolved doctrine, community-dependent enforcement) looks like an unpriced tax on anyone without the resources to navigate or exploit it. The engine's per-seat computation should reflect this: fsf_aligned_maintainers experience something closer to a functioning rope within their sphere, while small_commercial_adopters experience something closer to extractive uncertainty.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform vendors, dual-licensing companies, and legal intermediary firms sit near the beneficiary end: each has either the scale to absorb the ambiguity's risk, or a business model that depends on the ambiguity persisting. Small commercial adopters, compliance-uncertain startups, and downstream integrators sit near the target end: they bear the transaction cost of unresolved doctrine without the resources to price or exploit it, and their exit options are constrained or trapped by prior architectural commitments. Pragmatic flexible adopters are the interesting middle case — moderately powerful, mobile, and genuinely benefiting from the ambiguity by exploiting local low-enforcement-capacity conditions, which the derivation correctly places nearer the beneficiary end despite technically being 'adopters' like the victim group.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary capture of shared code via trivial technical circumvention) remains genuinely live in principle, which blocks a clean piton classification. But the specific mechanism now sustaining the ambiguity — non-litigation, non-legislation, and a compliance-consulting industry with an interest in non-resolution — has drifted from 'defending the coordination function' toward 'monetizing the unresolved question.' Classifying this as tangled_rope rather than snare or piton preserves the genuine coordination the plurality still permits (diverse ecosystems can each operate under locally coherent rules) while flagging the asymmetric extraction it also enables (diffuse cost transfer onto under-resourced adopters) as a live structural feature requiring enforcement-by-inaction to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_plurality_structural_vs_transitional,
    'Is the enforcement-vacuum condition a stable structural feature of software copyleft licensing (permanent plurality by design of the legal system''s incrementalism), or a transitional state awaiting a test case that will collapse it into one dominant reading?',
    'Track appellate docket activity for GPL derivative-work cases over a multi-year window; a circuit split or definitive ruling would resolve this empirically. Absence of any such case after a long interval would support the structural-permanence reading.',
    'If structural, this constraint''s tangled_rope classification is durable and the beneficiary/victim asymmetry it documents should be treated as an enduring feature of the licensing ecosystem. If transitional, this story describes a temporary state that a future sibling reading (post-precedent) would supersede.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_plurality_structural_vs_transitional, conceptual, 'Whether the interpretive vacuum is a permanent structural feature or an artifact awaiting judicial resolution.').

omega_variable(
    enforcement_capacity_measurement,
    'Can ''which interpretive community has enforcement capacity in a specific context'' be measured objectively, or is it itself contested and asserted differently by FSF-aligned actors versus industry actors?',
    'Compile a dataset of actual GPL compliance actions, settlements, and cease-and-desist outcomes across ecosystems, coded by which reading was applied and by which community initiated enforcement.',
    'If enforcement capacity is measurable and asymmetric as described, the beneficiary/victim structure authored here is empirically grounded. If enforcement patterns are more uniform or unpredictable than the FSF-aligned/industry-dominated dichotomy suggests, the story''s central mechanism should be revised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_measurement, empirical, 'Whether the community-dependent enforcement-capacity claim is empirically verifiable.').

omega_variable(
    kernel_framing_alternative_courts_as_kernel_owner,
    'An alternative framing treats the courts_and_legislatures'' non-intervention itself as the kernel-defining authority (silence as a form of ruling), rather than treating the GPL text as the kernel and courts as merely absent adjudicators. Under that framing, does this reading''s classification change?',
    'Compare classification outcomes under (a) GPL-text-as-kernel with courts as excluded/absent stakeholders (the framing used here) versus (b) judicial-non-intervention-as-kernel with the GPL text as a downstream artifact interpreted differently depending on which non-intervention regime applies.',
    'Framing (a), used in this story, treats the plurality as a property of the license text under contested reading. Framing (b) would treat the plurality as a property of judicial institutional behavior, potentially shifting beneficiaries toward ''those who benefit from judicial restraint'' rather than ''those who benefit from license ambiguity'' — a subtly different victim/beneficiary map that would not change the tangled_rope verdict but would change which agent is named agenda_setter.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_courts_as_kernel_owner, conceptual, 'Alternative CS framing: GPL-text-as-kernel versus judicial-non-intervention-as-kernel, and whether it shifts the stakeholder map.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 5, 0.21).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 25, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.1).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, narrow_scope_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_copyleft_scope kernel. strong_copyleft_reading and narrow_scope_reading each assert a specific, determinate doctrinal answer to the derivative-work boundary question with correspondingly different beneficiary/victim structures and epsilon values reflecting their own internal coherence. This reading (enforcement_vacuum_reading) instead asserts that the absence of resolution between those two readings is itself the operative structural constraint, with its own distinct low-to-moderate epsilon (0.38) reflecting diffuse transaction-cost extraction rather than a determinate compliance obligation. All three are linked as a constraint family; none averages or interpolates between the others per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
