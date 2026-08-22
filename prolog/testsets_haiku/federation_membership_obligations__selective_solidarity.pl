% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Tiered Free Movement Rights by Contribution Status
 *   domain: political_economy/federalism/welfare_policy
 *
 * SUMMARY:
 *   The European federation (or equivalent supranational union) gates free
 *   movement and welfare access on contribution history and economic activity
 *   status. The selective-solidarity reading asserts that this tiering is
 *   legitimate because federation membership carries obligations (economic
 *   contribution) and benefits (mobility rights, welfare access) should track
 *   those obligations. Employed mobile workers retain full movement rights
 *   and welfare eligibility; economically inactive migrants face mobility
 *   restrictions or welfare exclusion unless they can demonstrate sufficient
 *   contribution history. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination function bridging free movement and fiscal sustainability)
 *   while the authored metrics describe substantially extractive, actively
 *   enforced operation with rising theater ratio—the engine measures whether
 *   the claim holds or whether the constraint is more snare-like than
 *   rope-like.
 *
 * KEY AGENTS:
 *   - employed_mobile_workers (beneficiary, moderate power, mobile exit) — achieve full rights as contribution history accumulates
 *   - economically_inactive_migrants (victim, powerless, identity-locked exit) — restricted mobility, welfare exclusion, return to origin as only fully accessible option
 *   - host_state_fiscal_authorities (agenda-setter, institutional power) — establish contribution thresholds, administer eligibility, enforce boundaries
 *   - welfare_dependent_citizens (dual payer/beneficiary, powerless, trapped exit) — benefit from welfare boundary defense, bear cost of compressed generosity
 *   - origin_states_labor_exporters (payer, moderate power, constrained exit) — lose economic-activity citizens and remittance income
 *   - federation_coordinating_body (observer, institutional power) — monitors compliance, adjudicates disputes, manages tension between free movement and welfare autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.67).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.71).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.67).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Tiered Free Movement Rights by Contribution Status").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/welfare_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'e9d83595-02e3-4a1f-965a-f0ea18475ece').
narrative_ontology:cs_kernel_codification('e9d83595-02e3-4a1f-965a-f0ea18475ece', formalized).
narrative_ontology:cs_authority_grounding('e9d83595-02e3-4a1f-965a-f0ea18475ece', extraction).
narrative_ontology:cs_interpretation_layer_present('e9d83595-02e3-4a1f-965a-f0ea18475ece').
narrative_ontology:cs_reading_relation('e9d83595-02e3-4a1f-965a-f0ea18475ece', federation_membership_obligations__federation_integration_primary_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9d83595-02e3-4a1f-965a-f0ea18475ece', federation_membership_obligations__federation_member_sovereignty_primary_reading, influences).
narrative_ontology:cs_axiom('e9d83595-02e3-4a1f-965a-f0ea18475ece', foundational, contribution_conditions_membership_benefits).
narrative_ontology:cs_axiom_status(contribution_conditions_membership_benefits, holdable).
narrative_ontology:cs_axiom_grounding('e9d83595-02e3-4a1f-965a-f0ea18475ece', contribution_conditions_membership_benefits, conventional).
narrative_ontology:cs_axiom('e9d83595-02e3-4a1f-965a-f0ea18475ece', foundational, welfare_access_subordinate_to_fiscal_sustainability).
narrative_ontology:cs_axiom_status(welfare_access_subordinate_to_fiscal_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('e9d83595-02e3-4a1f-965a-f0ea18475ece', welfare_access_subordinate_to_fiscal_sustainability, instrumental).
narrative_ontology:cs_reference_frame('e9d83595-02e3-4a1f-965a-f0ea18475ece', federation_universal_welfare_access).
narrative_ontology:cs_drift_state('e9d83595-02e3-4a1f-965a-f0ea18475ece', contemporary_fiscal_constraint_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e9d83595-02e3-4a1f-965a-f0ea18475ece', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employed_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_fiscal_authorities).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, welfare_dependent_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, welfare_dependent_citizens).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, origin_states_labor_exporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can move freely across member states, access labor markets, and qualify for welfare benefits if they have sufficient contribution history. Their rights expand with documented employment and tax contributions. They face lower barriers to residency, family reunification, and access to social services once they achieve contributing status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employed_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Establish and enforce contribution thresholds for welfare access, manage integration tiers, and control the boundary between full and restricted member mobility. Justify restrictions on welfare access to non-contributors as protecting fiscal sustainability and preventing welfare magnet effects. Administer eligibility verification and contribution audits.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_fiscal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Face mobility restrictions or reduced welfare access if they lack sufficient contribution history—whether due to unemployment, caregiving, disability, or recent arrival. Their free movement rights are conditional on economic activity status; attempting to access welfare as non-contributors triggers enforcement action (benefit denial, residency challenges, deportation vulnerability). Return to origin state is the only fully accessible option but often entails loss of family/social ties built during residence.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, biographical, identity_locked, continental).

% Bear indirect cost as the constraint channels welfare access to contributors, which can compress benefit generosity or tighten eligibility for all claimants. Simultaneously, they benefit from framing that defends welfare system boundaries against external pressure. They are trapped in the host state's welfare regime; exit means loss of all accumulated benefits and social safety net.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, welfare_dependent_citizens, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, welfare_dependent_citizens, beneficiary).

% Lose economically active citizens to host states when workers qualify for full mobility rights; lose remittance income and human capital when contribution tiers incentivize permanent settlement in wealthy member states. Their capacity to renegotiate federation terms is constrained by dependency on receiving-state trade and investment.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, origin_states_labor_exporters, payer,
    moderate, generational, constrained, continental).

% Monitors compliance with contribution-tier framework, adjudicates disputes over welfare eligibility and contribution recognition, and can in principle harmonize thresholds across members. Operates under mandate to balance single-market principles against member-state welfare autonomy—a structural tension the constraint embodies.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federation_coordinating_body, observer,
    institutional, generational, analytical, continental).

% Argue that conditioning mobility and welfare on contribution history violates human dignity and citizenship principles; advocate for unconditional free movement and equal access regardless of employment status. Excluded from welfare access rule-setting and contribution threshold negotiation; their testimony reaches policy makers but governance remains with fiscal authorities.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, civil_society_advocates_mobility_rights, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, host_state_fiscal_authorities).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine multi-level coordination problem: prevents welfare system collapse under unrestricted mobility by tying resource distribution to demonstrated fiscal contribution; harmonizes free movement principles with member-state welfare sustainability; enables labor market integration by recognizing worker economic participation status.
% TRANSFER_FUNCTION: Transfers full mobility rights and welfare access from economically inactive migrants to employed contributors; transfers administrative authority over welfare boundaries from supranational federation to host-state fiscal institutions; redistributes the cost of welfare access selectivity onto non-contributing residents and origin states.
% ABSENT_VOICES: Economically inactive migrants are structurally excluded from welfare access rule-setting (they cannot meaningfully participate in threshold negotiation when their status depends on meeting the thresholds); civil society advocates for unconditional mobility are permitted comment but have no governing authority; origin states can raise objections but depend on receiving states for trade/investment, constraining their leverage.
% DISAPPEARANCE_RATIONALE: If contribution-tiered mobility and welfare access disappeared overnight, receiving states would face immediate pressure to either open welfare access unconditionally (raising fiscal demand on host-state taxpayers) or reinstate crude citizenship/nationality tests (fragmenting the single market). Labor mobility patterns would shift as the cost/benefit calculation for migration changed. Federation governance would reorganize around either stronger supranational welfare harmonization or explicit member-state closure authority—the current constraint bridges these irreconcilable positions, so its removal forces a restructuring.
% FOUNDING_PROBLEM: Early free movement and welfare access were unconditional on contribution; this produced both: (a) political backlash in receiving states from taxpayers who viewed welfare access as conditional on national membership, and (b) fiscal pressures as inactive residents and welfare claimants concentrated in wealthy states. The constraint was constructed to solve the perceived welfare magnet problem while preserving nominal free movement.
% FOUNDING_PROBLEM_CORROBORATION: Receiving-state fiscal authorities and member governments attest the problem is live: welfare expenditure projections, aging populations, and migration pressure require boundary-setting. Labor-rights advocates and origin-state governments dispute the framing: they attest that the constraint invents the problem by treating welfare access as a benefit allocation decision rather than a rights issue. Academic research on welfare magnets shows minimal empirical evidence that welfare access drives migration patterns (migrants respond primarily to employment opportunities and wage differentials), which corroborates the rights-based critique and suggests the founding problem is constructed rather than empirical.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurement series (0 to 25) spans the constraint's institutional maturation from initial framework through hardening enforcement. Extractiveness rises from 0.45 to 0.67 as contribution-tier administration solidifies and bureaucratic barriers accumulate (tighter documentation, slower eligibility determination, more aggressive verification). Suppression rises from 0.58 to 0.71 as member states invest in mobility enforcement and welfare-access gatekeeping becomes more sophisticated. Theater ratio rises from 0.28 to 0.42 as the constraint's public rationale (sustainability, labor market integration) accounts for an increasing share of its administrative activity, while the true operation (wealth-state closure disguised as contribution-tracking) accounts for a decreasing share—the rising theater suggests the constraint is becoming increasingly performative rather than functionally coordinating. The shared time grid ensures every metric is authored at every examined point. Cyclical dynamics are not dominant here; the drift is monotonic accumulation of extraction and theater as the constraint hardens institutionally.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (employed mobile workers, host-state fiscal authorities) and the victim seats (economically inactive migrants, welfare-dependent citizens) should compute substantially different types. From the beneficiary perspective, the constraint is genuine coordination: it solves the real problem of welfare system sustainability while preserving mobility and labor market access. From the victim perspective, the same structure is enforced extraction: it weaponizes contribution history to deny welfare access to those unable to work, and it fragments the federation's nominal free movement principle into tiers that track wealth and labor-market capacity rather than citizenship. The structural asymmetry is clear: beneficiaries control the rule-setting (host-state fiscal authorities are agenda-setters); victims are powerless and identity-locked (cannot move without losing accumulated benefits, cannot stay without restricted welfare). The engine should compute the constraint as rope-like from the beneficiary seat and snare-like from the victim seat—the divergence is the key finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Employed mobile workers derive d near the beneficiary end (0.1–0.25): they benefit from the constraint's preservation of mobility rights and its recognition of their contribution status. Host-state fiscal authorities derive d near the beneficiary end (0.05–0.15): they are the agenda-setters, they control the rules, and they collects the closure benefit (reduced welfare pressure). Economically inactive migrants derive d near the target end (0.75–0.90): they are powerless, face active enforcement, and have identity-locked exit (return means loss of family/social ties built during migration). Welfare-dependent citizens sit near symmetric (0.45–0.55): they benefit from boundary defense but bear cost as welfare generosity compresses. Origin-state labor exporters derive d moderately toward the target end (0.55–0.70): they lose human capital and remittance income but retain some leverage through federation negotiation. No directionality overrides are required; the structural data (beneficiary/victim declarations + power/exit atoms) produce accurate d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy tension via an asymmetric compromise: the founding problem (welfare magnet effects + unrestricted mobility driving receiving-state backlash) is technically ADDRESSED by contribution-tiering, but the addressing mechanism is substantially extractive. The constraint does not solve the underlying tension between free movement and welfare closure—it institutionalizes that tension as a tiered structure that benefits employed contributors and harms economically inactive persons. The mandatrophy verdict is RESOLVED ASYMMETRICALLY: the founding problem is declared LIVE by beneficiaries (they still enforce contribution tiers to manage welfare pressure) but DEAD by victims (the constraint now serves closure, not coordination). This asymmetry is the constraint's structural flaw—it cannot be stable long-term because the victim seats experience mandatrophy (the constraint persists without solving the coordination problem for them; it only extracts). The theater ratio rising from 0.28 to 0.42 signals this: the constraint's public rationale (labor market integration, contribution tracking) accounts for declining share of its actual operation (wealth-state closure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contribution_measurement_variance,
    'How is ''contribution history'' measured and recognized across member states with different tax systems, employment structures, and social insurance arrangements?',
    'Comparative analysis of harmonization efforts (EU recognition directives, bilateral agreements on contribution equivalence); documentation of appeal rates and reversals when contribution assessments are challenged.',
    'High variance in measurement produces de facto mobility differentiation by origin state (workers from states with high documentation burden face higher barriers). The constraint''s structural claim (rights follow contribution) decouples from its operation (rights follow origin-state bureaucratic capacity). This either establishes the constraint as covertly discriminatory or reveals measurement variance as a second constraint—decomposition may be warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contribution_measurement_variance, empirical, 'Whether contribution history can be measured consistently across federation members.').

omega_variable(
    inactive_status_definition_drift,
    'What constitutes ''economically inactive'' under the constraint? Does caregiving, education, disability, child-rearing, or part-time work count as contribution or inactivity?',
    'Audit of administrative case law and eligibility determination across member states; documentation of reclassification rates when status changes; comparison of household composition effects (joint incomes, spousal status) on mobility tier assignment.',
    'Narrow definition (full-time employment only) produces high suppression and high victim set (caregivers, students, disabled persons lose rights). Broad definition (part-time, caregiving count as contribution) narrows the victim set and softens the constraint toward rope. The lack of federation-wide definition allows member states to exploit the ambiguity for closure, making the constraint functionally a snare under the guise of contribution-neutrality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inactive_status_definition_drift, conceptual, 'Whether ''contribution'' includes non-market economic activity or only paid employment and tax contributions.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the mobility restriction for economically inactive migrants enforced structurally (legal barriers, benefit system design) or internalized (inactive migrants internalize unworthiness, avoid applying, self-select into constraint acceptance)?',
    'Post-exit trajectory analysis: if inactive migrants who return to origin states or achieve employment status report reduced perception of deservingness/unworthiness after mobility barriers are removed, suppression is partly internalized. Survey data on willingness to attempt welfare access among eligible non-contributors; administrative data on application rates vs. eligibility rates.',
    'If internalized, the constraint''s effective suppression is higher than structural measurement suggests (the target carries the suppression after exit). Policy remedies (communication campaigns, simplified access) would have different effectiveness than structural barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether mobility suppression is structural (external barriers) or internalized (target belief about deservingness).').

omega_variable(
    kernel_reading_contest__selective_solidarity_vs_integration_primary,
    'Does the selective-solidarity reading of the federation membership kernel foreclose the integration-primary reading (unconditional free movement as constitutive of federation citizenship), or do they coexist as live positions held by different parties?',
    'Textual/juridical analysis of the kernel''s founding commitments (federation charter, constitutional settlement): can a single legal framework coherently hold both ''free movement is unconditional'' and ''free movement is conditional on contribution''? If no single framework can hold both without internal contradiction, the reading relation is forecloses; if the framework is ambiguous enough to admit both readings by competing interpretive communities, the relation is coexists_with.',
    'If forecloses: the selective-solidarity reading is in direct conflict with the federation''s constitutive principle; pressure will accumulate toward reformulation or constitutional amendment. If coexists_with: the two readings can persist indefinitely across different institutional seats, but this indefinite coexistence signals a failing commitment system (contradiction unresolved). The constraint''s persistence depends on the reading relations remaining institutionally unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__selective_solidarity_vs_integration_primary, conceptual, 'Whether the selective-solidarity reading logically eliminates the integration-primary reading or whether both readings can coexist within a single federation framework.').

omega_variable(
    kernel_reading_contest__selective_solidarity_vs_member_sovereignty_primary,
    'Does the selective-solidarity reading influence or foreclose the member-sovereignty-primary reading (national welfare states retain closure authority)?',
    'Analysis of institutional pressure: if selective-solidarity becomes the dominant reading and is operationalized as supranational contribution-tier rules, does this constrain member states'' ability to set their own welfare boundaries (influences), or does it logically eliminate the member-sovereignty claim altogether (forecloses)?',
    'If influences: member states retain formal sovereignty but operate under federal contribution-tier constraints that compress their actual closure authority. If forecloses: the reading relation indicates a true constitutional conflict between federation-level contribution tiers and member-state welfare autonomy. This determines whether the constraint can stabilize long-term or whether constitutional renegotiation is structurally inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__selective_solidarity_vs_member_sovereignty_primary, conceptual, 'Whether the selective-solidarity reading''s operationalization constrains or eliminates member-state welfare closure authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(fede_tr_t20, projected).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__selective_solidarity, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(fede_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(fede_be_t20, projected).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__selective_solidarity, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(fede_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(fede_su_t20, projected).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__selective_solidarity, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(fede_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_integration_primary_reading).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_member_sovereignty_primary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_obligations kernel. The selective-solidarity reading (this story) operationalizes free movement and welfare access as tiered by contribution status. The integration-primary reading (separate story) operationalizes these as unconditional rights of federation citizenship. The member-sovereignty-primary reading (separate story) operationalizes these as subject to member-state welfare closure authority. All three readings share the same kernel (federation commitment to integrate member states) but instantiate different constraints with different ε values, beneficiary/victim structures, and classifications. They are linked via network.affects_constraints to enable contamination propagation analysis. The decomposition is necessary because the ε-invariance principle requires a single constraint to have a single, stable ε: a constraint that reads as 'free movement is unconditional' has fundamentally different extraction from one that reads as 'free movement is conditional on contribution'—they cannot coexist in one ε value. Each reading is its own constraint story, with its own beneficiary/victim declarations and its own computed type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
