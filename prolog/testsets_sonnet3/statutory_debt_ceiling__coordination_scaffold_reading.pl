% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling as Treasury Operational Coordination Mechanism
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   Under the coordination-scaffold reading, the statutory debt ceiling is a
 *   procedural device that consolidates what would otherwise be per-issuance
 *   congressional authorization of federal borrowing into a single periodic
 *   aggregate vote, freeing Treasury to manage day-to-day debt operations
 *   (auctions, maturities, extraordinary measures) without recurring
 *   legislative involvement in operational details. This reading treats the
 *   ceiling's periodic renewal as routine legislative housekeeping analogous
 *   to appropriations riders, not as a site of systemic hostage-taking. It is
 *   one of three readings of the same statutory kernel; the other two
 *   (extraction_snare_reading, constitutional_nullity_reading) are separate
 *   constraint stories with different ε values and different
 *   beneficiary/victim structures, linked here via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.18).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Treasury Operational Coordination Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:has_sunset_clause(statutory_debt_ceiling__coordination_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, 'bdeaf269-591c-4045-91de-2e4844ced7b1').
narrative_ontology:cs_kernel_codification('bdeaf269-591c-4045-91de-2e4844ced7b1', formalized).
narrative_ontology:cs_authority_grounding('bdeaf269-591c-4045-91de-2e4844ced7b1', practice).
narrative_ontology:cs_interpretation_layer_present('bdeaf269-591c-4045-91de-2e4844ced7b1').
narrative_ontology:cs_reading_relation('bdeaf269-591c-4045-91de-2e4844ced7b1', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdeaf269-591c-4045-91de-2e4844ced7b1', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('bdeaf269-591c-4045-91de-2e4844ced7b1', foundational, aggregate_authorization_reduces_transaction_costs).
narrative_ontology:cs_axiom_status(aggregate_authorization_reduces_transaction_costs, holdable).
narrative_ontology:cs_axiom_grounding('bdeaf269-591c-4045-91de-2e4844ced7b1', aggregate_authorization_reduces_transaction_costs, instrumental).
narrative_ontology:cs_axiom('bdeaf269-591c-4045-91de-2e4844ced7b1', secondary, periodic_renewal_is_routine_legislative_housekeeping).
narrative_ontology:cs_axiom_status(periodic_renewal_is_routine_legislative_housekeeping, holdable).
narrative_ontology:cs_axiom_grounding('bdeaf269-591c-4045-91de-2e4844ced7b1', periodic_renewal_is_routine_legislative_housekeeping, empirically_contingent).
narrative_ontology:cs_reference_frame('bdeaf269-591c-4045-91de-2e4844ced7b1', aggregate_authorization_replaces_per_issuance_consent).
narrative_ontology:cs_drift_state('bdeaf269-591c-4045-91de-2e4844ced7b1', post_2011_debt_ceiling_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bdeaf269-591c-4045-91de-2e4844ced7b1', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriations_committees).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, bondholders_seeking_predictable_issuance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, general_public_taxpayers).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, congressional_power_of_the_purse_doctrine).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, aggregate_borrowing_authority_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages actual issuance of bonds and bills within the aggregate ceiling Congress sets, using extraordinary measures when approaching the limit. Benefits from having a single aggregate authorization rather than needing item-by-item congressional approval for every bond sale; this frees day-to-day debt management from legislative micromanagement. Bears the operational burden when the ceiling binds and legislative renewal stalls.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, beneficiary).

% Sets spending and revenue policy through the ordinary appropriations and tax process, and separately authorizes the aggregate debt ceiling as a periodic checkpoint. Retains a formal, low-cost mechanism to register aggregate fiscal magnitude without re-litigating each Treasury auction. Can raise, suspend, or restructure the ceiling through ordinary majority legislation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriations_committees, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriations_committees, agenda_setter).

% Purchase Treasury securities and rely on predictable, routine issuance schedules. Under the coordination reading, the ceiling functions as one more procedural checkpoint in a system that has, on this reading, resolved routinely for decades, supporting confidence in the debt-management process rather than threatening it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, bondholders_seeking_predictable_issuance, beneficiary,
    organized, biographical, mobile, global).

% Forecasts when the ceiling will bind and coordinates with Treasury and Congress on timing of adjustment legislation. Provides analytical visibility into the mechanism's operation without directly bearing its costs or collecting its benefits.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, executive_branch_budget_office, observer,
    institutional, generational, analytical, national).

% Benefit indirectly from an orderly, periodically-checked borrowing process that this reading holds supports fiscal discipline signaling and low borrowing costs, without directly participating in the mechanism's operation. Have no direct exit from the fiscal consequences of federal borrowing policy.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, general_public_taxpayers, beneficiary,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single periodic checkpoint at which the legislature registers the aggregate scale of federal borrowing, substituting one aggregate vote for what would otherwise require item-by-item congressional authorization of every bond issuance, allowing Treasury to conduct routine debt management operationally.
% TRANSFER_FUNCTION: Under this reading, no systematic transfer occurs: the ceiling coordinates the relationship between the legislature's spending/taxing decisions and the executive's borrowing operations, converting many small authorization events into one periodic aggregate one, without moving resources between winners and losers.
% ABSENT_VOICES: Advocates of the extraction-snare reading would object that this account elides the mechanism's demonstrated capacity for weaponization by legislative minorities; advocates of the constitutional-nullity reading would object that any coordination benefit is irrelevant if the mechanism is void ab initio under the 14th Amendment. Both readings are treated here as separate constraints, not incorporated into this one.
% DISAPPEARANCE_RATIONALE: Under the coordination-scaffold reading, removing the ceiling would require Congress to substitute some other periodic mechanism for registering aggregate borrowing scale (or fold it fully into the appropriations process), a moderate but not catastrophic institutional adjustment. Whether the world 'rearranges' meaningfully or merely relocates the checkpoint is itself contested even within this reading, since Congress retains full authority to set spending and taxes regardless of the ceiling's existence.
% FOUNDING_PROBLEM: Prior to 1917, Congress authorized each individual bond issuance, which was administratively unworkable for financing World War I; the Second Liberty Bond Act created an aggregate ceiling so Treasury could manage issuance operationally within a legislatively-set total, replacing per-issuance authorization with periodic aggregate authorization.
% FOUNDING_PROBLEM_CORROBORATION: Treasury debt-management officials and congressional budget process scholars outside the set of legislators who benefit from ceiling brinksmanship attest that the operational coordination problem — Treasury needing an aggregate authorization framework rather than per-issuance votes — remains structurally live and would need to be re-solved by some other mechanism if the ceiling were abolished outright.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, contested).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because this reading holds that the mechanism, in its ordinary operation across most of its history, functioned as claimed: periodic increases passed via routine or near-routine votes for the majority of the interval, and Treasury's operational autonomy within the ceiling was not systematically weaponized. The modest upward drift in extractiveness and theater ratio reflects the reading's own acknowledgment that from the 1980s onward the renewal votes increasingly attracted rider politics and brinksmanship episodes (2011, 2013, 2023), which this reading treats as a departure from, not the essence of, the mechanism's coordination function. Suppression is low-to-moderate and rises slightly, reflecting the growing use of the renewal vote as a point of leverage, without conceding that leverage is the mechanism's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury and the appropriations committees are structural beneficiaries under this reading: the aggregate-ceiling structure reduces their transaction costs (Treasury avoids per-issuance votes; Congress avoids per-issuance oversight burden). Bondholders and the general public are diffuse beneficiaries of orderly issuance and, on this reading, no identifiable victim group exists — the mechanism is not read as systematically extracting from any party. This is the central structural claim distinguishing this reading from the extraction_snare_reading, which identifies federal beneficiaries, contractors, and financial markets as victims of brinksmanship-driven uncertainty and treats the same statutory text as an instrument of legislative-minority extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unworkable per-issuance bond authorization) remains live in this reading's account — Treasury still requires some aggregate authorization framework distinct from line-item appropriations. This is what prevents the coordination-scaffold reading from collapsing into a piton verdict: the coordination function persists in ordinary operation across most of the historical record, even though isolated crisis episodes generate theater and brinksmanship that a hostile reading (extraction_snare_reading) would treat as constitutive rather than incidental. The has_sunset_clause / scaffold framing reflects that periodic ceiling increases are themselves a designed transitional mechanism — each increase is meant to be superseded by the next, not a permanent settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_or_dormant_extraction_capacity,
    'Is the debt ceiling''s ordinary-operation coordination function genuinely separable from its demonstrated capacity for weaponization, or does the coordination reading merely describe the mechanism during periods when no faction has chosen to exploit its leverage?',
    'Comparative analysis of ceiling-renewal episodes: if the frequency and severity of brinksmanship episodes is trending upward and increasingly independent of underlying fiscal conditions, that would support the extraction-snare reading''s view that leverage capacity is the mechanism''s true operative feature, not an occasional departure from coordination.',
    'If leverage capacity is intrinsic rather than incidental, the coordination-scaffold reading''s low ε would need revision toward the extraction-snare reading''s account, and the two ''readings'' might need to be understood as describing a mechanism whose classification has drifted over time rather than two stable coexisting interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_dormant_extraction_capacity, conceptual, 'Whether the coordination reading describes the mechanism''s stable nature or only its state during non-crisis intervals.').

omega_variable(
    constitutional_nullity_mootness,
    'If the constitutional_nullity_reading is correct that the ceiling is void under the 14th Amendment''s public debt clause, does the coordination-scaffold reading''s account of the mechanism''s benefits become moot, or does it describe a real administrative practice that persists independent of its constitutional validity?',
    'Judicial resolution of the 14th Amendment Section 4 question, or authoritative executive branch legal opinion squarely addressing the ceiling''s constitutionality.',
    'A ruling of nullity would not necessarily eliminate the administrative coordination practice (Congress could re-enact an aggregate authorization voluntarily), but it would sever the coordination reading''s implicit claim that the ceiling''s binding force is what makes the coordination function operative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_nullity_mootness, conceptual, 'Whether administrative coordination benefit depends on constitutional validity of the binding mechanism.').

omega_variable(
    beneficiary_or_natural_administrative_fact,
    'Are Treasury and Congress genuine beneficiaries of a constructed procedural choice, or does the aggregate-authorization structure represent something closer to an administrative necessity that any large sovereign borrower would require regardless of the specific U.S. statutory form?',
    'Comparative study of debt-authorization mechanisms in other sovereign borrowers (UK, Japan, Eurozone members) that lack a discrete statutory debt ceiling but still require some aggregate borrowing authorization process.',
    'If comparable sovereigns achieve the same coordination function without a bright-line ceiling subject to periodic crisis renewal, that would suggest the U.S. ceiling''s specific form (rather than aggregate authorization generally) is a constructed choice with avoidable extraction risk, weakening this reading''s claim that the current mechanism is the natural or necessary form of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_or_natural_administrative_fact, empirical, 'Whether the specific ceiling mechanism is necessary to the coordination function or one contingent implementation among several.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(stat_tr_t1960, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(stat_tr_t1985, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1917, 0.08).
narrative_ontology:measurement(stat_be_t1960, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(stat_be_t1985, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1985, 0.12).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2011, 0.16).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1917, 0.1).
narrative_ontology:measurement(stat_su_t1960, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(stat_su_t1985, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1985, 0.15).
narrative_ontology:measurement(stat_su_t2000, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2000, 0.17).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2011, 0.2).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__coordination_scaffold_reading, 0.1).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language label 'the debt ceiling' per the ε-invariance principle. coordination_scaffold_reading (this file, ε=0.18, scaffold) treats the mechanism's ordinary operation as procedural coordination. extraction_snare_reading (ε authored high, snare) treats the same statutory text as a weaponized hostage-taking instrument with identifiable victims. constitutional_nullity_reading treats the mechanism as constitutionally void, making its coordination-vs-extraction character moot on that reading's own terms. All three share the same underlying statute (31 U.S.C. § 3101) but diverge on beneficiary/victim structure, extraction magnitude, and even on whether the mechanism validly exists — hence three separate stories rather than one story with a contested parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
