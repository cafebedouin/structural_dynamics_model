% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling â Constitutional Nullity Reading (14th Amendment Supersession)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_nullity_reading of
 *   the statutory_debt_ceiling kernel. The reading holds that the statutory
 *   debt ceiling is legally void under Section 4 of the Fourteenth Amendment,
 *   which guarantees the validity of public debt authorized by law. From this
 *   perspective, Treasury possesses continuous borrowing authority tied to
 *   appropriations, and congressional debt-ceiling votes are ceremonial
 *   performances of fiscal responsibility rather than operative legal
 *   constraints. The constraint is claimed as a piton: a statutory artifact
 *   whose primary function has atrophied, leaving only theatrical
 *   maintenance. The zero extractiveness and high theater ratio are authored
 *   independently; the engine may compute a different per-seat
 *   classification.
 *
 * KEY AGENTS:
 *   - congressional_ceiling_voters (institutional/constrained) â administer the ceremonial votes, agenda-setters without concentrated extraction
 *   - treasury_department (institutional/constrained) â executes borrowing under appropriations, bears diffuse friction costs of the ritual
 *   - sovereign_debt_holders (organized/analytical) â observe the theater without structural default exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.1).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.05).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, piton).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling â Constitutional Nullity Reading (14th Amendment Supersession)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, '317f8c7e-f727-4f6d-bd5d-b2089e749d89').
narrative_ontology:cs_kernel_codification('317f8c7e-f727-4f6d-bd5d-b2089e749d89', fixed_text).
narrative_ontology:cs_authority_grounding('317f8c7e-f727-4f6d-bd5d-b2089e749d89', lineage).
narrative_ontology:cs_interpretation_layer_present('317f8c7e-f727-4f6d-bd5d-b2089e749d89').
narrative_ontology:cs_reading_relation('317f8c7e-f727-4f6d-bd5d-b2089e749d89', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('317f8c7e-f727-4f6d-bd5d-b2089e749d89', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('317f8c7e-f727-4f6d-bd5d-b2089e749d89', foundational, fourteenth_amendment_supersedes_statutory_debt_limit).
narrative_ontology:cs_axiom_status(fourteenth_amendment_supersedes_statutory_debt_limit, holdable).
narrative_ontology:cs_axiom_grounding('317f8c7e-f727-4f6d-bd5d-b2089e749d89', fourteenth_amendment_supersedes_statutory_debt_limit, conventional).
narrative_ontology:cs_axiom('317f8c7e-f727-4f6d-bd5d-b2089e749d89', secondary, executive_borrowing_authority_derives_from_appropriations_and_section_four).
narrative_ontology:cs_axiom_status(executive_borrowing_authority_derives_from_appropriations_and_section_four, holdable).
narrative_ontology:cs_axiom_grounding('317f8c7e-f727-4f6d-bd5d-b2089e749d89', executive_borrowing_authority_derives_from_appropriations_and_section_four, conventional).
narrative_ontology:cs_reference_frame('317f8c7e-f727-4f6d-bd5d-b2089e749d89', fourteenth_amendment_debt_supremacy).
narrative_ontology:cs_drift_state('317f8c7e-f727-4f6d-bd5d-b2089e749d89', contemporary_fiscal_governance, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('317f8c7e-f727-4f6d-bd5d-b2089e749d89', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Periodically enacts and votes on statutory debt limit adjustments or suspensions. From this reading's perspective, these votes are legally unnecessary because the 14th Amendment supersedes the statute, making the votes functionally ceremonial. They are maintained as political theater and institutional habit rather than as binding fiscal controls, with no concentrated extraction flowing to the legislators.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_ceiling_voters, agenda_setter,
    institutional, biographical, constrained, national).

% Executes borrowing to fund congressionally appropriated expenditures. Under this reading, Treasury's constitutional duty to pay public debts and execute appropriations renders the statutory ceiling a legal nullity. It bears administrative and political-friction costs from recurring standoffs and legal uncertainty, despite the clarity of its constitutional authority to borrow without regard to the statutory cap.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, payer,
    institutional, biographical, constrained, national).

% Hold United States government debt instruments. Under the nullity reading, repayment is constitutionally guaranteed regardless of statutory ceiling politics, so the constraint does not alter credit risk or contractual expectations. They observe the legislative theater without structural exposure to legal default.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, sovereign_debt_holders, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The arrangement is legally inoperative under this reading. Treasury coordination with borrowing markets is governed directly by appropriations acts and the constitutional debt obligation of the Fourteenth Amendment, not by the statutory ceiling.
% TRANSFER_FUNCTION: No substantive transfer occurs. The constraint is a legal nullity; any ceremonial vote does not alter Treasury's constitutional duty to borrow and spend as appropriated.
% ABSENT_VOICES: Fiscal conservatives and legislative minorities who treat the ceiling as a binding limit on executive borrowing are structurally bypassed when Treasury proceeds under the Fourteenth Amendment. Their objections are rendered legally irrelevant, though they remain vocal in political discourse.
% DISAPPEARANCE_RATIONALE: Because the constraint is already constitutionally inoperative, its formal repeal would eliminate only a ceremonial vestige. Treasury borrowing, federal spending, and debt service would continue unchanged, governed by appropriations and the Fourteenth Amendment.
% FOUNDING_PROBLEM: To aggregate discrete congressional authorizations of individual bond issues into a single statutory limit, simplifying legislative oversight of executive borrowing during World War I.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Research Service fiscal historians and constitutional scholars outside the ritual-maintaining coalition (e.g., Buchanan and Dorf) attest that the original aggregation purpose is obsolete; the ceiling no longer functions as a meaningful fiscal limit and persists as an institutional relic.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.0, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).
:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set to 0.0 because the nullity reading assesses the standing arrangement as legally inoperative. Suppression is low (0.1) because the constraint lacks enforceable legal force, though residual political theater generates minor friction. Theater ratio is high (0.85) because congressional votes and standoffs are performative rather than functional. Accessibility collapse is minimal (0.05) because Treasury's alternative pathâdirect borrowing under the Fourteenth Amendmentâis legally open. Resistance is moderate (0.4) reflecting congressional institutional attachment to the ritual despite its legal irrelevance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Congress) experiences the constraint as a retained institutional prerogative and political stage; the payer seat (Treasury) experiences it as a source of legal friction and uncertainty. Because no beneficiaries or victims are declared, the engine will derive directionality from canonical fallbacks, but with Îµ=0 the effective extraction Ï is zero for all seats regardless of derived d.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because the nullity reading holds that no actor extracts legal rents from the constraint. Treasury is declared as payer because it bears administrative and political costs of navigating the ceremonial constraint. Congress is agenda-setter without beneficiary status: it administers the ritual but does not capture measurable extraction from it. The lack of explicit beneficiary and victim declarations means directionality reverts to the power atom's canonical fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (aggregating bond authorizations) is dead, and the constraint persists by inertia. This prevents mislabeling the nullity reading as a rope or snare: it is a piton because its mandate has outlived its function, no party profits enough to maintain it, and the political cost of repeal exceeds the diffuse friction it creates. The ceremonial maintenance is the diagnostic tell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_operability,
    'Is the statutory debt ceiling a legally operative constraint, or a nullity superseded by the Fourteenth Amendment?',
    'Authoritative Supreme Court ruling on the merits, or sustained executive adoption of the nullity reading followed by judicial acquiescence.',
    'If resolved in favor of operability, the constraint reclassifies toward coordination or extraction; if nullity is affirmed, it remains a legally inoperative piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_operability, conceptual, 'Contested kernel ambiguity on the debt ceiling''s legal operability').

omega_variable(
    political_theater_vs_legal_force,
    'Does recurring congressional standoff behavior demonstrate latent legal force, or purely theatrical positioning without operative effect?',
    'Observation of Treasury behavior during binding appropriation periods: if Treasury consistently borrows through standoffs without statutory increase, the constraint lacks legal force.',
    'If standouts alter Treasury behavior, the nullity reading understates extraction; if Treasury ignores the ceiling, the theatrical diagnosis is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_theater_vs_legal_force, empirical, 'Whether political theater has operative legal consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(stat_tr_t40, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(stat_tr_t60, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(stat_tr_t80, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 80, 0.72).
narrative_ontology:measurement(stat_tr_t100, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 100, 0.85).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(stat_be_t40, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement(stat_be_t60, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 60, 0.0).
narrative_ontology:measurement(stat_be_t80, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 80, 0.0).
narrative_ontology:measurement(stat_be_t100, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 100, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__constitutional_nullity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This constraint is the constitutional_nullity_reading of the statutory_debt_ceiling kernel, decomposed per the Îµ-invariance principle because the legal operability of the ceiling is contested. The nullity reading holds Îµ=0 (legally inoperative); sibling readings hold Îµ>0 (operative as coordination or extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
