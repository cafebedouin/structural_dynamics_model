% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Statutory Debt Ceiling as Constitutionally Void Under 14th Amendment Section 4
 *   domain: constitutional_law/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling, first enacted in 1917 and aggregated in 1939,
 *   purports to limit total federal borrowing. This reading holds that
 *   Section 4 of the 14th Amendment ('The validity of the public debt of the
 *   United States... shall not be questioned') renders the debt ceiling
 *   constitutionally void when it conflicts with congressionally appropriated
 *   spending. The constraint's extractiveness is zero because the
 *   Constitution itself forbids questioning debt validity; Treasury must
 *   borrow to execute appropriations. Congressional votes to raise or suspend
 *   the ceiling are ceremonial — the borrowing authority is already implicit
 *   in the appropriations power. Political theater (standoffs, 'extraordinary
 *   measures', default threats) reflects actors treating the void statute as
 *   operative, not the constraint's actual legal force.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling as Constitutionally Void Under 14th Amendment Section 4").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'ebd946b0-4bfb-41fd-8729-89ffad2da049').
narrative_ontology:cs_kernel_codification('ebd946b0-4bfb-41fd-8729-89ffad2da049', formalized).
narrative_ontology:cs_authority_grounding('ebd946b0-4bfb-41fd-8729-89ffad2da049', lineage).
narrative_ontology:cs_interpretation_layer_present('ebd946b0-4bfb-41fd-8729-89ffad2da049').
narrative_ontology:cs_reading_relation('ebd946b0-4bfb-41fd-8729-89ffad2da049', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('ebd946b0-4bfb-41fd-8729-89ffad2da049', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('ebd946b0-4bfb-41fd-8729-89ffad2da049', foundational, public_debt_validity_inviolable).
narrative_ontology:cs_axiom_status(public_debt_validity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('ebd946b0-4bfb-41fd-8729-89ffad2da049', public_debt_validity_inviolable, deontological).
narrative_ontology:cs_axiom('ebd946b0-4bfb-41fd-8729-89ffad2da049', foundational, appropriations_imply_borrowing_authority).
narrative_ontology:cs_axiom_status(appropriations_imply_borrowing_authority, holdable).
narrative_ontology:cs_axiom_grounding('ebd946b0-4bfb-41fd-8729-89ffad2da049', appropriations_imply_borrowing_authority, conventional).
narrative_ontology:cs_reference_frame('ebd946b0-4bfb-41fd-8729-89ffad2da049', fourteenth_amendment_section_4).
narrative_ontology:cs_drift_state('ebd946b0-4bfb-41fd-8729-89ffad2da049', contemporary_political_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ebd946b0-4bfb-41fd-8729-89ffad2da049', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congress_majority).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, bondholders).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_section_4).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, appropriations_power_implies_borrowing_authority).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, executive_duty_to_pay_debts).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, congressional_power_of_purse_not_legislative_veto).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Executes borrowing authority implied by congressional appropriations. Under this reading, has no legal discretion to honor the debt ceiling when it conflicts with appropriations; the 14th Amendment compels payment of valid debt. Political pressure may create practical constraints, but legally the ceiling is null.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_secretary, agenda_setter,
    institutional, biographical, analytical, national).

% Enacts appropriations that require borrowing. Benefits from the constitutional nullity reading because it removes the need for separate debt ceiling votes — appropriations alone authorize borrowing. However, political norms and institutional inertia maintain the ceremonial ceiling vote.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congress_majority, beneficiary).

% Hold US Treasury securities whose validity is constitutionally guaranteed. The 14th Amendment Section 4 directly protects their claims. They benefit from the constitutional nullity reading because it removes any legal basis for default on principal or interest.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bondholders, beneficiary,
    organized, generational, arbitrage, global).

% Would use the debt ceiling as leverage to extract policy concessions under threat of default. Under this reading, their leverage tool is constitutionally void — they are excluded from the constitutional framework that guarantees debt payment. Their exclusion is structural, not procedural.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, legislative_minority, excluded,
    organized, biographical, constrained, national).

% Would adjudicate any challenge to Treasury's borrowing past the statutory ceiling. Under this reading, courts must uphold the 14th Amendment over the statute (constitutional supremacy). No case has squarely presented this issue because political branches always resolve standoffs before judicial intervention.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The 14th Amendment Section 4 coordinates by constitutionally removing legislative veto power over debt service, ensuring that once Congress appropriates spending, the borrowing to fund it cannot be blocked by a subsequent statute. This solves the coordination problem of credible commitment to debt payment across political cycles.
% TRANSFER_FUNCTION: Nothing is transferred by the void debt ceiling itself. The constitutional guarantee transfers protection to bondholders (debt validity) and borrowing authority to the Executive (via appropriations), but the statutory ceiling transfers nothing because it is inoperative.
% ABSENT_VOICES: Proponents of the debt ceiling as a binding fiscal constraint (legislative minority, fiscal hawk groups, some originalist scholars) are structurally absent from this reading's framework. They would argue the ceiling is a valid exercise of Congress's borrowing power, not a 'questioning' of debt validity. They are excluded because this reading treats their position as constitutionally foreclosed.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling vanished overnight, Treasury would continue borrowing exactly as required by appropriations — which is what this reading says already happens. The world is already arranged as if the ceiling does not exist; its formal repeal would change nothing operationally.
% FOUNDING_PROBLEM: Post-Civil War assurance that Union debt would be honored and Confederate debt repudiated; Section 4 was designed to prevent future Congresses from repudiating federal obligations for political reasons.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional text itself (14th Amendment Section 4), the congressional debates of 1866 (recorded in Congressional Globe), and consistent Supreme Court dicta (Perry v. United States, 1935) affirm that the clause protects all federal debt validity, not just Civil War debt. No corroborating source outside the beneficiary set (bondholders) is needed — the constitutional text is the corroboration.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.0 because a void statute extracts nothing — the legal obligation runs from appropriations to borrowing, not from the ceiling. Suppression is 0.0 because no enforcement mechanism can uphold an unconstitutional limit; the constraint collapses alternatives completely (accessibility_collapse 0.92) because constitutional law admits no workaround. Resistance is near-zero (0.05) because no organized resistance to the 14th Amendment's debt clause exists. Theater_ratio 0.15 captures the political performance around a legally inoperative constraint — the gap between legal nullity and political practice.
 *
 * PERSPECTIVAL GAP:
 *   From the constitutional_nullity_reading seat, the debt ceiling is a mountain — constitutional law that cannot be changed by statute. From the coordination_scaffold_reading seat, it is a procedural scaffold with sunset-like features (periodic increases). From the extraction_snare_reading seat, it is a weaponized snare. The engine computes these as three distinct constraints because their ε values and structural profiles are non-overlapping. This reading's zero extractiveness is not a metric tuning — it is the definitional claim of the reading.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim structure exists because the constraint is void. The constitutional order (bondholders, appropriations power, executive duty) is the structural beneficiary of the 14th Amendment's guarantee, but this is a vindicated proposition, not a rent-collecting beneficiary. Political actors who threaten default (legislative minority) are would-be extractors whose tool is null; they appear in sibling readings, not this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The debt ceiling's founding problem (WWI bond issuance coordination) is dead — modern appropriations process and Treasury operations make the ceiling superfluous for coordination. Its persistence as political theater despite constitutional nullity is mandatrophy: a constraint whose function has atrophied but whose performance continues. This reading resolves the mandatrophy by declaring the constraint void, not degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_theater_vs_legal_operative,
    'Does the persistent political theater around debt ceiling crises constitute a de facto constraint despite constitutional nullity?',
    'Observe whether Treasury ever fails to pay principal/interest on debt when appropriations require it, or whether ''extraordinary measures'' and political brinkmanship ever result in actual default rather than resolved standoff.',
    'If political practice treats the void statute as binding, the constraint''s effective type shifts from mountain to piton or snare for operational seats, creating a dual-track reality: legal nullity but political force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_theater_vs_legal_operative, empirical, 'Whether political actors'' behavior creates a shadow constraint that mimics extraction despite legal voidness.').

omega_variable(
    constitutional_nullity_adoption,
    'Will any Treasury Secretary actually execute the constitutional nullity reading by ignoring the debt ceiling and borrowing per appropriations?',
    'Wait for a debt ceiling standoff where Treasury faces X-date with no congressional action, then observe whether the Secretary invokes 14th Amendment Section 4 and continues borrowing.',
    'If never adopted, the reading remains a theoretical mountain with zero operational force; if adopted, it becomes the operative constraint and the statutory ceiling becomes a pure piton (theatrical remnant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_nullity_adoption, empirical, 'Whether the constitutional reading ever transitions from academic/legal position to executive action.').

omega_variable(
    kernel_reading_structural_delta,
    'How does this reading''s zero-extractiveness profile structurally differ from its sibling readings that treat the same statute as operative?',
    'Compare the three readings'' metric profiles: this reading (ε=0, mountain), coordination_scaffold_reading (low ε, scaffold), extraction_snare_reading (high ε, snare). The divergence confirms they are distinct constraints sharing a label, per ε-invariance principle.',
    'Validates the kernel decomposition: one statutory label, three structurally distinct constraints with non-overlapping ε values and classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural differentiation of the three kernel readings per the BGS decomposition pattern.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1917, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1917, 0.02).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1939, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1939, 0.03).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1985, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t1995, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2011, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2013, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2021, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_tr_t2023, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1917, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1917, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1939, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1939, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1985, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1985, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t1995, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1995, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2011, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2011, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2013, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2013, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2021, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2021, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_be_t2023, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2023, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1917, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1917, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1939, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1939, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1985, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1985, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t1995, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1995, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2011, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2011, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2013, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2013, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2021, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2021, 0.0).
narrative_ontology:measurement(statutory_debt_ceiling__constitutional_nullity_reading_su_t2023, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2023, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition of the statutory_debt_ceiling kernel: this reading (mountain, ε=0) is the upstream constitutional floor; coordination_scaffold_reading (scaffold, low ε) and extraction_snare_reading (snare, high ε) are downstream political readings that treat the statute as operative. The constitutional reading influences both by establishing the legal nullity that political practice either ignores (snare) or works around (scaffold).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
