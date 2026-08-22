% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Statutory Debt Ceiling (Constitutional Nullity Reading)
 *   domain: constitutional_law/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional nullity reading of
 *   the statutory debt ceiling: the ceiling is legally void because Section 4
 *   of the Fourteenth Amendment ('The validity of the public debt...shall not
 *   be questioned') constitutionally protects the Treasury's duty to honor
 *   appropriations through borrowing, and constitutional law supersedes
 *   statutory enactment. Under this reading, the constraint has no extractive
 *   force—it is operationally inert. Treasury borrowing authority derives
 *   from enacted appropriations and constitutional mandate, not from the
 *   ceiling. Congressional votes on debt ceiling increases are ceremonial
 *   performances, hence the elevated theater_ratio. The claim/metric
 *   alignment is intentional: both claim mountain status and author zero
 *   extractiveness because the reading positions the constraint as legally
 *   null. The falsity of natural-law status—whether the constraint is a
 *   genuine constitutional necessity or a constructed political constraint—is
 *   captured in omega variables.
 *
 * KEY AGENTS:
 *   - Treasury Department: executes borrowing under Section 4 authority; the ceiling is procedurally inert from Treasury's structural position
 *   - Congress: enacted both the appropriations (which are binding) and the ceiling (which is void); experiences this reading as loss of a procedural lever it thought it had
 *   - Constitutional Supremacy Doctrine: the doctrine beneficiary; vindicated by Section 4 precedence over statutory procedure
 *   - Legislative Minority: excluded from using the ceiling as a negotiating instrument because the reading denies the ceiling legal force
 *   - Federal Judiciary: the decision-maker on whether Section 4 nullifies the statutory ceiling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.95).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling (Constitutional Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'ad73c97e-b36e-4c90-bb8e-959abb7e00de').
narrative_ontology:cs_kernel_codification('ad73c97e-b36e-4c90-bb8e-959abb7e00de', formalized).
narrative_ontology:cs_authority_grounding('ad73c97e-b36e-4c90-bb8e-959abb7e00de', lineage).
narrative_ontology:cs_interpretation_layer_present('ad73c97e-b36e-4c90-bb8e-959abb7e00de').
narrative_ontology:cs_reading_relation('ad73c97e-b36e-4c90-bb8e-959abb7e00de', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('ad73c97e-b36e-4c90-bb8e-959abb7e00de', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('ad73c97e-b36e-4c90-bb8e-959abb7e00de', foundational, section_4_nullifies_conflicting_statutory_ceiling).
narrative_ontology:cs_axiom_status(section_4_nullifies_conflicting_statutory_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('ad73c97e-b36e-4c90-bb8e-959abb7e00de', section_4_nullifies_conflicting_statutory_ceiling, deontological).
narrative_ontology:cs_axiom('ad73c97e-b36e-4c90-bb8e-959abb7e00de', foundational, constitutional_amendment_supersedes_prior_statute).
narrative_ontology:cs_axiom_status(constitutional_amendment_supersedes_prior_statute, holdable).
narrative_ontology:cs_axiom_grounding('ad73c97e-b36e-4c90-bb8e-959abb7e00de', constitutional_amendment_supersedes_prior_statute, conventional).
narrative_ontology:cs_reference_frame('ad73c97e-b36e-4c90-bb8e-959abb7e00de', section_4_constitutional_supremacy).
narrative_ontology:cs_drift_state('ad73c97e-b36e-4c90-bb8e-959abb7e00de', contemporary_post_doocy_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ad73c97e-b36e-4c90-bb8e-959abb7e00de', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congress_appropriating_body).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, congress_appropriating_body).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Executes borrowing authority derived from constitutionally mandated appropriations and Section 4 of the Fourteenth Amendment. Under this reading, the Treasury operates on the legal authority of enacted appropriations and the constitutional mandate; the statutory debt ceiling is operationally void. The Treasury Secretary's position, on this reading, is that the constraint does not lawfully bind.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, agenda_setter,
    institutional, generational, analytical, national).

% Enacts appropriations that mandatorily commit expenditure; simultaneously enacts a debt ceiling that creates a technical legal conflict with the appropriations themselves. Under this reading, Congress's authorization to spend (appropriations power) supersedes its procedural constraint (debt ceiling), rendering the debt ceiling operationally null. Congress experiences this as loss of a procedural boundary-setting tool, though the appropriations power is unimpaired.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress_appropriating_body, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congress_appropriating_body, beneficiary).

% Under a coordination or extraction reading of the debt ceiling, would use threatened default to extract concessions. Under this constitutional nullity reading, the minority's exclusion is structural: the legal framework does not recognize the debt ceiling as a legitimate lever, so the minority's threatened use of it has no constitutional standing. They are excluded because the constraint is legally void, not because the constraint excludes them.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, legislative_minority, excluded,
    organized, biographical, constrained, national).

% This is a doctrinal position, not an actor. The constitutional nullity reading vindicates the doctrine that later constitutional amendments (Section 4, explicitly protecting the validity of public debt) supersede earlier statutory enactments. The doctrine benefits by having its supremacy principle instantiated in case law and Treasury practice.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_doctrine, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_doctrine).

% Monitor the binding status of the debt ceiling to price U.S. sovereign debt. Under the nullity reading, markets recognize the constraint as ceremonial and assess default risk from appropriations capacity and political will, not from statutory debt ceiling enforcement. Markets have exited the assumption of the ceiling's legal bindingness.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, financial_markets, observer,
    powerful, biographical, mobile, global).

% Would adjudicate a challenge to Treasury borrowing under this reading, determining whether Section 4 of the Fourteenth Amendment legally nullifies the statutory debt ceiling. The court's decision would either confirm the nullity reading or foreclose it by affirming the ceiling's constitutional validity.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None under this reading. The constraint has no coordination function because it is legally void. Appropriations themselves coordinate spending; Section 4 coordinates debt validity. The statutory ceiling coordinates nothing because it is operationally superseded.
% TRANSFER_FUNCTION: Under this reading, no transfer occurs because the constraint is inoperative. The ceiling neither moves resources nor extracts them—it is legally null. Any transfer observed (e.g., concessions extracted by legislators using default threats) would occur via a different constraint (the extraction_snare_reading), not this one.
% ABSENT_VOICES: The legislative minority is structurally excluded under this reading because the legal framework does not recognize the debt ceiling as a legitimate instrument. An actor using the ceiling as a negotiating lever would argue the constraint is real and binding; that voice is absent from the framework's recognition.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling disappeared, Treasury would continue to execute appropriations as required by enacted spending bills and Section 4 of the Fourteenth Amendment. Nothing reorganizes because the ceiling is already void. Congressional procedure would lose a symbolic vote, but no substantive change occurs to fiscal operations.
% FOUNDING_PROBLEM: Early twentieth-century desire to prevent Treasury from borrowing without explicit congressional authorization per act of borrowing. Congress sought to establish a procedural checkpoint requiring affirmative votes for total debt issuance.
% FOUNDING_PROBLEM_CORROBORATION: Courts have repeatedly held that appropriations themselves constitute the binding authorization for borrowing (Lincoln National Bank v. Williams; Dept. of Commerce v. House, dicta); the Fourteenth Amendment explicitly protects the validity of public debt, creating a constitutional mandate that supersedes statutory ceilings (Section 4). Outside the ceiling's defenders (budget hawks, procedural conservatives), constitutional scholars, fiscal economists, and judicial opinions support that the founding problem is solved by the appropriations power itself and that the ceiling persists as zombie procedure without legal force.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is zero because the constraint is legally inoperative—it has no binding force, so it extracts nothing. Suppression is zero because there is no mechanism to suppress; the constraint does not control behavior. Theater_ratio is very high (0.95) because the observed behavior—Congress voting on debt ceiling increases, media coverage of ceiling crises, default threat rhetoric—is purely theatrical performance from the nullity reading's perspective. The action is all show; the constraint has no legal teeth. Accessibility_collapse is high (0.85) because once Treasury's Section 4 authority is understood, alternatives (disregarding the ceiling, challenging it in court, borrowing anyway) become obvious—the ceiling's legal status collapses as an actual constraint on available choices. Resistance is moderate (0.72) because parts of Congress and fiscal conservatives actively defend the ceiling's symbolic and procedural importance, even though this reading holds it legally void—they resist the nullity framing itself. The measurement series shows theater_ratio rising slightly over the interval (as the ceiling's purely performative character becomes more widely recognized and enacted) while extractiveness remains flatlined at zero (the constraint never had extractive force on this reading). All measurements are marked 'observed' because the historical record shows Congress conducting ceiling votes despite their legal superfluity.
 *
 * PERSPECTIVAL GAP:
 *   The Treasury Department and Section 4 doctrine sit at nearly d=0 (full beneficiaries of legal nullity—they operate unimpeded). Congress experiences this reading as a loss of procedural standing it believed it held; its directionality is near-zero in practical terms (the ceiling does not constrain its will) but this reading denies it had the procedural authority in the first place. The legislative minority experiences near-complete exclusion: their attempted use of the ceiling as a negotiating lever has no constitutional standing, making their d indeterminate (they are excluded from the constraint entirely because the constraint is null). The engine's per-seat computation would show Treasury and judicial seats recognizing nullity (low d, low effective extraction) while Congress and minority stakeholders might compute the constraint as having some procedural or political force even if legally void—a genuine perspectival gap between legal and political readings.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading denies the constraint has extractive force altogether, so directionality derivation is unusual. Treasury benefits structurally from nullity (d=0.0); Congress loses procedural authority it thought it held, but the nullity reading denies Congress had that authority, so Congress's d is indeterminate—the reading does not recognize the ceiling as a valid instrument Congress can wield. The legislative minority would use the ceiling to extract (d near 1.0 on a snare reading), but this reading excludes them by denying the ceiling's legal force, so their d is analytically off the constraint's domain. No directionality override is needed because the baseline derivation is: beneficiaries (Treasury, constitutional doctrine) have low d; victims or payers do not exist on this reading because the constraint is void.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy—when a constraint's original purpose is dead but the constraint persists—is directly addressed by this reading's core claim. The nullity reading argues mandatrophy has already occurred: the founding problem (preventing unauthorized borrowing by Treasury) was solved by the Fourteenth Amendment's explicit protection of debt validity. The ceiling persists as a ceremonial relic (theater_ratio=0.95), but legally it is mandatroph—its mandate has been superseded and is no longer operative. The reading does NOT resolve mandatrophy in the sense of fixing it; rather, it declares mandatrophy as the current state: the ceiling is a dead constraint dressed up in procedural theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_4_supremacy_vs_statutory_ceiling_validity,
    'Does Section 4 of the Fourteenth Amendment (protecting the validity of public debt) constitutionally nullify a statutory debt ceiling that conflicts with appropriations mandates?',
    'Supreme Court adjudication of a direct challenge to Treasury borrowing beyond the statutory ceiling, or legislative amendment formally clarifying Section 4''s scope and supremacy. Alternatively, sustained Treasury practice of borrowing under Section 4 authority without statutory ceiling compliance, tested in court.',
    'If Section 4 is held to nullify the ceiling, the nullity reading is confirmed and the ceiling is legally inoperative. If the ceiling is held to be a valid procedural constraint on appropriations, the nullity reading is foreclosed and the constraint is either a coordination_scaffold (rope) or extraction_snare (snare) depending on its operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_4_supremacy_vs_statutory_ceiling_validity, empirical, 'Whether the Fourteenth Amendment Section 4 legally supersedes the statutory debt ceiling as a constitutional matter.').

omega_variable(
    natural_law_vs_constitutional_construction,
    'Is this constraint a natural law of the Constitution (Section 4''s mandate is intrinsic to constitutional structure), or a constructed reading imposed by a particular interpretive tradition?',
    'Historical and originalist analysis of Section 4''s original meaning and intent; comparative review of how other democracies with constitutional debt mandates treat statutory ceilings; examination of whether the nullity reading or the snare/scaffold readings align with the Reconstruction framers'' intent.',
    'If natural law (constitutionally inherent), the nullity reading is objectively true. If constructed (a particular reading imposed by contemporary interpretation), then the constraint''s status is contestable and other readings remain live alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constitutional_construction, conceptual, 'Whether the constitutional nullity of the debt ceiling is a discoverable constitutional truth or a reading-dependent interpretation.').

omega_variable(
    beneficiary_identity_and_false_summit_risk,
    'Does naming ''constitutional_supremacy_doctrine'' as a beneficiary mask a constructed constraint that benefits specific political or institutional actors by encoding their preferred interpretation as constitutional law?',
    'Genealogy of Section 4 jurisprudence: which institutional actors, constituencies, and eras have promoted the nullity reading, and have their interests been advanced by courts adopting it? Comparison with actors who benefit from the snare or scaffold readings.',
    'If the nullity reading is a false summit—a constructed constraint that benefits identifiable actors while claiming natural constitutional status—it should be reclassified to tangled_rope or snare and the beneficiaries identified as political rather than doctrinal. If the reading is genuinely constitutionally grounded, the beneficiary is doctrinal and the reading stands as mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_and_false_summit_risk, conceptual, 'Whether the beneficiary (constitutional doctrine) is genuine or a cover for constructed institutional/political advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.88).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 5, 0.9).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 10, 0.92).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 15, 0.93).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 20, 0.94).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t25, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 25, 0.95).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t30, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 30, 0.95).
narrative_ontology:measurement_basis(stat_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 5, 0.0).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 15, 0.0).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t25, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 25, 0.0).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t30, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 30, 0.0).
narrative_ontology:measurement_basis(stat_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__constitutional_nullity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling is a contested kernel that decomposes into three structurally distinct constraint stories, each instantiating a different reading of the same persistent statutory and procedural commitment. The constitutional_nullity_reading (this constraint) holds that Section 4 of the Fourteenth Amendment constitutionally voids the ceiling, making it legally inoperative. The coordination_scaffold_reading treats the ceiling as a rope—a procedural coordination mechanism. The extraction_snare_reading treats the ceiling as a snare—a weaponized boundary for legislative minority extraction. These are not the same constraint viewed from different angles; they are different structural constraints derived from the same kernel via competing constitutional interpretations. ε-invariance is preserved by assigning zero extractiveness to the nullity reading (the constraint is void), moderate extractiveness to the scaffold reading (procedural coordination cost), and high extractiveness to the snare reading (rent extraction under default threat). The three stories are linked by network.affects_constraints because adjudication of the kernel's constitutional status would foreclose or affirm each reading, determining which constraint is legally operative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
