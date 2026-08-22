% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Statutory Debt Ceiling as Procedural Coordination Scaffold
 *   domain: constitutional/fiscal/political
 *
 * SUMMARY:
 *   This reading frames the statutory debt ceiling as a procedural
 *   coordination device that solves a genuine collective-action problem: how
 *   Congress exercises aggregate fiscal control without parallelizing
 *   Treasury operations through per-transaction votes. Under this reading,
 *   the ceiling operates as intended — periodic adjustments are routine
 *   fiscal decisions, extractiveness is low, and the constraint facilitates
 *   rather than obstructs executive fiscal management. This is the reading
 *   endorsed by executive Treasury officials and mainstream fiscal-governance
 *   scholars. The rival readings (constitutional_nullity_reading arguing the
 *   14th Amendment supersedes the ceiling, and extraction_snare_reading
 *   arguing the ceiling enables legislative-minority hostage-taking)
 *   represent different institutional understandings of the same statutory
 *   text. This reading is DISTINCT from both siblings and instantiates a
 *   specific normative and structural claim: that the ceiling coordinates
 *   genuine fiscal discipline without systematic extraction.
 *
 * KEY AGENTS:
 *   - Treasury Operations: needs operational autonomy within a fiscal boundary
 *   - Congress (aggregate function): needs to set that boundary periodically
 *   - Executive Budget Function: must conduct spending/borrowing within the limit
 *   - Credit Markets: benefit from a clear statutory debt limit
 *   - Constitutional Courts: adjudicate whether the ceiling is constitutional and distinct from 14th Amendment issues
 *   - Legislative Minorities: excluded from the core coordination under this reading (central under the extraction_snare_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.18).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Procedural Coordination Scaffold").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional/fiscal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, 'b48e6491-01f7-42d1-b761-c13a4fcabbfb').
narrative_ontology:cs_kernel_codification('b48e6491-01f7-42d1-b761-c13a4fcabbfb', formalized).
narrative_ontology:cs_authority_grounding('b48e6491-01f7-42d1-b761-c13a4fcabbfb', extraction).
narrative_ontology:cs_interpretation_layer_present('b48e6491-01f7-42d1-b761-c13a4fcabbfb').
narrative_ontology:cs_reading_relation('b48e6491-01f7-42d1-b761-c13a4fcabbfb', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('b48e6491-01f7-42d1-b761-c13a4fcabbfb', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('b48e6491-01f7-42d1-b761-c13a4fcabbfb', foundational, debt_ceiling_procedurally_coordinates_fiscal_control).
narrative_ontology:cs_axiom_status(debt_ceiling_procedurally_coordinates_fiscal_control, holdable).
narrative_ontology:cs_axiom_grounding('b48e6491-01f7-42d1-b761-c13a4fcabbfb', debt_ceiling_procedurally_coordinates_fiscal_control, instrumental).
narrative_ontology:cs_axiom('b48e6491-01f7-42d1-b761-c13a4fcabbfb', foundational, congress_retains_appropriations_authority_through_aggregate_limit).
narrative_ontology:cs_axiom_status(congress_retains_appropriations_authority_through_aggregate_limit, holdable).
narrative_ontology:cs_axiom_grounding('b48e6491-01f7-42d1-b761-c13a4fcabbfb', congress_retains_appropriations_authority_through_aggregate_limit, conventional).
narrative_ontology:cs_axiom('b48e6491-01f7-42d1-b761-c13a4fcabbfb', secondary, treasury_operational_autonomy_within_limit_is_functionally_necessary).
narrative_ontology:cs_axiom_status(treasury_operational_autonomy_within_limit_is_functionally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b48e6491-01f7-42d1-b761-c13a4fcabbfb', treasury_operational_autonomy_within_limit_is_functionally_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('b48e6491-01f7-42d1-b761-c13a4fcabbfb', procedural_fiscal_coordination_via_aggregate_limit).
narrative_ontology:cs_drift_state('b48e6491-01f7-42d1-b761-c13a4fcabbfb', contemporary_post_2011_crisis_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b48e6491-01f7-42d1-b761-c13a4fcabbfb', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, executive_treasury_operations).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congress_aggregate_constraint_function).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_operations).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, credit_markets).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, public_as_taxpayers).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, executive_budget_function).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, public_as_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under an aggregate debt limit that provides a known fiscal boundary while allowing day-to-day cash management autonomy. Treasury can borrow freely up to the limit without repeated congressional votes on each issuance, treating the ceiling as a standing authorization boundary rather than a renewed permission. Gains operational efficiency and predictability; exits only by constitutional amendment or congressional repeal.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_operations, beneficiary,
    institutional, biographical, constrained, national).

% Sets the debt limit via statute, establishing an upper bound on total federal borrowing. Can adjust the limit periodically when fiscal circumstances warrant. Avoids micromanaging every Treasury borrowing decision while retaining aggregate control through statutory adjustment. The periodic adjustment replaces hypothetical per-issuance votes.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congress_aggregate_constraint_function, agenda_setter,
    institutional, generational, arbitrage, national).

% Must conduct spending and borrowing within the aggregate limit Congress sets. The constraint forces ex-ante fiscal discipline: spending commitments made by the executive are upstream of the debt ceiling, so the limit disciplines total fiscal posture. The executive absorbs the constraint as a binding boundary on aggregate fiscal action.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, executive_budget_function, payer,
    institutional, biographical, constrained, national).

% Under this reading, minorities have no special leverage: adjustments are handled as routine fiscal decisions, not as hostage-taking moments. Minorities are excluded from the coordination frame because the constraint operates transparently — no crisis is manufactured to enable extractive demands. (Under the extraction_snare_reading, minorities would be central players.)
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, legislative_minorities, excluded,
    organized, biographical, constrained, national).

% Receive a clear statutory signal that federal debt is bounded by law, providing a known outer limit for credit risk assessment. The ceiling reduces ambiguity about total sovereign indebtedness trajectory, supporting long-term lending decisions. The routine adjustment mechanism signals stability: the limit is expected to move with fiscal need, not become a crisis point.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, credit_markets, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the costs of the debt service and the fiscal constraint: higher taxes, lower services, or crowded-out private investment. Simultaneously benefit from a stated fiscal boundary that prevents runaway debt accumulation without explicit vote. Exit requires collective democratic action (voting out officeholders) or emigration.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, public_as_taxpayers, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, public_as_taxpayers, beneficiary).

% Adjudicate whether the ceiling is within Congress's constitutional power and whether the 14th Amendment Section 4 (no question shall be raised of the validity of public debt) supersedes it. Under this reading, courts find the ceiling constitutional as a spending-constraint mechanism, distinct from the 14th Amendment's prohibition on doubting the debt itself once incurred.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces per-issuance congressional votes on Treasury borrowing with a single aggregate limit, eliminating repeated authorization delays while preserving congressional control over total fiscal commitment. Treasury and Congress solve a genuine collective-action problem: Congress needs to exercise fiscal discipline over aggregate federal debt, but delegating to Congress every single borrowing decision would gridlock Treasury operations; delegating fully to Treasury with no limit would undermine congressional appropriations authority.
% TRANSFER_FUNCTION: No direct transfer function under this reading: the constraint establishes a boundary, not a payment mechanism. Fiscal costs are distributed across taxpayers and creditors as a consequence of constrained-vs-unconstrained fiscal paths; these are diffuse opportunity costs, not extracted rents to identifiable beneficiaries.
% ABSENT_VOICES: Parties excluded from the coordination frame include: (1) future generations (taxpayers in the next generation bear debt service costs but have no vote on ceiling adjustments); (2) international creditors (who price US debt but do not participate in ceiling-adjustment decisions); (3) state and local governments (affected by federal fiscal crowding-out but not in the negotiation). Under the extraction_snare_reading, legislative minorities would also be central players (as hostage-takers); under this reading they are outside the core coordination problem.
% DISAPPEARANCE_RATIONALE: If the debt ceiling statute vanished, Congress would face the coordination problem it was built to solve: either Congress would vote on Treasury borrowing item-by-item (paralysis), or Congress would delegate borrowing authority entirely to Treasury (loss of appropriations control). The world would rearrange into one of those two modes. The ceiling's disappearance would not restore a prior equilibrium; it would force a new institutional solution.
% FOUNDING_PROBLEM: Early 20th century: Congress voted separately on each Treasury bond issuance, which was cumbersome as the federal government's fiscal scope grew. By the 1917 Liberty Bond acts, Congress needed a mechanism to authorize aggregate borrowing for World War I without micromanaging each transaction. The debt ceiling emerged as that mechanism: a single authorization to borrow up to a fixed amount.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the 1917 act and Congressional Budget Office analysis confirm that the ceiling was invented to streamline Treasury operations under wartime fiscal pressure (source: academic consensus in fiscal history, CBO foundational documents). Executive Treasury officials (including across Republican and Democratic administrations) regularly attest that the ceiling functions as an operational boundary allowing Treasury to manage cash flow without daily congressional votes. Academic fiscal-governance scholars and budget process experts outside the Treasury endorse this reading of the founding problem as still operative.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is LOW (0.18 at interval end) because the measured constraint is a procedural boundary, not a wealth transfer. The ceiling does not concentrate gains in any identifiable beneficiary seat; Treasury and Congress both benefit from the operational efficiency. Suppression is LOW (0.12) because the constraint's persistence does not depend on coercive silence or prevented exit — all parties can openly advocate for ceiling adjustments, and Congress routinely adjusts the limit when fiscal circumstances warrant. Theater is MODERATE (0.22) because ritual surrounds the adjustments (political theater, dramatic negotiations), but the underlying function remains real: the limit does constrain aggregate federal borrowing. Accessibility of alternatives is LOW (0.35) because the alternative coordination mechanisms (per-transaction votes or unconstrained executive borrowing) are each politically infeasible, making the ceiling sticky. Resistance is MODERATE-HIGH (0.41) because fiscal-constraint opponents (those who want higher spending without higher taxes) actively argue for higher ceilings or abolition, and 14th Amendment advocates resist the ceiling on constitutional grounds. The measurement series traces a modest rise in theater and extractiveness over 1917–2026, reflecting growing politicization of adjustments (especially post-2011 debt-ceiling crises), but the rise stays shallow because the core coordination function remains operative. This reading explicitly DOES NOT claim the ceiling is costless — it claims the costs are diffuse fiscal-discipline costs, not concentrated extraction.
 *
 * PERSPECTIVAL GAP:
 *   Treasury's seat and Congress's seat should compute divergently. From Treasury's position: the ceiling is a transparent operational boundary that facilitates cash management (near-beneficiary directionality, low per-seat extractiveness). From Congress's position: the ceiling is a self-imposed fiscal constraint that disciplines overall spending (ambiguous directionality — Congress both sets and is bound by it). From the public's position: the ceiling is a distributed fiscal cost (higher taxes or crowded-out services) in exchange for a distant, aggregate-level benefit (stated fiscal discipline). The engine computes these divergences from the structural data; this reading's claim does not pre-adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury benefits from autonomy within a boundary (d near beneficiary end). Congress as agenda-setter benefits from aggregate control without micromanagement (d near beneficiary end). The executive budget function bears the constraint as a binding limit (d near target end, though not extracted-from in the snare sense). The public bears diffuse opportunity costs but gains a stated fiscal boundary (d near symmetric). Credit markets benefit from clarity on total federal debt (d near beneficiary end). Legislative minorities under this reading are NOT central to the constraint's operation — they are excluded because extractive leverage is not the mechanism; under the extraction_snare_reading they would occupy a high-d target position. This directionality structure distinguishes this reading from the extraction_snare_reading, where minorities occupy a central extractive seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (repeated per-transaction borrowing votes paralyzing Treasury) was LIVE when the ceiling was adopted in 1917 and remains structurally live: the alternative (per-transaction authorization) would still paralyze modern Treasury. The founding problem is NOT dead. However, the extraction_snare_reading treats the ceiling as a vehicle for legislative-minority hostage-taking, which would make the founding problem secondary to the extraction function. This reading's classification as a LOW-extractiveness scaffold depends on treating the coordination function as primary and the political-theater surrounding adjustments as secondary (theater_ratio 0.22 reflects this split). If the extraction function became primary (triggering crisis-mode every adjustment, systematic hostage-taking), the constraint would reclassify to snare. The measurement series shows theater rising (1917 to 2026), which is consistent with either: (a) growing politicization of routine adjustments (scaffold degrading toward theater, but coordination function persists), or (b) early signs of extraction becoming the primary mechanism (scaffold transitioning to snare). An omega variable captures this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_drift,
    'As the debt ceiling becomes increasingly politicized and tied to legislative demands for policy concessions (2011 crisis onward), does the constraint retain its core coordination function or does extraction become the primary mechanism?',
    'Track adjustment frequency and coupling to policy demands: if adjustments occur predictably before hitting the ceiling and are decoupled from legislative extortion attempts, coordination remains primary; if adjustments are delayed to create crisis conditions and systematically coupled to minority demands, extraction becomes primary.',
    'If extraction is becoming primary, reclassification from scaffold to snare is warranted. If coordination persists, the rising theater_ratio reflects ritual without functional shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_drift, empirical, 'Whether the constraint''s primary function is shifting from procedural coordination to extractive leverage.').

omega_variable(
    id_14th_amendment_foreclosure,
    'Does 14th Amendment Section 4 (''the validity of the public debt... shall not be questioned'') logically foreclose the debt-ceiling reading, or are the two compatible?',
    'Constitutional adjudication and scholarly consensus on whether the 14th Amendment prohibits Congress from imposing a debt limit on new borrowing or only forbids calling into question debt already incurred.',
    'If the 14th Amendment forecloses the ceiling, this reading (coordination_scaffold_reading) and the extraction_snare_reading collapse into constitutional_nullity_reading, and the ceiling cannot function as either coordination or extraction — it would be void. If they are compatible, this reading remains structurally distinct from constitutional_nullity_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(id_14th_amendment_foreclosure, conceptual, 'Whether the statutory ceiling is constitutionally compatible with 14th Amendment Section 4 or foreclosed by it.').

omega_variable(
    procedural_vs_substantive_extraction,
    'Is the theater surrounding debt-ceiling adjustments (ritual negotiations, political theater, late-night votes) a symptom of the constraint becoming an extraction tool, or is it incidental ritual that preserves the core coordination function?',
    'Behavioral analysis: if theater is used to manufacture urgency and extract policy concessions, it signals extraction becoming primary; if theater is ritual that permits adjustments to occur despite political disagreement, it is incidental to coordination.',
    'If theater is the primary mechanism, reclassify to snare. If theater is incidental, scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_extraction, empirical, 'Whether ritual surrounding adjustments is incidental to coordination or a sign extraction is primary.').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading of a contested kernel: the statutory debt ceiling. Sibling readings instantiate extraction_snare_reading (ceiling as hostage mechanism) and constitutional_nullity_reading (ceiling as constitutionally void). What assumptions anchor this reading''s core premise (procedural coordination) versus the siblings'' premises?',
    'Comparative case analysis: (1) historical origins (was the ceiling adopted for coordination or control?); (2) contemporary adjustment mechanisms (are adjustments routine or crisis-driven?); (3) statutory language (does the statute permit or require procedural coordination?); (4) institutional actor testimony (do Treasury, Congress, courts treat the ceiling as coordination or control?).',
    'If the historical and institutional evidence supports coordination, this reading is empirically grounded. If evidence tilts toward extraction or constitutional incompatibility, sibling readings gain traction. If evidence is mixed, all three readings remain structurally live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'The irreducible interpretive choice among three readings of the same kernel: coordination, extraction, or constitutional nullity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1917, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(stat_tr_t1945, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(stat_tr_t1970, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1995, 0.16).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2011, 0.2).
narrative_ontology:measurement(stat_tr_t2026, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1917, 0.05).
narrative_ontology:measurement(stat_be_t1945, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement(stat_be_t1970, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2011, 0.17).
narrative_ontology:measurement(stat_be_t2026, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1917, 0.05).
narrative_ontology:measurement(stat_su_t1945, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1945, 0.07).
narrative_ontology:measurement(stat_su_t1970, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 1995, 0.1).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2011, 0.11).
narrative_ontology:measurement(stat_su_t2026, statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling constraint family (three readings of one kernel): (1) coordination_scaffold_reading (this constraint) — treats the ceiling as a procedural mechanism facilitating Treasury operations; (2) extraction_snare_reading — treats the ceiling as enabling legislative-minority hostage-taking under default threat; (3) constitutional_nullity_reading — treats the ceiling as superseded by the 14th Amendment Section 4. Each reading instantiates a structurally distinct constraint with different ε, beneficiary/victim structure, and type. The ε-invariance principle requires separate stories: the reading's endorsed referent (the standing arrangement under contest, assessed by the reading's own lights) produces a different ε in each case. Coordination reading: ε ≈ 0.18 (low extraction, procedural boundary). Extraction reading: ε ≈ 0.72 (high extraction, hostage mechanism). Nullity reading: ε ≈ 0.0 (the ceiling does not exist structurally — it is superseded). All three constraints share the same statutory text (the kernel) but differ in how that text is interpreted and what functions it is understood to serve. They are linked via network.affects_constraints because each reading's viability affects the others' plausibility — if coordination evidence becomes overwhelming, extraction reading weakens; if constitutional nullity is judicially affirmed, both other readings become moot.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
