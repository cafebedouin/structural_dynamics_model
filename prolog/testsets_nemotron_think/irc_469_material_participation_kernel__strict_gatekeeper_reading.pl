% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC §469 Material Participation — Strict Gatekeeper Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC §469 (enacted in the Tax Reform Act of 1986) limits deductions for
 *   passive activity losses against ordinary income. The statute defines
 *   'material participation' as the threshold for active status, but leaves
 *   the standard ambiguous. The strict gatekeeper reading — articulated in
 *   IRS regulations (Reg. §1.469-5T), reinforced by cases like *Kosonen v.
 *   Commissioner* and *Thompson v. Commissioner* — requires contemporaneous,
 *   hourly documentation of substantial personal services (generally 500+
 *   hours or 'significant participation' across activities). This reading
 *   narrows the qualifying population, imposes high compliance friction, and
 *   makes passive loss deductions rare for real estate investors who cannot
 *   meet the rigorous substantiation bar. The competing strategic shelter
 *   reading treats material participation as a permissive threshold
 *   achievable through aggressive hour-counting, grouping elections under
 *   Reg. §1.469-4, and minimal documentation. The two readings coexist as
 *   live interpretive positions; the strict reading extracts substantial
 *   compliance costs and forecloses deductions for marginally documented
 *   participation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.75).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.8).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC §469 Material Participation — Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '8ae1530a-2f22-4064-8a57-d87d6949c3ea').
narrative_ontology:cs_kernel_codification('8ae1530a-2f22-4064-8a57-d87d6949c3ea', formalized).
narrative_ontology:cs_authority_grounding('8ae1530a-2f22-4064-8a57-d87d6949c3ea', lineage).
narrative_ontology:cs_interpretation_layer_present('8ae1530a-2f22-4064-8a57-d87d6949c3ea').
narrative_ontology:cs_reading_relation('8ae1530a-2f22-4064-8a57-d87d6949c3ea', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('8ae1530a-2f22-4064-8a57-d87d6949c3ea', foundational, material_participation_requires_substantial_personal_labor).
narrative_ontology:cs_axiom_status(material_participation_requires_substantial_personal_labor, holdable).
narrative_ontology:cs_axiom_grounding('8ae1530a-2f22-4064-8a57-d87d6949c3ea', material_participation_requires_substantial_personal_labor, conventional).
narrative_ontology:cs_reference_frame('8ae1530a-2f22-4064-8a57-d87d6949c3ea', statutory_text_and_legislative_history).
narrative_ontology:cs_drift_state('8ae1530a-2f22-4064-8a57-d87d6949c3ea', contemporary_judicial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ae1530a-2f22-4064-8a57-d87d6949c3ea', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_professionals).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_investors_seeking_active_status).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, economic_substance_doctrine).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, statutory_textualism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Investors who actively manage rental properties or real estate businesses and seek to deduct losses against ordinary income. They must maintain contemporaneous hourly logs of participation, satisfy one of seven regulatory tests (500-hour, 100-hour+, significant participation, etc.), and file grouping elections timely. Compliance costs include CPA fees ($5K–$50K/year), software systems, and opportunity cost of documentation time. Failure means losses are suspended indefinitely. Exit options: sell properties (crystallize losses), restructure as short-term rentals (different rules), or accept passive status — all costly.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors, payer,
    organized, biographical, constrained, national).

% Limited partners, fractional owners, or part-time landlords who contribute capital and some labor but cannot meet the 500-hour or 'significant participation' thresholds. They face the same documentation demands as full-time investors but with far fewer hours to log. Many are retirees or professionals with real estate sideline. The constraint effectively forecloses active status for them — losses remain passive regardless of economic substance. Exit is trapped: they cannot increase hours without changing careers, and selling triggers suspended loss recognition at marginal rates.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_investors_seeking_active_status, payer,
    powerless, immediate, trapped, national).

% CPAs, tax attorneys, and enrolled agents who specialize in real estate tax compliance. The strict gatekeeper reading creates recurring revenue: annual hour-log reviews, grouping election management, audit defense, and planning for the 'real estate professional' status (Reg. §1.469-9). Fees scale with portfolio complexity. They benefit from the constraint's persistence and complexity. Exit is mobile — they can shift to other specialties — but the niche is lucrative and sticky.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_professionals, beneficiary,
    organized, biographical, mobile, national).

% The IRS (via SB/SE and LB&I divisions) administers the material participation standard through audits, technical advice memoranda, and litigation. The strict reading gives examiners a clear, document-driven test: no contemporaneous log = no deduction. This reduces examiner discretion and increases audit efficiency. The constraint also generates revenue from disallowed losses and penalties. Exit is analytical — the IRS could adopt a more permissive interpretation via regulation or litigation posture — but institutional culture, revenue pressure, and anti-abuse mandate favor the strict reading.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement, agenda_setter,
    institutional, generational, analytical, national).

% U.S. Tax Court and federal district/circuit courts adjudicate material participation disputes. They apply the regulatory tests but occasionally push back on IRS overreach (e.g., accepting reconstructed logs in *Kosonen*, narrowing 'significant participation' in *Thompson*). Their decisions shape the practical boundary but do not set the statutory standard. They are observers in the engine's sense: they neither collect from nor pay into the constraint, but their rulings modulate its effective suppression.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_courts, observer,
    institutional, generational, analytical, national).

% Mom-and-pop landlords with 1–3 units, incidental Airbnb hosts, and family-property managers who perform real labor but lack sophistication to meet the documentation bar. They are not represented in regulatory comment processes, lack access to specialist tax counsel, and typically discover the constraint only when audited. Their voices are absent from the legislative and judicial record. If present, they would argue for a facts-and-circumstances test that honors economic substance over hourly formalism.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, excluded_small_investors, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes genuine active business participation from passive investment for purposes of the passive activity loss limitation, preventing taxpayers from characterizing investment losses as business losses to offset ordinary income.
% TRANSFER_FUNCTION: Moves ordinary income deduction eligibility from passive investors to those meeting strict personal labor documentation standards; denies deductions to those who cannot substantiate hour-by-hour participation, transferring tax benefit to the Treasury and compliance revenue to the professional services industry.
% ABSENT_VOICES: Small-scale landlords and part-time real estate investors who lack resources for rigorous documentation but perform substantial labor; they are excluded from the rulemaking and judicial process and would argue for a facts-and-circumstances test honoring economic substance over hourly formalism.
% DISAPPEARANCE_RATIONALE: If the strict documentation standard vanished, passive loss deductions would expand dramatically, tax compliance costs would drop for investors, and the boundary between active trade/business and passive investment would blur — the IRS would lose a primary audit lever and the compliance industry would lose a major revenue line.
% FOUNDING_PROBLEM: Preventing taxpayers from characterizing passive investment losses as active business losses to offset ordinary income, which TRA 1986 identified as a primary tax shelter abuse mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Congressional committee reports on TRA 1986 and subsequent IRS guidance attest the passive loss rules were enacted to curb tax shelter abuse; tax policy scholars outside the enforcement apparatus confirm the abuse problem was real though they dispute whether the strict gatekeeper reading is the proportional response.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.75) is high because the strict documentation standard imposes significant compliance costs (professional fees, record-keeping systems, opportunity cost of time) and denies ordinary-income deductions to investors who perform real labor but cannot meet the hourly substantiation bar. Suppression (0.8) is very high because alternatives are structurally limited: taxpayers cannot easily exit the passive loss regime without meeting the standard, and the grouping elections that might ease qualification are themselves constrained by the same strict reading. Theater ratio (0.4) reflects that the coordination function — distinguishing active trade/business from passive investment — is genuine, but a growing share of the constraint's operation serves extraction (compliance industry revenue, IRS audit leverage) rather than the anti-abuse purpose. Accessibility collapse (0.7) is high because once a taxpayer understands the strict standard, the alternative of 'casual participation' collapses — the constraint reshapes behavior toward either full compliance or abandonment of active status. Resistance (0.55) is moderate: taxpayers challenge in court and lobby for safe harbors, but most high-stakes investors comply rather than litigate.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS/tax professional seats, the constraint appears as necessary coordination (preventing abuse, ensuring only genuine participants claim active losses). From the investor/payer seats, the same structure operates as enforced extraction — the documentation bar is set above what genuine participants can practically meet, and the compliance industry captures the surplus. The engine computes this divergence from the structural data; the claimed tangled_rope type captures the author's assessment that both functions are real and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Real estate investors and passive investors seeking active status are structural targets (payers) — they bear compliance costs and lose deductions; their exit is constrained (trapped for small investors, constrained for organized ones) because the tax code offers no comparable alternative vehicle for loss utilization. Tax professionals are structural beneficiaries — the strict standard creates sustained demand for specialized compliance services; they have mobile exit (can serve other tax domains) but benefit from the constraint's persistence. IRS enforcement is the agenda_setter — it administers the standard, audits for compliance, and collects revenue from disallowed deductions; its exit is analytical (it could change interpretation but institutional incentives favor strict enforcement). Tax courts are observers — they adjudicate disputes but do not set the standard. Excluded small investors (part-time landlords, incidental participants) would object if heard but lack representation in the rulemaking/judicial process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (curbing tax shelter abuse via passive loss limitations) remains live — shelters continue to evolve — but the strict gatekeeper reading has accumulated extraction (rising compliance costs, narrowing safe harbors) beyond what the original abuse problem proportionally requires. The constraint is not a piton (it is actively enforced and updated) nor a pure snare (the coordination function is real and valued by some participants). The tangled_rope classification correctly identifies the hybrid: a genuine anti-abuse coordination mechanism that has become a vehicle for asymmetric extraction through compliance friction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretation_ambiguity,
    'Does IRC §469''s material participation standard demand the strict gatekeeper reading (verifiable substantial personal labor) or permit the strategic shelter reading (permissive hour-counting with grouping elections)?',
    'Supreme Court review of conflicting circuit interpretations, or congressional amendment clarifying the statutory standard.',
    'If the strict reading is authoritatively mandated, the strategic shelter reading is foreclosed and compliance burden becomes settled law; if the strategic reading is validated, the strict gatekeeper''s extraction is recharacterized as IRS overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_ambiguity, conceptual, 'Core interpretive ambiguity of the kernel — whether the statute''s text and history support strict or permissive material participation.').

omega_variable(
    compliance_cost_proportionality,
    'Is the high documentation burden (contemporaneous logs, hourly substantiation, grouping election formalities) proportional to the anti-abuse purpose, or does it extract compliance rents beyond what abuse prevention requires?',
    'Empirical study of compliance costs versus measured shelter activity; regulatory impact analysis if Treasury proposes simplified safe harbors.',
    'If disproportionate, the constraint''s extraction component is structurally separable from its coordination function, strengthening the tangled_rope classification; if proportional, the burden is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_proportionality, empirical, 'Whether the compliance friction serves the coordination function or constitutes independent extraction.').

omega_variable(
    judicial_drift_direction,
    'Are federal courts drifting toward the strict gatekeeper reading, the strategic shelter reading, or maintaining an unstable equilibrium?',
    'Longitudinal analysis of Tax Court, Circuit, and Supreme Court opinions on material participation from 1986-present, coded for interpretive posture.',
    'Drift toward strict reading increases extraction and suppresses the strategic shelter; drift toward permissive reading reduces compliance burden but may enable shelter activity; unstable equilibrium sustains the interpretive contest and the compliance industry that services it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_drift_direction, empirical, 'Direction of judicial drift across the kernel''s two readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(irc__tr_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(irc__tr_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(irc__tr_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(irc__tr_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(irc__tr_t25, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(irc__tr_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 38, 0.4).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(irc__be_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(irc__be_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(irc__be_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(irc__be_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(irc__be_t25, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(irc__be_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 38, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(irc__su_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(irc__su_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(irc__su_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(irc__su_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(irc__su_t25, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(irc__su_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 38, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_passive_loss_limitation).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_audit_selection_real_estate).

% DUAL FORMULATION NOTE:
% This constraint and strategic_shelter_reading form a constraint family decomposing the IRC §469 material participation kernel. The strict reading has higher extractiveness (0.75 vs estimated 0.45 for strategic) and higher suppression (0.8 vs 0.55) because it imposes the documentation bar the strategic reading avoids. They are linked via affects_constraints and share the same kernel_id in the committer frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, organized, 0.35).
constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
