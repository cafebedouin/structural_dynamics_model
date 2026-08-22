% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   domain: tax/real_estate/regulatory
 *
 * SUMMARY:
 *   IRC §469 material participation is a single statutory standard that has
 *   fractured into two structurally distinct readings. The strict gatekeeper
 *   reading — instantiated here — demands contemporaneous, granular
 *   documentation of personal labor hours (750+ hours, more than half of
 *   personal service time, in real property trades) and rejects reconstructed
 *   or aggregated records. The strategic shelter reading (separate
 *   constraint) permits grouping elections, estimation methods, and broader
 *   activity definitions that widen the qualifying population. This reading's
 *   ε (0.72) reflects the extraction from genuine participants who fail the
 *   documentation bar; the strategic reading would author a lower ε but a
 *   different victim structure. The two readings share the kernel (the
 *   statutory text) but are different constraints with different ε, different
 *   beneficiaries/victims, and different computed types.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.72).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC §469 Material Participation — Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax/real_estate/regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e6c676cd-466a-4dbb-bb5f-be64fec0a8c4').
narrative_ontology:cs_kernel_codification('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', formalized).
narrative_ontology:cs_authority_grounding('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', extraction).
narrative_ontology:cs_interpretation_layer_present('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4').
narrative_ontology:cs_reading_relation('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', irc_469_material_participation_kernel__strategic_shelter_reading, influences).
narrative_ontology:cs_axiom('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', foundational, contemporaneous_documentation_required).
narrative_ontology:cs_axiom_status(contemporaneous_documentation_required, holdable).
narrative_ontology:cs_axiom_grounding('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', contemporaneous_documentation_required, conventional).
narrative_ontology:cs_axiom('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', foundational, grouping_elections_narrowly_construed).
narrative_ontology:cs_axiom_status(grouping_elections_narrowly_construed, holdable).
narrative_ontology:cs_axiom_grounding('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', grouping_elections_narrowly_construed, conventional).
narrative_ontology:cs_reference_frame('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', tra_1986_anti_shelter_framework).
narrative_ontology:cs_drift_state('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', post_tcja_2017, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e6c676cd-466a-4dbb-bb5f-be64fec0a8c4', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, treasury_revenue).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, large_institutional_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_compliance_industry).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, active_real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_scale_landlords).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professionals_without_formal_status).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_loss_limitation_integrity).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, income_characterization_principle).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, substance_over_form_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects tax revenue from disallowed passive losses that would otherwise offset ordinary income. The strict reading preserves the revenue base by limiting the population that can claim real estate professional status and material participation. Does not administer the rule directly but benefits from its restrictive application.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, treasury_revenue, beneficiary,
    institutional, generational, arbitrage, national).

% REITs, pension funds, and private equity real estate arms operate at scale where passive loss limitations are structurally irrelevant — they either never generate the losses or absorb them internally. The strict gatekeeper reading eliminates marginal competitors (active individual investors) who might otherwise use loss deductions to compete for assets, effectively raising barriers to entry.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, large_institutional_investors, beneficiary,
    institutional, generational, arbitrage, national).

% CPAs, tax attorneys, and specialty consultants capture substantial fees from the high documentation burden: contemporaneous time logs, calendar reconstruction, activity classification memos, and audit defense. The stricter the standard, the more professional hours are billable. They administer compliance but do not set the rule.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_compliance_industry, beneficiary,
    organized, biographical, mobile, national).

% Individuals who materially participate in rental operations — managing properties, supervising contractors, making capital decisions — but face disallowed losses because their documentation fails the strict standard (e.g., no contemporaneous logs, aggregated hours across activities, grouping elections denied). They bear the economic cost of nondeductible losses and the compliance cost of proving participation. Exit means selling assets or restructuring ownership, both costly.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, active_real_estate_investors, payer,
    moderate, biographical, constrained, national).

% Owners of 1–5 rental units who self-manage but lack systems for rigorous hour tracking. Their participation is genuine but informal — they fix toilets, screen tenants, handle emergencies — yet the documentation standard treats informal labor as non-participation. They cannot afford professional compliance help and cannot restructure efficiently. Exit means exiting real estate entirely.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_scale_landlords, payer,
    powerless, biographical, trapped, local).

% Individuals who work full-time in real estate (brokers, developers, property managers) but fail the §469(c)(7) real estate professional test because they cannot document 750+ hours in real property trades or businesses separate from their W-2 employment, or because their spouse's hours cannot be aggregated. They perform the labor but the rule's formal thresholds exclude them. Exit means changing career structure or marital tax strategy — high friction.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professionals_without_formal_status, payer,
    moderate, biographical, constrained, national).

% Apply the strict standard in audits: demand contemporaneous records, reject reconstructed logs, deny grouping elections where activities are not 'appropriate economic units.' Their enforcement posture defines the operational meaning of the rule. They are neither beneficiaries nor payers — they administer the constraint.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_examiners, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate material participation disputes. Their opinions (e.g., *Kavran*, *Goshorn*, *Estate of Adell*) construct the de facto standard by deciding what evidence suffices. They set precedent that narrows or widens the gate. They observe the structural effects but also authoritatively shape them.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court_judges, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court_judges, observer).

% Authored §469 in TRA 1986 to curb tax shelters. The strict reading vindicates the legislative purpose; the strategic reading frustrates it. They observe the drift but have not amended the statute to clarify the standard since 1993.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, congress_tax_writing_committees, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents taxpayers from converting passive investment losses (which should only offset passive income) into deductions against ordinary income by requiring genuine, verifiable personal labor as the gate to active treatment. Solves the coordination problem of distinguishing active trade-or-business participation from passive capital deployment.
% TRANSFER_FUNCTION: Moves deductible loss claims from active real estate participants (who would offset ordinary income) to the Treasury (which retains the revenue). The compliance industry captures a slice as fees for documentation and defense. Large institutional investors gain competitive advantage as marginal active investors exit.
% ABSENT_VOICES: Would-be real estate entrants who never invest because the loss-deduction uncertainty makes underwriting impossible. Low-income households who would benefit from increased rental supply if active investors could deploy capital more efficiently. These voices are not in the room because they are not yet taxpayers with standing.
% DISAPPEARANCE_RATIONALE: If the strict gatekeeper reading vanished overnight, the strategic shelter reading would become the default — passive losses would flow freely against ordinary income for anyone asserting participation. Treasury revenue would drop measurably; compliance industry revenue would collapse; active individual investors would flood back into rental markets; REITs and large funds would face renewed competition from tax-motivated buyers.
% FOUNDING_PROBLEM: TRA 1986 enacted §469 to stop high-income taxpayers from using leveraged real estate and other passive investments to generate artificial losses that sheltered ordinary income (salaries, business profits). The 'material participation' standard was meant to allow only genuine operators — not passive capital — to treat rental losses as active.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Committee on Taxation (JCT) legislative history and Treasury's 1986 blue book corroborate the anti-shelter purpose. However, the IRS's own National Taxpayer Advocate has repeatedly reported that the current standard 'penalizes genuine small business operators' — an independent executive-branch source outside the beneficiary set attesting that the rule now captures the wrong population.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is high because the constraint disallows losses from taxpayers who genuinely materially participate but cannot meet the forensic documentation standard — their economic loss is real but the tax loss is denied. The compliance industry and Treasury capture the value. Theater ratio (0.42) reflects that the 'anti-shelter' justification is real but increasingly performative: the shelters of 1986 (generic tax shelters with no economic substance) are gone; the rule now primarily catches small landlords and working professionals. Suppression (0.68) is structural: the documentation bar is enforced through audit threat and Tax Court precedent, not voluntary compliance. The rising extraction and theater over 38 years show Goodhart drift — the metric (documented hours) replaced the target (genuine participation).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (IRS examiners, Tax Court), the constraint is a necessary bright-line rule to prevent abuse — coordination with unavoidable administrative friction. From the payer seats, it is a snare: the coordination function (distinguishing active from passive) is real but the enforcement mechanism (contemporaneous logs) extracts from the very people the coordination function should protect. The engine computes this divergence; the claimed type (tangled_rope) reflects the authoring seat's judgment that both functions coexist structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury and large institutions are beneficiaries (d ~0.15–0.25): they gain revenue or competitive position without bearing compliance costs. The compliance industry is a beneficiary (d ~0.3) — it collects fees but faces market competition. Active investors, small landlords, and excluded professionals are payers (d ~0.7–0.85): they bear the full cost of compliance and the penalty of disallowed losses, with constrained or trapped exit. IRS examiners and Tax Court judges are agenda_setters (d ~0.5 symmetric): they administer and interpret but do not personally gain or lose. The derivation chain from beneficiary/victim + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1980s generic tax shelters) is largely dead — those shelters were killed by §469 itself and subsequent reforms. But the arrangement persists and has intensified. The strict gatekeeper reading treats the founding problem as 'contested' (not dead) because new shelter forms could emerge; the strategic reading treats it as dead and the rule as pure extraction. The mandate has atrophied: the rule no longer targets its original quarry but has not been repealed or narrowed. This is not a piton (no theatrical maintenance of a dead function) — the enforcement machinery is actively maintained and expanded. It is a tangled_rope: the coordination function survives in name, the extraction function dominates in operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_bar_vs_genuine_participation,
    'Does the strict documentation requirement (contemporaneous logs, calendar reconstruction) correlate with genuine material participation, or does it primarily filter for record-keeping capacity?',
    'Empirical study comparing audit outcomes for taxpayers with professional compliance help vs. self-represented taxpayers who perform equivalent labor. If outcomes diverge sharply, the bar filters for compliance capacity, not participation.',
    'If the bar filters for compliance capacity, the constraint''s extraction is structurally regressive — it extracts from those who cannot afford documentation, not from passive investors. This would increase the effective extraction for powerless payers and strengthen the snare character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_bar_vs_genuine_participation, empirical, 'Whether the documentation standard measures participation or record-keeping resources.').

omega_variable(
    strategic_reading_viability_post_strict_enforcement,
    'Can the strategic shelter reading survive as a viable compliance strategy when the IRS enforces the strict gatekeeper standard as the audit baseline?',
    'Track Tax Court and appellate decisions on grouping elections, estimation methods, and ''appropriate economic unit'' determinations. If the strategic reading''s positions are systematically rejected, it becomes a theoretical position with no practical viability.',
    'If the strategic reading loses practical viability, the kernel effectively collapses to a single reading (strict gatekeeper), and the constraint family reduces to one story. The ''contested'' founding_problem_status would resolve toward ''live'' for the strict reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_reading_viability_post_strict_enforcement, conceptual, 'Whether the sibling reading remains a live compliance option under current enforcement.').

omega_variable(
    passive_loss_limitation_revenue_impact,
    'What is the actual revenue effect of the strict gatekeeper reading vs. a permissive reading, measured in foregone deductions for active participants vs. prevented shelter deductions?',
    'JCT microsimulation model comparing revenue under strict vs. permissive material participation standards, segmented by taxpayer income and participation genuineness.',
    'If the strict reading''s revenue gain comes mostly from genuine participants (not shelter operators), the constraint is a net extraction from the wrong population — a structural error. If it comes mostly from shelter operators, the coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(passive_loss_limitation_revenue_impact, empirical, 'Revenue incidence of the strict standard: who actually pays.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 1987, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc_469_strict_tr_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1987, 0.18).
narrative_ontology:measurement(irc_469_strict_tr_t1993, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(irc_469_strict_tr_t2000, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(irc_469_strict_tr_t2005, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(irc_469_strict_tr_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(irc_469_strict_tr_t2015, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(irc_469_strict_tr_t2020, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(irc_469_strict_tr_t2025, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(irc_469_strict_be_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1987, 0.35).
narrative_ontology:measurement(irc_469_strict_be_t1993, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement(irc_469_strict_be_t2000, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(irc_469_strict_be_t2005, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(irc_469_strict_be_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(irc_469_strict_be_t2015, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(irc_469_strict_be_t2020, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(irc_469_strict_be_t2025, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(irc_469_strict_su_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1987, 0.45).
narrative_ontology:measurement(irc_469_strict_su_t1993, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(irc_469_strict_su_t2000, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(irc_469_strict_su_t2005, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2005, 0.59).
narrative_ontology:measurement(irc_469_strict_su_t2010, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(irc_469_strict_su_t2015, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(irc_469_strict_su_t2020, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2020, 0.67).
narrative_ontology:measurement(irc_469_strict_su_t2025, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.12).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_162_trade_or_business_standard).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_465_at_risk_rules).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_activity_loss_limitation_regime).

% DUAL FORMULATION NOTE:
% This story and strategic_shelter_reading form the irc_469_material_participation_kernel family. The strict reading has higher ε (0.72 vs. ~0.45 estimated for strategic) because it denies deductions to genuine participants who fail documentation. The strategic reading has lower ε but broader qualifying population — its extraction falls on Treasury (foregone revenue) rather than participants. They are linked because the strict reading's audit standards constrain the strategic reading's practical viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
