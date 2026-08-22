% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC §469 Material Participation — Strategic Shelter Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC §469's material participation test, read permissively, functions as a
 *   coordination mechanism for real estate capital deployment that has
 *   degraded into a systematic tax shelter. The strategic shelter reading
 *   allows investors to qualify as material participants by aggregating
 *   minimal hours across dozens of properties via grouping elections (Reg.
 *   §1.469-9) and logging time that often consists of reviewing monthly
 *   statements. This reading coordinates capital flow into real estate but
 *   extracts from the Treasury and compliant taxpayers. The claim/metric gap
 *   is deliberate: the constraint is CLAIMED as tangled_rope (it retains a
 *   genuine coordination function for capital allocation) while the authored
 *   metrics describe substantially extractive, actively enforced operation —
 *   the engine measures that divergence; do not reconcile the claim to the
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.62).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.28).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC §469 Material Participation — Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'b93c6cae-2330-40d2-a477-5d6292cbdd0a').
narrative_ontology:cs_kernel_codification('b93c6cae-2330-40d2-a477-5d6292cbdd0a', formalized).
narrative_ontology:cs_authority_grounding('b93c6cae-2330-40d2-a477-5d6292cbdd0a', extraction).
narrative_ontology:cs_interpretation_layer_present('b93c6cae-2330-40d2-a477-5d6292cbdd0a').
narrative_ontology:cs_reading_relation('b93c6cae-2330-40d2-a477-5d6292cbdd0a', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('b93c6cae-2330-40d2-a477-5d6292cbdd0a', foundational, bright_line_participation_suffices).
narrative_ontology:cs_axiom_status(bright_line_participation_suffices, holdable).
narrative_ontology:cs_axiom_grounding('b93c6cae-2330-40d2-a477-5d6292cbdd0a', bright_line_participation_suffices, conventional).
narrative_ontology:cs_axiom('b93c6cae-2330-40d2-a477-5d6292cbdd0a', foundational, grouping_election_unlimited_aggregation).
narrative_ontology:cs_axiom_status(grouping_election_unlimited_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('b93c6cae-2330-40d2-a477-5d6292cbdd0a', grouping_election_unlimited_aggregation, conventional).
narrative_ontology:cs_reference_frame('b93c6cae-2330-40d2-a477-5d6292cbdd0a', original_1986_participation_standard).
narrative_ontology:cs_drift_state('b93c6cae-2330-40d2-a477-5d6292cbdd0a', post_reg_1_469_9_1993, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b93c6cae-2330-40d2-a477-5d6292cbdd0a', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_re_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, re_syndicators).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, treasury).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_taxpayers).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_deduction_preservation).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, regulatory_malleability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the strategic shelter reading to qualify as material participants across multiple real estate activities through aggressive hour-counting (logging 100+ hours on many properties) and grouping elections, deducting passive losses against active income. They have capital to deploy and advisors to structure compliance. Exit is trivial — they simply reallocate capital if the reading is disallowed.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_re_investors, beneficiary,
    powerful, biographical, arbitrage, national).

% Design and sell the hour-logging templates, grouping election strategies, and audit defense packages that make the strategic shelter reading operational at scale. Their revenue depends on the reading's permissiveness; they shape the interpretive community's understanding through CPE courses and private letter ruling requests. Exit is moving to another specialty if the reading collapses.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors, agenda_setter).

% Structure syndications to deliver material participation qualification to limited partners who never visit properties, using the reading's permissive hour-counting and grouping rules as a marketing feature. Their business model embeds the reading; exit means restructuring all offerings or losing the tax-efficiency pitch.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, re_syndicators, beneficiary,
    institutional, generational, constrained, national).

% Administers IRC §469 and issues regulations, notices, and audit techniques. The strategic shelter reading creates a persistent enforcement gap — the IRS loses revenue from deductions it considers unjustified, but regulatory rewrites face political resistance from the real estate lobby. Exit is not available; they must enforce whatever the statute and regulations say.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, treasury, agenda_setter,
    institutional, generational, analytical, national).

% Real estate investors who actually spend 500+ hours per activity managing properties and do not use grouping elections. They bear higher audit scrutiny and implicit cost-shifting from the strategic shelter users' deductions. Their exit is constrained — they cannot stop being taxpayers, and changing investment strategy has real economic cost.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_taxpayers, payer,
    moderate, biographical, constrained, national).

% Adjudicate disputes when the IRS challenges material participation claims under the strategic shelter reading. Their opinions shape the reading's boundaries — some courts accept aggregated hours across grouped activities; others require per-activity substantiation. They observe the structural tension but do not set the statutory framework.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_court_judges, observer,
    institutional, generational, analytical, national).

% Enacted §469 in 1986 to curb tax shelters; has not amended the material participation standards since 1993 despite GAO reports documenting the strategic shelter reading's revenue impact. Legislative inertia and lobby pressure prevent correction. Exit is not available — they own the statute.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line test (500 hours / 100 hours / significant participation) that lets real estate investors self-certify active involvement without case-by-case IRS approval, enabling capital deployment decisions with tax certainty.
% TRANSFER_FUNCTION: Moves tax revenue from the Treasury to high-income real estate investors via passive loss deductions against active income, enabled by grouping elections and permissive hour-counting that aggregate minimal involvement across many properties into qualifying participation.
% ABSENT_VOICES: Wage earners and small business owners who cannot access passive loss deductions because they lack real estate activities to group. They would object to a two-tier system where real estate investors deduct losses against ordinary income while they cannot, but they are structurally excluded from the §469 conversation — it is a real estate investor and advisor ecosystem.
% DISAPPEARANCE_RATIONALE: If the strategic shelter reading vanished overnight, syndication offerings would lose their tax-efficiency marketing, high-income investors would reallocate capital from real estate to other assets, Treasury revenue would increase by an estimated $2-5B annually, and tax advisors would lose a major practice area. The real estate investment market would reorganize around economic returns rather than tax arbitrage.
% FOUNDING_PROBLEM: The 1986 Tax Reform Act created §469 to stop abusive tax shelters that generated paper losses from passive activities. The material participation test was meant to distinguish genuine active investors from passive limited partners. The founding problem was: how to let real entrepreneurs deduct losses while blocking shelter promoters.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Committee on Taxation's 2023 revenue estimate for §469 reform explicitly states the material participation standards no longer distinguish active from passive investors — the 100-hour and grouping rules have been exploited to recreate the shelter dynamic §469 was enacted to stop. The real estate lobby disputes this characterization; no independent academic study corroborates the lobby's position.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is high because the revenue loss is concentrated and persistent — high-income investors deduct real economic losses against wage/business income using a participation standard that has detached from actual labor. Suppression (0.28) is moderate: the IRS audits these returns but loses more often than it wins under current precedent; the constraint persists because regulatory correction is politically blocked, not because enforcement is impossible. Theater ratio (0.45) is rising: the hour-logging and grouping machinery performs 'active participation' while the economic reality is passive capital allocation. Accessibility collapse (0.38) is low — alternatives (genuine active management, REIT investment, accepting passive loss limits) remain available but are economically disfavored. Resistance (0.42) is moderate — the IRS pushes back via audits and proposed regulations, but the reading's beneficiaries have successfully defended it in court and Congress.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (investors, advisors, syndicators), the constraint is genuine coordination: it lets capital flow to real estate with tax certainty. From the Treasury and compliant taxpayer seats, the same structure is extraction: a participation standard gamed to convert passive capital into active deductions. The engine computes this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income RE investors, tax advisors, and syndicators are structural beneficiaries (d near 0.0-0.15) — they collect the tax arbitrage, control the interpretive infrastructure, and have arbitrage-grade exit. Treasury and compliant taxpayers are structural targets (d near 0.85-1.0) — they bear the revenue loss and audit asymmetry with constrained exit. Tax Court judges and Congress are agenda-setters with analytical exit — they observe and could change the constraint but face institutional inertia. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stopping 1980s-style abusive shelters) is dead — the current reading recreates the shelter dynamic through different mechanics. The constraint persists because its beneficiaries (investors, advisors, syndicators) have concentrated gains and political protection, while the costs are diffuse (Treasury revenue) and borne by a disfavored class (compliant taxpayers). This is mandatrophy: the mandate outlived its function, but the arrangement persists through institutional capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grouping_election_scope,
    'Does Reg. §1.469-9''s grouping election permit aggregating activities that share no operational unity beyond common ownership?',
    'Tax Court test case challenging a grouping election across geographically dispersed, managerially distinct properties with a single 100-hour log.',
    'If grouping requires operational unity, the strategic shelter reading''s core mechanic collapses — investors could not aggregate minimal hours across dozens of unrelated properties. Extraction would drop sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grouping_election_scope, empirical, 'Whether the grouping election''s scope is the structural enabler of the shelter').

omega_variable(
    hour_counting_verifiability,
    'Are the hour logs submitted under the strategic shelter reading verifiable as actual personal services, or are they performative documentation?',
    'IRS audit of a statistically significant sample of material participation logs, comparing claimed hours to calendar/appointment/email records.',
    'If logs are largely performative, the constraint''s theater ratio is understated and the coordination function is largely fictive — reclassification toward snare. If logs reflect real time, the coordination function is genuine but the 100-hour threshold is too low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hour_counting_verifiability, empirical, 'Whether the participation documentation reflects reality or theater').

omega_variable(
    kernel_framing_ambiguity,
    'Is the material participation test a coordination standard (bright-line for capital allocation) or a gatekeeping standard (verifiable labor requirement)?',
    'Legislative history analysis of the 1986 Act and 1993 amendments, plus comparison to the original regulatory proposals.',
    'If the kernel is inherently a coordination standard, the strategic shelter reading is the natural evolution and the strict gatekeeper reading is the deviation. If the kernel is inherently a gatekeeping standard, the strategic shelter reading is the deviation. This framing determines which reading bears the burden of justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel''s structural nature is coordination or gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1986, 0.05).
narrative_ontology:measurement(irc__tr_t1993, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(irc__tr_t2003, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(irc__tr_t2013, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(irc__tr_t2023, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2023, 0.45).
narrative_ontology:measurement(irc__tr_t2026, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(irc__be_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1986, 0.15).
narrative_ontology:measurement(irc__be_t1993, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1993, 0.25).
narrative_ontology:measurement(irc__be_t2003, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement(irc__be_t2013, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2013, 0.52).
narrative_ontology:measurement(irc__be_t2023, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement(irc__be_t2026, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1986, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1986, 0.08).
narrative_ontology:measurement(irc__su_t1993, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1993, 0.15).
narrative_ontology:measurement(irc__su_t2003, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2003, 0.22).
narrative_ontology:measurement(irc__su_t2013, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2013, 0.28).
narrative_ontology:measurement(irc__su_t2023, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2023, 0.28).
narrative_ontology:measurement(irc__su_t2026, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.18).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_passive_activity_loss_limitation).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, reg_1_469_9_grouping_elections).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_professional_status_469_c7).

% DUAL FORMULATION NOTE:
% This constraint is the strategic_shelter_reading of the irc_469_material_participation_kernel. Its sibling is strict_gatekeeper_reading. The kernel's ε-invariance principle requires separate stories: the strategic shelter reading has ε=0.62 (broad qualification, systematic deduction); the strict gatekeeper reading would have ε≈0.15 (narrow qualification, minimal deduction). They share the same statutory text but instantiate different constraints. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strategic_shelter_reading, organized, 0.1).
constraint_indexing:directionality_override(irc_469_material_participation_kernel__strategic_shelter_reading, powerful, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
