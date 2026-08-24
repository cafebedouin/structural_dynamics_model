% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: IRC 469 Material Participation — Strategic Shelter Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC 469's material participation tests were designed to distinguish
 *   active business operators from passive investors. The strategic shelter
 *   reading treats the seven statutory tests — especially the
 *   100-hour/500-hour rules and the grouping election under Reg. 1.469-9 — as
 *   a permissive threshold that high-income real estate investors can
 *   reliably meet through disciplined hour-logging and strategic aggregation
 *   of properties. This reading enables systematic passive loss deductions
 *   against active income, functioning as a wealth preservation mechanism.
 *   The claimed type is tangled_rope: there is a genuine coordination
 *   function (defining active participation) but it operates with asymmetric
 *   extraction — wealthy investors and their advisors capture the benefit
 *   while the tax base and compliant investors bear the cost, and active
 *   enforcement (audits, litigation) is required to maintain the boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.72).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.38).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC 469 Material Participation — Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '5995f81f-f5e8-49cd-a7ad-aa822d893768').
narrative_ontology:cs_kernel_codification('5995f81f-f5e8-49cd-a7ad-aa822d893768', formalized).
narrative_ontology:cs_authority_grounding('5995f81f-f5e8-49cd-a7ad-aa822d893768', lineage).
narrative_ontology:cs_interpretation_layer_present('5995f81f-f5e8-49cd-a7ad-aa822d893768').
narrative_ontology:cs_reading_relation('5995f81f-f5e8-49cd-a7ad-aa822d893768', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('5995f81f-f5e8-49cd-a7ad-aa822d893768', foundational, taxpayer_election_authority_governs_participation).
narrative_ontology:cs_axiom_status(taxpayer_election_authority_governs_participation, holdable).
narrative_ontology:cs_axiom_grounding('5995f81f-f5e8-49cd-a7ad-aa822d893768', taxpayer_election_authority_governs_participation, conventional).
narrative_ontology:cs_axiom('5995f81f-f5e8-49cd-a7ad-aa822d893768', secondary, grouping_election_enables_cross_property_aggregation).
narrative_ontology:cs_axiom_status(grouping_election_enables_cross_property_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('5995f81f-f5e8-49cd-a7ad-aa822d893768', grouping_election_enables_cross_property_aggregation, conventional).
narrative_ontology:cs_reference_frame('5995f81f-f5e8-49cd-a7ad-aa822d893768', statutory_material_participation_framework).
narrative_ontology:cs_drift_state('5995f81f-f5e8-49cd-a7ad-aa822d893768', post_regulatory_election_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5995f81f-f5e8-49cd-a7ad-aa822d893768', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_shelter_advisors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndicators).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, federal_tax_base).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_real_estate_investors).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, taxpayer_election_authority_governs_participation).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_deduction_as_wealth_preservation_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use grouping elections and aggressive hour-logging (property management oversight, contractor supervision, travel time) to meet material participation tests across multiple properties. This unlocks unlimited passive loss deductions against active income, reducing effective tax rates substantially. They have professional advisors who structure the compliance and defend it on audit.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors, beneficiary,
    powerful, biographical, arbitrage, national).

% Design and implement the hour-counting systems, grouping elections, and documentation protocols that make the permissive reading work. They charge substantial fees for structuring and audit defense. Their business model depends on the reading remaining viable; they lobby for regulatory stability and litigate aggressively.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_shelter_advisors, beneficiary,
    organized, biographical, mobile, national).

% Package real estate deals with built-in material participation structures for high-net-worth limited partners. The permissive reading is a selling point — investors buy the tax loss allocation as much as the property economics. They coordinate with advisors to maintain compliant hour logs across the investor pool.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndicators, beneficiary,
    organized, biographical, mobile, national).

% Absorbs the revenue loss from passive loss deductions that the permissive reading enables. Estimates suggest billions annually in deferred or eliminated tax liability from real estate investors using material participation to offset active income. No exit — the tax base cannot opt out of the revenue hole.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, federal_tax_base, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(irc_469_material_participation_kernel__strategic_shelter_reading, federal_tax_base).

% Bear the distributional consequence: either higher rates, reduced services, or increased deficit financing to cover the revenue gap. They cannot access the shelter (insufficient income, no real estate portfolio, no advisory access) and have no structural power to change the interpretation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers, payer,
    powerless, biographical, trapped, national).

% Invest in real estate without aggressive structuring — they either don't qualify for material participation or choose not to push the boundary. They compete for deals against syndicators and sheltered investors who can pay more because their after-tax return is higher. Their exit is accepting lower returns or adopting the shelter strategy.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, compliant_real_estate_investors, payer,
    moderate, biographical, constrained, national).

% Administers IRC 469 through audits, notices, and litigation. Issues regulations and guidance that attempt to cabin the permissive reading (e.g., hourly log requirements, anti-abuse rules). Resource-constrained relative to the sheltered population; wins some cases but loses others in Tax Court, creating a contested enforcement landscape.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_examination_division, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate material participation disputes. Their opinions swing between deferring to taxpayer elections (upholding the permissive reading) and imposing substantive economic reality tests (the strict gatekeeper reading). No unified precedent — the case law is a patchwork that both readings cite.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_court_judges, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines when a taxpayer is sufficiently engaged in a rental real estate activity to treat it as non-passive, allowing loss deductions against active income. The statutory tests (seven alternatives) are meant to distinguish active managers from passive investors.
% TRANSFER_FUNCTION: Moves tax revenue from the federal fisc to high-income real estate investors by recharacterizing passive rental losses as non-passive through grouping elections and hour-counting, enabling deduction against wages, business income, and portfolio income.
% ABSENT_VOICES: Low-income renters who indirectly bear incidence through market dynamics; state tax authorities who lose conformity revenue but lack independent enforcement capacity; academic tax policy voices who argue the provision distorts capital allocation — none are present in the structuring/audit/litigation loop.
% DISAPPEARANCE_RATIONALE: If the permissive reading vanished overnight, high-income investors would lose billions in annual tax deductions, syndicators would restructure offerings, advisors would lose a core product line, and the IRS would shift enforcement resources. The real estate investment market would reprice for after-tax returns without the shelter.
% FOUNDING_PROBLEM: The 1986 Tax Reform Act created the passive activity loss rules to stop taxpayers from using paper losses from tax shelters to offset active income. Material participation was the escape hatch for genuine business operators — but the statutory tests (especially the 100-hour/500-hour rules and grouping election) were written broadly enough to be gamed.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Committee on Taxation's post-1986 analyses and Treasury's 1992 regulatory preamble both state the purpose was limiting shelter deductions for passive investors. The permissive reading's broad qualifying population and low compliance friction are acknowledged by tax policy scholars outside the benefiting parties (e.g., Graetz, Schizer, Bankman) as having subverted the founding problem.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.72) because the revenue loss is large, concentrated, and persistent. Suppression is moderate (0.38) — the IRS audits and litigates but the statutory text and regulations give taxpayers substantial room; the constraint persists because the cost of tighter enforcement (political, administrative) exceeds the marginal revenue. Theater ratio (0.45) reflects that hour-logging and grouping elections are real compliance activity but increasingly performative — the logs are built to satisfy the test, not to reflect economic substance. Accessibility collapse is low (0.35) because alternative tax strategies exist and the constraint doesn't foreclose other wealth preservation paths. Resistance (0.55) is moderate — the IRS pushes back but courts frequently side with taxpayers on the permissive reading.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint is genuine coordination — the tests provide clear rules for when rental activity rises to business-level engagement. From the victim seats, it is engineered extraction — the rules are gamed via hour inflation and artificial grouping. The engine computes this divergence from the structural data; the authored claim (tangled_rope) acknowledges both coordination and extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income investors, advisors, and syndicators are structural beneficiaries (d near 0.0) — they collect the tax savings and fees. The federal tax base and general taxpayers are structural victims (d near 1.0) — they bear the diffuse revenue loss with no exit. Compliant investors sit in between (d ~ 0.5) — they face competitive disadvantage but could adopt the strategy. The IRS is the agenda setter but with constrained power (resource limits, adverse precedent). Tax Court judges are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stopping passive loss shelters) is dead — the permissive reading has inverted the provision into a shelter enabler. The arrangement persists because beneficiaries (investors, advisors, syndicators) are concentrated and powerful, while victims (tax base, general taxpayers) are diffuse and powerless. The IRS could tighten regulations but faces political capture and resource asymmetry. This is classic mandatrophy: the mandate has outlived its function but the constraint remains because no coalition with power and incentive exists to fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_stability,
    'Will the strategic shelter reading remain viable under renewed legislative or regulatory pressure, or will the strict gatekeeper reading become the enforced standard?',
    'Legislative action (e.g., closing the grouping election for rental real estate), regulatory amendment (Treasury narrowing the hour-counting rules), or a Supreme Court decision adopting the strict gatekeeper framework.',
    'If the strict reading becomes enforced, extractiveness drops sharply (the shelter closes), theater ratio collapses (compliance becomes substantive), and the constraint reclassifies toward rope or mountain. If the permissive reading holds, extractiveness continues accumulating and the constraint trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_stability, empirical, 'Whether the permissive reading survives the next enforcement or legislative cycle.').

omega_variable(
    economic_substance_vs_formal_compliance,
    'Does the hour-logging and grouping election compliance activity reflect genuine economic engagement, or is it purely formal performance with no operational reality?',
    'Empirical study of time-use data for investors claiming material participation vs. actual management decisions made; comparison to professional property manager benchmarks.',
    'If purely formal, the coordination function is a sham and the constraint is snare, not tangled rope. If partially substantive, the tangled rope classification holds with a lower coordination floor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_substance_vs_formal_compliance, empirical, 'Whether the coordination function has any economic substance beyond tax optimization.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel best framed as ''the material participation tests'' (statutory provisions) or as ''the passive loss limitation regime'' (the overall anti-shelter architecture of which 469 is one component)?',
    'Legislative history analysis of the 1986 Act: whether Congress viewed the seven tests as the operative constraint or as safe harbors within a broader ''material participation'' principle.',
    'If the kernel is the broader regime, the strategic shelter reading is a distortion of the entire architecture (stronger extraction claim). If the kernel is the seven tests specifically, the reading is a plausible textual interpretation (weaker extraction claim). This framing choice changes the ε referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel boundary is the specific tests or the overall passive loss limitation purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1987, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1987, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(irc__tr_t1992, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(irc__tr_t1997, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 1997, 0.31).
narrative_ontology:measurement(irc__tr_t2003, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement(irc__tr_t2010, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(irc__tr_t2017, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2017, 0.44).
narrative_ontology:measurement(irc__tr_t2025, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(irc__be_t1987, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1987, 0.25).
narrative_ontology:measurement(irc__be_t1992, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement(irc__be_t1997, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(irc__be_t2003, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2003, 0.61).
narrative_ontology:measurement(irc__be_t2010, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(irc__be_t2017, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2017, 0.7).
narrative_ontology:measurement(irc__be_t2025, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1987, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1987, 0.2).
narrative_ontology:measurement(irc__su_t1992, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1992, 0.28).
narrative_ontology:measurement(irc__su_t1997, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 1997, 0.33).
narrative_ontology:measurement(irc__su_t2003, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2003, 0.35).
narrative_ontology:measurement(irc__su_t2010, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement(irc__su_t2017, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement(irc__su_t2025, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.18).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_grouping_election_reg_1_469_9).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_real_estate_professional_exception).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, passive_activity_loss_limitation_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the irc_469_material_participation_kernel. The sibling strict_gatekeeper_reading imposes a high substantive bar. The two readings coexist in the case law and enforcement landscape. The strategic shelter reading exploits the textual breadth of the seven tests and the grouping election; the strict gatekeeper reading imposes an economic substance overlay. They are linked via network.affects_constraints in both stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strategic_shelter_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
