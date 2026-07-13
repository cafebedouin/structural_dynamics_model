% ============================================================================
% CONSTRAINT STORY: clock_incompatibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clock_incompatibility_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: clock_incompatibility_reading
 *   human_readable: Hardware Depreciation Clock vs. Labor Market Absorption Clock (Timed-Out Displacement Axiom)
 *   domain: political_economy/labor_economics/technology_governance
 *
 * SUMMARY:
 *   Automation and AI-driven hardware substitution is frequently justified by
 *   appeal to the historical pattern that technological displacement is
 *   followed, eventually, by new market creation that reabsorbs displaced
 *   labor (agricultural mechanization producing an industrial workforce,
 *   computerization producing a services and software workforce). This
 *   reading argues the mechanism itself is not in dispute — new markets do
 *   form and do eventually employ comparable numbers of workers — but that
 *   the timescale on which capital deployment decisions are actually made (a
 *   24-36 month hardware/software depreciation and refresh cycle) is roughly
 *   half to a third the length of the empirically observed absorption
 *   timescale (5-7 years). The axiom is not falsified; it is timed-out,
 *   systematically arriving too late relative to the decision clock that
 *   triggers displacement in the first place.
 *
 * KEY AGENTS:
 *   - capital_owners_of_automation_hardware: institutional beneficiary controlling deployment timing
 *   - displaced_middle_skill_workers: primary payer bearing the clock-gap income loss
 *   - junior_labor_market_entrants: powerless payer experiencing the gap as an entry barrier
 *   - labor_economists_and_displacement_researchers: analytical observer documenting the timescale mismatch
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clock_incompatibility_reading, 0.68).
domain_priors:suppression_score(clock_incompatibility_reading, 0.45).
domain_priors:theater_ratio(clock_incompatibility_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clock_incompatibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(clock_incompatibility_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(clock_incompatibility_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(clock_incompatibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(clock_incompatibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clock_incompatibility_reading, tangled_rope).
narrative_ontology:human_readable(clock_incompatibility_reading, "Hardware Depreciation Clock vs. Labor Market Absorption Clock (Timed-Out Displacement Axiom)").
narrative_ontology:topic_domain(clock_incompatibility_reading, "political_economy/labor_economics/technology_governance").

domain_priors:requires_active_enforcement(clock_incompatibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(clock_incompatibility_reading, 'fe89e9bc-9ba4-4af9-8402-1c5c52ecec61').
narrative_ontology:cs_kernel_codification('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', distributed).
narrative_ontology:cs_authority_grounding('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', distributed).
narrative_ontology:cs_reading_relation('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', technological_displacement_axiom__temporal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', technological_displacement_axiom__skills_mismatch_reading, influences).
narrative_ontology:cs_axiom('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', foundational, absorption_mechanism_is_real_but_rate_limited).
narrative_ontology:cs_axiom_status(absorption_mechanism_is_real_but_rate_limited, holdable).
narrative_ontology:cs_axiom_grounding('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', absorption_mechanism_is_real_but_rate_limited, empirically_contingent).
narrative_ontology:cs_axiom('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', foundational, deployment_clock_and_absorption_clock_are_structurally_distinct_and_independently_set).
narrative_ontology:cs_axiom_status(deployment_clock_and_absorption_clock_are_structurally_distinct_and_independently_set, holdable).
narrative_ontology:cs_axiom_grounding('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', deployment_clock_and_absorption_clock_are_structurally_distinct_and_independently_set, empirically_contingent).
narrative_ontology:cs_created_at('fe89e9bc-9ba4-4af9-8402-1c5c52ecec61', '').
narrative_ontology:cs_kernel_id(clock_incompatibility_reading, technological_displacement_axiom).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clock_incompatibility_reading, capital_owners_of_automation_hardware).
narrative_ontology:constraint_beneficiary(clock_incompatibility_reading, apex_technical_specialists).
narrative_ontology:constraint_beneficiary(clock_incompatibility_reading, compute_infrastructure_vendors).
narrative_ontology:constraint_victim(clock_incompatibility_reading, displaced_middle_skill_workers).
narrative_ontology:constraint_victim(clock_incompatibility_reading, junior_labor_market_entrants).
narrative_ontology:constraint_victim(clock_incompatibility_reading, regional_labor_markets_dependent_on_displaced_sectors).
narrative_ontology:constraint_vindicates(clock_incompatibility_reading, creative_destruction_absorption_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns or finances the compute/hardware stock that substitutes for labor. Depreciation schedules (accounting and technical) run 24-36 months, which sets the pace of deployment decisions independent of what happens to displaced workers. Captures productivity gains immediately upon substitution and can redeploy capital to the next hardware generation before any new-market absorption of displaced labor has time to materialize.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, capital_owners_of_automation_hardware, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(clock_incompatibility_reading, capital_owners_of_automation_hardware, agenda_setter).

% Small cohort whose skills complement rather than compete with the new hardware generation. Wages and demand for this group rise as substitution accelerates. They face essentially no version of the 36-month clock as a threat; if anything the clock working faster increases their scarcity premium.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, apex_technical_specialists, beneficiary,
    powerful, biographical, mobile, global).

% Sell the hardware/software stack that drives the depreciation-forced substitution cycle. Every accelerated replacement cycle is a sales event; they have a direct commercial interest in the 36-month clock running fast relative to any labor-absorption clock, since a slower substitution pace would reduce unit sales.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, compute_infrastructure_vendors, beneficiary,
    institutional, biographical, arbitrage, global).

% Occupy roles substitutable within one to two hardware refresh cycles. The mechanism that would eventually re-employ them in markets created by the new technology is real but historically takes 5-7 years to generate comparable job volume — roughly double to triple the 36-month window in which their existing role is retired. They bear the income and skill-obsolescence gap in the interval between clocks, with retraining programs typically calibrated to the wrong (slower) timeline.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, displaced_middle_skill_workers, payer,
    moderate, biographical, constrained, national).

% Enter a labor market where entry-level roles are the first substituted (cheapest to automate, least institutionally protected) and where the new-market jobs the axiom promises have not yet been created at the point they need first employment. Unlike incumbent workers they have no prior earnings cushion and no seniority-based protection; they experience the clock gap as an entry barrier rather than a displacement event, which is harder to measure and harder to compensate.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, junior_labor_market_entrants, payer,
    powerless, generational, trapped, national).

% Local economies concentrated in the displaced sector absorb the fiscal and social cost of the clock gap collectively — falling tax base, rising local unemployment services demand, out-migration of working-age population — well before any new-market job creation, which tends to concentrate in different regions entirely, reaches them.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, regional_labor_markets_dependent_on_displaced_sectors, payer,
    powerless, generational, trapped, regional).

% Design retraining and transition programs but are structurally excluded from the deployment-timing decision itself, which is made unilaterally by capital owners on a depreciation schedule. They are asked to solve a 5-7 year absorption problem with instruments funded and politically justified on a 12-24 month legislative cycle, a mismatch neither clock accommodates.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, policy_and_retraining_institutions, excluded,
    organized, biographical, constrained, national).

% Study historical absorption timelines (agricultural mechanization, containerization, computerization waves) and document the empirical 5-7 year lag between displacement and comparable-scale market-creation employment. Their findings underlie this reading's claim that the axiom's mechanism is real but structurally too slow relative to the hardware clock.
narrative_ontology:constraint_stakeholder(clock_incompatibility_reading, labor_economists_and_displacement_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(clock_incompatibility_reading, capital_owners_of_automation_hardware).
narrative_ontology:fixing_cost_class(clock_incompatibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rapid hardware substitution genuinely coordinates a real efficiency gain: newer compute/automation generations perform tasks at lower marginal cost, and capital owners coordinating fast replacement cycles do solve a real technical-obsolescence problem (older hardware becomes uneconomic to run, maintain, and secure).
% TRANSFER_FUNCTION: The arrangement moves employment security and income stability from displaced middle-skill and entry-level workers to capital owners and apex specialists, mediated by the mismatch between the 36-month depreciation-driven substitution clock and the 5-7 year new-market labor-absorption clock; the gap period's income losses are absorbed by workers and dependent regions, not by the capital that triggered the substitution.
% ABSENT_VOICES: Displaced workers and junior entrants have no seat in the depreciation-schedule or capital-deployment decision; that decision is made purely on hardware economics. Regional governments bear the fiscal consequence without having been consulted on deployment timing. If present, they would argue for staged substitution schedules pegged to the empirical absorption timeline rather than the equipment refresh timeline.
% DISAPPEARANCE_RATIONALE: If the clock mismatch were resolved (e.g., by pegging substitution pace to absorption capacity), capital owners and vendors would lose the compressed-cycle sales and productivity-capture advantage they currently enjoy, while displaced workers and regions would gain a genuine transition runway — so the two sides dispute whether 'the world rearranges' or 'nothing changes,' because each side's baseline assumption about which clock is legitimate differs.
% FOUNDING_PROBLEM: The underlying axiom (new markets absorb displaced labor) was articulated to explain and justify historical technological transitions where substitution and absorption, while never perfectly synchronized, occurred on comparable timescales relative to a human working life. The founding problem was distinguishing genuine technological progress from simple immiseration.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists and displacement researchers, external to both capital owners and displaced workers, attest that the absorption mechanism itself remains empirically live (5-7 year new-market job creation is observed repeatedly across technology waves) but that the hardware depreciation clock has compressed substantially faster than absorption timelines have shortened — meaning the founding problem's *solution* (the axiom) is not dead, but the *timing assumption* underlying its adequacy has become false. No party inside the beneficiary set (capital owners, vendors, apex specialists) has independently corroborated the timing mismatch; it is documented primarily by academic labor economists and by policy institutions excluded from deployment decisions.
narrative_ontology:disappearance_verdict(clock_incompatibility_reading, contested).
narrative_ontology:founding_problem_status(clock_incompatibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(clock_incompatibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(clock_incompatibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(clock_incompatibility_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clock_incompatibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(clock_incompatibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(clock_incompatibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as substantial and rising through the substitution-heavy early-to-mid interval (0.42 to a peak of 0.68 around month 48) because during the clock gap, productivity gains accrue to capital while displaced labor's income losses are uncompensated — the gap itself is the extraction window. Extractiveness is authored as declining in the back half of the interval (48 to 84) as the 5-7 year absorption mechanism finally begins to operate and new-market job creation catches up, consistent with this reading's own claim that the mechanism is real, just delayed. Theater ratio rises alongside extractiveness because retraining and transition programs are visibly funded and publicized during the gap period but are calibrated to legislative (12-24 month) rather than absorption (5-7 year) timescales, producing programs that perform responsiveness without matching the actual problem's duration — theater peaks near month 36-48 and recedes as absorption catches up and the mismatch resolves itself independent of the programs. Suppression is authored moderately (0.45 peak) reflecting that this is not primarily a coercive constraint — workers are not physically prevented from seeking new employment — but there is real suppression in the form of contractual and institutional lock-in (non-competes, licensing requirements, geographic immobility) that slows individual adaptation during exactly the window when speed matters most.
 *
 * PERSPECTIVAL GAP:
 *   From the capital owner's seat, the arrangement looks like efficient allocation of resources to their most productive use, with adjustment costs being an unfortunate but temporary externality that markets will eventually correct — consistent with a rope or even mountain framing ('this is just how technological progress works'). From the displaced worker's seat, the same structure is a tangled rope: a real coordination function (efficient resource reallocation) riding alongside an asymmetric extraction (productivity gains captured by capital immediately, costs borne by labor for years). The engine should compute these as structurally different seat-level classifications from the same base data, which is exactly the point of this reading: the axiom's mechanism-reality does not settle whether the current deployment practice is defensible.
 *
 * DIRECTIONALITY LOGIC:
 *   Capital owners and compute vendors sit at the beneficiary end: they set deployment timing, capture productivity gains immediately, and bear none of the interval-gap cost — their exit option is arbitrage (redeploy capital to the next generation regardless of labor outcomes). Apex technical specialists are also beneficiaries by complementarity rather than by control: their skill is scarce precisely because it is not substitutable, so acceleration of the clock increases rather than threatens their position. Displaced middle-skill workers and junior entrants sit at the target end: their exit options are constrained or trapped respectively, and their d value should reflect that junior entrants face an even harder version of the constraint than incumbents (no cushion, no seniority) despite appearing structurally similar. Regional labor markets are powerless and trapped at a structural level distinct from individual workers — geographic immobility of capital-intensive local economies compounds the individual worker's own mobility constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists two mislabeling failures. First, it does not let the historically real absorption mechanism launder the current extraction as purely coordinative (the mountain/rope framing capital owners prefer) — the tangled_rope classification requires naming victims and enforcement precisely because the coordination function's genuineness does not erase the asymmetric cost distribution during the gap. Second, it does not treat the axiom as simply false (which would support a pure snare framing with no real coordination function) — the declining extraction and theater trajectory in the back half of the measurement interval reflects the reading's own claim that the mechanism eventually operates. The founding_problem status of 'contested' captures exactly this: the problem (justifying technological transition) is not dead, but whether the current arrangement still solves it on a defensible timescale is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_clock_incompatibility,
    'Is the technological_displacement_axiom''s failure best characterized as a temporal mismatch between two real clocks (this reading), as no failure at all because the clocks are comparable enough (temporal_equivalence_reading), or as a persistent skills mismatch that no amount of additional time would resolve for the specific displaced cohort (skills_mismatch_reading)?',
    'Track cohort-level re-employment outcomes for displaced workers at 3, 5, 7, and 10 years post-displacement, disaggregated by whether the new jobs created require retraining versus require different innate or credentialed skill profiles the cohort could plausibly acquire. If most displaced workers are eventually absorbed given sufficient time, the clock-incompatibility reading is supported. If a specific subpopulation remains unabsorbed even after 10+ years regardless of retraining investment, the skills-mismatch reading better explains that subpopulation''s experience.',
    'If skills_mismatch_reading is correct for a meaningful subpopulation, this reading''s remedy path (resynchronize deployment pace to absorption pace) would not fix that subpopulation''s outcomes even if fully implemented — the beneficiary/victim structure would need a third victim category (permanently non-absorbable labor) not captured here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_clock_incompatibility, conceptual, 'Which of three sibling readings of the displacement axiom kernel best fits observed re-employment data.').

omega_variable(
    depreciation_clock_endogeneity,
    'Is the 36-month hardware depreciation clock itself a fixed technical fact, or is it partly a choice made by vendors and capital owners who could extend refresh cycles at some cost to their own margins?',
    'Compare hardware depreciation schedules across regulatory environments with different capital-goods tax treatment and different vendor market structures (monopolistic vs. competitive); if depreciation pace varies substantially with policy and market structure rather than tracking pure technical obsolescence, the clock is partly endogenous to the beneficiary group''s own incentives.',
    'If the clock is substantially endogenous, the beneficiaries are not merely faster-moving than a fixed absorption timescale but are actively setting the pace of the mismatch that generates their own extraction — this would strengthen the case for reclassifying toward snare rather than tangled_rope, since the coordination function''s independence from the extraction becomes harder to sustain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(depreciation_clock_endogeneity, empirical, 'Whether the substitution clock is a fixed technical constraint or a partly chosen parameter set by the beneficiary group.').

omega_variable(
    absorption_mechanism_naturalness,
    'Is the 5-7 year absorption timescale itself a stable empirical regularity across technology waves, or has it been lengthening across successive waves (mechanization, computerization, AI), such that this reading''s remedy (resynchronize the clocks) becomes progressively less achievable?',
    'Compile absorption timelines across at least four historical technology transitions with comparable measurement methodology and test for a trend in the gap length over time.',
    'If the absorption timescale is lengthening while the deployment clock is shortening, the mismatch is structurally worsening rather than a one-time gap, which would push this reading''s classification toward a harder-to-fix, more persistently extractive tangled_rope or toward reconsidering whether the axiom''s mechanism itself is weakening, not merely mistimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_mechanism_naturalness, empirical, 'Whether the clock gap this reading identifies is stable or widening across successive technology waves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clock_incompatibility_reading, 0, 84).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cloc_tr_t0, clock_incompatibility_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(cloc_tr_t0, observed).
narrative_ontology:measurement(cloc_tr_t12, clock_incompatibility_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(cloc_tr_t12, observed).
narrative_ontology:measurement(cloc_tr_t24, clock_incompatibility_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(cloc_tr_t24, observed).
narrative_ontology:measurement(cloc_tr_t36, clock_incompatibility_reading, theater_ratio, 36, 0.55).
narrative_ontology:measurement_basis(cloc_tr_t36, observed).
narrative_ontology:measurement(cloc_tr_t48, clock_incompatibility_reading, theater_ratio, 48, 0.56).
narrative_ontology:measurement_basis(cloc_tr_t48, projected).
narrative_ontology:measurement(cloc_tr_t60, clock_incompatibility_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement_basis(cloc_tr_t60, projected).
narrative_ontology:measurement(cloc_tr_t72, clock_incompatibility_reading, theater_ratio, 72, 0.46).
narrative_ontology:measurement_basis(cloc_tr_t72, projected).
narrative_ontology:measurement(cloc_tr_t84, clock_incompatibility_reading, theater_ratio, 84, 0.4).
narrative_ontology:measurement_basis(cloc_tr_t84, projected).

% Extraction over time
narrative_ontology:measurement(cloc_be_t0, clock_incompatibility_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cloc_be_t0, observed).
narrative_ontology:measurement(cloc_be_t12, clock_incompatibility_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(cloc_be_t12, observed).
narrative_ontology:measurement(cloc_be_t24, clock_incompatibility_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(cloc_be_t24, observed).
narrative_ontology:measurement(cloc_be_t36, clock_incompatibility_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(cloc_be_t36, observed).
narrative_ontology:measurement(cloc_be_t48, clock_incompatibility_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(cloc_be_t48, projected).
narrative_ontology:measurement(cloc_be_t60, clock_incompatibility_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement_basis(cloc_be_t60, projected).
narrative_ontology:measurement(cloc_be_t72, clock_incompatibility_reading, base_extractiveness, 72, 0.55).
narrative_ontology:measurement_basis(cloc_be_t72, projected).
narrative_ontology:measurement(cloc_be_t84, clock_incompatibility_reading, base_extractiveness, 84, 0.45).
narrative_ontology:measurement_basis(cloc_be_t84, projected).

% Suppression requirement over time
narrative_ontology:measurement(cloc_su_t0, clock_incompatibility_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(cloc_su_t0, observed).
narrative_ontology:measurement(cloc_su_t12, clock_incompatibility_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement_basis(cloc_su_t12, observed).
narrative_ontology:measurement(cloc_su_t24, clock_incompatibility_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(cloc_su_t24, observed).
narrative_ontology:measurement(cloc_su_t36, clock_incompatibility_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement_basis(cloc_su_t36, observed).
narrative_ontology:measurement(cloc_su_t48, clock_incompatibility_reading, suppression_requirement, 48, 0.44).
narrative_ontology:measurement_basis(cloc_su_t48, projected).
narrative_ontology:measurement(cloc_su_t60, clock_incompatibility_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement_basis(cloc_su_t60, projected).
narrative_ontology:measurement(cloc_su_t72, clock_incompatibility_reading, suppression_requirement, 72, 0.35).
narrative_ontology:measurement_basis(cloc_su_t72, projected).
narrative_ontology:measurement(cloc_su_t84, clock_incompatibility_reading, suppression_requirement, 84, 0.3).
narrative_ontology:measurement_basis(cloc_su_t84, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clock_incompatibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(clock_incompatibility_reading, 0.12).
narrative_ontology:affects_constraint(clock_incompatibility_reading, temporal_equivalence_reading).
narrative_ontology:affects_constraint(clock_incompatibility_reading, skills_mismatch_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the technological_displacement_axiom kernel. temporal_equivalence_reading denies a structural break exists (the two clocks are close enough for the historical pattern to hold without remedy). skills_mismatch_reading relocates the failure from timing to a persistent skills-cohort mismatch that time alone does not resolve. This reading (clock_incompatibility_reading) is distinguished by proposing a specific, in-principle-fixable mechanism (resynchronizing deployment pace to absorption pace) and by introducing regional labor markets and junior entrants as a distinct victim class not necessarily present in the other readings' beneficiary/victim structure. ε for this reading is authored independently of the siblings; it should not be averaged or reconciled against their values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
