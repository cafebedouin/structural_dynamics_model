% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: Exogenous Override Reading: State Decree as Sufficient Cause of Practice Displacement
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   legitimacy_of_imposed_practice kernel: the claim that state decree
 *   authority is sufficient by itself to displace prior calendrical and
 *   sartorial practice, with compliance following from legal mandate
 *   regardless of internalization. As authored, the reading's own account of
 *   its object shows the override succeeding cleanly on the calendar axis
 *   (legal abolition achieves near-total displacement in official contexts,
 *   though rural non-compliance persists informally) and only partially on
 *   the dress axis (coercive enforcement produces code-switching rather than
 *   genuine displacement). The reading treats this partial, theater-heavy
 *   compliance as sufficient — the metrics measured here are what THIS
 *   reading takes as evidence of successful override, not a hedged average
 *   across readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "Exogenous Override Reading: State Decree as Sufficient Cause of Practice Displacement").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '175a8fe8-7db8-4c35-ba50-1e7b91bc2e77').
narrative_ontology:cs_kernel_codification('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', formalized).
narrative_ontology:cs_authority_grounding('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', extraction).
narrative_ontology:cs_interpretation_layer_present('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77').
narrative_ontology:cs_reading_relation('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', foundational, decree_authority_self_sufficient_for_displacement).
narrative_ontology:cs_axiom_status(decree_authority_self_sufficient_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', decree_authority_self_sufficient_for_displacement, conventional).
narrative_ontology:cs_axiom('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', secondary, compliance_measured_independent_of_internalization).
narrative_ontology:cs_axiom_status(compliance_measured_independent_of_internalization, holdable).
narrative_ontology:cs_axiom_grounding('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', compliance_measured_independent_of_internalization, instrumental).
narrative_ontology:cs_reference_frame('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', sovereign_decree_supremacy).
narrative_ontology:cs_drift_state('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', post_enforcement_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('175a8fe8-7db8-4c35-ba50-1e7b91bc2e77', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, central_bureaucratic_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_reform_elites).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_calendar_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, customary_dress_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reform program that authored the decrees treats legal abolition of the old calendar and coercive suppression of traditional dress as sufficient in themselves to produce a modern, legible population. It measures success by statute passage and enforcement incidence, not by whether the population has internalized the new practices, and it collects legitimacy, international recognition, and administrative uniformity from the appearance of successful transition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, agenda_setter).

% Drafts and enforces the decrees: abolishes the prior calendar in official records and law, criminalizes or heavily taxes traditional dress in public and official contexts. Deploys police, registrars, and administrative penalties to compel visible compliance. Can escalate or relax enforcement at will and answers primarily to the modernization agenda, not to the populations affected.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, central_bureaucratic_apparatus, agenda_setter,
    institutional, biographical, arbitrage, national).

% Already aligned with the new calendar and dress codes by class position, education, and proximity to state institutions. Gain status, employment access, and international legibility from the decreed changes without bearing meaningful adjustment costs; their existing practice was already convergent with the mandate.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_reform_elites, beneficiary,
    powerful, biographical, mobile, national).

% Bear the practical cost of the calendar abolition and dress mandate without having been consulted: agricultural cycles, religious observance, and market timing were built around the abolished calendar, and reworking them around the decreed replacement imposes ongoing coordination costs. Continue using the old calendar informally and maintain traditional dress outside official contexts wherever enforcement reach is thin, producing a persistent underground non-compliance the state does not fully suppress.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, generational, trapped, regional).

% Ritual specialists, farmers, and market organizers whose functional role depended on the abolished calendar's festivals and reckonings. The decree strips their calendar of legal standing overnight; they continue operating it in practice for agricultural and ceremonial purposes but lose access to official registration, courts, and state services that now run exclusively on the new calendar, creating a permanent friction cost for using either system.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_calendar_practitioners, payer,
    powerless, generational, constrained, regional).

% Subject to fines, harassment, or exclusion from official spaces for wearing customary dress. Enforcement is inconsistent and geographically uneven, so some communities substantially retain the old dress in daily life while adopting mandated dress only for encounters with officials — producing a code-switching pattern rather than genuine displacement, at the cost of constant vigilance about which context they are in.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, customary_dress_communities, payer,
    powerless, biographical, constrained, regional).

% Local officials tasked with enforcing the decrees against populations they live among. They observe the gap between decreed compliance and actual practice daily but have little formal channel to report that enforcement is producing workaround compliance rather than internalization; their career incentives reward reporting compliance, not diagnosing its shallowness.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, field_administrators, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, field_administrators, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Superficially solves a genuine coordination problem: a single official calendar and dress code simplifies administration, taxation, scheduling of state functions, and international commerce/diplomacy, all of which benefit from uniform, legible conventions.
% TRANSFER_FUNCTION: Moves administrative simplicity, international legibility, and cultural prestige to the state and aligned urban elites, while moving the cost of practical re-coordination — lost calendrical infrastructure, enforcement exposure, code-switching burden — onto rural and traditional-practice populations who were not consulted on the change.
% ABSENT_VOICES: Rural populations, calendar practitioners, and dress communities were not party to the decree's drafting; their objection — that decree alone does not produce functioning replacement infrastructure for the practices being abolished — is structurally excluded from the process that authored the mandate.
% DISAPPEARANCE_RATIONALE: From the state's vantage, removing the decree would cause an immediate rearrangement — official records, courts, and diplomacy would fracture across calendar and dress systems again. From the rural vantage, since large tracts of practice never actually displaced (the old calendar persists informally, dress code-switches by context), removal of the legal mandate would change little on the ground beyond ending the enforcement risk — the world such populations already inhabit is barely altered by the decree's continued existence.
% FOUNDING_PROBLEM: A fragmented, multi-calendar and multi-dress-code society posed genuine coordination costs for a centralizing state seeking administrative uniformity, tax collection efficiency, and international legibility.
% FOUNDING_PROBLEM_CORROBORATION: The central bureaucracy and aligned historians attest the founding problem was real and is substantially resolved by decree compliance in official contexts. Independent ethnographic and administrative-audit accounts from outside the state apparatus — reporting persistent informal calendar use and dress code-switching decades after the decree — corroborate that the underlying coordination problem was never actually resolved by legal mandate alone, only its official appearance.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the real transfer of adjustment cost onto rural and traditional-practice populations who bear the burden of maintaining dual systems (informal old-calendar use alongside mandatory new-calendar compliance; contextual dress code-switching) without having been consulted, while urban elites and the state apparatus capture administrative and diplomatic benefits at negligible personal cost. Theater ratio rises over the interval (0.20 to 0.42) as enforcement increasingly produces the appearance of displacement (official registers, court filings, formal dress-code compliance) while informal practice underneath remains stubbornly persistent — this is Goodhart drift: compliance-with-decree substitutes for actual practice change as the measured proxy. Suppression is high initially (0.85) reflecting the coercive apparatus needed to force visible compliance, and gradually declines (to 0.70) as normalization sets in among the compliant urban population, even though rural non-compliance is never fully suppressed. Resistance (0.71) is high and durable because the populations bearing the cost were never given voice in the decree's design.
 *
 * PERSPECTIVAL GAP:
 *   From the central bureaucracy's seat, the decree is doing exactly what decrees do — displacing a fragmented prior practice with a uniform one, full stop. From the rural and traditional-practice seats, the same instrument looks like an imposed cost that produces workaround compliance rather than actual change: the old calendar persists informally, dress code-switches by context. The engine computes these as structurally different experiences of the same constraint; the exogenous_override reading's error is treating the bureaucracy's seat as the only one that matters for determining whether displacement 'succeeded.'
 *
 * DIRECTIONALITY LOGIC:
 *   The state modernization agenda and central bureaucracy are structural beneficiaries and agenda-setters simultaneously — they author and enforce the mandate and collect its legitimacy dividends, placing them at the low-d, low-extraction end. Urban reform elites are secondary beneficiaries with mobile exit and prior alignment, so the mandate costs them little. Rural populations, calendar practitioners, and dress communities are structural victims: trapped or constrained exit, powerless, bearing the practical cost of a mandate authored without their input — high d, high effective extraction, amplified further by the regional/national scope mismatch between where enforcement is authored and where its cost lands.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being mistaken for either a pure Rope (the coordination story the state tells about itself) or a pure Snare (which would deny any real coordination function existed). A single official calendar and dress code IS a genuine administrative simplification with real value to a centralizing state — the coordination function is not fabricated. But the reading's own metrics show that function riding on an asymmetric transfer: the beneficiaries capture administrative and diplomatic value while a non-consulted population absorbs the friction cost of actually re-coordinating daily life. That combination — real coordination function plus asymmetric extraction sustained by active enforcement — is exactly the tangled_rope signature, and the exogenous_override reading's claim that decree-compliance is sufficient (independent of internalization) is precisely what keeps the extraction hidden inside a coordination narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_vs_internalization_gap,
    'Is legal-mandate compliance (official-context dress code adherence, formal-calendar recordkeeping) a genuine measure of practice displacement, or does it measure only the state''s enforcement reach, leaving actual practice unchanged beneath it?',
    'Longitudinal ethnographic tracking of informal calendar and dress use in private/rural contexts across the enforcement period, compared against official compliance statistics; divergence between the two series would indicate the override reading''s success metric measures enforcement theater rather than displacement.',
    'If the gap is wide and persistent, the exogenous_override reading''s central claim (decree is sufficient) is falsified by its own object — actual practice change requires the internalization pathway the endogenous_climb reading names, and this story''s theater_ratio trajectory should be read as diagnostic of that failure rather than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_vs_internalization_gap, empirical, 'Whether official compliance data reflects real displacement or enforcement theater masking persistent informal practice.').

omega_variable(
    kernel_framing_choice,
    'Is the decree episode better modeled as a single kernel with three competing readings (as done here), or does the calendar axis and dress axis actually warrant separate kernels given how differently the override reading performs on each (near-total on calendar, partial on dress)?',
    'Compare the ε and structural profile that would result from splitting calendar-override and dress-override into two separate constraint stories versus this unified reading; if the two axes produce substantially different ε and victim sets, they may be two constraints under one label rather than one reading with mixed evidence.',
    'If split, the calendar axis alone might classify closer to snare (near-complete legal override with persistent underground resistance) while the dress axis alone might classify closer to a weaker tangled_rope or even scaffold (partial, evolving displacement) — the combined reading''s tangled_rope classification could be an artifact of averaging two structurally distinct override processes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether calendar-override and dress-override are one constraint or two, given their divergent displacement trajectories.').

omega_variable(
    rural_powerlessness_persistence,
    'Does the informal persistence of old-calendar use and traditional dress among rural populations constitute effective coalition resistance, or is it merely a byproduct of thin state enforcement reach that would collapse if enforcement capacity increased?',
    'Examine whether informal practice persistence correlates with organized resistance activity (petitions, local leadership coordination) or purely with geographic/administrative distance from enforcement centers.',
    'If organized, rural populations have more effective structural power than their ''powerless'' designation suggests, which would push their computed directionality toward the symmetric range rather than full-target; if merely a distance artifact, the powerless/trapped designation stands and extraction remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_powerlessness_persistence, empirical, 'Whether rural non-compliance reflects coalition agency or simple enforcement-reach limits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__exogenous_override_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the legitimacy_of_imposed_practice kernel. endogenous_climb_reading holds displacement requires internalization and fails without bottom-up adoption; hybrid_scaffolding_reading holds that ideological scaffolding atop decree achieves partial, durable displacement where pure decree and pure climb both fall short. Each reading is authored as its own constraint with its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; this reading's ε (0.68) reflects the override reading's own evidentiary standard (decree-compliance), not a hedge across the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
