% ============================================================================
% CONSTRAINT STORY: channel_conversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_channel_conversion_reading, []).

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
 *   constraint_id: channel_conversion_reading
 *   human_readable: H-2A Visa Exit-Mobility Constraint (Channel Conversion Reading)
 *   domain: administrative_law/labor_economics/immigration_policy
 *
 * SUMMARY:
 *   The H-2A agricultural guest-worker visa program pairs an adverse-effect
 *   wage-rate guarantee with employer-specific sponsorship: a worker admitted
 *   under the program is bound to the petitioning employer and cannot
 *   lawfully change employers without a new petition. Interior immigration
 *   enforcement simultaneously raises the cost of remaining in the
 *   unauthorized labor market. This reading holds that the combination —
 *   cheap legal entry plus intensified enforcement of the alternative —
 *   functions as a conversion mechanism: workers who previously had informal
 *   exit power (leaving one unauthorized job for another with minimal
 *   consequence) are moved into a formal status that eliminates that power,
 *   and the resulting employer-side monopsony rent is what actually erodes
 *   wages, independent of whether the wage-rate calculation itself is
 *   administered correctly.
 *
 * KEY AGENTS:
 *   - agricultural_employer_associations: primary beneficiary (organized/arbitrage) — captures monopsony rent from bound labor
 *   - h2a_visa_workers: primary target (powerless/identity_locked) — bears the mobility loss the visa imposes
 *   - displaced_unauthorized_farmworkers: secondary target (powerless/trapped) — pushed out by the enforcement half of the conversion
 *   - interior_enforcement_agencies: co-agenda-setter (institutional) — the enforcement intensity is half the mechanism
 *   - department_of_labor_wage_office: analytical observer misreading the mechanism — treats wage measurement as primary when mobility is primary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(channel_conversion_reading, 0.71).
domain_priors:suppression_score(channel_conversion_reading, 0.79).
domain_priors:theater_ratio(channel_conversion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(channel_conversion_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(channel_conversion_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(channel_conversion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(channel_conversion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(channel_conversion_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(channel_conversion_reading, tangled_rope).
narrative_ontology:human_readable(channel_conversion_reading, "H-2A Visa Exit-Mobility Constraint (Channel Conversion Reading)").
narrative_ontology:topic_domain(channel_conversion_reading, "administrative_law/labor_economics/immigration_policy").

domain_priors:requires_active_enforcement(channel_conversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(channel_conversion_reading, '44886d0c-0890-4041-85e1-4156ef2e3cc4').
narrative_ontology:cs_kernel_codification('44886d0c-0890-4041-85e1-4156ef2e3cc4', formalized).
narrative_ontology:cs_authority_grounding('44886d0c-0890-4041-85e1-4156ef2e3cc4', extraction).
narrative_ontology:cs_interpretation_layer_present('44886d0c-0890-4041-85e1-4156ef2e3cc4').
narrative_ontology:cs_reading_relation('44886d0c-0890-4041-85e1-4156ef2e3cc4', adverse_effect_guarantee_kernel__instrument_dependent_reading, coexists_with).
narrative_ontology:cs_reading_relation('44886d0c-0890-4041-85e1-4156ef2e3cc4', adverse_effect_guarantee_kernel__textualist_severability_reading, coexists_with).
narrative_ontology:cs_reading_relation('44886d0c-0890-4041-85e1-4156ef2e3cc4', adverse_effect_guarantee_kernel__coverage_neutral_reading, influences).
narrative_ontology:cs_reading_relation('44886d0c-0890-4041-85e1-4156ef2e3cc4', adverse_effect_guarantee_kernel__capture_reading, influences).
narrative_ontology:cs_axiom('44886d0c-0890-4041-85e1-4156ef2e3cc4', foundational, mobility_structure_is_load_bearing_mechanism).
narrative_ontology:cs_axiom_status(mobility_structure_is_load_bearing_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('44886d0c-0890-4041-85e1-4156ef2e3cc4', mobility_structure_is_load_bearing_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('44886d0c-0890-4041-85e1-4156ef2e3cc4', secondary, wage_measurement_failure_is_downstream_symptom).
narrative_ontology:cs_axiom_status(wage_measurement_failure_is_downstream_symptom, holdable).
narrative_ontology:cs_axiom_grounding('44886d0c-0890-4041-85e1-4156ef2e3cc4', wage_measurement_failure_is_downstream_symptom, empirically_contingent).
narrative_ontology:cs_reference_frame('44886d0c-0890-4041-85e1-4156ef2e3cc4', hold_up_problem_coordination_baseline).
narrative_ontology:cs_drift_state('44886d0c-0890-4041-85e1-4156ef2e3cc4', contemporary_program_administration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44886d0c-0890-4041-85e1-4156ef2e3cc4', '').
narrative_ontology:cs_kernel_id(channel_conversion_reading, adverse_effect_guarantee_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(channel_conversion_reading, agricultural_employer_associations).
narrative_ontology:constraint_beneficiary(channel_conversion_reading, labor_contractors).
narrative_ontology:constraint_victim(channel_conversion_reading, displaced_unauthorized_farmworkers).
narrative_ontology:constraint_victim(channel_conversion_reading, h2a_visa_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(channel_conversion_reading, domestic_agricultural_workers).
narrative_ontology:constraint_vindicates(channel_conversion_reading, legal_channel_availability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobbies for interior enforcement intensity and for streamlined H-2A processing simultaneously, and petitions for the specific workers it wants bound to it. Captures monopsony rent once a worker is inside the visa: the worker cannot shop wages to a competing employer without restarting the entire petition process, so the employer sets terms close to the regulatory floor rather than the market clearing wage. Faces essentially no exit cost from this arrangement — it can walk away from any single worker and petition for another.
narrative_ontology:constraint_stakeholder(channel_conversion_reading, agricultural_employer_associations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(channel_conversion_reading, agricultural_employer_associations, beneficiary).

% Acts as intermediary between growers and the visa system, filing petitions and managing worker placement. Collects fees and retains informational leverage over both the grower and the worker; benefits from the worker's single-employer binding because it makes the contractor's placement service indispensable rather than merely convenient.
narrative_ontology:constraint_stakeholder(channel_conversion_reading, labor_contractors, beneficiary,
    organized, biographical, arbitrage, national).

% Previously worked without authorization, retaining informal exit power: could leave a bad employer for another farm, a different sector, or simply disappear from that employer's payroll without triggering removal proceedings targeted at them specifically. Interior enforcement intensification converts that informal leverage into deportation risk, pushing them out of the labor pool entirely or into deeper underground work with less bargaining position than before. They receive no formal channel into the H-2A system and are simply displaced by it.
narrative_ontology:constraint_stakeholder(channel_conversion_reading, displaced_unauthorized_farmworkers, payer,
    powerless, biographical, trapped, regional).

% Admitted through the cheapened legal channel, but the visa binds them to the single petitioning employer. Leaving that employer for a better wage requires the new employer to file a fresh petition, and workers who leave without a lined-up sponsor face immediate loss of status and removal. This lack of mobility is what lets the employer hold wages near the regulatory floor: the worker cannot exit informally the way the unauthorized workforce could, so the wage floor exists on paper but the worker has no practical way to enforce or improve on it.
narrative_ontology:constraint_stakeholder(channel_conversion_reading, h2a_visa_workers, payer,
    powerless, biographical, identity_locked, national).

% Compete for the same jobs and experience downward wage pressure once the bound H-2A workforce sets the going local rate near the regulatory floor. Are not parties to the visa petition process and have no seat in setting the adverse-effect wage rate that is supposed to protect them; their objection is structurally unheard because the rate-setting apparatus treats the wage guarantee as the operative mechanism rather than examining the mobility structure that undermines it.
narrative_ontology:constraint_stakeholder(channel_conversion_reading, domestic_agricultural_workers, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(channel_conversion_reading, domestic_agricultural_workers, excluded).

% Administers the adverse-effect wage rate calculation and certifies petitions, treating wage measurement as the primary protective mechanism. Does not audit or regulate the mobility structure of the visa itself; from this seat the constraint appears to be functioning as designed because the wage number on paper looks defensible, even as the on-the-ground bargaining position of the bound worker deteriorates.
narrative_ontology:constraint_stakeholder(channel_conversion_reading, department_of_labor_wage_office, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(channel_conversion_reading, department_of_labor_wage_office, agenda_setter).

% Conducts workplace and residential enforcement that raises the cost of remaining unauthorized. This enforcement intensity is the other half of the conversion mechanism: it is what makes the cheapened legal channel comparatively attractive to employers and coercive in effect on workers, jointly producing the shift from an unauthorized workforce with informal exit power to a formally admitted workforce without it.
narrative_ontology:constraint_stakeholder(channel_conversion_reading, interior_enforcement_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(channel_conversion_reading, agricultural_employer_associations).
narrative_ontology:fixing_cost_class(channel_conversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine hold-up problem: employers who invest in recruiting, housing, and training a seasonal worker need some assurance the worker will not immediately defect to a competitor offering marginally higher wages once the season starts, and workers need assurance of a lawful, predictable channel into agricultural work rather than continued exposure to unauthorized status.
% TRANSFER_FUNCTION: Moves bargaining power from the worker to the petitioning employer: the worker's mobility (the ability to shop wages across employers) is transferred to the employer in exchange for formal legal status, and the wage floor that is supposed to compensate for this transfer is set and administered by an office that does not examine the mobility structure it is meant to offset.
% ABSENT_VOICES: Domestic workers competing in the same local labor markets have no seat in the H-2A certification process despite bearing displaced wage pressure; the previously-unauthorized workforce that is pushed out entirely by intensified interior enforcement is not consulted or compensated in any part of the process that replaces them.
% DISAPPEARANCE_RATIONALE: If the exit-binding structure of the visa were removed while keeping the wage floor and the coordination function intact — i.e., if H-2A workers could freely change employers without a new petition — the employer's monopsony rent would collapse, wages would rise toward the true adverse-effect rate, and the current pattern of employer-favorable petitioning behavior would have to reorganize around actually competitive terms.
% FOUNDING_PROBLEM: Seasonal agricultural employers faced a hold-up problem (workers defecting after recruitment investment) and a labor-supply problem (insufficient willing domestic labor at prevailing terms); the visa program was built to solve both by creating a lawful, employer-sponsored channel with a wage floor meant to prevent the channel from becoming a race to the bottom.
% FOUNDING_PROBLEM_CORROBORATION: Department of Labor program documents and employer association testimony attest the hold-up problem is still live and the wage floor still functions as designed. Independent labor economists studying H-2A wage compression, and farmworker advocacy organizations documenting employer-controlled mobility, attest from outside the beneficiary set that the wage floor is nominally intact but practically unenforceable given the mobility structure, and that the founding hold-up problem has been substantially resolved in the employer's favor beyond what the coordination rationale requires.
narrative_ontology:disappearance_verdict(channel_conversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(channel_conversion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(channel_conversion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(channel_conversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(channel_conversion_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(channel_conversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(channel_conversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(channel_conversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored substantial (0.71 at interval end) and rising, tracking the accumulation of monopsony rent as more of the workforce converts from unauthorized (mobile) to visa-bound (immobile) status. Suppression is authored high (0.79) because the constraint's persistence depends on active interior enforcement suppressing the informal exit channel that would otherwise discipline employer behavior — this is a raw structural property, not scaled by directionality. Theater ratio is moderate (0.42) and rising: the wage-rate certification process performs protective function on paper while an increasing share of actual outcomes are governed by the mobility structure the certification does not examine. All three tracked metrics share the single time grid (0,4,8,12,16,20).
 *
 * PERSPECTIVAL GAP:
 *   From the Department of Labor wage office's seat, the constraint looks like a functioning rope: a wage floor is calculated and certified, coordination problem solved. From the H-2A worker's seat, the same structure computes as extractive despite the wage floor being nominally satisfied, because the floor is unenforceable without exit power to walk away from a violating employer. This is the seat divergence the tangled_rope classification is built to capture: a genuine coordination function (solving the seasonal hold-up problem) coexists with asymmetric extraction (employer-side monopsony rent), and the wage-rate office's institutional position structurally prevents it from seeing the extraction because its mandate is scoped to the wage number, not the mobility structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Agricultural employer associations sit near the full-beneficiary end: they set enforcement lobbying agendas, petition for named workers, and capture the wage-floor/market-wage spread as rent — d derived low. H-2A visa workers sit near the full-target end: identity-locked exit (their legal status is bound to the employer relationship itself, not merely their job), powerless, and structurally unable to arbitrage — d derived high. Displaced unauthorized farmworkers are also targets but through the enforcement channel rather than the visa-binding channel; they bear cost without ever entering the formal system, which is why they are named as a distinct victim group rather than folded into the visa-worker group. Domestic agricultural workers experience diffuse downward wage pressure without being formal parties to either mechanism — their exclusion from the certification process is itself part of the constraint's suppression profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (seasonal labor hold-up, insufficient willing domestic supply at prevailing terms) remains partially live, which is why this is authored as tangled_rope rather than snare: there is a real coordination function being solved, not merely coordination theater. But the mobility-binding component of the solution has outgrown what solving the hold-up problem requires — employers do not need exclusive multi-month sponsorship lock-in to prevent worker defection during a single harvest; the current binding duration and re-petition cost exceed what the hold-up rationale justifies. This is the mandatrophy signature: the mandate (prevent defection during the investment window) has been extended into a mandate (prevent defection ever, from this employer, for the life of the visa) that the founding problem does not require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adverse_effect_guarantee_kernel_reading_selection,
    'Is the wage-measurement guarantee''s failure best explained by the exit/mobility structure of the visa (this reading), by evidentiary unprovability of the wage calculation itself (instrument_dependent_reading), by statutory severability of program components (textualist_severability_reading), by coverage-neutral administrative drift (coverage_neutral_reading), or by direct capture of the wage-setting office (capture_reading)?',
    'Compare wage outcomes across H-2A program variants that differ in mobility binding (e.g., portable-visa pilot programs) while holding the wage-calculation methodology constant. If wage compression tracks mobility binding independent of calculation method, this reading is supported over the instrument_dependent and capture readings.',
    'If this reading is correct, remedies should target visa portability rather than wage-formula reform — the sibling readings would prescribe formula fixes or severability litigation that would leave the actual mechanism (immobility-driven monopsony) untouched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adverse_effect_guarantee_kernel_reading_selection, conceptual, 'Which reading of the adverse-effect-guarantee kernel locates the load-bearing mechanism correctly.').

omega_variable(
    enforcement_intensity_causal_weight,
    'How much of the observed conversion from unauthorized-mobile to visa-bound-immobile workforce composition is caused by interior enforcement intensification versus by the legal channel becoming independently more attractive (lower petition cost, faster processing)?',
    'Natural experiment across jurisdictions or time periods with varying enforcement intensity but similar H-2A processing costs; if conversion rates track enforcement intensity independent of channel cost, enforcement is the dominant lever.',
    'If enforcement is the dominant lever, the coordination-function defense of the visa program weakens further, since the conversion would be substantially coerced rather than voluntarily chosen by workers comparing channels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_intensity_causal_weight, empirical, 'Relative causal weight of enforcement versus channel-cost in driving the unauthorized-to-visa conversion.').

omega_variable(
    hold_up_problem_residual_scope,
    'What portion of the original seasonal hold-up problem still requires employer-exclusive sponsorship binding, versus how much binding duration/scope exceeds what defection-prevention during a single harvest cycle requires?',
    'Compare defection rates and employer investment recovery timelines under current binding rules against a counterfactual shorter-binding or portable-sponsorship regime.',
    'Determines how much of the current mobility restriction is genuine coordination cost (Boltzmann floor) versus excess extraction — feeds directly into whether this constraint''s tangled_rope classification should shift toward snare if the residual coordination scope proves small.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hold_up_problem_residual_scope, empirical, 'How much of the current exit-binding is required by the founding coordination problem versus excess.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(channel_conversion_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chan_tr_t0, channel_conversion_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(chan_tr_t4, channel_conversion_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(chan_tr_t8, channel_conversion_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(chan_tr_t12, channel_conversion_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(chan_tr_t16, channel_conversion_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(chan_tr_t20, channel_conversion_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(chan_be_t0, channel_conversion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(chan_be_t4, channel_conversion_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(chan_be_t8, channel_conversion_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(chan_be_t12, channel_conversion_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(chan_be_t16, channel_conversion_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(chan_be_t20, channel_conversion_reading, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(chan_su_t0, channel_conversion_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(chan_su_t4, channel_conversion_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(chan_su_t8, channel_conversion_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(chan_su_t12, channel_conversion_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(chan_su_t16, channel_conversion_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(chan_su_t20, channel_conversion_reading, suppression_requirement, 20, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(channel_conversion_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(channel_conversion_reading, 0.15).
narrative_ontology:affects_constraint(channel_conversion_reading, instrument_dependent_reading).
narrative_ontology:affects_constraint(channel_conversion_reading, textualist_severability_reading).
narrative_ontology:affects_constraint(channel_conversion_reading, coverage_neutral_reading).
narrative_ontology:affects_constraint(channel_conversion_reading, capture_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the adverse_effect_guarantee_kernel, decomposed per the ε-invariance principle because the natural-language label 'the H-2A wage guarantee' covers structurally distinct claims about where the guarantee's failure is located. This reading (channel_conversion_reading) holds ε high and rising, driven by mobility-binding monopsony rent, and classifies the exit constraint itself as tangled_rope. The sibling readings should be authored as separate files with their own ε values and victim/beneficiary structures; each links back here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
