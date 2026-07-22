% ============================================================================
% CONSTRAINT STORY: h2a_exit_mobility_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_h2a_exit_mobility_axis, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: h2a_exit_mobility_axis
 *   human_readable: H-2A Employer-Tied Exit/Mobility Constraint
 *   domain: administrative_law/labor_economics/immigration_policy
 *
 * SUMMARY:
 *   This story isolates the exit/mobility structure of the H-2A visa as a
 *   constraint with its own observable — worker separation/transfer rates
 *   before contract end, and the existence and design of portability
 *   provisions — independent of the separate question of whether DOL's
 *   wage-measurement instrument (OEWS substitution for the discontinued farm
 *   labor survey) satisfies or defeats the INA's adverse-effect guarantee.
 *   That instrument-substitution question is the subject of a sibling kernel
 *   reading (adverse_effect_guarantee_kernel, instrument_dependent_reading
 *   and its four siblings) and is deliberately NOT re-litigated here per the
 *   ε-invariance principle: whichever reading of the wage-measurement dispute
 *   prevails, the employer-tied legal structure independently constrains exit
 *   through a mechanism that does not depend on how wages are measured. The
 *   primary observable for THIS constraint — separation/transfer rates before
 *   contract end — is currently unpublished by DOL, which this story treats
 *   as the central unresolved empirical fulcrum: without it, the magnitude of
 *   the exit-collapse effect cannot be directly measured and must be inferred
 *   from portability-rule design and litigation record.
 *
 * KEY AGENTS:
 *   - certified_h2a_employers: primary beneficiary (organized/arbitrage) — collects the wage-suppression rent that immobility enables and helps shape the rules that maintain it
 *   - h2a_workers: primary target (powerless/trapped) — bears the exit collapse; leaving the job risks lawful status and housing simultaneously
 *   - displaced_unauthorized_workforce: excluded party under the channel-conversion reading — retained informal exit power that the formal channel removes
 *   - us_department_of_labor: agenda_setter (institutional/analytical) — administers certification and portability design, has not published the key observable
 *   - domestic_farmworker_unions: excluded/moderate — argues immobility suppresses wages market-wide but lacks direct authority over visa design
 *   - reviewing_courts: analytical observer — adjudicates but cannot itself design a portability regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(h2a_exit_mobility_axis, 0.71).
domain_priors:suppression_score(h2a_exit_mobility_axis, 0.78).
domain_priors:theater_ratio(h2a_exit_mobility_axis, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(h2a_exit_mobility_axis, extractiveness, 0.71).
narrative_ontology:constraint_metric(h2a_exit_mobility_axis, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(h2a_exit_mobility_axis, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(h2a_exit_mobility_axis, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(h2a_exit_mobility_axis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(h2a_exit_mobility_axis, tangled_rope).
narrative_ontology:human_readable(h2a_exit_mobility_axis, "H-2A Employer-Tied Exit/Mobility Constraint").
narrative_ontology:topic_domain(h2a_exit_mobility_axis, "administrative_law/labor_economics/immigration_policy").

domain_priors:requires_active_enforcement(h2a_exit_mobility_axis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(h2a_exit_mobility_axis, certified_h2a_employers).
narrative_ontology:constraint_victim(h2a_exit_mobility_axis, h2a_workers).
narrative_ontology:constraint_vindicates(h2a_exit_mobility_axis, hold_up_problem_solution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petition for and sponsor specific workers, controlling the legal basis of their continued presence in the country. Can terminate a worker's authorization by ending the employment relationship, which simultaneously ends the worker's lawful status and housing. Lobbies DOL rulemaking on wage methodology and portability rules through agricultural trade associations, giving it direct influence over the instrument that sets the wage floor and the rules that govern whether workers can transfer between employers.
narrative_ontology:constraint_stakeholder(h2a_exit_mobility_axis, certified_h2a_employers, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(h2a_exit_mobility_axis, certified_h2a_employers, agenda_setter).

% Hold a visa valid only for continued work with the sponsoring employer; there is no general portability provision allowing self-directed transfer to another certified employer without a new petition, a gap in processing time, and employer cooperation. Reporting wage violations, unsafe conditions, or seeking to leave risks both the job and lawful status simultaneously, collapsing the normal labor-market exit option that would otherwise discipline employer behavior. Housing is frequently employer-provided and tied to the same relationship, compounding the exit collapse.
narrative_ontology:constraint_stakeholder(h2a_exit_mobility_axis, h2a_workers, payer,
    powerless, biographical, trapped, national).

% Under the channel-conversion reading, this population previously did the same agricultural work while retaining informal exit power (the ability to simply leave and find another employer without immigration consequence). As interior enforcement intensifies and the H-2A channel cheapens for employers, this workforce is displaced by, or converted into, the formally admitted H-2A workforce whose exit power the visa structure removes. They have no seat in H-2A rulemaking or litigation and are rarely named as a party with standing.
narrative_ontology:constraint_stakeholder(h2a_exit_mobility_axis, displaced_unauthorized_workforce, excluded,
    powerless, biographical, trapped, national).

% Administers the certification process, sets wage methodology, and could design or strengthen portability provisions through rulemaking. Has discretion over whether transfer between certified employers requires a new labor certification cycle or can proceed on an expedited basis. Faces sustained employer-side lobbying pressure on methodology and portability design, and has not published separation/transfer rate data that would make the mobility constraint's real-world bite observable.
narrative_ontology:constraint_stakeholder(h2a_exit_mobility_axis, us_department_of_labor, agenda_setter,
    institutional, generational, analytical, national).

% Argue that the absence of portability, combined with the wage-measurement gap, allows certified employers to suppress wages for both H-2A and domestic workers by removing the credible threat of worker departure. Files comments in DOL rulemakings and litigates adverse-effect claims, but has no direct authority over visa design and competes with better-resourced agricultural employer associations for regulatory attention.
narrative_ontology:constraint_stakeholder(h2a_exit_mobility_axis, domestic_farmworker_unions, excluded,
    moderate, biographical, constrained, national).

% Adjudicate APA arbitrary-and-capricious challenges to DOL wage methodology and, less frequently, to portability rule design. Can vacate or remand agency action but cannot itself design a portability regime; relies on the administrative record DOL produces, which currently lacks published separation/transfer rate data.
narrative_ontology:constraint_stakeholder(h2a_exit_mobility_axis, reviewing_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(h2a_exit_mobility_axis, certified_h2a_employers).
narrative_ontology:fixing_cost_class(h2a_exit_mobility_axis, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The H-2A structure solves a genuine hold-up problem: employers make relationship-specific investments (housing, transport, training, advance recruitment costs) in seasonal workers who could otherwise depart mid-season for a better offer, leaving crops unharvested; tying the visa to a specific employer for a fixed term gives employers assurance to make those investments.
% TRANSFER_FUNCTION: Moves bargaining power over wages and working conditions from H-2A workers to certified employers, by converting what would otherwise be an ordinary employment relationship (exit disciplines the employer) into one where exit costs the worker both the job and lawful presence simultaneously.
% ABSENT_VOICES: H-2A workers themselves have no organized voice in DOL rulemaking comparable to employer associations; the displaced unauthorized workforce whose informal exit power the channel conversion eliminates has no standing in H-2A proceedings at all and is rarely named as an interested party.
% DISAPPEARANCE_RATIONALE: If employer-tied status were replaced with a general portability provision — the ability to transfer to any certified employer without triggering a new petition cycle — the credible exit threat would return, wage suppression tied to immobility would collapse, and the seasonal-labor hold-up problem employers currently solve through the visa tie would need a different solution (bonding, escrow, or advance-payment mechanisms are the usual substitutes proposed in the literature).
% FOUNDING_PROBLEM: Seasonal agricultural employers face a hold-up problem: without some binding mechanism, a worker recruited at cost (transport, housing setup, training) could depart mid-season for a competing offer, leaving crops unharvested at a moment when substitute labor is unavailable. The employer-tied visa structure was built to solve this by giving employers assurance that the worker they invest in will complete the season.
% FOUNDING_PROBLEM_CORROBORATION: Agricultural employer associations attest the hold-up problem remains live and that the tie is the minimum necessary solution. Independent labor economists (monopsony/search-friction literature) and DOL's own advance notices of proposed rulemaking on portability attest that the tie now exceeds what the hold-up problem requires — narrower solutions (limited portability windows, bonding) would solve the same problem without eliminating exit entirely; this corroboration comes from academic economists and DOL's technical staff, both outside the beneficiary class.
narrative_ontology:disappearance_verdict(h2a_exit_mobility_axis, world_rearranges).
narrative_ontology:founding_problem_status(h2a_exit_mobility_axis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(h2a_exit_mobility_axis, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(h2a_exit_mobility_axis, 'none', 1).
narrative_ontology:epsilon_provenance(h2a_exit_mobility_axis, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(h2a_exit_mobility_axis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(h2a_exit_mobility_axis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(h2a_exit_mobility_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71 at interval end) reflects the wage-suppression effect of removing the credible exit threat, which the monopsony/search-friction literature treats as a standard consequence of employer-tied work authorization regardless of how the wage floor itself is measured. Suppression (0.78) is authored higher than extraction because the mobility constraint is actively enforced — a worker who leaves loses status immediately, a legal consequence layered on top of ordinary labor-market friction, not merely a byproduct of weak bargaining power. Theater ratio is comparatively low (0.28): the certification and housing-standards apparatus performs real functions (recruitment verification, housing inspection) alongside its extractive core, so this is not a mostly-inertial piton. Accessibility collapse (0.62) reflects that alternatives — general portability, bonding schemes, escrow-based hold-up solutions — exist in the policy literature and in other guest-worker systems (e.g., some EU seasonal schemes) but have not been adopted; the collapse is partial, not near-total, because the alternatives are known and named, just not implemented. Resistance (0.55) reflects active organizing by farmworker unions and litigation pressure, but tempered by the workers' own structural powerlessness to resist directly.
 *
 * PERSPECTIVAL GAP:
 *   From the employer's seat this is a legitimate, narrowly tailored solution to a genuine seasonal hold-up problem — the tie exists because employers make sunk investments in specific workers. From the worker's seat the same tie operates as a mechanism that converts a labor contract into a status-contingent relationship where any attempt to invoke ordinary labor-market exit (quitting for better pay or conditions elsewhere) triggers loss of lawful presence. The engine should compute these divergently: employer directionality near the beneficiary end (d low), worker directionality near the full-target end (d high, amplified by trapped exit_options and powerless power atom).
 *
 * DIRECTIONALITY LOGIC:
 *   certified_h2a_employers benefit directly from immobility-driven wage suppression and additionally shape the rules (via lobbying on portability rulemaking) that maintain that immobility — this is a case for beneficiary+agenda_setter dual role, not pure passive collection. h2a_workers are the clearest target: trapped exit_options plus powerless power atom pushes derived d toward the full-target end, and no override is needed because the derivation already captures the structural reality accurately. displaced_unauthorized_workforce is excluded rather than a direct party to the visa relationship, but under the channel-conversion reading is a co-victim of the same conversion mechanism — included here for completeness even though it sits outside the base_properties victim declaration (which names h2a_workers as the visa's direct legal victim; the displaced workforce's harm runs through a different, indirect causal channel and is documented in the omega below rather than double-counted in beneficiaries/victims).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the seasonal hold-up problem) has a genuine, still-live component — crops still require timely harvest and relationship-specific investment still occurs — which is why this is authored as tangled_rope rather than snare: there IS a real coordination function, not merely a coordination story covering pure extraction. But the specific mechanism used to solve it — full loss of lawful status upon exit, rather than a narrower bonding or limited-portability solution — exceeds what the hold-up problem requires, and that excess is where the extraction lives. Classifying this as snare would erase the genuine coordination function DOL's own advance rulemaking notices and the broader monopsony literature both recognize; classifying it as mountain or rope would erase the asymmetric extraction that trapped exit options make possible. Tangled rope is the structurally honest reading: coordination function real, extraction real, both riding the same mechanism, requiring active enforcement (status termination) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unpublished_separation_rate_data,
    'What are the actual H-2A worker separation/transfer rates before contract end, and how do they compare to attrition rates in employment relationships with ordinary exit options?',
    'DOL publication of certification-linked separation and transfer data, or FOIA-compelled release of the underlying administrative records; alternatively, matched survey data comparing H-2A worker mobility to domestic seasonal worker mobility.',
    'If separation/transfer rates before contract end are near zero despite documented wage or condition grievances, this corroborates the exit-collapse mechanism as severe and active; if transfer rates are substantial despite the legal friction, the mobility constraint''s practical bite is smaller than the legal architecture alone suggests, and effective extraction should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unpublished_separation_rate_data, empirical, 'The central unpublished observable this story is built around.').

omega_variable(
    displaced_workforce_causal_weight,
    'Does the H-2A program''s expansion, combined with interior immigration enforcement, causally displace an unauthorized workforce that previously retained informal exit power, or does it primarily absorb labor-demand growth that would otherwise go unmet?',
    'Panel data tracking regional agricultural workforce composition (unauthorized share vs. H-2A share) alongside interior enforcement intensity and H-2A certification volume over the same period and geography.',
    'If displacement is substantial, the true victim set for the exit-collapse mechanism should include the displaced unauthorized workforce alongside h2a_workers, strengthening the case for tangled_rope with a wider victim class; if H-2A growth is primarily additive rather than substitutive, the displacement channel is weaker and the mobility constraint''s harm is more narrowly confined to the h2a_workers already named.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_workforce_causal_weight, empirical, 'Whether the channel-conversion mechanism operates at the scale its proponents claim.').

omega_variable(
    hold_up_problem_minimum_solution,
    'Is full loss of lawful status upon exit the minimum mechanism necessary to solve the seasonal hold-up problem, or would a narrower mechanism (bonding, escrow, limited-window portability) solve the same coordination problem with less extraction?',
    'Comparative institutional analysis against other guest-worker systems (e.g., seasonal schemes in other jurisdictions with portability or bonding provisions) measuring whether employer investment and worker retention outcomes are comparable under less restrictive mobility regimes.',
    'If narrower mechanisms achieve comparable coordination outcomes, the excess restriction is evidence the current design is calibrated toward extraction beyond what coordination requires, supporting the tangled_rope classification''s asymmetric-extraction prong more strongly; if narrower mechanisms are empirically shown to fail (increased mid-season attrition, crop losses), the current tie is closer to the coordination-function floor and less of the measured extraction is attributable to excess design choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hold_up_problem_minimum_solution, empirical, 'Whether the exit constraint exceeds what its stated coordination function requires.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(h2a_exit_mobility_axis, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(h2a__tr_t0, h2a_exit_mobility_axis, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(h2a__tr_t0, observed).
narrative_ontology:measurement(h2a__tr_t4, h2a_exit_mobility_axis, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(h2a__tr_t4, observed).
narrative_ontology:measurement(h2a__tr_t8, h2a_exit_mobility_axis, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(h2a__tr_t8, observed).
narrative_ontology:measurement(h2a__tr_t12, h2a_exit_mobility_axis, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(h2a__tr_t12, observed).
narrative_ontology:measurement(h2a__tr_t16, h2a_exit_mobility_axis, theater_ratio, 16, 0.24).
narrative_ontology:measurement_basis(h2a__tr_t16, observed).
narrative_ontology:measurement(h2a__tr_t20, h2a_exit_mobility_axis, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(h2a__tr_t20, observed).
narrative_ontology:measurement(h2a__tr_t24, h2a_exit_mobility_axis, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(h2a__tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(h2a__be_t0, h2a_exit_mobility_axis, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(h2a__be_t0, observed).
narrative_ontology:measurement(h2a__be_t4, h2a_exit_mobility_axis, base_extractiveness, 4, 0.59).
narrative_ontology:measurement_basis(h2a__be_t4, observed).
narrative_ontology:measurement(h2a__be_t8, h2a_exit_mobility_axis, base_extractiveness, 8, 0.63).
narrative_ontology:measurement_basis(h2a__be_t8, observed).
narrative_ontology:measurement(h2a__be_t12, h2a_exit_mobility_axis, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(h2a__be_t12, observed).
narrative_ontology:measurement(h2a__be_t16, h2a_exit_mobility_axis, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(h2a__be_t16, observed).
narrative_ontology:measurement(h2a__be_t20, h2a_exit_mobility_axis, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(h2a__be_t20, observed).
narrative_ontology:measurement(h2a__be_t24, h2a_exit_mobility_axis, base_extractiveness, 24, 0.71).
narrative_ontology:measurement_basis(h2a__be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(h2a__su_t0, h2a_exit_mobility_axis, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(h2a__su_t0, observed).
narrative_ontology:measurement(h2a__su_t4, h2a_exit_mobility_axis, suppression_requirement, 4, 0.65).
narrative_ontology:measurement_basis(h2a__su_t4, observed).
narrative_ontology:measurement(h2a__su_t8, h2a_exit_mobility_axis, suppression_requirement, 8, 0.69).
narrative_ontology:measurement_basis(h2a__su_t8, observed).
narrative_ontology:measurement(h2a__su_t12, h2a_exit_mobility_axis, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(h2a__su_t12, observed).
narrative_ontology:measurement(h2a__su_t16, h2a_exit_mobility_axis, suppression_requirement, 16, 0.75).
narrative_ontology:measurement_basis(h2a__su_t16, observed).
narrative_ontology:measurement(h2a__su_t20, h2a_exit_mobility_axis, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(h2a__su_t20, projected).
narrative_ontology:measurement(h2a__su_t24, h2a_exit_mobility_axis, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(h2a__su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(h2a_exit_mobility_axis, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(h2a_exit_mobility_axis, 0.12).
narrative_ontology:affects_constraint(h2a_exit_mobility_axis, h2a_wage_measurement_instrument_substitution).

% DUAL FORMULATION NOTE:
% This constraint and h2a_wage_measurement_instrument_substitution (the story instantiating the adverse_effect_guarantee_kernel's instrument-dependent or coverage-neutral readings) are siblings addressing the SAME underlying phenomenon — depressed H-2A wage and working-condition outcomes — through two structurally distinct and independently specifiable mechanisms. The wage-measurement story asks whether the statutory adverse-effect guarantee survives an instrument substitution; this story asks whether the visa's exit/mobility architecture independently suppresses bargaining power regardless of how wages are measured. Per the ε-invariance principle, these are NOT two measurements of one constraint: they have different primary observables (wage-floor calculation methodology vs. separation/transfer rates and portability rule design), different beneficiary/victim mechanics (evidentiary unprovability vs. status-contingent exit cost), and could in principle have different truth values — the wage-measurement instrument could be found fully adequate while the mobility constraint remains extractive, or vice versa. They are linked here because policy remedies to one (e.g., restoring a farm-specific wage survey) would not resolve the other (a general portability provision would be required to address the mobility constraint independently).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
