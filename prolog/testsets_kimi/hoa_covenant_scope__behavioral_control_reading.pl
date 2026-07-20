% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant â Behavioral Control and Aesthetic Conformity Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This is the behavioral_control_reading of the hoa_covenant_scope kernel.
 *   It models a deed-restricted homeowners association covenant whose
 *   operative function is the enforcement of aesthetic uniformity and
 *   behavioral conformity as a property-value maximization strategy. The
 *   covenant is presented as a protective coordination device, but its
 *   enforcement scope has expanded into subjective aesthetic judgments,
 *   lifestyle restrictions, and speech suppression (yard signs, flags). The
 *   claim/metric independence is maintained: the constraint is CLAIMED as
 *   coordination by its beneficiaries while the authored metrics describe a
 *   substantially extractive, actively enforced arrangement.
 *
 * KEY AGENTS:
 *   - hoa_governance_board: Agenda-setter (organized/constrained) â administers subjective enforcement and collects fines.
 *   - conformist_majority: Primary beneficiary (organized/constrained) â receives homogeneity and perceived value protection.
 *   - board_aligned_homeowners: Secondary beneficiary (moderate/constrained) â receives preferential variance treatment.
 *   - nonconformists: Primary target (moderate/constrained) â bears fines and expression restrictions.
 *   - marginal_aesthetics: Secondary target (moderate/constrained) â bears costs of aesthetic conformity.
 *   - civil_liberties_advocates: Excluded observer (organized/mobile) â structurally absent from private governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.46).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.72).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant â Behavioral Control and Aesthetic Conformity Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'b1f3b562-e146-4194-b713-beaae8bcaacc').
narrative_ontology:cs_kernel_codification('b1f3b562-e146-4194-b713-beaae8bcaacc', formalized).
narrative_ontology:cs_authority_grounding('b1f3b562-e146-4194-b713-beaae8bcaacc', lineage).
narrative_ontology:cs_interpretation_layer_present('b1f3b562-e146-4194-b713-beaae8bcaacc').
narrative_ontology:cs_reading_relation('b1f3b562-e146-4194-b713-beaae8bcaacc', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1f3b562-e146-4194-b713-beaae8bcaacc', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('b1f3b562-e146-4194-b713-beaae8bcaacc', foundational, homogeneity_as_investment_protection).
narrative_ontology:cs_axiom_status(homogeneity_as_investment_protection, holdable).
narrative_ontology:cs_axiom_grounding('b1f3b562-e146-4194-b713-beaae8bcaacc', homogeneity_as_investment_protection, instrumental).
narrative_ontology:cs_axiom('b1f3b562-e146-4194-b713-beaae8bcaacc', foundational, collective_approval_over_neighbor_expression).
narrative_ontology:cs_axiom_status(collective_approval_over_neighbor_expression, holdable).
narrative_ontology:cs_axiom_grounding('b1f3b562-e146-4194-b713-beaae8bcaacc', collective_approval_over_neighbor_expression, conventional).
narrative_ontology:cs_reference_frame('b1f3b562-e146-4194-b713-beaae8bcaacc', protectable_neighborhood_character).
narrative_ontology:cs_drift_state('b1f3b562-e146-4194-b713-beaae8bcaacc', contemporary_subjective_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1f3b562-e146-4194-b713-beaae8bcaacc', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformists).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces deed covenants regarding paint colors, landscaping, yard signs, flags, and lifestyle conduct; issues fines, compliance notices, and liens; justifies enforcement as protecting community character and property values.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_governance_board, agenda_setter,
    organized, biographical, constrained, local).

% Homeowners who prefer or accept prevailing aesthetic and behavioral norms; they benefit from neighborhood homogeneity and the perceived protection of property values; they support enforcement through board elections, amendment votes, and informal social sanction.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, constrained, local).

% A subset of homeowners who enjoy preferential treatment in variance requests and early notice of enforcement actions; they benefit from the suppression of aesthetic alternatives that might differentiate or compete with their property presentation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Homeowners whose political expression, lifestyle, or aesthetic choices conflict with subjective covenant interpretations; they receive fines, corrective orders, and legal threats for yard signs, unconventional landscaping, or visible noncompliance.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformists, payer,
    moderate, biographical, constrained, local).

% Residents practicing low-cost, culturally distinct, or religious aesthetic traditions that deviate from the dominant neighborhood norm; they bear the cost of conformity, remediation, or accelerated exit via lien-forced sale.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics, payer,
    moderate, biographical, constrained, local).

% Organizations that would challenge speech and aesthetic restrictions on constitutional or fair-housing grounds; they are structurally absent from covenant governance because only property owners vote and the private-contractual framing keeps public-law scrutiny at arm's length.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mutual expectations about neighborhood appearance and land use to prevent perceived negative externalities on residential property values.
% TRANSFER_FUNCTION: Moves compliance with aesthetic and behavioral norms from nonconformists and marginal-aesthetic practitioners to conformist homeowners and board-aligned residents, materializing as protected homogeneity and stabilized property-value claims.
% ABSENT_VOICES: Nonconformist homeowners who have already exited via foreclosure, forced sale, or capitulation, and civil liberties advocates challenging speech restrictions, are excluded from governance because covenant amendment typically requires supermajority approval and board elections are low-turnout.
% DISAPPEARANCE_RATIONALE: If the covenant's behavioral and aesthetic enforcement vanished, conformist homeowners would lose the mechanism that insulates their property values from neighborhood heterogeneity; nonconformists would reclaim autonomy over their property expression; the local housing market would reprice to reflect unregulated aesthetic variety.
% FOUNDING_PROBLEM: Preventing land-use discord and free-riding on neighborhood attractiveness that depresses property values in a common ownership or deed-restricted community.
% FOUNDING_PROBLEM_CORROBORATION: Urban economists and fair housing researchers attest that deed restrictions historically solved genuine infrastructure coordination but that aesthetic and behavioral covenants now primarily serve exclusion; no independent corroboration exists that subjective aesthetic judgments are necessary to solve the original externality problem.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.46, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.46) is moderate: the constraint does produce verifiable property-value effects for beneficiaries, but the mechanism is decoupled from objective maintenance and rides on subjective preference imposition. Suppression (0.72) is high because persistence depends on active exclusion of nonconforming expression and the threat of lien-based property dispossession. Theater_ratio (0.42, rising to 0.58) reflects that an increasing share of enforcement activity is performative signaling of community character rather than genuine externality abatement. Accessibility_collapse (0.58) captures that non-HOA alternatives exist in the broader market but are often inaccessible to locked-in owners with underwater liens or limited mobility. Resistance (0.48) reflects isolated, legally outgunned opposition.
 *
 * PERSPECTIVAL GAP:
 *   The conformist majority and board-aligned homeowners experience the constraint as protective investment insurance; the board experiences it as legitimate administrative authority. Nonconformists and marginal-aesthetic practitioners experience the same structure as targeted suppression of identity and expression. The engine computes this divergence from the structural data â the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (conformist_majority, board_aligned_homeowners) sit near the beneficiary end of directionality: the constraint subsidizes their property-value claims and social preferences through the compliance of others. Victims (nonconformists, marginal_aesthetics) sit near the full-target end: they pay through fines, remediation costs, forced capitulation, and lost autonomy. The board sits between agenda-setter and partial beneficiary: it does not personally capture all extraction, but its authority is constituted by the enforcement apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy risk is addressed by distinguishing this reading from the coordination_reading. The coordination reading would show low extractiveness, objective infrastructure focus, and no identifiable victims. This reading shows moderate extractiveness, subjective enforcement scope, and clear victim classes. The divergence prevents mislabeling: the founding problem (genuine externality prevention) is contested, the enforcement scope has drifted from objective maintenance to behavioral control, and the victim set is non-empty â all of which gate against rope classification and support snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this covenant a behavioral control mechanism, a genuine coordination device, or a revenue extraction tool, and do these readings describe one constraint or three structurally distinct constraints?',
    'Comparative analysis of enforcement patterns: if aesthetic and lifestyle enforcement dominates the enforcement budget while infrastructure coordination is underfunded, the behavioral control reading is primary; if fine revenue exceeds infrastructure spending, the extraction reading gains support.',
    'Determines whether the constraint is classified as snare, rope, or tangled_rope and which stakeholder seats bear extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Uncertainty about which reading of the covenant kernel is structurally true').

omega_variable(
    subjective_aesthetic_externality,
    'Does subjective aesthetic judgment (e.g., paint color, landscaping style, yard signage) constitute a negative externality on neighboring property values, or is the externality claim a constructed justification for majority preference imposition?',
    'Econometric analysis of property value impacts in jurisdictions with and without aesthetic covenants, controlling for objective maintenance standards, crime, and school quality.',
    'If aesthetic judgment shows no independent price effect, the coordination story collapses and the constraint''s extraction base is exposed as pure preference enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjective_aesthetic_externality, empirical, 'Whether subjective aesthetic enforcement tracks genuine economic externality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(hoa__tr_t50, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(hoa__be_t50, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(hoa__su_t50, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the behavioral_control_reading of the hoa_covenant_scope kernel. The kernel decomposes into three structurally distinct constraints because the readings assign different Îµ values, beneficiary/victim structures, and enforcement scopes. This reading focuses on aesthetic uniformity and behavioral conformity as the operative function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
