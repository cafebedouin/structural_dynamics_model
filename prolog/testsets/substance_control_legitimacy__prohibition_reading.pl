% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Substance Prohibition via Criminalization (Duty to Prevent Harm Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition reading of the
 *   substance-control-legitimacy kernel: the claim that substance use is
 *   inherently harmful and state authority derives from moral duty to prevent
 *   harm through criminalization. This is ONE reading of a three-way
 *   contested kernel. The sibling readings are harm_reduction_reading
 *   (substance use is a health issue; state duty is to minimize harm without
 *   criminalization) and legalization_reading (competent adults have
 *   autonomy; state limited to third-party harm prevention). The structural
 *   difference is sharp: prohibition reading places users in the victim set
 *   via criminalization and generates high carceral extractiveness;
 *   harm-reduction reading places users in a beneficiary set (through access
 *   to treatment and health services) with lower extraction; legalization
 *   reading places users in a beneficiary set (through autonomy) with
 *   extraction limited to third-party harm. This story focuses ONLY on the
 *   prohibition reading's structure, metrics, and axioms. The other readings
 *   are separate constraint stories.
 *
 * KEY AGENTS:
 *   - state_enforcement_apparatus (institutional, arbitrage exit) — sets criminalization policy, administers courts and prisons, benefits from enforcement legitimacy and budget
 *   - substance_users (powerless, identity_locked exit) — subject to criminal penalties, criminalization creates barriers to treatment, identity fusion from years of exclusion
 *   - low_income_communities (powerless, trapped exit) — bear concentrated policing and incarceration burden, limited access to private treatment alternatives
 *   - law_enforcement_organizations (organized, arbitrage exit) — receive enforcement budget and authority from drug criminalization, measure success via arrest/incarceration counts
 *   - private_treatment_industry (powerful, arbitrage exit) — high-cost treatment models thrive under criminalization as mandatory punishment pathway
 *   - harm_reduction_advocates (excluded, powerful) — would argue for decriminalization and health-first approaches but are systematically barred from policy-making
 *   - public_health_researchers (observer, analytical) — document that criminalization amplifies harms (disease, delayed treatment, violence) beyond the use itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.88).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Substance Prohibition via Criminalization (Duty to Prevent Harm Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, 'b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d').
narrative_ontology:cs_kernel_codification('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', formalized).
narrative_ontology:cs_authority_grounding('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', extraction).
narrative_ontology:cs_interpretation_layer_present('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d').
narrative_ontology:cs_reading_relation('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', substance_control_legitimacy__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', foundational, inherent_harm_doctrine).
narrative_ontology:cs_axiom_status(inherent_harm_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', inherent_harm_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', foundational, state_moral_duty_criminalization).
narrative_ontology:cs_axiom_status(state_moral_duty_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', state_moral_duty_criminalization, deontological).
narrative_ontology:cs_reference_frame('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', moral_prohibition_duty).
narrative_ontology:cs_drift_state('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', contemporary_public_health_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b36c32ef-67a6-4f44-b6ab-5e9f9acdc70d', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, state_public_health_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, abstinence_norm_maintainers).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, low_income_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, families_of_incarcerated).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 to 0.81 over the interval, tracking the intensification of carceral infrastructure (three-strikes laws, mandatory minimums, expanded enforcement budgets). Suppression requirement rises from 0.72 to 0.88, indicating that persistence of the constraint increasingly depends on active enforcement (rising policing budget, expanded surveillance, stricter penalties) rather than on voluntary compliance or perceived legitimacy. Theater ratio rises from 0.24 to 0.42, tracking the growing share of enforcement activity devoted to possession and low-level offenses (performative enforcement maintaining the prohibition frame) versus violent crime response. These rising trajectories indicate a constraint whose initial coordination function (preventing disorder) has degraded and whose persistence now depends heavily on institutional rent-seeking and identity-lock suppression rather than genuine coordination benefit. The shared time grid ensures every metric is authored at each time point for consistency.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus seat (agenda_setter, institutional power, arbitrage exit) experiences the constraint as legitimate coordination protecting public health; from this seat the extraction appears justified by the social order maintained. The substance-user seat (powerless, identity_locked exit) experiences the same constraint as criminal victimization in which the 'inherent harm' framing obscures institutional extraction and black-market violence externality; criminalization creates barriers to health treatment and social reintegration. Low-income communities sit in a structural trap: enforcement is concentrated in their neighborhoods, outcomes are worse (longer sentences, lower treatment access), and they lack resources to challenge or exit the constraint. The engine computes this divergence from directionality data: agenda_setter with arbitrage exit sits near d=0.0 (beneficiary); powerless with identity_locked exit sits near d=1.0 (full target). This asymmetry is the seat-divergence the six-questions interview captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the state apparatus (receives enforcement authority and legitimacy), law enforcement organizations (receive enforcement budget and measurable success metrics), and abstinence-advocacy organizations (receive institutional platform and funding). All three are named in base_properties.beneficiaries[]. Victims are substance users (criminalization creates barriers to health, generates incarceration trauma, triggers identity fusion), low-income communities (concentrated enforcement burden), and families of incarcerated (income/childcare/housing loss). The black-market organizations are technically payers (they pay enforcement costs and violence costs to maintain the constraint) but are themselves creatures of the constraint — they would not exist in their current form without criminalization creating supply scarcity. The directionality chain flows from these beneficiary/victim declarations: beneficiaries derive d near 0.0 (low effective extraction), victims derive d near 1.0 (high effective extraction). The engine scales this per-scope (national scope makes enforcement harder to verify, scaling extraction modestly upward). Identity_locked exit is the critical suppression mechanism for substance users: they cannot simply choose to leave the constraint's jurisdiction; they must undergo identity transformation (ceasing use entirely) or carry the criminal record through exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope (not snare) because it carries a genuine coordination function — the founding problem (preventing social disorder from unregulated substance use) was real at the founding moment. The tangled_rope classification captures that the constraint solves a real coordination problem (unified moral/legal standard across institutions) while simultaneously extracting from those it criminalizes. The high suppression and rising theater_ratio suggest the coordination function has degraded (enforcement is increasingly performative, low-level), but the classification stands because the coordinating infrastructure (criminal law, courts, enforcement agencies) was genuinely built for the coordination function and still serves it partially. If the founding_problem_status is contested (which it is: enforcement agencies say it's live, public health says it's been solved or was misdiagnosed), the constraint becomes a mandatrophy candidate — the founding problem is no longer widely believed to justify the arrangement, yet the arrangement persists due to institutional inertia and rent-seeking. The measurement series showing rising extraction and suppression supports the mandatrophy hypothesis: the arrangement persists not because the founding coordination problem requires it, but because institutions built around it now extract rents and defend their own legitimacy. However, the classification remains tangled_rope because the coordination machinery itself is still operational and partially justified by some stakeholders (enforcement apparatus, abstinence organizations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_harm_doctrine_empirical_status,
    'Is substance use inherently and universally harmful, or does harm depend on set, setting, dose, and regulatory context?',
    'Comparative public health analysis across decriminalized/medicalized jurisdictions (Portugal, Switzerland, Canada) versus criminalized ones, controlling for socioeconomic factors. Epidemiological data on substance-use outcomes under harm reduction versus criminalization.',
    'If harm is context-dependent and criminalization amplifies it (via black-market violence, delayed treatment-seeking, incarceration health burden), the axiom ''inherent harm doctrine'' loses epistemic grounding and the reading''s legitimacy claim weakens. If harm is inherent regardless of context, the prohibition framing gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_harm_doctrine_empirical_status, empirical, 'Whether harm is an intrinsic property of substance use or contingent on policy environment.').

omega_variable(
    carceral_extraction_vs_coordination_function,
    'To what extent is the measured extraction (0.81) the necessary cost of coordination (preventing disorder), versus excess rent-seeking by enforcement institutions?',
    'Cost-benefit analysis comparing criminalization regime''s total social cost (incarceration, healthcare, violence, family disruption, lost productivity) against harm-reduction regime''s costs in comparable jurisdictions. Audits of enforcement agency budgets and allocation patterns (drug enforcement versus violent crime).',
    'A low cost-benefit ratio for criminalization would establish the constraint as primarily extractive (victims include the drug-using population itself). A high ratio would support the coordination framing. Current evidence from public health literature suggests the ratio is highly negative (criminalization costs exceed benefits), but the framing suppresses this analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carceral_extraction_vs_coordination_function, empirical, 'Whether carceral burden is justified coordination cost or institutional rent-seeking.').

omega_variable(
    identity_lock_mechanism_substance_users,
    'For substance users with identity_locked exit, is the lock driven by physiological dependence, psychological identity fusion, social exclusion preventing reintegration, or some combination?',
    'Longitudinal tracking of users post-decriminalization/treatment access to observe whether identity-lock persists after structural barriers are removed. Qualitative interviews with users on self-concept and reintegration pathways.',
    'If lock is primarily structural (criminal records, social exclusion, lack of treatment access), opening those barriers should enable exit and reclassify the constraint''s seat-wise impact. If lock is primarily psychological identity fusion cultivated by years of exclusion, exit remains blocked even after structural barriers shift — suggesting the constraint''s suppression effect is internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_substance_users, empirical, 'Suppression mechanism for substance users: structural, internalized, or both.').

omega_variable(
    reading_contingency_state_moral_authority,
    'Does state authority to criminalize substance use derive legitimately from ''moral duty to prevent harm,'' or is that framing a post-hoc justification for state monopoly on supply and enforcement budget capture?',
    'Historical analysis of prohibition origins (1930s US context: alcohol prohibition preceded drug prohibition; both driven by institutional interests in enforcement power, not purely health reasoning). Comparative institutional analysis: which jurisdictions frame drug policy as health issue versus enforcement issue, and what outcomes result?',
    'If the moral-duty framing is secondary to institutional interests, the reading loses axiom grounding; the constraint becomes pure institutional extraction wearing a health-protection costume. This reading would shift classification or trigger false-summit detection if beneficiaries are named (which they are).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_state_moral_authority, conceptual, 'Whether prohibition reading''s legitimacy claim is primary or secondary to enforcement institution interests.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Can a single institutional framework hold BOTH the prohibition reading (inherent harm justifies criminalization) and the harm-reduction reading (state duty is to minimize harm, not criminalize)?',
    'Policy analysis of jurisdictions attempting hybrid approaches (decriminalized possession + criminalized trafficking). Doctrinal analysis of legal theory: does accepting ''duty to minimize harm'' logically require rejecting ''duty to prevent through criminalization''?',
    'If the two readings logically foreclose each other (one framework cannot hold both as live options), the relationship should be ''forecloses.'' If different parties can legitimately hold each within their own institutional commitments (e.g., conservative law-and-order frameworks versus progressive health frameworks), the relationship is ''coexists_with.'' Current evidence suggests institutional coexistence with deep conflict over which reading governs policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Logical relationship between prohibition reading and harm-reduction reading in policy space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_prohibition_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement(substance_prohibition_tr_t6, substance_control_legitimacy__prohibition_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(substance_prohibition_tr_t12, substance_control_legitimacy__prohibition_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(substance_prohibition_tr_t18, substance_control_legitimacy__prohibition_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(substance_prohibition_tr_t24, substance_control_legitimacy__prohibition_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(substance_prohibition_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(substance_prohibition_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(substance_prohibition_be_t6, substance_control_legitimacy__prohibition_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(substance_prohibition_be_t12, substance_control_legitimacy__prohibition_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(substance_prohibition_be_t18, substance_control_legitimacy__prohibition_reading, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(substance_prohibition_be_t24, substance_control_legitimacy__prohibition_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(substance_prohibition_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(substance_prohibition_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(substance_prohibition_su_t6, substance_control_legitimacy__prohibition_reading, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(substance_prohibition_su_t12, substance_control_legitimacy__prohibition_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(substance_prohibition_su_t18, substance_control_legitimacy__prohibition_reading, suppression_requirement, 18, 0.84).
narrative_ontology:measurement(substance_prohibition_su_t24, substance_control_legitimacy__prohibition_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(substance_prohibition_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, black_market_violence_externality).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, incarceration_health_burden).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_legitimacy kernel. The harm_reduction_reading and legalization_reading are sibling constraints in the same kernel family. The prohibition reading foreecloses or coexists with siblings depending on institutional framework. The black_market_violence_externality and incarceration_health_burden constraints are downstream consequences of this reading's instantiation — if prohibition reading dominates policy, those downstream constraints become active. The three kernel readings form a constraint family where each reading produces different downstream effects and different ε values for the same nominal phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
