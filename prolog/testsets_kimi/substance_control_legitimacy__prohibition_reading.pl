% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Substance Prohibition via Moral Duty and Criminalization
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the prohibition reading of the
 *   substance_control_legitimacy kernel: the claim that substance use is
 *   inherently harmful and that state authority is morally obligated to
 *   prevent such harm through criminalization. Under this reading, users are
 *   not patients or autonomous agents but offenders; the state's role is
 *   punitive and prophylactic. The constraint operates through statutory
 *   prohibition, carceral enforcement, and stigmatization, generating high
 *   extractiveness via incarceration and black market externalities. It is
 *   claimed as coordination (protecting society from harm) but structurally
 *   functions as asymmetric extraction, concentrating gains in the carceral
 *   complex and licit industries while dispersing costs onto users and
 *   marginalized communities. This story is authored as one reading of a
 *   three-way kernel; sibling readings (harm reduction, legalization) are
 *   structurally distinct constraints.
 *
 * KEY AGENTS:
 *   - State prohibition authority (agenda_setter, institutional): sets criminal penalties and schedules substances.
 *   - Carceral enforcement complex (beneficiary, institutional): collects budget, mission, and personnel from prohibition enforcement.
 *   - Substance users (payer, powerless): bear direct carceral extraction and health risks.
 *   - Marginalized communities (payer, powerless): bear enforcement disparities and black market violence.
 *   - Licit substance industries (beneficiary, powerful): gain competitive protection from suppressed illicit markets.
 *   - Harm reduction advocates (excluded, organized): excluded from policy design in prohibition frameworks.
 *   - Public health institutions (observer, institutional): provide data often disregarded by enforcement authorities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.78).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.82).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Substance Prohibition via Moral Duty and Criminalization").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, 'f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a').
narrative_ontology:cs_kernel_codification('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', formalized).
narrative_ontology:cs_authority_grounding('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', lineage).
narrative_ontology:cs_interpretation_layer_present('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a').
narrative_ontology:cs_reading_relation('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', substance_use_inherently_harmful, empirically_contingent).
narrative_ontology:cs_axiom('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', foundational, state_moral_duty_to_criminalize_harm).
narrative_ontology:cs_axiom_status(state_moral_duty_to_criminalize_harm, holdable).
narrative_ontology:cs_axiom_grounding('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', state_moral_duty_to_criminalize_harm, deontological).
narrative_ontology:cs_reference_frame('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', moral_paternalist_public_order).
narrative_ontology:cs_drift_state('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', contemporary_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f924d63e-d49c-4fc7-bd5e-eaaf06d6f06a', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, carceral_enforcement_complex).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, licit_substance_industries).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives legitimacy from a claimed moral duty to prevent harm; sets criminal penalties, schedules substances, and funds enforcement apparatus. Maintains the legal framework that criminalizes possession and use, and justifies continued coercion through public safety rhetoric.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, state_prohibition_authority, agenda_setter,
    institutional, generational, constrained, national).

% Receives budgetary allocations, personnel authority, equipment, and institutional mission from drug prohibition. Includes police narcotics divisions, prosecutors, prisons, and probation systems whose size and funding depend directly on the volume of drug arrests and convictions.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, carceral_enforcement_complex, beneficiary,
    institutional, generational, identity_locked, national).

% Subject to arrest, incarceration, criminal records, civil forfeiture, and stigmatization for substance use. Bear direct carceral extraction and health risks from unregulated supply. Exit requires total abstinence or successful evasion, both of which carry severe personal costs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, biographical, trapped, local).

% Bear disproportionate enforcement burdens, family disruption from incarceration, and black market violence externalities. Neighborhoods destabilized by underground economies, property crime associated with illicit markets, and militarized policing practices.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, marginalized_communities, payer,
    powerless, generational, trapped, local).

% Pharmaceutical, alcohol, and tobacco industries benefit from suppressed competition from prohibited substances. Operate in regulated legal markets with access to banking, advertising, and political lobbying while prohibited competitors remain underground.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, licit_substance_industries, beneficiary,
    powerful, biographical, mobile, national).

% Public health advocates and affected community organizations arguing for decriminalization, treatment access, and safe supply. Systematically excluded from prohibition policy design; their expertise treated as subordinate to enforcement logic and their constituencies regarded as criminal.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% Medical and epidemiological institutions that measure health outcomes of prohibition versus alternative regimes. Provide data that is frequently disregarded by prohibition authorities when it contradicts the moral duty framing.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, carceral_enforcement_complex).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective behavior and public morality by criminalizing substance possession and use, ostensibly to prevent harm to individuals and maintain social order through state deterrence and punishment.
% TRANSFER_FUNCTION: Moves liberty, bodily autonomy, and economic resources from substance users and affected communities to the carceral enforcement complex and state authority, while concentrating market advantages in licit substance industries and externalizing violence costs to marginalized neighborhoods.
% ABSENT_VOICES: Substance users facing incarceration, harm reduction advocates, and residents of high-enforcement zones are systematically excluded from policy design; their testimony is treated as criminal self-interest or moral failure rather than legitimate expertise.
% DISAPPEARANCE_RATIONALE: If the prohibition framework vanished overnight, the carceral enforcement complex would lose its primary mission and funding, illicit markets would begin transition toward legal regulation, criminal dockets and prison populations would collapse, and public health frameworks would absorb substance use governance. The institutional landscape of criminal justice and community safety would fundamentally reorganize.
% FOUNDING_PROBLEM: Perceived social, moral, and public health harms from unregulated substance use, including addiction, family breakdown, public disorder, and threats to social cohesion.
% FOUNDING_PROBLEM_CORROBORATION: Prohibition advocates and carceral institutions attest the problem remains live and requires criminalization. Public health researchers, harm reduction organizations, and affected communities attest the founding problem has shifted in form or is better addressed through non-carceral means; external evaluations from medical and criminological fields outside the benefiting parties dispute the efficacy of criminalization as a solution.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint extracts liberty, health, and economic resources from users through incarceration and unregulated supply. Suppression (0.82) is higher still: the constraint persists only through active policing, prosecution, and incarceration, while suppressing legal alternatives. Theater ratio (0.55) reflects that a growing share of enforcement activity performs moral condemnation and budget justification rather than reducing supply or demand. Accessibility collapse (0.72) is high because legal access to substances and non-carceral alternatives collapse under criminalization. Resistance (0.68) is substantial: reform movements, legal challenges, and shifting public opinion push back, though unevenly. Temporal measurements track the prohibition lifecycle from expansion (T0âT20) through peak carceral extraction (T20âT30) to modest reform-driven decline (T40âT50), while theater rises as the coordination story erodes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this constraint as legitimate public safety coordination; the carceral complex experiences it as institutional identity and revenue. The payer seats experience it as extraction dressed in moral language. The engine computes this divergence from structural data: agenda-setters and beneficiaries have institutional power and constrained or identity-locked exit, while payers are powerless with trapped exit. The perspectival gap is wide and stable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by beneficiary and victim declarations. The carceral enforcement complex and licit substance industries are beneficiaries: they collect rents, budgets, and market protection from the constraint, placing them near the beneficiary end of the directionality spectrum. Substance users and marginalized communities are declared victims: they bear the carceral and violence costs, placing them near the target end. The state prohibition authority is agenda-setter rather than direct rent-collector, but its power and constrained exit still orient it away from the full-target pole. No override is needed because the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mislabeling because the coordination story (preventing social harm) is structurally paired with declared beneficiaries who capture the enforcement budget and market protection. A pure coordination story would show symmetric benefit and no concentrated extraction; here, the gains concentrate in the carceral complex while costs fall on powerless populations. The founding problem status is contested and corroborated from outside the beneficiary set, preventing the prohibition authority from self-certifying the mandate. The rising theater ratio over time signals that performative maintenance is substituting for declining coordination efficacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'How would the classification change if this constraint were read through the harm_reduction_reading or legalization_reading of the same kernel?',
    'Compare the victim/beneficiary structures across sibling constraint stories; the prohibition reading criminalizes users while the harm reduction reading recasts them as patients and the legalization reading as autonomous agents.',
    'In the prohibition reading, users are victims via criminalization and carceral extraction is high; in sibling readings, users shift to beneficiary or neutral status and the state''s enforcement role diminishes or inverts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural delta between prohibition and sibling readings of the substance control legitimacy kernel.').

omega_variable(
    moral_harm_empirical_status,
    'Is the inherent harm of substance use empirically sufficient to justify the carceral extraction this constraint produces?',
    'Comparative policy analysis across jurisdictions with different regulatory regimes; epidemiological measurement of harms under prohibition versus harm reduction or legalization.',
    'If empirical harm is low relative to carceral and black market costs, the coordination story weakens and extraction dominates; if harm is severe, the tangled rope tightens toward rope-like legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_harm_empirical_status, empirical, 'Whether empirical substance harm warrants prohibition-level coercion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (carceral enforcement, legal barriers) or internalized (moral stigma, self-policing by users)?',
    'Post-decriminalization suppression trajectory: if stigma and self-restraint persist after enforcement removal, reclassify suppression as partially internalized.',
    'Internalized suppression would mean effective extraction exceeds the structural measure â the constraint operates even where enforcement is formally absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in drug prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_prohibition_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(substance_prohibition_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(substance_prohibition_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(substance_prohibition_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(substance_prohibition_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.54).
narrative_ontology:measurement(substance_prohibition_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(substance_prohibition_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(substance_prohibition_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(substance_prohibition_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(substance_prohibition_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(substance_prohibition_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(substance_prohibition_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(substance_prohibition_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(substance_prohibition_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(substance_prohibition_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(substance_prohibition_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(substance_prohibition_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(substance_prohibition_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__prohibition_reading, 0.1).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel decomposes into three structurally distinct constraints. The prohibition reading claims high carceral extractiveness with users as victims; the harm reduction reading removes users from the victim set and lowers extraction; the legalization reading reframes users as autonomous rights-bearers. Each reading has a stable, distinct epsilon and must be evaluated separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
