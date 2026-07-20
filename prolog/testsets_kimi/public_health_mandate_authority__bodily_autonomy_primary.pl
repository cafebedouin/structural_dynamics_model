% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate Authority â Bodily Autonomy Primary Reading
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the contested public_health_mandate_authority kernel. Under this reading,
 *   state authority to compel medical intervention is a categorical violation
 *   of bodily sovereignty; no collective benefit can justify non-consensual
 *   medical intervention. The constraint is the mandate authority itself,
 *   which extracts bodily compliance from unvaccinated individuals under
 *   threat of penalty and exclusion. The immunocompromised are excluded from
 *   the victim set because this reading rejects any duty to protect via
 *   bodily invasion. Public-health-primary advocates experience zero
 *   extractivenessâthey are not coerced. The claim/metric independence is
 *   maintained: the claimed type is snare because, from this reading's
 *   perspective, the coordination story (herd immunity) is cover for pure
 *   extraction; the metrics are authored descriptively to reflect the actual
 *   coercion and resistance observed during mandate deployment.
 *
 * KEY AGENTS:
 *   - Unvaccinated individuals: Primary target (moderate/constrained) â bear direct coercion and bodily violation.
 *   - Public health authorities: Agenda setter (institutional/arbitrage) â designs and enforces mandates, accrues enforcement power and compliance data.
 *   - Immunocompromised population: Beneficiary (moderate/constrained) â receives claimed protection without facing coercion.
 *   - Public health primary advocates: Beneficiary (organized/mobile) â normative beneficiaries of the mandate, experience zero extractiveness.
 *   - Civil liberties scholars: Analytical observer (analytical/analytical) â argues autonomy is an absolute side-constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.92).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.88).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.92).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate Authority â Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '1878b15e-58d7-4ac6-a081-957081c6732f').
narrative_ontology:cs_kernel_codification('1878b15e-58d7-4ac6-a081-957081c6732f', formalized).
narrative_ontology:cs_authority_grounding('1878b15e-58d7-4ac6-a081-957081c6732f', lineage).
narrative_ontology:cs_interpretation_layer_present('1878b15e-58d7-4ac6-a081-957081c6732f').
narrative_ontology:cs_reading_relation('1878b15e-58d7-4ac6-a081-957081c6732f', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('1878b15e-58d7-4ac6-a081-957081c6732f', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('1878b15e-58d7-4ac6-a081-957081c6732f', foundational, bodily_sovereignty_absolute).
narrative_ontology:cs_axiom_status(bodily_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('1878b15e-58d7-4ac6-a081-957081c6732f', bodily_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('1878b15e-58d7-4ac6-a081-957081c6732f', foundational, non_consensual_intervention_categorically_impermissible).
narrative_ontology:cs_axiom_status(non_consensual_intervention_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('1878b15e-58d7-4ac6-a081-957081c6732f', non_consensual_intervention_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('1878b15e-58d7-4ac6-a081-957081c6732f', bodily_autonomy_absolute).
narrative_ontology:cs_drift_state('1878b15e-58d7-4ac6-a081-957081c6732f', contemporary_public_health_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1878b15e-58d7-4ac6-a081-957081c6732f', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They are compelled by state mandate to undergo medical intervention against their will, facing penalties, exclusion from workplaces and schools, loss of livelihood, and social sanction if they refuse. They bear the direct cost of bodily violation and loss of autonomy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% They design, announce, and enforce vaccination mandates, invoking police power and epidemiological necessity. They collect compliance data, administer penalties for non-compliance, and accrue institutional power from the mandate's operation.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% They receive claimed protection from reduced community transmission when mandates raise vaccination rates, though they do not themselves face the coercion of the mandate and cannot easily protect themselves individually.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_population, beneficiary,
    moderate, biographical, constrained, national).

% They advocate for collective health outcomes and support mandates as legitimate exercises of state power. They are not themselves coerced by the mandate and experience zero extractiveness from its operation.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, beneficiary,
    organized, generational, mobile, national).

% They analyze the ethical and constitutional boundary between state police power and bodily integrity, arguing from principle that autonomy is an absolute side-constraint against which mandates must fail regardless of empirical outcome.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, civil_liberties_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing infectious disease transmission and protecting healthcare infrastructure from surge by achieving near-universal vaccination coverage in a population where individual opt-out would produce free-riding and suboptimal collective immunity.
% TRANSFER_FUNCTION: Moves bodily compliance (medical intervention) from unvaccinated individuals to the collective health benefit pool under state compulsion; transfers autonomy and decisional authority from individuals to public health institutions.
% ABSENT_VOICES: Individuals with deeply held religious or philosophical objections who lack organized representation in public health policy forums; dissenting medical professionals who regard mandates as disproportionate but are institutionally sidelined; and unvaccinated persons facing employment termination whose voices are filtered through legal rather than policy channels.
% DISAPPEARANCE_RATIONALE: If the authority to mandate medical intervention vanished overnight, unvaccinated individuals would no longer face coerced bodily invasion; public health authorities would lose a primary enforcement lever; school, workplace, and travel entry rules would reorganize around persuasion and private choice; and the political and legal balance between police power and bodily sovereignty would shift.
% FOUNDING_PROBLEM: The free-rider problem in population immunity, where individual refusal to vaccinate exposes vulnerable populations and risks healthcare system overload during epidemic surge.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiologists attest the problem is live. Civil liberties scholars and dissenting bioethicists attest the problem is exaggerated or solvable through less coercive means, corroborating from outside the benefiting parties that the founding problem does not mandate this specific arrangement. No neutral consensus exists.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.92, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the constraint compels invasive bodily action, a severe cost. Suppression is high (0.88) because persistence depends on active enforcementâfines, employment exclusion, and social sanctions. Theater is moderate (0.35) because while enforcement is functional, a substantial fraction of public communication performatively asserts 'community duty' rather than acknowledging raw coercion. Accessibility collapse is high (0.78) because once mandates are normalized, alternatives (unvaccinated participation in public life) collapse rapidly. Resistance is high (0.82) due to organized legal challenges, protests, and non-compliance. The temporal series trace a pandemic mandate lifecycle: escalation, peak enforcement, and partial decay as resistance institutionalizes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (unvaccinated) experiences the constraint as pure coercion and bodily violation, computing toward snare. The beneficiary seats (immunocompromised, advocates) experience the same rule as protective coordination, computing toward rope or tangled rope. The agenda setter (public health authorities) sees a necessary enforcement tool. The engine computes this divergence from structural data; the authored claim reflects the payer-seat structural truth under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals are declared victims with constrained exit, producing a high directionality toward full target. Public health authorities are agenda setters with arbitrage-grade exit (they can alter policy), producing low directionality. Immunocompromised population and public health primary advocates are declared beneficiaries with mobile or constrained exit, producing low directionality. The effective extraction is thus amplified for the unvaccinated and damped or inverted for the other seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by rejecting the mandate's coordination story outright. If the founding problem (herd immunity free-riding) were treated as still live without interrogation, the constraint might be misclassified as tangled rope or scaffold. The bodily_autonomy_primary reading insists that the coordination benefit, even if real, cannot justify the meansâthereby exposing the extraction as non-contingent and avoiding the mandatrophy trap where an obsolete or overreached mandate persists because its original health rationale is still asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'How does the bodily_autonomy_primary reading of the public_health_mandate_authority kernel differ structurally from its siblings?',
    'Comparison of victim/beneficiary sets across the three readings: public_health_primary would assign victimhood to immunocompromised via neglected duty; proportionality_reading would produce variable victimhood based on threat magnitude and coercion severity.',
    'Determines whether the constraint is classified as categorical snare (this reading), tangled rope with shifting victims (proportionality), or rope-like collective obligation (public_health_primary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural location of this reading within the contested kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression against unvaccinated individuals primarily structural (legal penalties, exclusion orders) or internalized (social stigma, shame, self-censorship)?',
    'Post-mandate suppression trajectory: if social and professional penalties persist after formal mandates are lifted, reclassify suppression as partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates partly through identity-lock rather than explicit coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    herd_immunity_efficacy,
    'Does the mandate actually produce the claimed coordination benefit of protecting the immunocompromised, or does transmission continue via breakthrough and variant evolution?',
    'Epidemiological measurement of transmission rates among immunocompromised cohorts in high-mandate versus low-mandate jurisdictions, controlling for vaccine type and variant.',
    'If the coordination benefit is illusory, the mandate''s extraction is pure snare; if real but coerced, it remains snare under this reading but with contested empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_efficacy, empirical, 'Whether the claimed coordination benefit is empirically realized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 6, 0.3).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 12, 0.38).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 18, 0.42).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.4).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 30, 0.36).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 36, 0.35).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 12, 0.88).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 18, 0.92).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.9).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 36, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 18, 0.92).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.88).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 36, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the public_health_mandate_authority kernel, which decomposes into three structurally distinct constraints. Each reading produces a different epsilon, victim set, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
