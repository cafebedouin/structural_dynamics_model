% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: Eligibility Drift Mechanism in Assisted-Dying Frameworks
 *   domain: medical ethics / bioethics / end-of-life policy
 *
 * SUMMARY:
 *   This constraint models the empirically observed pattern, documented
 *   across several jurisdictions with legalized medical assistance in dying,
 *   in which frameworks initially justified by and restricted to competent,
 *   terminally ill, currently-consenting adults expand — through
 *   eligibility-criteria reinterpretation, review-board precedent, and
 *   legislative amendment — to cover incompetent patients (via surrogate
 *   consent or advance directive), non-terminal chronic suffering, and in
 *   some cases treatment-resistant psychiatric illness. This is the
 *   slippery_slope_mechanism reading of the end_of_life_authority kernel: it
 *   is NOT the autonomy_reading (which holds the original competent-terminal
 *   framework is a rights vindication) and NOT the sanctity_reading (which
 *   holds intentional life-ending is categorically wrong regardless of
 *   consent). This reading's specific claim is structural and empirical: the
 *   mechanism by which autonomy-grounded frameworks systematically drift
 *   beyond their founding population, converting an initially
 *   rights-protective coordination arrangement into a vehicle that
 *   increasingly implicates the sanctity concern (killing of those who cannot
 *   or did not clearly consent) while retaining autonomy's rhetorical
 *   justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.61).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Eligibility Drift Mechanism in Assisted-Dying Frameworks").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical ethics / bioethics / end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '6b51790b-cecc-4511-9fe6-6c02afcbe292').
narrative_ontology:cs_kernel_codification('6b51790b-cecc-4511-9fe6-6c02afcbe292', distributed).
narrative_ontology:cs_authority_grounding('6b51790b-cecc-4511-9fe6-6c02afcbe292', distributed).
narrative_ontology:cs_reading_relation('6b51790b-cecc-4511-9fe6-6c02afcbe292', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('6b51790b-cecc-4511-9fe6-6c02afcbe292', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('6b51790b-cecc-4511-9fe6-6c02afcbe292', foundational, eligibility_boundaries_are_empirically_unstable_under_autonomy_justification).
narrative_ontology:cs_axiom_status(eligibility_boundaries_are_empirically_unstable_under_autonomy_justification, holdable).
narrative_ontology:cs_axiom_grounding('6b51790b-cecc-4511-9fe6-6c02afcbe292', eligibility_boundaries_are_empirically_unstable_under_autonomy_justification, empirically_contingent).
narrative_ontology:cs_axiom('6b51790b-cecc-4511-9fe6-6c02afcbe292', secondary, consent_exercised_by_surrogate_or_under_drifted_criteria_is_not_equivalent_to_original_competent_consent).
narrative_ontology:cs_axiom_status(consent_exercised_by_surrogate_or_under_drifted_criteria_is_not_equivalent_to_original_competent_consent, holdable).
narrative_ontology:cs_axiom_grounding('6b51790b-cecc-4511-9fe6-6c02afcbe292', consent_exercised_by_surrogate_or_under_drifted_criteria_is_not_equivalent_to_original_competent_consent, conventional).
narrative_ontology:cs_reference_frame('6b51790b-cecc-4511-9fe6-6c02afcbe292', competent_terminal_only_eligibility).
narrative_ontology:cs_drift_state('6b51790b-cecc-4511-9fe6-6c02afcbe292', contemporary_expanded_eligibility_regimes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6b51790b-cecc-4511-9fe6-6c02afcbe292', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, assisted_dying_program_administrators).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, healthcare_cost_payers).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, original_competent_terminal_advocates).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_under_surrogate_consent).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronically_suffering_non_terminal_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, disabled_persons_facing_eligibility_pressure).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, psychiatric_patients_seeking_assisted_death).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer eligibility review boards and set criteria interpretation guidance. Have institutional incentive to expand throughput and reduce administrative friction; each successful expansion of eligibility criteria becomes precedent for the next expansion, and the administering body bears no direct cost from criteria drift.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, assisted_dying_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Insurers and public health systems bear the cost of extended chronic and palliative care. As eligibility expands to chronic and non-terminal suffering, a fraction of costly long-term care is replaced by lower-cost assisted death procedures; this incentive is rarely stated but structurally present in budget documents and utilization review.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, healthcare_cost_payers, beneficiary,
    institutional, generational, arbitrage, national).

% The advocacy coalition that won the original narrow, competent-terminal-patient framework benefits from the legitimacy and precedent the law established, but has largely lost control over the law's subsequent expansion; many now object publicly to the drift but their founding coalition's arguments (autonomy, dignity, choice) are the very grounds cited to justify each expansion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, original_competent_terminal_advocates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, original_competent_terminal_advocates, excluded).

% Patients who can no longer express current preferences have their fate decided by surrogates, advance directives interpreted retroactively, or family/physician consensus. They cannot exercise the autonomy the framework was built to protect, yet the same legal machinery that was justified by their (or others') autonomy is applied to them. Exit is impossible by definition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_under_surrogate_consent, payer,
    powerless, immediate, trapped, national).

% Patients with years or decades of remaining life expectancy but severe chronic suffering (psychiatric illness, disability-related pain, degenerative but non-fatal conditions) become eligible as criteria drift from 'terminal' to 'unbearable and hopeless.' Social and economic pressures — inadequate palliative and disability support, family burden narratives — can make assisted death appear as the only accessible relief.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronically_suffering_non_terminal_patients, payer,
    powerless, biographical, constrained, national).

% Disability-rights communities report that expanded eligibility criteria increasingly capture disabled people whose suffering is substantially attributable to inadequate social support rather than an irremediable medical condition. They experience the expanded framework as a structural message that their lives are less worth sustaining, and lack the political power of the original advocacy coalition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disabled_persons_facing_eligibility_pressure, payer,
    powerless, biographical, constrained, national).

% Patients whose primary suffering is psychiatric rather than somatic are, in some jurisdictions, deemed eligible despite well-documented difficulty distinguishing treatment-resistant illness from transient suicidality. They are structurally vulnerable to the same drift mechanism that moved eligibility from terminal to chronic to psychiatric suffering.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, psychiatric_patients_seeking_assisted_death, payer,
    powerless, immediate, trapped, national).

% Raised warnings during original legislative debates that predicted exactly this expansion pattern; were largely characterized as fear-mongering by the original autonomy coalition and excluded from eligibility-criteria review boards, which are typically staffed by physicians and ethicists sympathetic to expansion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_organizations, excluded,
    organized, generational, constrained, national).

% Courts and legislatures periodically review eligibility criteria and program data. Some jurisdictions have paused or narrowed expansions after high-profile cases; others have ratified the expansion post hoc. Their analytical position gives them visibility into the drift pattern but institutional incentives (deference to medical expertise, reluctance to relitigate settled rights) often favor letting expansion continue.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, judicial_and_legislative_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original framework solved a genuine coordination problem: providing a legally sanctioned, medically supervised, dignity-preserving option for competent, terminally ill adults facing unbearable suffering, replacing clandestine or violent self-administered alternatives.
% TRANSFER_FUNCTION: As eligibility criteria drift, the arrangement increasingly moves decision-making authority away from the specific competent, terminally-ill, currently-consenting patient the framework was designed around, and toward administering institutions, surrogates, and cost-bearing systems acting on broader and more contestable categories of 'unbearable suffering.'
% ABSENT_VOICES: Disability-rights organizations, psychiatric-patient advocates, and palliative-care providers warning about resource substitution were present in early debates but systematically excluded from ongoing eligibility-review governance; incompetent patients by definition cannot voice objection to their own inclusion.
% DISAPPEARANCE_RATIONALE: If the eligibility-expansion mechanism were reversed and criteria returned strictly to competent, terminal, currently-consenting adults, several jurisdictions' program populations would shrink substantially, surrogate-consent and non-terminal-chronic-suffering pathways would close, and disability and psychiatric advocacy groups' central objection would be resolved even as the original narrow coordination function remained intact.
% FOUNDING_PROBLEM: The founding problem was providing competent, terminally ill adults facing unbearable suffering a legal, medically supervised alternative to suicide, coercion-free family assistance, or prolonged involuntary suffering.
% FOUNDING_PROBLEM_CORROBORATION: Original advocacy coalitions and program administrators attest the founding problem remains live and the expanded criteria simply extend the same compassionate logic to more sufferers. Disability-rights organizations, some palliative-care physicians, and dissenting members of legislative review committees — parties outside the group that benefits from program continuation — attest that the founding problem (competent terminal patients lacking a dignified option) has been substantially solved in the original narrow population, and that the expanded criteria address a structurally different problem (inadequate social and palliative support for chronic and disabled populations) using the wrong instrument.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.22) reflecting the genuine coordination function of the original narrow framework, and rises steadily to 0.68 as eligibility criteria expand to populations for whom the autonomy justification is structurally weaker (incompetent patients) or contested (non-terminal chronic sufferers, psychiatric patients). Suppression rises in parallel (measured via suppression_requirement climbing from 0.2 to 0.61) as maintaining the expanded framework requires increasingly active administrative and judicial work to reclassify hard cases as falling within 'autonomous, hopeless suffering' rather than acknowledging the category has shifted. Theater ratio rises to 0.42 as review-board processes increasingly perform diligence (documentation, waiting periods, second opinions) without those processes reliably screening out the contested cases they were designed to catch — the process becomes more elaborate as its discriminating power weakens.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrators and cost-bearing institutional payers sit near the beneficiary end: they gain administrative simplicity, legal cover, and cost substitution from expansion, with arbitrage-grade exit (they can adjust policy without personal exposure). The original advocacy coalition is a partial beneficiary turned partial excluded party — they retain legitimacy from the founding win but have lost control of the mechanism's trajectory. Incompetent patients, non-terminal chronic sufferers, disabled persons, and psychiatric patients are targets: trapped or severely constrained exit, bearing the full weight of a category expansion they did not request and in the incompetent-patient case cannot meaningfully consent to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competent terminal patients lacking a dignified option) is largely solved in the population it was built for — this is a live, corroborated fact attested even by parties outside the beneficiary set. What has NOT been resolved is the mandate's boundary: the administrative and legal apparatus built to serve that narrow problem has not sunset or re-scoped itself as it drifted into adjacent, structurally different problems (inadequate palliative and disability support). This is the mandatrophy signature — a coordination mechanism whose original function is discharged continuing to operate, now extracting legitimacy from its founding success while addressing a population for which that founding success provides no clear justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drift_inevitability_vs_contingent_policy_failure,
    'Is eligibility expansion an inherent structural consequence of autonomy-based frameworks (the mechanism this reading claims), or a contingent failure of specific jurisdictions'' review-board design and legislative oversight that better-designed frameworks could avoid?',
    'Comparative jurisdictional analysis: identify autonomy-based frameworks with strict, judicially-enforced sunset/scope-review provisions and determine whether they show the same expansion trajectory as jurisdictions without such provisions over a matched time horizon.',
    'If expansion is structurally inherent, this reading names a genuine mechanism requiring architectural correction (e.g., mandatory scope re-ratification). If contingent, the mechanism is better classified as a scaffold that failed to sunset rather than an inherent tangled-rope dynamic, which would change the claimed_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_inevitability_vs_contingent_policy_failure, empirical, 'Whether eligibility drift is structurally inherent to autonomy frameworks or a contingent design failure.').

omega_variable(
    surrogate_consent_authenticity,
    'When surrogate consent is exercised for an incompetent patient under an advance directive, is this a genuine extension of the original patient''s autonomy (validating the framework''s internal logic) or a structurally different act performed by a third party under the same legal label?',
    'Empirical study of advance-directive specificity at the time of drafting versus circumstances at time of application; legal philosophy analysis of whether autonomy can be meaningfully exercised prospectively for unforeseeable future states.',
    'If surrogate consent is not a genuine extension of autonomy, then incompetent-patient inclusion is not merely an expansion of the same right but a category substitution — strengthening this reading''s claim that the framework becomes a vehicle for something autonomy_reading cannot itself justify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surrogate_consent_authenticity, conceptual, 'Whether surrogate/advance-directive consent for incompetent patients is continuous with or discontinuous from the original autonomy right.').

omega_variable(
    cost_substitution_causal_weight,
    'To what extent does healthcare cost substitution (assisted death being cheaper than extended chronic or palliative care) causally drive eligibility expansion, versus expansion being driven purely by evolving ethical consensus about suffering and dignity?',
    'Analysis of program utilization data cross-referenced with regional healthcare budget pressures and palliative-care funding levels; comparison of expansion rates in jurisdictions with strong versus weak palliative-care infrastructure.',
    'Strong causal weight for cost substitution would substantially raise confidence in the tangled_rope classification (concrete institutional beneficiary capturing rents from the same structure that extracts from vulnerable patients); weak causal weight would support a more benign (though still contested) ethical-evolution account, pushing the classification toward a contested rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_substitution_causal_weight, empirical, 'Whether cost substitution or ethical evolution better explains observed eligibility expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.15).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.22).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.3).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.37).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.1).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the end_of_life_authority kernel. autonomy_reading authors the original competent-terminal framework as a rights vindication with low ε. sanctity_reading authors intentional life-ending as categorically impermissible with ε assessed from that premise's own lights. This story (slippery_slope_mechanism) authors the empirically observed expansion dynamic as a distinct structural claim with its own beneficiary/victim set (incompetent and non-terminal patients enter as victims here, absent from autonomy_reading's original narrow population) and its own rising ε trajectory. The three do not share ε — each is a separate constraint per the ε-invariance principle, linked here for contamination-propagation analysis: if this reading's classification degrades (e.g., toward snare), that pressure should propagate to reduce confidence in autonomy_reading's rope classification, since the autonomy framework is the vehicle in both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
