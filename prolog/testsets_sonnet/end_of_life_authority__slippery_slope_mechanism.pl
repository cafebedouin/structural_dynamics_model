% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the slippery_slope_mechanism reading of the
 *   end_of_life_authority kernel: the empirical claim that autonomy-based
 *   assisted-dying frameworks, once established for competent terminal
 *   patients, structurally tend to expand eligibility to incompetent patients
 *   (via substituted judgment/advance directives) and non-terminal
 *   populations (chronic suffering, disability-linked suffering, and in some
 *   jurisdictions psychiatric suffering). This is NOT the autonomy_reading
 *   (the normative claim that autonomy grounds a right to control death) or
 *   the sanctity_reading (the normative claim that intentional life-ending is
 *   categorically wrong). This reading makes a structural-empirical claim
 *   about mechanism: safeguards calibrated to one population get
 *   administratively repurposed for populations the safeguards were never
 *   validated against, and the coordination function of the original
 *   framework becomes a vehicle carrying sanctity-reading-style harms
 *   (irreversible death without robust contemporaneous consent) even while
 *   retaining autonomy-reading language.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.61).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.52).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.61).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Eligibility Drift Mechanism in Assisted-Dying Frameworks").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'd914769e-e4ac-4107-aee8-69c8081a57e0').
narrative_ontology:cs_kernel_codification('d914769e-e4ac-4107-aee8-69c8081a57e0', distributed).
narrative_ontology:cs_authority_grounding('d914769e-e4ac-4107-aee8-69c8081a57e0', distributed).
narrative_ontology:cs_reading_relation('d914769e-e4ac-4107-aee8-69c8081a57e0', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('d914769e-e4ac-4107-aee8-69c8081a57e0', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('d914769e-e4ac-4107-aee8-69c8081a57e0', foundational, safeguard_validity_is_population_specific).
narrative_ontology:cs_axiom_status(safeguard_validity_is_population_specific, holdable).
narrative_ontology:cs_axiom_grounding('d914769e-e4ac-4107-aee8-69c8081a57e0', safeguard_validity_is_population_specific, empirically_contingent).
narrative_ontology:cs_axiom('d914769e-e4ac-4107-aee8-69c8081a57e0', foundational, eligibility_expansion_is_administratively_self_reinforcing).
narrative_ontology:cs_axiom_status(eligibility_expansion_is_administratively_self_reinforcing, holdable).
narrative_ontology:cs_axiom_grounding('d914769e-e4ac-4107-aee8-69c8081a57e0', eligibility_expansion_is_administratively_self_reinforcing, empirically_contingent).
narrative_ontology:cs_reference_frame('d914769e-e4ac-4107-aee8-69c8081a57e0', competent_terminal_autonomy_framework).
narrative_ontology:cs_drift_state('d914769e-e4ac-4107-aee8-69c8081a57e0', post_expansion_jurisdictions_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d914769e-e4ac-4107-aee8-69c8081a57e0', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, assisted_dying_program_administrators).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, healthcare_cost_containment_bodies).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, original_autonomy_advocacy_groups).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_under_substituted_judgment).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronically_ill_non_terminal_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, disabled_patients_facing_systemic_pressure).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, psychiatric_patients_seeking_eligibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_vindicates(end_of_life_authority__slippery_slope_mechanism, sanctity_of_life_slippery_slope_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer eligibility review boards and set the operational criteria that translate statutory language into practice. Each expansion of eligibility (from terminal illness to chronic suffering, from competent to incompetent-via-advance-directive or substituted judgment) is proposed and approved through the same administrative apparatus originally built to serve a narrow competent-terminal population. They collect institutional legitimacy and expanded caseload authority from each expansion and bear none of the downstream harm if criteria are later judged to have moved too far.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, assisted_dying_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Budget-constrained payers and health systems face declining costs when the pool of eligible patients with expensive chronic or long-term care needs is widened to include assisted-dying pathways. They do not set the criteria directly but benefit from every incremental broadening and have no structural incentive to resist drift.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, healthcare_cost_containment_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Organizations that won the original competent-terminal legislation continue lobbying for expansion, framing each broadened category as a natural extension of the founding autonomy principle. They gain continued institutional relevance and donor support by pursuing expansion, and they set much of the public narrative that legitimizes each new category.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, original_autonomy_advocacy_groups, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, original_autonomy_advocacy_groups, agenda_setter).

% The originally intended population: competent adults with a terminal diagnosis and unbearable suffering who obtain a genuinely voluntary, well-safeguarded death. Their situation is largely unaffected by the drift mechanism itself, though the credibility of the safeguards they relied on is diluted as the same apparatus is stretched to cover populations it was not built to evaluate.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    moderate, immediate, constrained, regional).

% Patients who can no longer express contemporaneous consent — dementia patients under advance directives interpreted by others, patients under guardianship — are brought into eligibility through substituted judgment or advance-directive extension. They cannot verify that the decision reflects a stable, informed present will; the safeguard architecture built for competent self-report does not transfer to their situation, yet the same approval apparatus is applied to them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_under_substituted_judgment, payer,
    powerless, immediate, trapped, regional).

% Patients with severe but non-terminal chronic illness or disability-linked suffering become eligible once 'unbearable suffering' is decoupled from imminent death. They face social, familial, and economic pressure that resembles undue influence, but the eligibility framework treats their request as an autonomous choice on the same terms as the original terminal cohort, without the terminal diagnosis that once bounded the population.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronically_ill_non_terminal_patients, payer,
    powerless, biographical, constrained, regional).

% Disability rights communities report that once eligibility widens beyond terminal illness, disabled patients experience assisted dying as the socially cheapest option offered to them in systems that underfund long-term disability support — the choice architecture around them changes even though no one directly coerces them. Exit from this dynamic requires resources (independent living support, advocacy access) that this population disproportionately lacks.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disabled_patients_facing_systemic_pressure, payer,
    powerless, biographical, trapped, national).

% In jurisdictions where eligibility extends to unbearable and untreatable psychiatric suffering, patients whose competence to make an irreversible decision is itself clinically contested are evaluated by the same apparatus built for physically terminal, cognitively intact adults. The suffering is real, but the capacity-assessment tools were never validated for this population, and this population had no voice in the original legislative design.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, psychiatric_patients_seeking_eligibility, payer,
    powerless, biographical, trapped, national).

% Raised warnings during the original terminal-only legislative debates that safeguards would not hold under expansion pressure. Their objections were treated as slippery-slope alarmism at the time and were structurally outside the coalition that drafted and passed the original framework; they remain largely outside the administrative bodies that now approve each expansion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_and_psychiatric_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Courts and legislatures periodically review expanded eligibility criteria, hear testimony from all sides, and can narrow, freeze, or further expand the framework. They are the primary seat with the standing and analytical distance to evaluate whether the drift mechanism has outrun its founding justification.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, judicial_and_legislative_oversight_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original coordination problem was genuine: give competent, terminally ill adults facing unbearable suffering a lawful, safeguarded, medically supervised alternative to unregulated or violent self-ending, replacing ad hoc and unsafe practices with a reviewable process.
% TRANSFER_FUNCTION: Administrative and legitimacy capital flows to program administrators and advocacy organizations with each expansion of eligible categories; cost relief flows to payers and health systems as the eligible pool widens; the corresponding risk of erroneous, pressured, or under-scrutinized deaths is transferred onto incompetent, non-terminal, disabled, and psychiatric populations who were not party to the original safeguard design.
% ABSENT_VOICES: Disability rights organizations and psychiatric patient advocates warned during original legislative debates that safeguards calibrated for competent terminal patients would not hold under category expansion; they were characterized as engaging in slippery-slope fallacy rather than treated as stakeholders in the design of the eligibility apparatus, and they remain structurally outside the review bodies that approve each new expansion.
% DISAPPEARANCE_RATIONALE: If the expansion mechanism itself were frozen or reversed — eligibility criteria locked to the original competent-terminal population — administrative bodies would lose the expanded caseload and legitimacy that repeated broadening provides, cost-containment benefits from a widened pool would disappear, and populations currently exposed to under-validated eligibility review (incompetent, non-terminal, disabled, psychiatric patients) would exit the risk pool entirely. This is a real rearrangement, not a cosmetic one.
% FOUNDING_PROBLEM: Competent adults with terminal illness facing unbearable suffering had no lawful, medically supervised option to control the timing and manner of their death, forcing some toward unsafe, unregulated, or violent alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Original autonomy advocacy groups and program administrators attest the founding problem is still live and that expansion is the natural fulfillment of the same autonomy principle. Disability rights organizations, psychiatric advocacy groups, and independent jurisdictional review reports (from outside the advocacy and administrative coalition) attest that the founding problem — as originally scoped to competent terminal patients — has been substantially addressed, and that continued expansion serves a different, unstated function: cost containment and institutional continuation rather than the original narrow autonomy claim.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness starts low (0.18) at founding, when the framework served only competent terminal patients with tight safeguards, and rises to 0.61 by interval end as eligibility categories widen and the approval apparatus is stretched across populations for which its validation does not hold. Theater ratio rises in parallel (0.12 to 0.44): as the underlying safeguard function degrades relative to newly admitted populations, procedural review activity (committee hearings, capacity assessments using tools not validated for the new population) increasingly performs assurance rather than providing it. Suppression rises modestly (0.20 to 0.52) as institutional momentum and precedent make narrowing eligibility politically and legally difficult once expanded categories exist — resistance to expansion is treated as illegitimate relitigation of settled autonomy claims. All three series share the single time grid (0, 5, 10, 15, 20, 25).
 *
 * PERSPECTIVAL GAP:
 *   From the administrator/advocacy seat, each expansion looks like coherent, principled extension of settled autonomy doctrine — a rope. From the seat of incompetent or non-terminal patients newly brought into eligibility, the same apparatus looks like a tangled rope at best: real coordination function for the original population, riding alongside a structure that now extracts irreversible risk from populations the safeguards were never built to serve, and that requires active enforcement (institutional and legal defense of expanded eligibility against narrowing challenges) to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Program administrators and cost-containment bodies sit near the beneficiary end: each expansion increases their institutional scope or reduces their cost exposure, and they bear none of the downstream risk if an expansion is later judged unsafe. Original advocacy groups occupy a genuine dual position — real beneficiaries of continued relevance who also, in good faith, believe expansion serves the founding autonomy principle. Incompetent, non-terminal, disabled, and psychiatric populations sit near the full-target end: they did not design the safeguards now applied to them, cannot easily exit the risk once brought into the eligible category, and bear the consequence if the expanded apparatus mis-evaluates their case. Competent terminal patients (the original beneficiary class) are largely unaffected by drift itself, though the credibility of their safeguards is diluted by association.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no lawful option for competent terminal patients) is substantially solved in jurisdictions with mature frameworks — the founding_problem_status is authored as contested precisely because administrators and advocates assert continuity of purpose while independent review bodies and excluded advocacy groups assert the original mandate has been exceeded. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating the entire framework as pure extraction (it retains a real, uncontested coordination function for the original competent-terminal population) and treating the expansion dynamic as natural or inevitable (it is a contestable, actively defended administrative and legislative choice, not a structural necessity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_versus_policy_choice,
    'Is the observed eligibility expansion a structurally inevitable feature of autonomy-based frameworks once operationalized, or a contingent policy choice made by specific advocacy coalitions and administrative bodies in specific jurisdictions that could have been (and in some jurisdictions was) resisted?',
    'Comparative jurisdictional analysis: identify frameworks that have maintained terminal-only, competent-only eligibility for extended periods (decades) without drift, versus those that expanded within a short window, controlling for legislative design (e.g., explicit statutory eligibility freezes vs. broad principle-based statutory language).',
    'If expansion correlates strongly with statutory design choices (vague ''unbearable suffering'' language vs. explicit terminal-diagnosis requirements) rather than with autonomy-framing per se, the mechanism is better characterized as a drafting/design failure mode than an intrinsic property of autonomy-based frameworks — this would narrow the claimed_type toward scaffold-with-poor-sunset rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_versus_policy_choice, empirical, 'Whether eligibility drift is intrinsic to autonomy frameworks or a contingent drafting failure.').

omega_variable(
    sanctity_reading_vindication_scope,
    'Does the empirical drift documented here vindicate the sanctity_reading''s substantive claim (that intentional life-ending is wrong) or only its predictive claim (that autonomy frameworks will not stay bounded)?',
    'Distinguish, in jurisdictional data, cases where expanded eligibility produced outcomes the original competent-terminal safeguards would have flagged as inadequate consent (predictive vindication) versus outcomes that were procedurally sound but normatively contested by sanctity advocates regardless of process quality (substantive vindication).',
    'If only the predictive claim is vindicated, this reading remains a pure mechanism story compatible with a well-designed autonomy framework; if the substantive claim is also implicated, the mechanism reading and the sanctity_reading converge on policy recommendations even while remaining logically distinct claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_reading_vindication_scope, conceptual, 'Whether documented drift supports sanctity''s predictive claim only, or its substantive claim too.').

omega_variable(
    disability_pressure_causal_attribution,
    'Is the reported social/economic pressure on disabled and chronically ill patients toward assisted-dying eligibility caused by the eligibility framework itself, or by independent underfunding of long-term care and disability support that would produce similar pressure with or without an assisted-dying framework?',
    'Compare rates of reported pressure and uptake in jurisdictions with expanded eligibility and well-funded disability/palliative infrastructure versus jurisdictions with expanded eligibility and underfunded infrastructure.',
    'If pressure is primarily attributable to underfunded alternatives rather than the eligibility framework per se, the extraction attributed to this constraint should be partially reattributed to a separate constraint (disability/long-term-care funding policy), reducing this constraint''s own ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_pressure_causal_attribution, empirical, 'Whether pressure on disabled patients is caused by this constraint or by an adjacent funding-policy constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.19).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.27).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.34).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.4).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.44).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the end_of_life_authority kernel. autonomy_reading and sanctity_reading are stable normative positions authored as separate constraints with their own ε values; this constraint (slippery_slope_mechanism) makes an empirical/structural claim about how autonomy-grounded frameworks behave once operationalized, and its victim set (incompetent, non-terminal, disabled, and psychiatric patients) is the structural delta that distinguishes it from a pure autonomy_reading story. Do not average this constraint's ε with the siblings' ε — they are different constraints answering different questions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
