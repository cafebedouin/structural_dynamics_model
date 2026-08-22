% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: State Substance-Use Authority — Harm Reduction / Medicalization Reading
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the harm-reduction reading of the
 *   substance-control-legitimacy kernel: the state's authority to intervene
 *   in substance use is grounded not in moral condemnation (prohibition) nor
 *   in respect for adult autonomy limited only by third-party harm
 *   (legalization), but in a duty to minimize harm through medicalization.
 *   Under this reading, users are diverted from prosecution into treatment
 *   and monitoring; supervised consumption and needle-exchange programs are
 *   tolerated as harm-reducing infrastructure; but supply remains
 *   criminalized and treatment participation is frequently coerced by the
 *   threat of reactivated prosecution. The reading produces a hybrid
 *   structure: genuine coordination gains (fewer overdose deaths, less
 *   disease transmission, reduced incarceration for personal possession)
 *   riding alongside a persistent black market and a treatment-industry rent
 *   structure that benefits from mandate volume regardless of individual
 *   clinical need.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.48).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "State Substance-Use Authority — Harm Reduction / Medicalization Reading").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'aabc02f2-76f5-4350-b5e0-120914e7e35f').
narrative_ontology:cs_kernel_codification('aabc02f2-76f5-4350-b5e0-120914e7e35f', distributed).
narrative_ontology:cs_authority_grounding('aabc02f2-76f5-4350-b5e0-120914e7e35f', expertise).
narrative_ontology:cs_interpretation_layer_present('aabc02f2-76f5-4350-b5e0-120914e7e35f').
narrative_ontology:cs_reading_relation('aabc02f2-76f5-4350-b5e0-120914e7e35f', substance_control_legitimacy__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('aabc02f2-76f5-4350-b5e0-120914e7e35f', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('aabc02f2-76f5-4350-b5e0-120914e7e35f', foundational, substance_use_disorder_is_medical_not_moral).
narrative_ontology:cs_axiom_status(substance_use_disorder_is_medical_not_moral, holdable).
narrative_ontology:cs_axiom_grounding('aabc02f2-76f5-4350-b5e0-120914e7e35f', substance_use_disorder_is_medical_not_moral, empirically_contingent).
narrative_ontology:cs_axiom('aabc02f2-76f5-4350-b5e0-120914e7e35f', foundational, state_may_coerce_treatment_absent_criminal_sanction).
narrative_ontology:cs_axiom_status(state_may_coerce_treatment_absent_criminal_sanction, holdable).
narrative_ontology:cs_axiom_grounding('aabc02f2-76f5-4350-b5e0-120914e7e35f', state_may_coerce_treatment_absent_criminal_sanction, instrumental).
narrative_ontology:cs_reference_frame('aabc02f2-76f5-4350-b5e0-120914e7e35f', public_health_harm_minimization_duty).
narrative_ontology:cs_drift_state('aabc02f2-76f5-4350-b5e0-120914e7e35f', contemporary_opioid_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aabc02f2-76f5-4350-b5e0-120914e7e35f', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_industry_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_service_operators).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, unregulated_supply_consumers).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, informal_drug_market_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, substance_use_disorder_is_a_medical_condition).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, state_duty_to_minimize_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the diversion-to-treatment framework: substance use is reclassified as a medical condition, criminal penalties are formally set aside in favor of mandated assessment, treatment enrollment, and monitoring. Sets thresholds for what counts as compliance and what triggers escalation back toward the justice system. Claims legitimacy from a duty to reduce harm rather than punish.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Operate licensed treatment programs that receive referrals and funding streams created by the diversion mandate. Revenue scales with the number of people processed through mandated treatment, independent of whether treatment is clinically indicated or effective for a given individual. Can relocate across jurisdictions or expand into adjacent contract categories if a particular mandate structure changes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_industry_providers, beneficiary,
    organized, biographical, mobile, national).

% Run needle exchanges, supervised consumption sites, and naloxone distribution under the umbrella of the same medicalized legitimacy claim. Genuinely reduce overdose deaths and disease transmission. Depend on continued political tolerance for the harm-reduction framing to keep operating, which ties their survival to the state's chosen justification rather than to an independent legal right to operate.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, harm_reduction_service_operators, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, harm_reduction_service_operators, agenda_setter).

% Diverted from criminal prosecution into court-monitored or agency-monitored treatment programs, often under threat that noncompliance reactivates criminal charges. Bear program fees, mandatory drug testing costs, and the loss of autonomy over their own treatment choices. Exiting the mandate risks incarceration, so compliance is not voluntary even though the label is medical rather than penal.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_users, payer,
    powerless, immediate, trapped, local).

% Continue to obtain substances through an illicit market that persists because the harm-reduction framework does not legalize supply, only softens the treatment of demand-side users caught by enforcement. Bear the risks of contamination, violence, and unpredictable potency the black market carries, since supply remains criminalized even as personal use is medicalized.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, unregulated_supply_consumers, payer,
    powerless, immediate, trapped, local).

% Low-level sellers and couriers who remain fully criminalized under this reading even as users are diverted to treatment. Absorb nearly all of the residual criminal-enforcement burden the reading displaces from users, often themselves substance users without access to the diversion protections extended to the buyer side.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, informal_drug_market_workers, payer,
    powerless, biographical, trapped, local).

% Retain authority to arrest and refer, now recast as a gateway into treatment rather than solely into prosecution. Preserve budget and personnel by rebranding drug enforcement as a public-health handoff function rather than ceding the enforcement role outright. Can redirect resources toward supply-side enforcement, which remains fully criminalized under this reading.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, law_enforcement_agencies, beneficiary).

% Argue from opposite premises — that harm reduction under-punishes genuine moral wrongdoing, or that it under-respects adult autonomy by retaining coercive treatment mandates and leaving supply criminalized. Neither side's preferred framework governs; both contest the medicalization compromise from outside the arrangement that actually administers it.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prohibition_and_legalization_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose deaths, disease transmission, and criminal-record harms by routing substance users toward medical assessment and treatment instead of prosecution, while preserving state capacity to intervene when use causes measurable third-party or public-health harm.
% TRANSFER_FUNCTION: Moves users out of the criminal docket and into a treatment-and-monitoring apparatus funded partly by diverted enforcement budgets and partly by new public health appropriations; moves referral volume and revenue to licensed treatment providers; leaves supply-side enforcement costs concentrated on low-level market workers.
% ABSENT_VOICES: Prohibitionists who see the reading as insufficiently punitive, and legalization advocates who see mandated treatment as coercion by another name, are both structurally outside the administering coalition of health agencies, treatment providers, and law enforcement that jointly designed and defend this compromise.
% DISAPPEARANCE_RATIONALE: If the harm-reduction legitimacy claim were withdrawn, the diversion infrastructure funding treatment providers would lose its justification and likely collapse, users currently diverted would face renewed criminal prosecution exposure, and harm-reduction services would lose the political cover that currently lets them operate — the arrangement is actively load-bearing for multiple institutions, not a redundant label on an otherwise unchanged system.
% FOUNDING_PROBLEM: Mass criminalization of substance users was producing high incarceration costs, overdose deaths from unsupervised use, and disease transmission, without measurably reducing use rates — the state needed a legitimacy basis for reducing harm that did not require abandoning all coercive authority over substance use.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers and harm-reduction operators outside the treatment-industry beneficiary group attest that overdose and disease-transmission harms are measurably reduced where diversion and supervised-use programs operate. Independent criminal-justice reform analysts and formerly mandated participants attest that the underlying coercive-enforcement problem persists in relabeled form, with supply-side criminalization and treatment-mandate coercion continuing to produce the incarceration and disruption harms the reform was meant to solve — corroboration is split between outside public-health evaluators (problem partly solved) and outside justice-reform evaluators (problem persists under new administration).
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end, rising from 0.33) because the mandate replaces overt criminal penalty with treatment-industry rents and monitoring costs that are not fully decoupled from coercion — users who exit mandated treatment risk reactivated prosecution, so the 'voluntary' medical framing carries residual criminal leverage. Suppression is moderate and slowly declining (0.50 to 0.42) as diversion programs mature and become more routine, reducing the raw coercive intensity of the initial handoff from arrest to treatment, even as the treatment industry's financial stake grows (theater_ratio rising from 0.18 to 0.33, reflecting increasing compliance-monitoring activity that serves program metrics more than individual health outcomes). Accessibility collapse is moderate (0.40): once diverted, exit from the treatment-monitoring apparatus is genuinely difficult, but the option of avoiding the system entirely by not being caught, or by accessing harm-reduction services without formal enrollment, keeps collapse from being total. Resistance is moderately high (0.55) reflecting active contestation from both prohibitionist and legalization camps, plus growing pushback from mandated participants against coerced treatment.
 *
 * PERSPECTIVAL GAP:
 *   From the administering seats (public health agencies, law enforcement, treatment providers) this looks like humane reform — real reductions in death and incarceration achieved through legitimate medical authority. From the mandated-user and informal-market-worker seats, the same structure looks like criminal-justice coercion wearing a medical label, with exit blocked by the same prosecutorial threat that existed before, now routed through a treatment bureaucracy that has its own financial stake in continued referrals.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and law enforcement jointly administer the reading and retain authority either way it cuts — public health agencies gain a durable mandate and law enforcement retains a rebranded gatekeeping role, so both sit near the beneficiary/agenda-setter end. Treatment providers are structural beneficiaries: referral volume is a revenue stream substantially decoupled from individual treatment necessity. Mandated users are near the full-target end: trapped exit options (reactivated prosecution threat), immediate time horizon, and no meaningful voice in program design. Unregulated-supply consumers and informal market workers are targets of the reading's incompleteness — the black market persists precisely because supply-side criminalization is untouched by this reading, so its harms fall on people the medicalization frame does not reach at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass incarceration and unmanaged overdose/disease harm from blanket criminalization — is only partially resolved. Diversion and harm-reduction services demonstrably reduce mortality and transmission (the coordination function is live and real), which is why this reading should not be collapsed into a pure snare. But the persistence of supply-side criminalization and coercive treatment mandates means the original problem has been relabeled rather than fully solved for a significant subset of the affected population (informal market workers, users who resist treatment enrollment). Classifying this as tangled_rope rather than rope or snare captures both halves: genuine coordination gains for the diverted-user and public-health population, riding on asymmetric extraction imposed on market-side actors and coerced-treatment participants who bear costs the medicalized legitimacy claim does not fully account for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medicalization_coercion_boundary,
    'Is the treatment mandate a genuine medical intervention responsive to individual clinical need, or is it a relabeled criminal sanction whose primary function is compliance monitoring rather than health improvement?',
    'Compare treatment-completion and relapse outcomes for mandated versus voluntary treatment populations, and assess whether program design varies with individual clinical presentation or is uniform regardless of assessed need.',
    'If treatment intensity and duration are uncorrelated with individual clinical indicators and instead track fixed program schedules or funding cycles, that supports reading the mandate as extraction with a medical veneer rather than genuine coordination; if outcomes and program design are individually responsive, that supports the coordination-function claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medicalization_coercion_boundary, empirical, 'Whether mandated treatment is clinically responsive or structurally coercive.').

omega_variable(
    black_market_persistence_attribution,
    'Does the persistence of the unregulated supply market under this reading reflect an inherent limit of demand-side-only reform, or a deliberate choice to preserve supply-side enforcement leverage and revenue?',
    'Compare jurisdictions that pair demand-side diversion with supply-side decriminalization or regulated-market pilots against jurisdictions that pair diversion with continued aggressive supply enforcement; measure black-market size and associated harms in each.',
    'If black-market harms persist similarly regardless of supply-side enforcement intensity, that suggests structural limits inherent to partial reform; if harms track enforcement intensity, that suggests the persistence is a policy choice this reading elects to retain rather than an unavoidable feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_persistence_attribution, empirical, 'Whether the persistent black market is structurally inherent or a retained policy choice.').

omega_variable(
    reading_selection_under_determination,
    'Given the same underlying facts about substance-use harm, is the choice among harm-reduction, prohibition, and legalization readings determined by evidence, or does it reflect a prior normative commitment about the proper scope of state authority over personal conduct?',
    'Examine whether jurisdictions with similar epidemiological substance-use data adopt different readings, and whether reading choice correlates more strongly with measured harm data or with independent political/moral commitments (e.g. general orientation toward paternalism vs. autonomy in unrelated policy domains).',
    'If reading choice tracks prior normative commitments rather than harm data, this substantiates treating harm_reduction_reading, prohibition_reading, and legalization_reading as genuinely distinct kernels contested on value grounds, not as competing empirical hypotheses resolvable by better data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether kernel reading selection is evidence-driven or values-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 24, 0.33).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.33).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 4, 0.37).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_legitimacy kernel. prohibition_reading authors full criminalization grounded in moral-harm prevention (higher extraction, higher suppression, no treatment-mandate hybrid). legalization_reading authors autonomy-limited-by-third-party-harm with minimal state intervention (lower extraction, near-rope classification, no persistent black market by design). This file (harm_reduction_reading) sits structurally between them: moderate extraction via treatment mandates, a tangled_rope hybrid combining real coordination (overdose/disease reduction) with asymmetric extraction (coerced treatment, unaddressed supply-side criminalization). Each reading has its own ε — they are not measurements of one constraint from different angles but three structurally distinct constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
