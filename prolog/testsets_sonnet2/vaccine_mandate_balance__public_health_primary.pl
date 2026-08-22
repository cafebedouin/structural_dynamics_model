% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Vaccine Mandate Regime — Public Health Primary Reading
 *   domain: public_health/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the vaccine
 *   mandate kernel: when voluntary vaccination fails to reach the coverage
 *   level needed for herd immunity and immunocompromised or otherwise
 *   medically vulnerable people face lethal exposure risk as a result, this
 *   reading holds that collective protection overrides individual consent as
 *   the controlling premise. This is a distinct constraint from the
 *   bodily-autonomy-primary reading (which holds consent inviolable
 *   regardless of collective benefit) and the proportionality reading (which
 *   permits mandates only under strict, continuously-tested proportionality
 *   thresholds with robust exemptions). Under this reading's own lights, the
 *   standing arrangement is the mandate regime as actually enforced — a
 *   regime whose ε is authored here as substantial, because achieving the
 *   threshold requires enforcement machinery (exclusion from work, school,
 *   and public life) that this reading endorses as necessary, not as a
 *   defect. The referent for ε is this arrangement under contest, not the
 *   rights-respecting alternative any sibling reading would prefer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.68).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Vaccine Mandate Regime — Public Health Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'fa920e60-3ebc-48ff-a40d-708d1f9842d2').
narrative_ontology:cs_kernel_codification('fa920e60-3ebc-48ff-a40d-708d1f9842d2', distributed).
narrative_ontology:cs_authority_grounding('fa920e60-3ebc-48ff-a40d-708d1f9842d2', distributed).
narrative_ontology:cs_reading_relation('fa920e60-3ebc-48ff-a40d-708d1f9842d2', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('fa920e60-3ebc-48ff-a40d-708d1f9842d2', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('fa920e60-3ebc-48ff-a40d-708d1f9842d2', foundational, collective_lethality_risk_overrides_individual_consent).
narrative_ontology:cs_axiom_status(collective_lethality_risk_overrides_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('fa920e60-3ebc-48ff-a40d-708d1f9842d2', collective_lethality_risk_overrides_individual_consent, instrumental).
narrative_ontology:cs_axiom('fa920e60-3ebc-48ff-a40d-708d1f9842d2', secondary, voluntary_compliance_failure_licenses_compulsion).
narrative_ontology:cs_axiom_status(voluntary_compliance_failure_licenses_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('fa920e60-3ebc-48ff-a40d-708d1f9842d2', voluntary_compliance_failure_licenses_compulsion, empirically_contingent).
narrative_ontology:cs_reference_frame('fa920e60-3ebc-48ff-a40d-708d1f9842d2', epidemiological_necessity_threshold).
narrative_ontology:cs_drift_state('fa920e60-3ebc-48ff-a40d-708d1f9842d2', post_pandemic_mandate_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa920e60-3ebc-48ff-a40d-708d1f9842d2', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_and_medically_vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, healthcare_system_capacity).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_workers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, bodily_autonomy_advocates).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, herd_immunity_threshold_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, collective_welfare_supersedes_individual_veto).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot be vaccinated themselves or mount adequate immune response, and depend entirely on the vaccination rate of everyone around them for protection from lethal exposure. Under this reading, when voluntary compliance falls short of the herd immunity threshold, they face direct, lethal, and otherwise unpreventable risk. They have no personal lever to raise the population vaccination rate — their safety is wholly a function of others' compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_and_medically_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, national).

% Sets vaccination requirements for employment, school enrollment, and public accommodation access, and enforces them through exclusion, fines, or termination for noncompliance. Justifies the mandate by pointing to modeled herd immunity thresholds and measured voluntary uptake shortfalls. Bears no personal cost from the mandate's operation; its institutional legitimacy and mission are what the mandate vindicates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Represents hospital and ICU throughput, which is protected when disease incidence stays below surge thresholds. Not an actor itself, but its preservation is cited as a beneficiary of the mandate's suppression of transmission chains.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, healthcare_system_capacity, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_balance__public_health_primary, healthcare_system_capacity).

% Face job loss, exclusion from school, or loss of public accommodation access for declining vaccination. Under this reading their individual medical decision is treated as a collective risk factor rather than a private choice; their stated reasons (medical caution, prior adverse reaction concern, distrust of rushed approval timelines) are not treated as sufficient grounds for exemption once voluntary uptake has failed to reach threshold. Exit means accepting economic or social exclusion, not a genuine alternative path to continued participation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_workers, payer,
    powerless, biographical, constrained, national).

% Hold sincere religious or conscience-based objections to vaccination. Under this reading, once the herd immunity threshold is unmet, the calculus explicitly subordinates the value of accommodating individual conscience to the population-level lethality risk to vulnerable groups; robust exemption pathways are narrowed or closed as compliance shortfalls persist.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, trapped, national).

% Argue that no collective benefit, however large, licenses compelled medical intervention on a competent adult without consent. Under the public-health-primary reading, this objection is heard but explicitly does not control the outcome once the herd immunity/lethality trigger is met — their framework is not the one adjudicating the mandate in this reading, though it remains their live counter-position elsewhere.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, bodily_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% Adjudicate challenges to mandates, weighing epidemiological evidence, exemption breadth, and enforcement proportionality. Their rulings determine how far this reading's logic — collective necessity overriding consent — is permitted to extend into enforcement mechanisms.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, courts_and_reviewing_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises population vaccination coverage above the epidemiological threshold needed to interrupt transmission chains, protecting those who cannot be protected by their own vaccination status.
% TRANSFER_FUNCTION: Moves bodily-decision authority from the individual to the state during a compliance shortfall, and moves exposure risk away from immunocompromised and medically vulnerable people onto vaccine-hesitant and objecting individuals who must now either comply or bear exclusion costs.
% ABSENT_VOICES: Bodily autonomy advocates and objecting individuals are heard procedurally (comment periods, litigation) but their controlling premise — that consent cannot be overridden by aggregate benefit — is not the premise this reading operates on; under this reading it is deliberately subordinated once the lethality trigger is met, not merely outvoted.
% DISAPPEARANCE_RATIONALE: If the mandate were withdrawn without any substitute, voluntary uptake (already established as sub-threshold in this reading's own premise) would leave herd immunity unachieved, and immunocompromised populations would face restored lethal exposure risk with no mitigating structure between them and community transmission; public health authorities would lose their primary compliance-forcing instrument.
% FOUNDING_PROBLEM: Voluntary vaccination campaigns were not reaching the coverage level epidemiological models identified as necessary to interrupt transmission and protect people who cannot be vaccinated or do not respond adequately to vaccination themselves.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and immunocompromised-patient advocacy organizations outside the enforcing public health agencies attest the coverage gap and resulting exposure risk are real and ongoing. Civil liberties organizations and objecting workers, also outside the benefiting public-health-authority seat, attest that the mandate mechanism has outrun the narrowly defined threshold trigger into durable employment and access conditions independent of current transmission data — this is the live contest this reading resolves in favor of the collective-protection premise.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because achieving the coordination goal (herd immunity) under this reading's own terms requires compelling compliance from individuals who have not consented — the transfer from objecting individuals to the collective good is direct and substantial, and this reading treats that transfer as justified rather than incidental. Suppression is authored high (0.72) because the mechanism's function depends on foreclosing the exemption pathways and exit options that would otherwise let objectors opt out without cost — under this reading, robust exemptions would defeat the coordination purpose once voluntary compliance has already failed. Theater ratio is kept low (0.20) because the enforcement mechanisms (exclusion, employment conditions) are functionally connected to the coordination goal, not decorative; there is little pure performance here, this reading is not primarily theatrical.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised and medically vulnerable populations are the clearest structural beneficiaries: they gain the protection the mandate exists to produce and bear none of its compliance costs, placing them near the full-beneficiary end. Public health authorities are also beneficiaries at the institutional level — their mission and legitimacy are vindicated by the mandate's operation. Vaccine-hesitant workers and religious/philosophical objectors are the targets: the mandate's costs (exclusion, job loss, compelled medical intervention) land on them directly, and their exit options are constrained-to-trapped because opting out means accepting exclusion from employment, schooling, or public life, not a genuine parallel path. Under this reading's own premises, the coerced-but-unvaccinated are NOT reclassified as victims of an illegitimate imposition — the reading holds that their consent is properly subordinated once the lethality trigger is met, so their cost is authored honestly as extraction (high ε) without treating that extraction as inherently illegitimate the way a bodily-autonomy-primary reading would.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy misclassification in one specific direction: it does NOT let 'the mandate has always been necessary' become an unfalsifiable claim, because the trigger condition (voluntary compliance failing to reach herd immunity threshold, vulnerable populations facing lethal exposure) is itself measurable and could in principle become false — if coverage rises or the vulnerable population shrinks, the founding problem's status shifts from live toward dead within this reading's own terms. The founding_problem_status is authored as contested precisely because reasonable outside observers (epidemiologists, civil liberties groups) disagree about whether the trigger condition still holds in any given jurisdiction and moment, not because the framework itself is indeterminate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_public_health_primary,
    'Is subordinating individual medical consent to collective protection the structurally correct resolution of the vaccine mandate kernel, or do the sibling readings (bodily_autonomy_primary, proportionality_reading) better capture the legitimate balance?',
    'This is not resolvable by data alone — it is a live normative and legal dispute. Courts applying strict scrutiny, proportionality review, or rational-basis review to mandate challenges effectively select among these readings jurisdiction by jurisdiction. Track which reading controlling case law adopts over time as partial evidence, while recognizing the dispute is fundamentally about which value should be lexically prior.',
    'If the bodily_autonomy_primary reading is adopted instead, the victim set would flip to include vaccine-hesitant and objecting individuals as victims of compelled intervention regardless of lethality risk, and immunocompromised populations would not appear as beneficiaries of a legitimate mandate. If proportionality_reading is adopted, the mandate''s legitimacy and hence its ε and enforcement scope would fluctuate with disease severity and vaccine safety data rather than being fixed once the herd-immunity trigger is met.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_public_health_primary, preference, 'Which of the three sibling readings of the vaccine mandate kernel should control — a genealogically irreducible value dispute, not an empirical one.').

omega_variable(
    threshold_measurement_reliability,
    'How reliably can ''voluntary compliance has failed to achieve herd immunity'' be measured in real time, given uncertainty in transmission parameters, waning immunity, and variant evolution?',
    'Compare mandate-triggering coverage estimates at the time of imposition against post-hoc epidemiological reconstruction of actual population immunity; assess how often mandates were imposed or maintained after the threshold was in fact already met or no longer applicable.',
    'If threshold measurement is frequently wrong or stale, this reading''s own legitimating premise (mandate imposed only when voluntary compliance has genuinely failed and lethal risk is genuinely present) is undermined from within, independent of which kernel reading one holds — it would mean this reading''s ε is being applied to cases outside its own stated trigger condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_measurement_reliability, empirical, 'Whether the epidemiological trigger condition this reading depends on is measured accurately and updated as conditions change.').

omega_variable(
    exemption_narrowing_trajectory,
    'As voluntary compliance shortfalls persist, does exemption narrowing (religious, medical, philosophical) proceed in proportion to measured risk, or does it ratchet toward permanent closure independent of the trigger condition''s continued presence?',
    'Track exemption approval rates and legal exemption categories over the interval against contemporaneous coverage and incidence data; a ratchet pattern (exemptions narrow but never widen even as coverage improves) would indicate the enforcement mechanism has decoupled from its own stated justification.',
    'A ratchet pattern would support reclassifying trajectory-late-stage instances of this reading''s operation toward snare (enforcement outlives and exceeds the trigger condition) even while the reading''s founding premise remains intact for the initial imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_narrowing_trajectory, empirical, 'Whether exemption policy tracks the trigger condition symmetrically or only ever tightens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__public_health_primary, theater_ratio, 6, 0.1).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.14).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_balance__public_health_primary, theater_ratio, 18, 0.16).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__public_health_primary, theater_ratio, 24, 0.18).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__public_health_primary, theater_ratio, 30, 0.19).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_balance__public_health_primary, theater_ratio, 36, 0.2).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__public_health_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_balance__public_health_primary, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__public_health_primary, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__public_health_primary, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_balance__public_health_primary, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__public_health_primary, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_balance__public_health_primary, suppression_requirement, 18, 0.67).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__public_health_primary, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__public_health_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_balance__public_health_primary, suppression_requirement, 36, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.1).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the vaccine_mandate_balance kernel (public_health_primary, bodily_autonomy_primary, proportionality_reading). Each reading is authored as a separate, ε-invariant constraint with its own beneficiary/victim structure per the kernel decomposition rule. This reading authors the highest ε among the three because it treats sustained, robust enforcement as structurally necessary rather than as a proportionality failure or an illegitimate imposition. The proportionality_reading constrains this reading's legitimate scope by imposing a continuous re-justification requirement; the bodily_autonomy_primary reading forecloses this reading's core premise entirely wherever it controls.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
