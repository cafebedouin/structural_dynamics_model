% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Gated Vaccine Mandate Framework
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the vaccine
 *   mandate balance kernel: mandates are permissible only when disease
 *   severity, transmission risk, and vaccine safety jointly clear a strict
 *   threshold, and any mandate must carry robust exemptions. This is
 *   deliberately a middle reading between two categorical siblings —
 *   bodily_autonomy_primary (consent is inviolable regardless of collective
 *   benefit) and public_health_primary (collective protection supersedes
 *   consent once voluntary compliance fails). The proportionality reading
 *   rejects both categorical premises: it treats mandate legitimacy as a
 *   function of pathogen-specific parameters rather than a fixed rule in
 *   either direction. Because legitimacy is conditional on disease
 *   parameters, epsilon is authored here for the standing arrangement as the
 *   proportionality reading's own test currently administers it (moderate,
 *   contested) — not for either sibling's endorsed alternative, and not
 *   averaged across pathogens. A smallpox-level mandate under this test would
 *   show near-zero extraction; a seasonal-flu-level mandate forced through
 *   the same administrative apparatus would show much higher extraction. This
 *   story authors the ε for the framework's actual operating record across
 *   the pathogens it has in fact been applied to during the interval, which
 *   is mixed-severity, hence the moderate 0.42.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.48).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Gated Vaccine Mandate Framework").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'ee473327-c678-4a5e-b296-cec01d84d1a8').
narrative_ontology:cs_kernel_codification('ee473327-c678-4a5e-b296-cec01d84d1a8', distributed).
narrative_ontology:cs_authority_grounding('ee473327-c678-4a5e-b296-cec01d84d1a8', distributed).
narrative_ontology:cs_reading_relation('ee473327-c678-4a5e-b296-cec01d84d1a8', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('ee473327-c678-4a5e-b296-cec01d84d1a8', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('ee473327-c678-4a5e-b296-cec01d84d1a8', foundational, mandate_legitimacy_is_conditional_not_categorical).
narrative_ontology:cs_axiom_status(mandate_legitimacy_is_conditional_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('ee473327-c678-4a5e-b296-cec01d84d1a8', mandate_legitimacy_is_conditional_not_categorical, conventional).
narrative_ontology:cs_axiom('ee473327-c678-4a5e-b296-cec01d84d1a8', foundational, exemption_robustness_is_a_necessary_condition_of_legitimacy).
narrative_ontology:cs_axiom_status(exemption_robustness_is_a_necessary_condition_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ee473327-c678-4a5e-b296-cec01d84d1a8', exemption_robustness_is_a_necessary_condition_of_legitimacy, deontological).
narrative_ontology:cs_reference_frame('ee473327-c678-4a5e-b296-cec01d84d1a8', tiered_scrutiny_public_health_jurisprudence).
narrative_ontology:cs_drift_state('ee473327-c678-4a5e-b296-cec01d84d1a8', post_pandemic_mandate_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee473327-c678-4a5e-b296-cec01d84d1a8', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, general_population_below_herd_threshold).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_workers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, borderline_exemption_claimants).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, low_severity_pathogen_mandate_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, general_population_below_herd_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the proportionality test — assesses disease severity, transmission risk, and vaccine safety data before authorizing a mandate, and is responsible for calibrating exemption criteria. Benefits from legitimacy when the test is applied honestly, but bears reputational and legal risk when the threshold is misjudged in either direction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be vaccinated themselves or face lethal risk from exposure; depend entirely on herd-level compliance achieved through mandates that meet the proportionality bar. They have no exit from the epidemiological environment others create around them and no voice in setting the threshold.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, immediate, trapped, local).

% Gains protection when the mandate is correctly calibrated to a genuinely severe, transmissible, low-risk-vaccine pathogen; bears the mandate's compliance burden (time, minor medical risk, occasional side effects) in exchange for that protection. Whether this population nets as beneficiary or payer depends on whether the proportionality test was applied honestly for the specific pathogen.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, general_population_below_herd_threshold, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, general_population_below_herd_threshold, payer).

% Face employment or access conditioned on vaccination status once a mandate clears the proportionality threshold. Their genuine safety or autonomy concerns are only heard through the exemption process; if exemptions are narrowly drawn, they bear the mandate's coercive weight even when their individual risk calculus differs from the population-level threshold that justified the policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_workers, payer,
    moderate, biographical, constrained, national).

% Have medical, religious, or conscience objections that sit near the edge of what counts as a 'robust' exemption. Whether their claim is honored depends on how strictly the exemption robustness standard is administered — a standard tightened for compliance optics can convert a legitimate exemption claimant into a mandate target.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, borderline_exemption_claimants, payer,
    powerless, immediate, trapped, local).

% Are subject to a mandate for a pathogen whose severity or transmission risk turns out, on later or more rigorous review, not to have cleared the proportionality bar it was claimed to clear. They bear the mandate's costs under a proportionality claim that was asserted rather than rigorously verified at the time.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, low_severity_pathogen_mandate_targets, payer,
    powerless, immediate, trapped, regional).

% Reviews whether a specific mandate's proportionality showing (severity, transmission, safety, exemption robustness) meets the legal threshold, and can strike down or narrow mandates that fail the test. Neither collects from nor pays into the arrangement; adjudicates it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, falsifiable test — severity, transmission risk, vaccine safety, exemption robustness — that lets a mandate be authorized only when collective protection genuinely outweighs the individual burden, rather than authorized or denied by political convenience.
% TRANSFER_FUNCTION: When the test is satisfied, moves compliance burden (bodily intervention, minor risk, time cost) from the vulnerable population's exposure risk onto the vaccinated population; when the test is not honestly applied, moves the same burden onto workers and exemption claimants without the compensating collective benefit that justifies it.
% ABSENT_VOICES: Individuals whose personal risk-benefit calculus diverges from the population-level threshold (rare adverse-reaction risk, prior infection, unusual occupational exposure) are represented only through the abstract 'vaccine safety' variable in the test, not as individuated voices; the exemption process is their only channel, and it is administered by the same agency that set the mandate.
% DISAPPEARANCE_RATIONALE: Public health agencies would say vanishing the proportionality standard reverts mandate authority to unconstrained political discretion, exposing vulnerable populations to under-protection or workers to over-mandate depending on who holds power. Civil liberties advocates would say the standard already fails to reliably constrain mandates in practice and its disappearance would only make explicit what selective enforcement already permits. Both agree something would change; they disagree on direction.
% FOUNDING_PROBLEM: Early pandemic-era mandates were issued or resisted on largely political or categorical grounds (either 'safety trumps everything' or 'consent trumps everything'), producing mandates poorly matched to actual epidemiological severity and eroding public trust in both directions.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside public health agencies (constitutional law academics, several appellate courts applying tiered-scrutiny analysis) attest the proportionality framework is a genuine constraint that has struck down or narrowed mandates failing the test — corroboration external to the agencies that administer it. Civil liberties organizations counter that the threshold has in practice been calibrated post hoc to justify mandates already decided on political grounds, meaning the founding problem persists disguised as solved.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, contested).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the proportionality test, as actually administered, has been satisfied cleanly for some mandates (high-severity, high-transmission, well-established vaccine safety) and applied more loosely for others (moderate-severity pathogens where the threshold showing was asserted rather than rigorously demonstrated) - producing a mixed record rather than either pole. Suppression (0.48) is moderate: the framework does have enforcement teeth (workplace and access conditions) but is explicitly bounded by the exemption requirement, which is a genuine, if imperfectly administered, release valve. Theater ratio (0.3) captures that some proportionality showings are procedurally performed - the severity/transmission/safety memo is produced, but the underlying data review is sometimes thin, especially at the margins where mandate authorization is politically convenient.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised populations and the general population below herd threshold are structural beneficiaries when the test is honestly satisfied — they receive real collective protection at bounded personal cost. Vaccine-hesitant workers, borderline exemption claimants, and low-severity-pathogen mandate targets are victims precisely to the degree the proportionality showing for their specific pathogen event was weak or the exemption standard was drawn narrowly. Directionality here is genuinely conditional on the pathogen-specific facts, which is the structural signature this reading is built to capture — unlike the categorical siblings, this reading's ε and victim set are functions of the disease parameters, not fixed regardless of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading is designed specifically to prevent mandatrophy in both directions: it prevents public-health mandates from calcifying into permanent extraction once a pathogen's severity has genuinely declined (mandate that outlives the disease parameters that justified it), and it prevents bodily-autonomy claims from being used to block mandates that remain genuinely proportionate. The mixed measurement record (ε rising slightly from 0.30 to 0.42 over the interval) reflects mandates increasingly being tested against pathogens of declining severity — the framework's own trend line shows it drifting toward looser application of its own standard, which is exactly the drift the framework exists to catch, and exactly the kind of drift its critics (both sibling readings) point to as evidence the proportionality test does not reliably self-correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the proportionality reading a genuinely distinct normative position, or an unstable compromise that collapses into one of the two categorical siblings under real-world pressure to decide specific cases?',
    'Track case-by-case outcomes: if courts and agencies applying the proportionality test converge predictably (e.g., systematically favoring mandate authorization or systematically favoring exemption) across a large sample of pathogen-specific applications, that convergence pattern would indicate the ''balance'' reading functions as one of the categorical siblings in practice despite its stated framework.',
    'If the reading collapses into public_health_primary in practice, its ε and victim set should track that sibling''s higher-suppression profile; if it collapses into bodily_autonomy_primary, mandates would rarely if ever clear the threshold and ε would trend toward zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether the proportionality reading is a stable third position or an unstable compromise between the two categorical siblings.').

omega_variable(
    pathogen_specific_epsilon_variance,
    'How much does epsilon actually vary across pathogens governed by the same proportionality framework — is a single ε value for this reading defensible, or does the framework itself split into as many constraints as there are pathogen risk profiles?',
    'Compile mandate-by-mandate proportionality showings across multiple pathogens (smallpox, measles, seasonal influenza, a novel pandemic pathogen) and measure the dispersion of resulting extraction and suppression scores under the same administrative test.',
    'If the dispersion is very high (e.g., near-zero ε for smallpox-tier mandates versus high ε for flu-tier mandates), the ε-invariance principle would argue for decomposing this single story into pathogen-tier-specific constraints rather than one aggregate proportionality-reading story; the current 0.42 would then be understood as a population-weighted average masking two structurally distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_specific_epsilon_variance, empirical, 'Whether pathogen-specific variance is large enough to require further decomposition beyond the three kernel readings.').

omega_variable(
    exemption_robustness_administration,
    'Is the exemption-robustness requirement enforced as a genuine check on mandate scope, or is ''robust exemptions'' a nominal criterion that is administratively narrowed whenever compliance targets are prioritized?',
    'Compare exemption grant rates and appeal outcomes across jurisdictions and time periods with differing political pressure toward high compliance; a systematic tightening of exemption criteria correlated with compliance-target pressure (rather than with new safety or medical evidence) would indicate the robustness requirement functions as theater.',
    'If exemption robustness is administratively hollow, the theater_ratio and suppression figures understate the framework''s actual coercive weight on borderline claimants, and the tangled_rope classification would strengthen toward a snare-leaning profile for that subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_administration, empirical, 'Whether the exemption requirement is a real constraint on mandate scope or a nominal one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__proportionality_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__proportionality_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__proportionality_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__proportionality_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__proportionality_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the vaccine_mandate_balance kernel. public_health_primary treats collective protection as categorically superseding consent once voluntary compliance fails; bodily_autonomy_primary treats consent as categorically inviolable regardless of collective benefit. This proportionality_reading rejects both categorical premises in favor of a pathogen-conditional test, producing a moderate, contested ε (0.42) rather than either sibling's more extreme value. All three share the same underlying kernel (the legitimacy conditions for state-compelled vaccination) but instantiate structurally distinct constraints with distinct victim sets, distinct beneficiary sets, and distinct persistence conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
