% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The proportionality reading of legitimate health intervention holds that
 *   state interference with bodily integrity must be calibrated to the
 *   objective threat posed by a specific disease — its transmissibility,
 *   case-fatality rate, population vulnerability, and availability of less
 *   restrictive alternatives. This is not a fixed rule but a structured
 *   framework: measles (R0~12-18, CFR~0.1-0.3% in developed settings)
 *   justifies school mandates; seasonal flu (R0~1.3, CFR~0.1%) does not
 *   justify universal mandates; Ebola (R0~1.5-2.5, CFR~50%) justifies
 *   aggressive isolation. The constraint is the proportionality principle
 *   itself — a legal/ethical standard that structures how public health
 *   authority is exercised. Its extractiveness varies with disease context
 *   (hence the conditional structure), but the principle as a standing
 *   arrangement has moderate base extractiveness because it empowers state
 *   action while limiting it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.35).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.25).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Principle for Public Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, 'b6433a41-b6fe-4bc9-a7da-67f458a47b04').
narrative_ontology:cs_kernel_codification('b6433a41-b6fe-4bc9-a7da-67f458a47b04', formalized).
narrative_ontology:cs_authority_grounding('b6433a41-b6fe-4bc9-a7da-67f458a47b04', lineage).
narrative_ontology:cs_interpretation_layer_present('b6433a41-b6fe-4bc9-a7da-67f458a47b04').
narrative_ontology:cs_reading_relation('b6433a41-b6fe-4bc9-a7da-67f458a47b04', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('b6433a41-b6fe-4bc9-a7da-67f458a47b04', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('b6433a41-b6fe-4bc9-a7da-67f458a47b04', foundational, proportionality_as_legitimacy_condition).
narrative_ontology:cs_axiom_status(proportionality_as_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('b6433a41-b6fe-4bc9-a7da-67f458a47b04', proportionality_as_legitimacy_condition, conventional).
narrative_ontology:cs_axiom('b6433a41-b6fe-4bc9-a7da-67f458a47b04', foundational, disease_characteristics_weight_both_values).
narrative_ontology:cs_axiom_status(disease_characteristics_weight_both_values, holdable).
narrative_ontology:cs_axiom_grounding('b6433a41-b6fe-4bc9-a7da-67f458a47b04', disease_characteristics_weight_both_values, empirically_contingent).
narrative_ontology:cs_reference_frame('b6433a41-b6fe-4bc9-a7da-67f458a47b04', jacobson_reasonableness_standard).
narrative_ontology:cs_drift_state('b6433a41-b6fe-4bc9-a7da-67f458a47b04', post_covid_emergency_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6433a41-b6fe-4bc9-a7da-67f458a47b04', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_population).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, civil_liberties_advocates).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_disproportionate_measures).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, disproportionately_burdened_groups).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, proportionality_as_legitimacy_condition).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, evidence_based_threat_assessment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and implement health interventions (mandates, quarantines, closures) subject to proportionality review. Must justify severity against threat evidence. Bear political cost of both under- and over-action. Their decisions are reviewed by courts.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Receives protection from both disease spread and excessive state intrusion. Benefits when proportionality calibrates interventions to actual threat. Bears residual risk when interventions are judged proportionate but imperfect. Exit is constrained — cannot opt out of public health system or disease exposure.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_population, beneficiary,
    organized, biographical, constrained, national).

% Use proportionality doctrine to challenge overbroad mandates in court. Benefit from the constraint's requirement that state justify intrusion. Their advocacy shapes the doctrinal tests (strict scrutiny, least restrictive means). Can shift jurisdiction or forum but operate within the same constitutional framework.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, civil_liberties_advocates, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct burden of interventions later judged disproportionate — e.g., parents fined for vaccine refusal during low-threat outbreaks, workers terminated for refusing mandates with weak evidence base. Their autonomy interest is fused with bodily integrity or parental authority; exit means abandoning core identity commitments (faith, parenting philosophy, bodily sovereignty).
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_disproportionate_measures, payer,
    powerless, immediate, identity_locked, local).

% Marginalized communities (racial minorities, low-income, institutionalized) historically subjected to harsher interventions for same threat level — e.g., forced vaccination campaigns targeting immigrants, quarantine of poor neighborhoods. Structural vulnerability compounds: cannot access legal challenge, cannot relocate, bear compounding harms from both disease and state response.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, disproportionately_burdened_groups, payer,
    powerless, biographical, trapped, regional).

% Adjudicate proportionality challenges. Develop and apply doctrinal tests (Jacobson reasonableness, strict scrutiny, intermediate scrutiny). Their rulings define the operational boundary of the constraint. Do not design interventions but determine which survive review. Exit is analytical — they interpret, not participate.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured framework for balancing collective health protection against individual bodily integrity, replacing ad hoc state power with calibrated scrutiny keyed to disease characteristics (transmissibility, severity, population vulnerability).
% TRANSFER_FUNCTION: Allocates decision authority: when threat is high (measles, Ebola), authority shifts toward state; when threat is low (seasonal flu, low-transmission conditions), authority remains with individual. Transfers the burden of justification to the state for any liberty restriction.
% ABSENT_VOICES: Those most burdened by specific interventions — incarcerated populations, undocumented migrants, children in state custody, institutionalized disabled persons — are rarely parties to the proportionality litigation that shapes doctrine. Their experience of 'proportionate' measures (forced medication, isolation, vaccination) is filtered through state representatives.
% DISAPPEARANCE_RATIONALE: Without proportionality, the legal framework collapses to either Jacobson-era near-plenary police power (state wins always) or strict bodily autonomy absolutism (individual wins always). Both extremes have been rejected in modern jurisprudence; the doctrinal middle is what makes calibrated review possible.
% FOUNDING_PROBLEM: Early 20th century tension: Jacobson v. Massachusetts (1905) upheld compulsory smallpox vaccination under a reasonableness standard, but gave no principled limit — state could mandate any intervention for any disease. Later civil liberties expansion (Skinner, Roe, Cruzan) created pressure for a limiting principle that respected both public health necessity and constitutional liberty.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars (Gostin, Jacobson, Bayer & Fairchild) document the historical arc from plenary power to calibrated review. Courts in multiple jurisdictions (US, Canada, EU, South Africa) have independently converged on proportionality as the governing framework. No single beneficiary group controls this consensus.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.35 reflects that the principle enables state action (extraction from autonomy) but constrains it (protection from overreach). The 2020 spike (0.45) reflects COVID-era expansion of mandates under emergency frameworks; the 2024 reversion reflects judicial pushback. Theater ratio stays low (0.15) because proportionality review is functional — courts regularly strike down overbroad measures. Suppression is low-moderate (0.25) because the constraint is a limit on suppression, not a suppressive mechanism itself. Accessibility collapse 0.55: the pure autonomy and pure utilitarian alternatives remain intellectually available but are institutionally marginalized. Resistance 0.50: contested from both sides — autonomy absolutists say it permits too much; public health absolutists say it permits too little.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat, proportionality is a coordination tool that legitimizes necessary action. From the payer seats (especially identity_locked and trapped), the same doctrine can feel like a rubber stamp — courts defer to agency expertise, 'least restrictive means' becomes a paper test. The engine will compute this divergence from the structural data: same constraint, different χ across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda_setters (d ~0.3 — they wield the constraint but are bound by it). General population and civil liberties advocates are beneficiaries (d ~0.2-0.3 — constraint protects their interests). Individuals subject to disproportionate measures and disproportionately burdened groups are payers (d ~0.7-0.9 — they bear the cost when proportionality fails or is misapplied). Courts are observers (d ~0.5 — analytical seat). The identity_locked exit for individuals_subject_to_disproportionate_measures reflects that bodily integrity and parental authority are identity-constitutive; the trapped exit for disproportionately_burdened_groups reflects structural inability to exit the jurisdiction or access review.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary state power vs. unchecked disease) remains live — COVID-19 demonstrated that novel pathogens still test the calibration. Proportionality has not atrophied into piton; it is actively litigated and doctrinally refined. But mandatrophy risk exists in the 'emergency exception' tendency: crises expand state power, and the proportionality ratchet may not fully reverse. The 2020-2024 extraction spike and partial reversion illustrates this dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_characteristic_thresholds,
    'What specific transmissibility and severity thresholds trigger which level of scrutiny? Where is the line between ''proportionate'' and ''disproportionate'' for a given disease?',
    'Systematic analysis of judicial decisions across disease contexts (measles, TB, HIV, COVID, flu, HPV) to extract implicit thresholds. Legislative codification of trigger metrics (e.g., R0 > X, CFR > Y% triggers strict scrutiny).',
    'If thresholds are vague, the constraint operates as a standardless delegation to agencies (higher effective extraction). If thresholds are precise, the constraint becomes a genuine coordination mechanism with predictable boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_characteristic_thresholds, empirical, 'Whether proportionality has operationalizable disease-characteristic thresholds or functions as an open-ended balancing test.').

omega_variable(
    judicial_deference_to_agency_expertise,
    'How much deference do courts give to public health authorities'' threat assessments? Does proportionality review become toothless under ''rational basis with bite''?',
    'Empirical study of proportionality challenge outcomes: what fraction of mandates survive review? How often do courts second-guess epidemiological judgments?',
    'High deference → constraint is theater (higher theater_ratio, higher effective extraction). Low deference → constraint is a genuine check (lower extraction, higher resistance from authorities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_to_agency_expertise, empirical, 'Whether judicial review of proportionality is substantive or deferential.').

omega_variable(
    committer_frame_proportionality_reading,
    'This constraint is one reading (proportionality_reading) of the contested kernel legitimate_health_intervention. How does this reading''s conditional structure (ε scaling with disease characteristics) affect its classification relative to the sibling readings?',
    'Compare ε and victim sets across the three readings when instantiated for the same disease scenarios. The proportionality reading''s victim set should vary with disease severity; the sibling readings'' victim sets should be relatively stable.',
    'If the proportionality reading''s conditional structure is structurally coherent, it should show lower extraction for low-threat diseases and higher for high-threat diseases — demonstrating genuine calibration. If extraction is invariant, the conditionality is rhetorical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_proportionality_reading, conceptual, 'Commitment-system framing: this reading instantiates a conditional constraint structure that distinguishes it from the absolute sibling readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to challenge, pre-enforcement review limitations) or internalized (chilling effect, normalization of mandates)?',
    'Post-enforcement trajectory analysis: if suppression metrics decline after legal victories but compliance behavior remains constrained, internalized component is significant.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the population carries the suppression with them after formal barriers are removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in the proportionality framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 1905, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1905, legitimate_health_intervention__proportionality_reading, theater_ratio, 1905, 0.1).
narrative_ontology:measurement(legi_tr_t1942, legitimate_health_intervention__proportionality_reading, theater_ratio, 1942, 0.15).
narrative_ontology:measurement(legi_tr_t1973, legitimate_health_intervention__proportionality_reading, theater_ratio, 1973, 0.2).
narrative_ontology:measurement(legi_tr_t1990, legitimate_health_intervention__proportionality_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(legi_tr_t2005, legitimate_health_intervention__proportionality_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(legi_tr_t2020, legitimate_health_intervention__proportionality_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(legi_tr_t2024, legitimate_health_intervention__proportionality_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(legi_be_t1905, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1905, 0.65).
narrative_ontology:measurement(legi_be_t1942, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1942, 0.55).
narrative_ontology:measurement(legi_be_t1973, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1973, 0.4).
narrative_ontology:measurement(legi_be_t1990, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(legi_be_t2005, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2005, 0.3).
narrative_ontology:measurement(legi_be_t2020, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(legi_be_t2024, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1905, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1905, 0.2).
narrative_ontology:measurement(legi_su_t1942, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1942, 0.3).
narrative_ontology:measurement(legi_su_t1973, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1973, 0.25).
narrative_ontology:measurement(legi_su_t1990, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(legi_su_t2005, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(legi_su_t2020, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(legi_su_t2024, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, mandatory_vaccination_school_entry).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, quarantine_authority).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, emergency_health_powers).

% DUAL FORMULATION NOTE:
% This constraint (proportionality_reading) is one of three readings of the legitimate_health_intervention kernel. The public_health_primary reading treats population-level mortality reduction as the sole legitimacy ground (higher ε for autonomy interests). The bodily_autonomy_primary reading treats informed consent as necessary (higher ε for state interests). This reading's conditional structure — ε scales with transmissibility and CFR — is the structural delta. All three stories should be linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__proportionality_reading, institutional, 0.25).
constraint_indexing:directionality_override(legitimate_health_intervention__proportionality_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
