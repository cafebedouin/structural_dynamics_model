% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public Health Primary Vaccine Mandate (Jacobson Police Power Reading)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primary reading of
 *   the vaccine_mandate_balance kernel. The reading holds that state police
 *   power legitimately compels vaccination when voluntary compliance fails to
 *   achieve herd immunity and vulnerable populations face lethal exposure.
 *   The mandate is framed as genuine coordination (solving the free-rider
 *   problem in epidemic prevention) but operates with high extraction
 *   (enforcement penalties on the unvaccinated) and high suppression (narrow
 *   exemptions, active enforcement). The claimed_type is 'rope' — the
 *   reading's own framing as pure coordination — while authored metrics
 *   describe substantial extractive and suppressive operation. The engine
 *   will compute per-seat classifications from the structural data; the
 *   divergence between claim and computed type is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.78).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.85).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primary Vaccine Mandate (Jacobson Police Power Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '676c7cc2-610e-47d6-9e2e-819c4169ca12').
narrative_ontology:cs_kernel_codification('676c7cc2-610e-47d6-9e2e-819c4169ca12', formalized).
narrative_ontology:cs_authority_grounding('676c7cc2-610e-47d6-9e2e-819c4169ca12', lineage).
narrative_ontology:cs_interpretation_layer_present('676c7cc2-610e-47d6-9e2e-819c4169ca12').
narrative_ontology:cs_reading_relation('676c7cc2-610e-47d6-9e2e-819c4169ca12', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('676c7cc2-610e-47d6-9e2e-819c4169ca12', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('676c7cc2-610e-47d6-9e2e-819c4169ca12', foundational, collective_protection_supersedes_individual_consent).
narrative_ontology:cs_axiom_status(collective_protection_supersedes_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('676c7cc2-610e-47d6-9e2e-819c4169ca12', collective_protection_supersedes_individual_consent, conventional).
narrative_ontology:cs_axiom('676c7cc2-610e-47d6-9e2e-819c4169ca12', secondary, herd_immunity_threshold_justifies_coercion).
narrative_ontology:cs_axiom_status(herd_immunity_threshold_justifies_coercion, holdable).
narrative_ontology:cs_axiom_grounding('676c7cc2-610e-47d6-9e2e-819c4169ca12', herd_immunity_threshold_justifies_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('676c7cc2-610e-47d6-9e2e-819c4169ca12', jacobson_police_power_framework).
narrative_ontology:cs_drift_state('676c7cc2-610e-47d6-9e2e-819c4169ca12', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('676c7cc2-610e-47d6-9e2e-819c4169ca12', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_infrastructure).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, general_population_herd_immunity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, general_population_herd_immunity).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, police_power_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, herd_immunity_threshold_theory).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, collective_protection_supersedes_individual_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue and enforce vaccine mandates under police power authority (Jacobson v. Massachusetts). Define mandate scope, exemptions, and penalties. Bear political accountability for outbreak outcomes. Their authority derives from statutory and constitutional delegation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be vaccinated or mount adequate immune response; depend on herd immunity for survival. Face lethal exposure risk when community vaccination drops below threshold. Have no exit from vulnerability — their protection is structurally contingent on others' compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Receive protection from disease circulation via herd immunity. Bear compliance costs (time, access, rare adverse events) but gain collective risk reduction. Exit is constrained — opting out reduces community protection and may trigger mandate enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, general_population_herd_immunity, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, general_population_herd_immunity, payer).

% Subject to mandate penalties (fines, employment loss, school exclusion, movement restrictions) for non-compliance. This reading holds their consent is subordinated to collective necessity — they are not victims but bearers of a distributed burden. Exit options: comply, accept penalties, or relocate to jurisdictions without mandates (constrained by resources and borders).
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced, payer,
    moderate, immediate, constrained, national).

% Organize around distrust of mandate authority; frame mandates as bodily autonomy violation. Their opposition is structurally excluded from mandate design — exemptions are narrow (medical only). Identity-locked exit: leaving the community means abandoning core belief structure and social network.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_communities, excluded,
    organized, biographical, identity_locked, regional).

% Argue mandates must meet strict proportionality thresholds (disease severity, transmission risk, vaccine safety, least restrictive means). They do not reject mandates per se but demand calibrated scope. Their seat is analytical — they influence judicial review and legislative design but do not set or bear the mandate directly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, proportionality_advocates, observer,
    institutional, generational, analytical, national).

% Hold that individual consent is inviolable regardless of collective benefit. Structurally excluded from this reading's framework — their premise is foreclosed by the police power doctrine. Would argue for absolute exemption rights; their absence from the mandate's operational logic is a structural feature, not an oversight.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, bodily_autonomy_advocates, excluded,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieve and maintain herd immunity thresholds to interrupt pathogen transmission, thereby protecting individuals who cannot be vaccinated or who remain vulnerable despite vaccination. The mandate solves the collective action problem where voluntary compliance fails to reach the epidemiological threshold.
% TRANSFER_FUNCTION: Moves the burden of disease risk from immunocompromised/vulnerable populations (who face lethal exposure without herd immunity) to the unvaccinated-coerced (who bear mandate penalties and compliance costs). Transfers decision authority from individual choice to public health authorities. Transfers enforcement costs to state apparatus.
% ABSENT_VOICES: Bodily autonomy advocates (who would reject any compelled medical intervention) and proportionality advocates (who would demand narrower, evidence-calibrated mandates with robust exemptions) are structurally absent from the mandate's operational logic. Vaccine-hesitant communities are present as regulated parties but excluded from agenda-setting. Their objections are heard in litigation and legislature but do not shape the mandate's enforcement architecture.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, voluntary compliance would drop below herd immunity thresholds within months (empirically observed in jurisdictions that removed mandates). Immunocompromised populations would face lethal exposure risk. Disease circulation would increase, causing preventable deaths and healthcare system strain. The collective protection architecture would collapse — the world rearranges around uncontrolled transmission.
% FOUNDING_PROBLEM: Recurring infectious disease outbreaks killing vulnerable populations when voluntary vaccination fails to achieve herd immunity thresholds. The 1905 Jacobson case arose from a smallpox outbreak where voluntary compliance was insufficient; the founding problem is the structural gap between individual rationality (free-riding on herd immunity) and collective survival needs.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data from measles outbreaks in undervaccinated communities (Disneyland 2015, New York 2019, Ohio 2022) corroborates that voluntary compliance fails to sustain herd immunity. Historical record of pre-mandate eras shows recurrent epidemics. Immunocompromised patient advocacy organizations and public health agencies attest the problem persists. The operator (public health authorities) attests it is live; bodily autonomy advocates contest it — but corroboration comes from outside the benefiting parties: independent epidemiological modeling, historical mortality data, and immunocompromised patient testimony.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because mandate enforcement apparatus (school exclusion, employment mandates, fines, movement restrictions) imposes substantial costs on non-compliers. Suppression (0.85) is very high because alternatives (exemptions, opt-outs) are structurally narrowed to medical contraindications only — the coordination function depends on near-universal compliance. Theater ratio (0.25) is low-moderate: the public health function is real and measurable (disease incidence drops), but a growing share of enforcement energy targets compliance rather than service delivery. Accessibility collapse (0.65) reflects that once the mandate regime is understood, exit requires geographic relocation or acceptance of severe penalties. Resistance (0.72) is high and rising — legal challenges, legislative exemptions, and non-compliance movements indicate active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authorities seat, this is a genuine coordination mechanism (rope) solving a clear collective action problem. From the unvaccinated_coerced seat, the same structure operates as enforced extraction with narrow exit. From the immunocompromised_populations seat, it is a survival necessity — without it, they are victims of uncontrolled transmission. The engine computes this seat divergence from the declared roles, power, exit_options, and spatial_scope. The bodily_autonomy_advocates seat would compute as snare (total extraction, foreclosed exit), but they are excluded from this reading's framework — their classification belongs to the bodily_autonomy_primary reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda_setters (d near 0.0 — they write and enforce the rules). Immunocompromised populations are trapped beneficiaries (d ~ 0.1 — they collect protection but have no exit). General population are constrained beneficiaries/payers (d ~ 0.4 — gain herd immunity, bear compliance costs). Unvaccinated_coerced are payers bearing enforcement costs (d ~ 0.7 — constrained exit, penalties for non-compliance). Vaccine-hesitant communities are identity-locked excluded (d ~ 0.9 — their opposition is structurally excluded). Bodily autonomy advocates are identity-locked excluded (d ~ 1.0 — their premise is foreclosed). Proportionality advocates are analytical observers (d ~ 0.5). The reading explicitly denies victim status to unvaccinated_coerced — their consent is 'subordinated to necessity' — so they are not in base_properties.victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (voluntary compliance failing to protect vulnerable populations) remains live — corroborated by recurrent outbreaks in undervaccinated pockets. However, the mandate's scope has expanded from emergency smallpox control to routine childhood schedules to COVID-19 universal mandates, while disease severity and transmission dynamics vary. The mandatrophy question: does the enforcement apparatus now extract beyond what the founding problem justifies? The rising extractiveness and suppression trajectories suggest mandate creep — the coordination function (herd immunity for high-mortality pathogens) may not justify the same enforcement intensity for lower-severity diseases. The proportionality_reading exists precisely to police this boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the vaccine_mandate_balance kernel a single constraint with contested interpretation, or are the three readings structurally distinct constraints with different ε, beneficiaries, and victims?',
    'Apply the ε-invariance test: if measuring the mandate''s extraction via bodily_autonomy_primary (coercion as rights violation) yields ε ≈ 0.9 while public_health_primary (coercion as necessary coordination) yields ε ≈ 0.78, the observable changes ε — indicating distinct constraints. Decompose into separate stories linked by network.affects_constraints.',
    'If distinct constraints, each reading gets its own classification (bodily_autonomy_primary likely computes as snare; public_health_primary as tangled_rope; proportionality_reading as scaffold). If single constraint, the engine must reconcile contradictory beneficiary/victim declarations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints per the BGS decomposition principle.').

omega_variable(
    enforcement_extraction_boundary,
    'How much of the measured extractiveness (ε=0.78) is necessary coordination cost (achieving herd immunity) versus excess extraction (mandate scope exceeding epidemiological justification)?',
    'Compare mandate enforcement intensity against disease-specific herd immunity thresholds, infection fatality rates, and vaccine efficacy data. If enforcement exceeds what is needed for threshold achievement (e.g., mandates for low-transmission diseases, universal mandates where targeted protection suffices), the excess is extractive overhead.',
    'If substantial excess exists, the constraint reclassifies from rope toward tangled_rope or snare at the unvaccinated_coerced seat. The proportionality_reading exists to police this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_extraction_boundary, empirical, 'Whether mandate enforcement tracks epidemiological necessity or has accumulated rent-seeking layers.').

omega_variable(
    immunocompromised_victim_set_absence,
    'Does the immunocompromised_populations'' victim status when mandates are ABSENT (per structural delta) create a counterfactual victim set that should inform this reading''s classification?',
    'Model the counterfactual: if mandate disappears, immunocompromised become victims of lethal exposure (disappearance_verdict = world_rearranges). The engine''s directionality derivation currently only reads declared victims under the mandate. A counterfactual victim set is not captured in base_properties.victims.',
    'If counterfactual victims are structurally real, the constraint''s coordination function is vindicated — but the engine has no mechanism to weight counterfactual victimhood in χ computation. This may understate the mandate''s legitimacy from the public_health_primary seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_victim_set_absence, conceptual, 'Whether counterfactual victimhood (victims in the mandate''s absence) should modulate the constraint''s effective extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t1905, vaccine_mandate_balance__public_health_primary, theater_ratio, 1905, 0.1).
narrative_ontology:measurement(vacc_tr_t1925, vaccine_mandate_balance__public_health_primary, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(vacc_tr_t1955, vaccine_mandate_balance__public_health_primary, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(vacc_tr_t1977, vaccine_mandate_balance__public_health_primary, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(vacc_tr_t2000, vaccine_mandate_balance__public_health_primary, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_balance__public_health_primary, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(vacc_tr_t2025, vaccine_mandate_balance__public_health_primary, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(vacc_be_t1905, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1905, 0.45).
narrative_ontology:measurement(vacc_be_t1925, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1925, 0.52).
narrative_ontology:measurement(vacc_be_t1955, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1955, 0.58).
narrative_ontology:measurement(vacc_be_t1977, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1977, 0.62).
narrative_ontology:measurement(vacc_be_t2000, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(vacc_be_t2025, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t1905, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1905, 0.6).
narrative_ontology:measurement(vacc_su_t1925, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1925, 0.65).
narrative_ontology:measurement(vacc_su_t1955, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1955, 0.7).
narrative_ontology:measurement(vacc_su_t1977, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1977, 0.75).
narrative_ontology:measurement(vacc_su_t2000, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2020, 0.83).
narrative_ontology:measurement(vacc_su_t2025, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, school_vaccine_requirements).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, healthcare_worker_mandates).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, travel_vaccine_requirements).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is the public_health_primary reading of the vaccine_mandate_balance kernel. It decomposes the colloquial label 'vaccine mandate' into an ε-invariant constraint with its own beneficiary/victim structure, metrics, and classification. The bodily_autonomy_primary reading (ε higher, unvaccinated as victims, immunocompromised not beneficiaries) and proportionality_reading (ε calibrated to thresholds, robust exemptions) are separate constraint stories linked via affects_constraints. The upstream police_power_doctrine (Jacobson lineage) influences all three; this reading inherits the broadest enforcement authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, moderate, 0.65).
constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
