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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public Health Primary Vaccine Mandate Balance
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primary reading of the
 *   vaccine_mandate_balance kernel: collective protection supersedes
 *   individual consent when voluntary compliance fails to achieve herd
 *   immunity and vulnerable populations face lethal exposure risk. The
 *   mandate structure coordinates population-level immunity to protect the
 *   immunocompromised_exposed who cannot vaccinate, but does so through
 *   active enforcement that coerces the unvaccinated_coerced and sometimes
 *   harms the medical_exemption_denied. The claimed_type is tangled_rope
 *   because the constraint has a genuine coordination function (herd immunity
 *   for the vulnerable) AND asymmetric extraction (unvaccinated_coerced bear
 *   costs without proportional individual benefit; medical_exemption_denied
 *   are harmed by the enforcement machinery). The engine will compute
 *   per-seat classifications from the structural data — the
 *   immunocompromised_exposed seat should experience this as rope
 *   (coordination benefit, no extraction), the unvaccinated_coerced seat as
 *   snare or tangled_rope (coercion without consent), and the
 *   medical_exemption_denied seat as snare (pure harm from the structure).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.68).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primary Vaccine Mandate Balance").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'a7ea9f1a-1b86-4ad5-881a-e5d38830bb02').
narrative_ontology:cs_kernel_codification('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', formalized).
narrative_ontology:cs_authority_grounding('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', lineage).
narrative_ontology:cs_interpretation_layer_present('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02').
narrative_ontology:cs_reading_relation('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', foundational, herd_immunity_threshold_obligation).
narrative_ontology:cs_axiom_status(herd_immunity_threshold_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', herd_immunity_threshold_obligation, instrumental).
narrative_ontology:cs_axiom('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', foundational, vulnerable_population_protection_primacy).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', vulnerable_population_protection_primacy, deontological).
narrative_ontology:cs_reference_frame('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', jacobson_police_power_framework).
narrative_ontology:cs_drift_state('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', post_covid_mandate_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7ea9f1a-1b86-4ad5-881a-e5d38830bb02', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, general_population_herd_immunity).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, medical_exemption_denied).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, implement, and enforce vaccine mandates through school entry requirements, healthcare worker mandates, and emergency declarations. Control the criteria for medical exemptions and the enforcement mechanisms (fines, exclusion from institutions, loss of licensure). Justify mandates as necessary to achieve herd immunity thresholds that protect vulnerable populations who cannot be vaccinated. Bear the political and administrative costs of enforcement but control the policy apparatus.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Cannot receive live vaccines or mount adequate immune response due to conditions like chemotherapy, organ transplantation, primary immunodeficiency, or advanced HIV. Their survival depends on herd immunity maintained by high population vaccination rates. Have no exit from exposure risk — they must navigate schools, hospitals, and public spaces where unvaccinated individuals circulate. When mandates are absent or weakly enforced, they bear lethal exposure risk without consent or recourse.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed, beneficiary,
    powerless, biographical, trapped, national).

% Benefits from reduced disease transmission, healthcare system stability, and economic continuity when herd immunity thresholds are met. Bears the cost of vaccination (time, minor side effects, rare serious adverse events) but distributes this cost across the population. Has moderate exit options: can choose vaccination (low individual cost) or seek exemptions where available. The coordination benefit is real and widely shared but depends on sufficient participation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, general_population_herd_immunity, beneficiary,
    organized, biographical, mobile, national).

% Face mandates requiring vaccination for school attendance, employment, or public accommodation access. May object on grounds of bodily autonomy, religious conviction, or distrust of pharmaceutical/state institutions. Exit options are constrained: homeschooling, job change, relocation to less restrictive jurisdictions, or compliance. The cost of non-compliance escalates (fines, exclusion, loss of livelihood). Under this reading, their consent is subordinated to collective necessity — they are not classified as victims because the structural logic treats the mandate as legitimate coordination, not extraction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced, payer,
    moderate, biographical, constrained, national).

% Have legitimate medical contraindications to vaccination (anaphylaxis history, immunodeficiency, etc.) but are denied exemptions due to overly narrow criteria, bureaucratic barriers, or discretionary denial by public health officials. Bear the full coercive force of the mandate without the protection herd immunity was meant to provide. Unlike the immunocompromised_exposed who benefit from mandates, this group is harmed by the enforcement machinery itself — their medical reality is overridden by the same structure that claims to protect the vulnerable.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, medical_exemption_denied, payer,
    powerless, immediate, trapped, national).

% Adjudicate challenges to mandate authority, scope, and exemption processes. Apply frameworks ranging from Jacobson v. Massachusetts (broad police power) to strict scrutiny of fundamental rights. Their rulings shape the enforcement envelope — defining what counts as a legitimate medical exemption, whether religious exemptions are required, and what procedural due process applies. Do not bear the mandate's costs or collect its benefits; their role is structural interpretation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves and maintains herd immunity thresholds that interrupt pathogen transmission, protecting those who cannot be vaccinated and preventing healthcare system collapse during outbreaks. Solves the collective action problem where individual vaccination decisions create positive externalities but free-riding undermines the threshold.
% TRANSFER_FUNCTION: Moves the burden of disease risk from immunocompromised_exposed (who would face lethal exposure in an unvaccinated population) to unvaccinated_coerced (who bear mandate compliance costs, adverse event risk, and autonomy loss). Also transfers enforcement authority to public_health_authorities who administer the mandate apparatus.
% ABSENT_VOICES: Children too young to consent but subject to school mandates; undocumented populations who avoid healthcare systems and are invisible to both mandate enforcement and herd immunity benefits; future generations who inherit the precedent of state-compelled medical intervention. These voices are structurally excluded from the policy negotiation.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, vaccination rates would drop below herd immunity thresholds within 2-3 years (observed in jurisdictions that weakened mandates). Immunocompromised_exposed would face sharply rising exposure risk and mortality. Disease outbreaks would resurge, destabilizing schools, healthcare, and economies. Public_health_authorities would lose their primary coordination tool. The world rearranges because arrangements of schooling, healthcare access, and immunocompromised survival depend on the mandate structure.
% FOUNDING_PROBLEM: Recurrent epidemic cycles of vaccine-preventable diseases (measles, polio, pertussis) causing child mortality, healthcare overload, and economic disruption — compounded by the ethical problem that those most at risk (immunocompromised, infants) cannot protect themselves through vaccination.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians and epidemiologists outside the benefiting authorities document that pre-mandate eras had cyclical epidemics with high childhood mortality; the founding problem is empirically grounded. However, critics (including some bioethicists and legal scholars not employed by health authorities) argue the problem has mutated: modern vaccines have higher safety profiles but mandates now target diseases with lower severity (varicella, hepatitis B) and the enforcement apparatus has expanded beyond the original life-threatening epidemic rationale.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects that mandate enforcement has expanded beyond acute epidemic diseases to routine childhood schedules, COVID-19 emergency mandates, and healthcare worker requirements — the compliance burden and enforcement apparatus have grown faster than the marginal herd immunity benefit. Suppression (0.72) is high because the constraint actively eliminates alternatives: non-medical exemptions narrowed or eliminated in multiple jurisdictions, homeschooling burden increased, employment termination for non-compliance. Theater_ratio (0.22) is moderate-low: the public health function is real and evidenced, but a growing share of enforcement activity targets low-transmission-risk settings (e.g., remote workers, low-contact occupations) suggesting mission creep. Accessibility_collapse (0.45) reflects that alternatives exist but are costly (homeschooling, relocation, unemployment). Resistance (0.58) is substantial and rising — legal challenges, legislative rollback efforts, and compliance avoidance are measurable.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (unvaccinated_coerced, medical_exemption_denied) and the agenda-setter seat (public_health_authorities) should compute dramatically different types: from the authority position, the mandate is a genuine coordination mechanism solving a lethal collective action problem; from the coerced unvaccinated position, it is enforced extraction of bodily autonomy; from the wrongly denied medical exemption position, it is a snare that harms the very vulnerable it claims to protect. The engine computes this divergence from the structural data — the authored claim does not adjudicate it. This seat divergence IS the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   public_health_authorities are the agenda_setter and primary beneficiary (control the apparatus, collect institutional legitimacy and budget authority) — d near beneficiary end. immunocompromised_exposed are pure beneficiaries with trapped exit — d at beneficiary extreme (constraint subsidizes their survival). general_population_herd_immunity are beneficiaries with mobile exit — d slightly beneficiary of symmetric. unvaccinated_coerced are payers with constrained exit — d near target end (coercion without consent, but this reading treats it as legitimate coordination cost). medical_exemption_denied are victims with trapped exit — d at target extreme (harmed by the structure they cannot escape). constitutional_courts are observers with analytical exit — d symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epidemic cycles of lethal childhood diseases) was substantially solved for the original target diseases (smallpox, polio, measles). However, the mandate apparatus expanded to diseases with lower severity and transmission profiles, and enforcement mechanisms hardened (removing philosophical/religious exemptions, adding adult mandates). This reading treats the expansion as legitimate adaptation to new threats; the bodily_autonomy_primary reading treats it as mandatrophy — the coordination function atrophied while the extraction apparatus grew. The proportionality_reading treats it as contested: some expansions meet proportionality, others do not. The engine's computed types across seats will reveal which reading the structure supports.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine instantiation of the public_health_primary reading, or does it collapse into the proportionality_reading when empirical thresholds are applied?',
    'Empirical analysis of whether current mandate scopes (diseases targeted, populations covered, exemption narrowness) exceed what herd immunity thresholds and vulnerable population protection actually require. If mandates exceed epidemiological necessity, the reading collapses toward proportionality or bodily_autonomy.',
    'If the reading collapses, the claimed tangled_rope classification may be too generous — the constraint may be snare from more seats than this reading acknowledges. The beneficiary/victim structure would shift: unvaccinated_coerced would become clearer victims, medical_exemption_denied would expand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the public_health_primary reading''s structural claims hold under empirical scrutiny of mandate scope vs. epidemiological necessity.').

omega_variable(
    coercion_mechanism_ambiguity,
    'Is the measured suppression (0.72) primarily structural (legal penalties, institutional exclusion) or does it include internalized suppression (social stigma, moral pressure, identity fusion with compliance)?',
    'Post-mandate relaxation studies: if compliance persists after legal penalties are removed, internalized suppression is significant. Survey data on stated reasons for vaccination (protection vs. mandate avoidance vs. social pressure).',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure — the unvaccinated_coerced carry the suppression internally after formal exit. This would increase effective extraction for that seat and strengthen the snare classification from their position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanisms in vaccine mandate compliance.').

omega_variable(
    mandate_scope_creep,
    'Has the mandate apparatus expanded beyond the epidemiological conditions that originally justified it (lethal epidemics, no treatments, sterilizing vaccines)?',
    'Historical comparison of mandate scope (diseases, populations, enforcement severity) against disease severity metrics (IFR, R0, vaccine sterilizing efficacy, treatment availability) over the interval.',
    'If scope creep is documented, the mandatrophy analysis is validated — the coordination function atrophied relative to the enforcement apparatus. This would shift the constraint toward snare or piton classification from more seats, and increase extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_scope_creep, empirical, 'Whether mandate scope tracks epidemiological necessity or has decoupled into self-sustaining enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t1905, vaccine_mandate_balance__public_health_primary, theater_ratio, 1905, 0.1).
narrative_ontology:measurement(vacc_tr_t1955, vaccine_mandate_balance__public_health_primary, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(vacc_tr_t1980, vaccine_mandate_balance__public_health_primary, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(vacc_tr_t2000, vaccine_mandate_balance__public_health_primary, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(vacc_tr_t2015, vaccine_mandate_balance__public_health_primary, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_balance__public_health_primary, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(vacc_tr_t2025, vaccine_mandate_balance__public_health_primary, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(vacc_be_t1905, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1905, 0.35).
narrative_ontology:measurement(vacc_be_t1955, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1955, 0.42).
narrative_ontology:measurement(vacc_be_t1980, vaccine_mandate_balance__public_health_primary, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(vacc_be_t2000, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(vacc_be_t2015, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(vacc_be_t2025, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t1905, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1905, 0.4).
narrative_ontology:measurement(vacc_su_t1955, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(vacc_su_t1980, vaccine_mandate_balance__public_health_primary, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(vacc_su_t2000, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(vacc_su_t2015, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(vacc_su_t2025, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, school_entry_requirements).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, healthcare_worker_mandates).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, emergency_health_powers).

% DUAL FORMULATION NOTE:
% Part of the vaccine_mandate_balance constraint family. This reading (public_health_primary) claims collective protection supersedes individual consent under herd immunity failure conditions. The bodily_autonomy_primary reading claims individual consent is inviolable. The proportionality_reading claims mandates require strict proportionality thresholds. These are structurally distinct constraints with different beneficiary/victim sets and extractiveness profiles, linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, moderate, 0.75).
constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
