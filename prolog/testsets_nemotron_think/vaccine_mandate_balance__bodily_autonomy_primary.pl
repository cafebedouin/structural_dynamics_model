% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Vaccine Mandate Balance — Bodily Autonomy Primary Reading
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the vaccine_mandate_balance kernel. The standing arrangement under
 *   contest is the COVID-era vaccine mandate regime (employment, education,
 *   travel, military mandates) assessed from the reading that individual
 *   consent is inviolable regardless of collective benefit. The reading
 *   claims the mandate arrangement is a Snare — pure extraction wearing
 *   public health coordination as cover. Beneficiaries are the state public
 *   health apparatus, pharmaceutical industry, and public health officials
 *   who gain guaranteed markets, expanded authority, and career capital.
 *   Victims are the unvaccinated-coerced who face compulsion, exclusion, and
 *   identity rupture. Immunocompromised-exposed are not victims in this
 *   reading — risk acceptance is inherent to liberty. The claim/metric
 *   independence is maintained: claimed_type is 'snare' (the mandate
 *   arrangement's true operation) while the reading's rhetoric invokes bodily
 *   autonomy as a Mountain (natural law).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.78).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Vaccine Mandate Balance — Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '0c1af67d-f99d-4a86-a61d-b3a1dfe10790').
narrative_ontology:cs_kernel_codification('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', formalized).
narrative_ontology:cs_authority_grounding('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', lineage).
narrative_ontology:cs_interpretation_layer_present('0c1af67d-f99d-4a86-a61d-b3a1dfe10790').
narrative_ontology:cs_reading_relation('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', foundational, state_no_compel_medical).
narrative_ontology:cs_axiom_status(state_no_compel_medical, holdable).
narrative_ontology:cs_axiom_grounding('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', state_no_compel_medical, deontological).
narrative_ontology:cs_reference_frame('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', classical_liberal_bodily_integrity).
narrative_ontology:cs_drift_state('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', post_covid_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0c1af67d-f99d-4a86-a61d-b3a1dfe10790', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_apparatus).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, public_health_officials).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, bodily_integrity_absolute).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, informed_consent_inviolable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compelled to accept medical intervention against conscience; face employment termination, education exclusion, travel bans, and social ostracism for refusal. Exit requires emigration or rupture of ideological/religious identity fused with bodily autonomy conviction. No meaningful alternative to compliance within jurisdiction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced, payer,
    powerless, biographical, identity_locked, national).

% Designs and enforces mandate policy through emergency powers, administrative rulemaking, and legislative delegation. Claims authority from police power precedent (Jacobson v. Massachusetts) and public health necessity doctrine. Collects compliance metrics, legitimacy, and expanded regulatory scope as institutional capital.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives guaranteed market for vaccine products via government purchase commitments and mandate-driven demand. Liability shielded through PREP Act and similar frameworks. Lobbies for mandate expansion and booster regimens; captures regulatory agencies through revolving-door personnel and advisory committee influence.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, pharmaceutical_industry, beneficiary,
    powerful, biographical, mobile, global).

% Career advancement tied to mandate compliance metrics and outbreak suppression statistics. Institutional authority and budget grow with enforcement scope. Professional identity fused with mandate paradigm; dissent within ranks sanctioned. Exit constrained by credential specialization and institutional loyalty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_officials, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, public_health_officials, agenda_setter).

% Face elevated exposure risk from unvaccinated individuals in shared spaces. This reading treats their risk as accepted cost of liberty — not a claim on others' bodies. They are not consulted on policy; their vulnerability is invoked by mandate proponents but their agency to accept risk is denied by both mandate and anti-mandate framings.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_exposed, excluded,
    moderate, biographical, constrained, national).

% Analyze whether mandates violate constitutional bodily integrity, informed consent doctrine, and Nuremberg Code principles. Split between readings: originalist/textualist scholars tend toward bodily autonomy; living-constitution scholars defer to state police power. No enforcement power; influence operates through judicial appointment pipelines and amicus briefs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, legal_scholars_constitutional, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate arrangement claims to solve disease transmission coordination through universal vaccination, but from this reading it solves no genuine coordination problem — it imposes a single medical intervention on non-consenting individuals, suppressing the coordination that would emerge from voluntary risk assessment and diverse protection strategies.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making from individuals to state apparatus; moves vaccine uptake compliance from voluntary choice to coerced mandate; moves liability risk from manufacturers to public via injury compensation programs; moves epidemiological uncertainty onto non-consenting bodies.
% ABSENT_VOICES: Unvaccinated-coerced individuals (especially those with religious/philosophical objections) are structurally excluded from policy design; immunocompromised individuals who would accept risk for liberty are not represented; proportionality advocates are marginalized as 'anti-science'; natural immunity holders are erased from the immunity calculus.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, vaccination would revert to voluntary informed consent; disease dynamics would shift to natural immunity accumulation, voluntary protection, and targeted protection of vulnerable; state would lose a primary coercive lever over bodies; pharmaceutical guaranteed markets would collapse; public health bureaucracy would shrink to advisory role.
% FOUNDING_PROBLEM: Smallpox and polio eradication campaigns established precedent for compulsory vaccination as public health tool using sterilizing vaccines against lethal pathogens; COVID-19 pandemic reactivated this precedent for novel mRNA platforms against a respiratory virus with age-stratified severity and non-sterilizing vaccines.
% FOUNDING_PROBLEM_CORROBORATION: Jacobson v. Massachusetts (1905) established state police power for mandates — attested by legal precedent. Bodily autonomy advocates cite Nuremberg Code (1947), Helsinki Declaration (1964), and post-COVID injury surveillance data (VAERS, V-safe, Yellow Card) as corroboration that founding problem (lethal contagion requiring sterilizing vaccine) no longer justifies novel-platform mandates. No consensus exists outside state/public health beneficiary circle.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the mandate arrangement transfers bodily autonomy, medical decision rights, and liability risk from individuals to state/pharma with no reciprocal benefit to the coerced. Suppression (0.85) is very high because enforcement uses employment termination, education exclusion, travel bans, and professional license revocation — alternatives collapse once the mandate is understood. Theater ratio (0.30) is moderate: the 'public health' framing performs coordination while the mechanism is compulsion; the gap widens as vaccine efficacy against transmission wanes but mandates persist. Accessibility collapse (0.70) reflects that exit requires emigration or identity rupture — few practical alternatives. Resistance (0.65) is significant: mass non-compliance, litigation waves, political realignment, and alternative institution building (parallel medical/economic systems).
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (unvaccinated_coerced) experiences the constraint as Snare — pure extraction with no coordination benefit. The agenda_setter seat (state apparatus) experiences it as Rope — genuine coordination solving collective action problem. The beneficiary seats (pharma, officials) experience it as Scaffold — transitional coordination they wish to make permanent. The engine computes this divergence from the structural data; the authored claim (snare) reflects the payer seat's reality as structurally dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: unvaccinated_coerced are full targets (d→1.0) — they bear the compulsion, have identity_locked exit (ideological/religious fusion with refusal), and no arbitrage. State apparatus is full beneficiary (d→0.0) — it sets rules, collects compliance, faces no personal cost. Pharma is beneficiary (d→0.15) — captures guaranteed revenue, liability-shielded, mobile exit. Public health officials are beneficiaries (d→0.2) — institutional gain, constrained exit. Immunocompromised_exposed are excluded (not in directionality computation) — reading treats their risk as liberty's cost. Legal scholars are analytical (d=0.5) — symmetric observer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the mandate arrangement (Snare) from the bodily autonomy principle (Mountain). The mandate arrangement extracts via coercion; the principle would protect. Mandatrophy is unresolved: the Jacobson precedent (1905) was built for sterilizing vaccines against lethal pathogens; its application to non-sterilizing platforms against age-stratified risk is mandate drift. The founding problem (smallpox/polio eradication) is contested as live — state says yes, this reading says no. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags capture/zombie dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_natural_vs_constructed,
    'Is bodily autonomy a genuine natural law (Mountain) or a constructed right whose inviolability is contested?',
    'Cross-cultural/historical survey of medical consent norms; philosophical analysis of whether ''inviolable'' survives lifeboat/trolley counterexamples; legal test of whether any state has successfully suspended it without legitimacy collapse.',
    'If natural law, the mandate arrangement is categorically illegitimate (Snare) and FSM would trigger on any Mountain claim for the principle. If constructed, the mandate arrangement is a policy choice (Tangled Rope or Snare depending on coordination function) and the principle''s Mountain claim is a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_natural_vs_constructed, conceptual, 'Natural law status of bodily autonomy — determines whether principle or arrangement is the Mountain.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.85) primarily structural (legal penalties, economic exclusion) or partially internalized (coerced individuals believing they deserve punishment)?',
    'Post-mandate longitudinal study: if suppression sensations persist after legal penalties lifted, internalized component confirmed. Psychological measurement of moral injury vs. external coercion perception.',
    'If internalized, effective suppression is higher than structural measure — targets carry suppression after exit. Would increase χ for identity_locked seat beyond structural derivation. Affects piton vs snare classification if mandates lapse but compliance persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in coerced medical intervention.').

omega_variable(
    proportionality_threshold_location,
    'Where exactly does the proportionality_reading draw its severity/transmission/safety thresholds, and are they objectively measurable?',
    'Formal specification of proportionality criteria by proportionality advocates; empirical test against COVID-19 data (IFR by age, VE vs transmission, AE rates). If thresholds cannot be specified without arbitrary choices, proportionality_reading collapses to public_health_primary.',
    'If proportionality thresholds are incoherent, proportionality_reading forecloses to public_health_primary, leaving only binary kernel contest (bodily_autonomy vs public_health). If thresholds are coherent, three-way contest persists with distinct structural predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_location, conceptual, 'Whether proportionality_reading has determinate structural content or is a semantic midpoint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t1, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 1, 0.22).
narrative_ontology:measurement(vacc_tr_t2, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 2, 0.28).
narrative_ontology:measurement(vacc_tr_t3, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 3, 0.3).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 4, 0.3).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vacc_be_t1, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 1, 0.62).
narrative_ontology:measurement(vacc_be_t2, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 2, 0.73).
narrative_ontology:measurement(vacc_be_t3, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 3, 0.77).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 4, 0.78).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 5, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vacc_su_t1, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 1, 0.75).
narrative_ontology:measurement(vacc_su_t2, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 2, 0.82).
narrative_ontology:measurement(vacc_su_t3, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 4, 0.85).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 5, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% Kernel vaccine_mandate_balance decomposes into three readings with mutually exclusive ε values: bodily_autonomy_primary (ε≈0.78, Snare), proportionality_reading (ε≈0.45, Tangled Rope), public_health_primary (ε≈0.25, Rope). The ε-invariance principle requires separate stories because the mandate arrangement's extractiveness differs by reading — not by measurement basis. This story links to both siblings as structural dependents: the mandate regime's legitimacy conditions change when any reading gains authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, organized, 0.2).
constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
