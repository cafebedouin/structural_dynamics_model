% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Public Health Primary Reading of Vaccine Mandate Balance
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primary reading of the
 *   vaccine_mandate_balance kernel: the normative and legal principle that
 *   collective protection against lethal infectious disease supersedes
 *   individual medical consent when voluntary uptake is insufficient to
 *   protect vulnerable populations. In this reading, immunocompromised and
 *   other vulnerable groups are the beneficiaries of coercion applied to the
 *   unvaccinated; the unvaccinated are structurally coerced but framed as
 *   non-victims because their consent is legitimately subordinated to
 *   necessity. The constraint is claimed as justified coordination by public
 *   health authorities, but the authored metrics capture the high
 *   extractiveness of enforcement infrastructure and the active suppression
 *   of non-compliance.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda setter (institutional/arbitrage) â sets mandate terms, enforces, collects institutional authority
 *   - vulnerable_populations: primary beneficiary (powerless/trapped) â gain protection from herd immunity but lack exit from vulnerability
 *   - unvaccinated_individuals: payer (moderate/constrained) â bear coercion, penalties, and medical risk; structurally targeted by enforcement
 *   - civil_liberties_advocates: excluded (organized/constrained) â object to the precedent but are sidelined in emergency frameworks
 *   - constitutional_courts: observer (institutional/analytical) â adjudicate the boundary between police power and individual rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.76).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.65).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.76).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primary Reading of Vaccine Mandate Balance").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '2df8e4a0-a132-4a18-99bf-e53b75e99904').
narrative_ontology:cs_kernel_codification('2df8e4a0-a132-4a18-99bf-e53b75e99904', formalized).
narrative_ontology:cs_authority_grounding('2df8e4a0-a132-4a18-99bf-e53b75e99904', lineage).
narrative_ontology:cs_interpretation_layer_present('2df8e4a0-a132-4a18-99bf-e53b75e99904').
narrative_ontology:cs_reading_relation('2df8e4a0-a132-4a18-99bf-e53b75e99904', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('2df8e4a0-a132-4a18-99bf-e53b75e99904', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('2df8e4a0-a132-4a18-99bf-e53b75e99904', foundational, collective_protection_supersedes_consent).
narrative_ontology:cs_axiom_status(collective_protection_supersedes_consent, holdable).
narrative_ontology:cs_axiom_grounding('2df8e4a0-a132-4a18-99bf-e53b75e99904', collective_protection_supersedes_consent, conventional).
narrative_ontology:cs_axiom('2df8e4a0-a132-4a18-99bf-e53b75e99904', secondary, vulnerable_population_protection_as_state_duty).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_as_state_duty, holdable).
narrative_ontology:cs_axiom_grounding('2df8e4a0-a132-4a18-99bf-e53b75e99904', vulnerable_population_protection_as_state_duty, deontological).
narrative_ontology:cs_reference_frame('2df8e4a0-a132-4a18-99bf-e53b75e99904', police_power_public_health_authority).
narrative_ontology:cs_drift_state('2df8e4a0-a132-4a18-99bf-e53b75e99904', post_emergency_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2df8e4a0-a132-4a18-99bf-e53b75e99904', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, herd_immunity_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, police_power_public_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the mandate policy, enforces vaccination requirements through employment and public-space exclusions, and justifies the framework under constitutional police power. Collects institutional authority and expanded administrative scope from the mandate's operation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive reduced exposure risk due to higher community vaccination rates, but have no direct control over the mandate design and cannot exit their medically vulnerable status.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Face employment termination, fines, or exclusion from public accommodations if they refuse vaccination; their medical refusal is overridden by the collective-interest calculus. They bear the direct cost of compliance or penalty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Argue that bodily autonomy is inviolable and that the mandate creates a precedent for state medical coercion; they are structurally excluded from emergency public health decision-making tables.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Review challenges to the mandate, weighing state police power against substantive due process and liberty claims; their rulings determine whether the enforcement framework survives legal scrutiny.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves herd immunity threshold to protect vulnerable populations who cannot be vaccinated or mount an effective immune response, by solving the free-rider problem in voluntary vaccination uptake during lethal outbreaks.
% TRANSFER_FUNCTION: Moves compliance burden and medical risk acceptance from unvaccinated individuals to the protected pool of vulnerable persons; also transfers enforcement authority and expanded administrative scope to public health institutions.
% ABSENT_VOICES: Civil liberties advocates and bodily-autonomy absolutists are excluded from the decision frame; their objections are treated as superseded by necessity. Religious and medical exemption claimants are heard procedurally but their objections are structurally overruled by the collective-interest calculus.
% DISAPPEARANCE_RATIONALE: If the principle vanished, mandates would lose legal foundation, vaccination rates would fall to voluntary equilibrium, vulnerable populations would face renewed lethal exposure, and public health authorities would lose emergency police-power precedent.
% FOUNDING_PROBLEM: Infectious disease outbreaks where voluntary vaccination uptake is insufficient to prevent community transmission to vulnerable populations, creating preventable mortality and healthcare system collapse.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and immunologists outside the enforcement apparatus attest to the lethality risk for vulnerable populations; civil libertarians and some jurists contest that the problem as stated justifies superseding consent. The corroboration is split: scientific consensus supports the risk, but the mandate's necessity is contested by voices outside the benefiting parties.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.76, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.76 at interval end) because the constraint authorizes significant coercion (employment exclusion, fines, social participation limits) to extract compliance from a targeted population. Suppression is substantial (0.65 at end, peaked higher) because the constraint's operation depends on marginalizing bodily-autonomy framings and excluding non-compliant individuals from public and economic life. Theater ratio rises over the interval (0.40 at end) as emergency conditions wane but enforcement rituals persist â masking requirements, credential checks, and compliance tracking continue past the point of marginal epidemiological benefit, indicating drift toward performative maintenance. Accessibility collapse is moderate-high (0.68): once the mandate is in force, the alternative of uncoordinated personal choice collapses as a viable option for those inside the jurisdiction. Resistance is moderate (0.58): anti-mandate movements, litigation, and non-compliance generate persistent friction. The claimed type is rope (the reading's self-presentation as legitimate coordination) but the metrics describe a heavily enforced, extractive arrangement â the divergence is the signal the corpus is designed to capture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health authorities) experiences the constraint as necessary coordination solving a genuine free-rider problem; the payer seat (unvaccinated individuals) experiences it as state coercion overriding bodily integrity. The beneficiary seat (vulnerable populations) experiences protection but also dependency on a mechanism they do not control. The excluded seat (civil liberties advocates) sees a dangerous precedent. These divergences are structurally determined by power and exit: authorities have arbitrage-grade exit (can pivot policy), unvaccinated individuals are constrained by legal and economic penalties, and vulnerable populations are trapped by biology.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are vulnerable_populations (low directionality: subsidized by others' compliance) and public_health_authorities (low directionality: the constraint amplifies their institutional power and budget). The unvaccinated_individuals are payers (high directionality: they are the direct targets of enforcement and bear the compliance cost). Civil liberties advocates are excluded (no directionality; they are outside the constraint's operation). Courts are analytical (no directionality). The engine will compute high effective extraction for the unvaccinated and low or negative extraction for authorities and vulnerable groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the genuine coordination function (protecting vulnerable groups from infection) from the asymmetric extraction (coercing unvaccinated individuals). A pure snare reading would ignore the protection of vulnerable populations; a pure rope reading would ignore the coercion. The divergence between the rope claim and the extractive metrics captures this tension. The rising theater ratio over time flags potential piton drift: if the emergency ends but enforcement persists as ritual, the coordination function decays into inertial performance. The R5 genealogy (founding problem: insufficient voluntary uptake during lethal outbreak) is contested â some corroborators say the problem is dead, others say it persists â which feeds the mandatrophy detector without authoring a resolved boolean.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_subordination_legitimacy,
    'Does subordinating informed consent to collective necessity create an irreversible precedent for medical coercion, or is it safely cabined to genuine public health emergencies?',
    'Comparative analysis across jurisdictions: if the principle expands to non-lethal or non-communicable conditions, it was structurally un-cabined; if it contracts post-emergency, it was genuinely emergency-scoped.',
    'If un-cabined, effective suppression and extractiveness are higher than measured because the constraint''s scope creep is not yet visible; if cabined, the constraint may be a scaffold rather than a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_subordination_legitimacy, conceptual, 'Whether the constraint''s supersession of consent is emergency-limited or a general precedent.').

omega_variable(
    immunocompromised_beneficiary_validity,
    'Are immunocompromised populations genuine beneficiaries of the mandate, or are they instrumentalized as moral justification for state power expansion?',
    'Independent outcome measurement comparing vulnerable-population mortality in mandate versus non-mandate jurisdictions with similar baseline health infrastructure.',
    'If mortality outcomes are unchanged, the beneficiary claim is cover and the constraint is more extractive; if improved, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_beneficiary_validity, empirical, 'Whether vulnerable populations actually gain protection or serve as rhetorical cover.').

omega_variable(
    enforcement_cost_bearing,
    'Who structurally bears the cost of enforcement â the unvaccinated individuals subject to penalties, or the public purse funding the enforcement apparatus?',
    'Fiscal tracing of mandate enforcement costs (court cases, administrative tracking, exclusion infrastructure) versus individual penalties.',
    'If costs fall primarily on individuals, directionality for unvaccinated is higher; if on the public, extraction is diffuse and the constraint may be a piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_cost_bearing, empirical, 'Whether enforcement costs are borne by targeted individuals or diffuse taxpayers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__public_health_primary, theater_ratio, 8, 0.2).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__public_health_primary, theater_ratio, 16, 0.25).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__public_health_primary, theater_ratio, 24, 0.3).
narrative_ontology:measurement(vacc_tr_t32, vaccine_mandate_balance__public_health_primary, theater_ratio, 32, 0.35).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_balance__public_health_primary, theater_ratio, 40, 0.38).
narrative_ontology:measurement(vacc_tr_t48, vaccine_mandate_balance__public_health_primary, theater_ratio, 48, 0.4).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__public_health_primary, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__public_health_primary, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__public_health_primary, base_extractiveness, 24, 0.82).
narrative_ontology:measurement(vacc_be_t32, vaccine_mandate_balance__public_health_primary, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_balance__public_health_primary, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(vacc_be_t48, vaccine_mandate_balance__public_health_primary, base_extractiveness, 48, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__public_health_primary, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__public_health_primary, suppression_requirement, 16, 0.85).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__public_health_primary, suppression_requirement, 24, 0.88).
narrative_ontology:measurement(vacc_su_t32, vaccine_mandate_balance__public_health_primary, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_balance__public_health_primary, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(vacc_su_t48, vaccine_mandate_balance__public_health_primary, suppression_requirement, 48, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
