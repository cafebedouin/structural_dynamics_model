% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Compulsion for Medical Intervention under Public Health Primacy
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the 'public_health_primary' reading of the
 *   coercion_legitimacy_boundary kernel: the state may compel medical
 *   intervention when collective harm-prevention outweighs individual
 *   autonomy. The reading emerged from Jacobson v. Massachusetts (1905) and
 *   was stress-tested across the 20th century (smallpox, polio) before facing
 *   its sharpest contestation during COVID-19. The structural delta from
 *   sibling readings is precise: unvaccinated individuals enter the victim
 *   set as coerced subjects bearing mandates, fines, and exclusion;
 *   immunocompromised populations exit the victim set as protected
 *   beneficiaries of herd immunity; the enforcement apparatus (public health
 *   orders, occupational mandates, school-entry requirements, digital
 *   passports) generates high base extractiveness. The constraint is claimed
 *   as tangled_rope — it solves a genuine coordination problem (disease
 *   transmission externalities) while extracting asymmetrically from those
 *   who refuse intervention — and the metrics describe an arrangement whose
 *   extraction has grown over the interval while its coordination
 *   justification has narrowed.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda_setter (institutional/generational/arbitrage/global) — sets mandates, defines thresholds, operates enforcement
 *   - immunocompromised_populations: beneficiary (powerless/generational/trapped/local) — gains protection from herd immunity, cannot exit vulnerability
 *   - unvaccinated_individuals: payer (moderate/biographical/constrained/national) — bears mandate costs, fines, exclusion; exit via compliance or relocation
 *   - bodily_autonomy_advocates: payer (organized/biographical/mobile/national) — bears advocacy costs, litigation; exit via legal challenge
 *   - minority_communities_historically_medically_marginalized: payer (powerless/biographical/trapped/local) — bears disproportionate enforcement, historical distrust compounds coercion
 *   - healthcare_infrastructure_operators: beneficiary (institutional/generational/arbitrage/global) — gains predictable demand, liability protection, resource allocation priority
 *   - legislatures_courts: observer (institutional/generational/analytical/national) — adjudicates threshold, reviews proportionality, can rewrite the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.72).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.85).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Compulsion for Medical Intervention under Public Health Primacy").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '6c15b6ec-1469-411c-96db-786ca87bb6d5').
narrative_ontology:cs_kernel_codification('6c15b6ec-1469-411c-96db-786ca87bb6d5', formalized).
narrative_ontology:cs_authority_grounding('6c15b6ec-1469-411c-96db-786ca87bb6d5', extraction).
narrative_ontology:cs_interpretation_layer_present('6c15b6ec-1469-411c-96db-786ca87bb6d5').
narrative_ontology:cs_reading_relation('6c15b6ec-1469-411c-96db-786ca87bb6d5', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('6c15b6ec-1469-411c-96db-786ca87bb6d5', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('6c15b6ec-1469-411c-96db-786ca87bb6d5', foundational, collective_harm_prevention_trumps_bodily_integrity).
narrative_ontology:cs_axiom_status(collective_harm_prevention_trumps_bodily_integrity, holdable).
narrative_ontology:cs_axiom_grounding('6c15b6ec-1469-411c-96db-786ca87bb6d5', collective_harm_prevention_trumps_bodily_integrity, conventional).
narrative_ontology:cs_axiom('6c15b6ec-1469-411c-96db-786ca87bb6d5', foundational, state_police_power_extends_to_bodily_intrusion_for_health).
narrative_ontology:cs_axiom_status(state_police_power_extends_to_bodily_intrusion_for_health, holdable).
narrative_ontology:cs_axiom_grounding('6c15b6ec-1469-411c-96db-786ca87bb6d5', state_police_power_extends_to_bodily_intrusion_for_health, conventional).
narrative_ontology:cs_axiom('6c15b6ec-1469-411c-96db-786ca87bb6d5', secondary, herd_immunity_threshold_justifies_universal_mandate).
narrative_ontology:cs_axiom_status(herd_immunity_threshold_justifies_universal_mandate, holdable).
narrative_ontology:cs_axiom_grounding('6c15b6ec-1469-411c-96db-786ca87bb6d5', herd_immunity_threshold_justifies_universal_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('6c15b6ec-1469-411c-96db-786ca87bb6d5', jacobson_police_power_framework).
narrative_ontology:cs_drift_state('6c15b6ec-1469-411c-96db-786ca87bb6d5', post_covid_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6c15b6ec-1469-411c-96db-786ca87bb6d5', '2026-08-03T14:22:11Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, healthcare_infrastructure_operators).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_advocates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, minority_communities_historically_medically_marginalized).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, collective_harm_prevention_principle).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, state_police_power_for_health_emergencies).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, herd_immunity_threshold_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccination mandates, defines the 'outweighs' threshold, operates enforcement machinery (orders, fines, exclusion powers, digital verification). Collects compliance data and political legitimacy from successful disease control. Can shift jurisdiction, redefine threats, and control the narrative. Exit is arbitrage-grade: they administer the constraint and can reform it from inside.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Gains herd immunity protection when mandates achieve high coverage. Cannot exit vulnerability — medical condition is not chosen. Has no leverage over mandate design or enforcement. Depends entirely on others' compliance for protection. The constraint's coordination function is existentially real for this seat.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, generational, trapped, local).

% Bears the bodily intrusion of mandated vaccination, financial penalties for non-compliance, exclusion from schools/workplaces/public venues, and social stigma. Exit options: comply (bodily cost), pay penalties (financial cost), relocate (high friction), or seek exemptions (narrow, discretionary). The constraint extracts directly from this seat.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Bears litigation costs, advocacy expenditure, and political capital defending exemption claims and challenging mandates. Can shift legal strategy, jurisdiction, or public messaging — exit is mobile at the organizational level. Does not personally bear the bodily intrusion but pays the contestation cost.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_advocates, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_advocates, observer).

% Bears disproportionate enforcement: stricter mandate application in segregated settings, fewer granted exemptions, harsher penalties for non-compliance, and compounding historical distrust of medical institutions. Geographic and socioeconomic exit is often unavailable. The constraint's extraction lands heaviest here — not by epidemiological necessity but by structural positioning.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, minority_communities_historically_medically_marginalized, payer,
    powerless, biographical, trapped, local).

% Gains predictable patient flows, liability protection for mandate-compliant care, priority resource allocation during surges, and regulatory certainty. Can influence mandate design through lobbying and advisory roles. Exit is arbitrage-grade: they benefit from the constraint and help shape it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, healthcare_infrastructure_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Adjudicates the 'outweighs' threshold, reviews proportionality, hears exemption claims, and can rewrite or strike down the constraint. Does not directly bear bodily costs or collect compliance revenue. Their decisions reshape the constraint's operating envelope for all other seats.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, legislatures_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the disease transmission externality problem: individual vaccination decisions affect population-level herd immunity; mandates align private choice with collective protection threshold, preventing outbreaks that overwhelm healthcare infrastructure.
% TRANSFER_FUNCTION: Moves bodily autonomy, financial resources (fines, compliance costs), and social access (school, work, public space) from unvaccinated individuals to the collective protection pool administered by public health authorities. Immunocompromised populations receive the protection benefit; healthcare operators receive operational certainty.
% ABSENT_VOICES: Future generations who inherit the precedent of state bodily compulsion; children of unvaccinated parents who bear exclusion without agency; undocumented immigrants who face mandate enforcement without political representation; global south populations whose vaccine access determines whether mandates are globally coherent or nationally extractive.
% DISAPPEARANCE_RATIONALE: If the compulsion framework vanished overnight, vaccination rates would drop below herd immunity thresholds for multiple diseases within months, outbreaks would recur, healthcare systems would face surge crises, and immunocompromised populations would lose their primary protection. The state would need to rebuild voluntary uptake infrastructure or accept endemic disease. The world rearranges.
% FOUNDING_PROBLEM: Early 20th century smallpox epidemics in dense urban populations where voluntary vaccination failed to achieve herd immunity, and the state lacked tools to compel compliance — Jacobson v. Massachusetts established the police power to mandate.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the problem is live (ongoing outbreaks, vaccine hesitancy, novel pathogens). Bodily autonomy advocates and proportionality_reading adherents attest the founding problem is substantially solved for many diseases (safe effective vaccines exist, voluntary uptake is high where trust exists) and the arrangement persists as institutional self-preservation. Independent epidemiological historians and legal scholars outside the benefiting parties document the drift from epidemic emergency to routine governance.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the enforcement apparatus compels bodily intrusion and imposes material penalties on refusers — the transfer from unvaccinated individuals to the collective protection pool is substantial and non-voluntary. Suppression (0.85) is very high because alternatives (exemptions, alternative protections, opt-out with testing) are structurally narrow and politically contested; the constraint's persistence depends on actively suppressing exit routes. Theater ratio (0.28) is moderate-low: the public health coordination function is real (disease control works), but a growing share of enforcement activity serves institutional self-preservation and political signaling rather than marginal epidemiological benefit. Accessibility collapse (0.78) is high because once the mandate regime is understood, the practical alternatives for non-compliers shrink to compliance, penalty, or exit from public life. Resistance (0.62) is substantial — legal challenges, political mobilization, and non-compliance are persistent — confirming this is not a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (public health authorities) experiences this as genuine coordination: they see disease curves bend, hospitals decompress, and the mandate apparatus working as designed. The payer seats (unvaccinated individuals, minority communities) experience the same structure as enforced extraction: the bodily intrusion is non-negotiable, the penalties are material, and the justification feels post-hoc when threat levels fluctuate but mandates persist. The engine computes this divergence from the structural data — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the structural agenda_setters (d ~ 0.15): they write the rules, collect the compliance, and bear minimal personal cost. Immunocompromised populations are pure beneficiaries (d ~ 0.10): they gain protection without enforcing or paying. Healthcare infrastructure operators are institutional beneficiaries (d ~ 0.20): they gain operational certainty and liability shields. Unvaccinated individuals are primary payers (d ~ 0.85): they bear the bodily intrusion, financial penalties, and social exclusion; their exit is constrained (compliance or costly relocation). Bodily autonomy advocates are organized payers (d ~ 0.70): they bear litigation and advocacy costs; exit is mobile (can shift jurisdiction or strategy). Minority communities are trapped payers (d ~ 0.90): historical medical marginalization compounds coercion — distrust is not a choice, and geographic/socioeconomic exit is often unavailable. Courts/legislatures are analytical observers (d = 0.50): they adjudicate but do not directly bear or collect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epidemic disease control via herd immunity) remains live but has narrowed: for many diseases, vaccines now exist that make mandates less necessary; for others, the threat has receded. Yet the compulsion framework persists and has expanded to lower-severity diseases. This is not pure mandatrophy — the coordination function is real for high-threat pathogens — but the extraction apparatus has grown beyond what the current threat profile justifies. The constraint is a Tangled Rope where the rope is fraying: coordination benefit concentrates on high-severity/high-transmission diseases, while extraction applies broadly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the ''public_health_primary'' reading of the coercion_legitimacy_boundary kernel. How does the sibling ''bodily_autonomy_primary'' reading (categorical impermissibility of non-consensual intervention) structurally relate — does it coexist, influence, or foreclose within a single commitment framework?',
    'Map the logical structure of both readings: if a legal framework adopts public_health_primary as its operating principle, does it logically entail rejection of bodily_autonomy_primary''s core axiom, or can both operate as live positions across different institutional seats? Court doctrine and legislative history would resolve.',
    'If forecloses: this reading''s gain_flow captures the kernel''s legitimacy rent entirely; if coexists_with: the kernel is a contested field with multiple live readings, and extraction is partitioned across institutional factions; if influences: this reading''s dominance shifts the operating environment for bodily_autonomy_primary without eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural relationship of this kernel reading to its siblings').

omega_variable(
    proportionality_threshold_ambiguity,
    'Where does the ''outweighs'' threshold operate — is it a fixed epidemiological trigger (R0, IFR, hospitalization rate) or a political judgment delegated to health authorities? The extraction level depends on whether the threshold is structurally determinate or manipulable.',
    'Analyze statutory language, regulatory guidance, and judicial review standards across jurisdictions. Code whether ''collective harm-prevention outweighs individual autonomy'' has operational criteria or is an open-ended delegation.',
    'If threshold is fixed/epidemiological: extraction is bounded by objective conditions. If threshold is political judgment: the enforcement apparatus can expand extraction by redefining ''outweighs'' — a classic Snare/Tangled Rope drift signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, empirical, 'Whether the compulsion trigger is structurally determinate or delegated judgment').

omega_variable(
    enforcement_apparatus_elasticity,
    'Does the enforcement apparatus (mandates, fines, exclusion from public life, physical compulsion) scale with the epidemiological threat, or does it ratchet upward and persist after the threat recedes?',
    'Longitudinal study of mandate scope, penalty severity, and duration relative to disease metrics across multiple pandemic cycles. Compare post-emergency dismantling rates.',
    'If elastic (scales down with threat): Tangled Rope coordination function is genuine. If ratcheting (persists after threat): extraction has decoupled from coordination — drift toward Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_elasticity, empirical, 'Whether enforcement scales with threat or ratchets independent of it').

omega_variable(
    minority_community_extraction_disparity,
    'Do historically medically marginalized communities bear disproportionate extraction (stricter enforcement, fewer exemptions, harsher penalties) under this reading, and is that disparity structural or incidental?',
    'Disaggregate enforcement data by race, class, immigration status, and institutional trust metrics. Test whether disparity persists after controlling for exposure risk and compliance behavior.',
    'If structural: the constraint operates as a stratified extraction mechanism — a Tangled Rope with an embedded Snare layer. If incidental: disparity reflects downstream implementation, not the reading''s core structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_community_extraction_disparity, empirical, 'Whether marginalized communities face structurally disproportionate extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clbp_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(clbp_tr_t0, observed).
narrative_ontology:measurement(clbp_tr_t5, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(clbp_tr_t5, observed).
narrative_ontology:measurement(clbp_tr_t10, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(clbp_tr_t10, observed).
narrative_ontology:measurement(clbp_tr_t15, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(clbp_tr_t15, observed).
narrative_ontology:measurement(clbp_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(clbp_tr_t20, observed).
narrative_ontology:measurement(clbp_tr_t25, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(clbp_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(clbp_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(clbp_be_t0, observed).
narrative_ontology:measurement(clbp_be_t5, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(clbp_be_t5, observed).
narrative_ontology:measurement(clbp_be_t10, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(clbp_be_t10, observed).
narrative_ontology:measurement(clbp_be_t15, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clbp_be_t15, observed).
narrative_ontology:measurement(clbp_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(clbp_be_t20, observed).
narrative_ontology:measurement(clbp_be_t25, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(clbp_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(clbp_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clbp_su_t0, observed).
narrative_ontology:measurement(clbp_su_t5, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(clbp_su_t5, observed).
narrative_ontology:measurement(clbp_su_t10, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(clbp_su_t10, observed).
narrative_ontology:measurement(clbp_su_t15, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(clbp_su_t15, observed).
narrative_ontology:measurement(clbp_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.82).
narrative_ontology:measurement_basis(clbp_su_t20, observed).
narrative_ontology:measurement(clbp_su_t25, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 25, 0.85).
narrative_ontology:measurement_basis(clbp_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__public_health_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, vaccine_mandate_enforcement_infrastructure).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, public_health_emergency_powers_architecture).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, school_entry_vaccination_requirements).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, occupational_health_mandates).

% DUAL FORMULATION NOTE:
% This constraint family (coercion_legitimacy_boundary) decomposes the kernel into three readings with distinct ε values and beneficiary/victim structures. public_health_primary has high ε (0.72) because the enforcement apparatus is the referent; bodily_autonomy_primary would have ε ≈ 0 for the arrangement it endorses (no compulsion) but high ε for the standing arrangement it opposes; proportionality_reading would have intermediate ε scaling with disease severity. Each reading is a separate constraint story linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, institutional, 0.15).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, powerless, 0.9).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, moderate, 0.85).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
