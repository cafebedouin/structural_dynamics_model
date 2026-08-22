% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: Harm Reduction Reading of State Drug Control Authority
 *   domain: public_health/political_economy/criminal_justice
 *
 * SUMMARY:
 *   This constraint story captures the harm reduction reading of state drug
 *   control authority: the state accepts that drug use occurs and deploys
 *   public health interventions to minimize harms, while retaining criminal
 *   prohibition on supply and declining full legal regulation of drug
 *   markets. It is one reading of the contested kernel
 *   'substance_control_authority' — the sibling readings are
 *   prohibition_reading (criminalize use/possession) and legalization_reading
 *   (regulate as legal commerce). This reading emerged in the 1980s-90s as a
 *   pragmatic response to HIV epidemics among people who inject drugs, was
 *   codified in various national strategies (e.g., Swiss four-pillar model,
 *   Portuguese decriminalization 2001, North American supervised consumption
 *   sites), and now operates as the dominant public health paradigm in many
 *   jurisdictions while remaining politically contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.38).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "Harm Reduction Reading of State Drug Control Authority").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health/political_economy/criminal_justice").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '19225549-d035-49dc-9ca8-8c3a46b36f19').
narrative_ontology:cs_kernel_codification('19225549-d035-49dc-9ca8-8c3a46b36f19', formalized).
narrative_ontology:cs_authority_grounding('19225549-d035-49dc-9ca8-8c3a46b36f19', practice).
narrative_ontology:cs_interpretation_layer_present('19225549-d035-49dc-9ca8-8c3a46b36f19').
narrative_ontology:cs_reading_relation('19225549-d035-49dc-9ca8-8c3a46b36f19', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('19225549-d035-49dc-9ca8-8c3a46b36f19', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('19225549-d035-49dc-9ca8-8c3a46b36f19', foundational, health_harm_reduction_primary_over_criminal_sanction).
narrative_ontology:cs_axiom_status(health_harm_reduction_primary_over_criminal_sanction, holdable).
narrative_ontology:cs_axiom_grounding('19225549-d035-49dc-9ca8-8c3a46b36f19', health_harm_reduction_primary_over_criminal_sanction, empirically_contingent).
narrative_ontology:cs_axiom('19225549-d035-49dc-9ca8-8c3a46b36f19', foundational, decriminalization_without_commercial_legalization).
narrative_ontology:cs_axiom_status(decriminalization_without_commercial_legalization, holdable).
narrative_ontology:cs_axiom_grounding('19225549-d035-49dc-9ca8-8c3a46b36f19', decriminalization_without_commercial_legalization, conventional).
narrative_ontology:cs_reference_frame('19225549-d035-49dc-9ca8-8c3a46b36f19', pragmatic_health_first_authority).
narrative_ontology:cs_drift_state('19225549-d035-49dc-9ca8-8c3a46b36f19', contemporary_polysubstance_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19225549-d035-49dc-9ca8-8c3a46b36f19', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs_decriminalized).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, third_party_communities_affected).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs_health_harms).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, emergency_services_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, emergency_services_personnel).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs_decriminalized).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, pragmatic_health_first_drug_policy).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, decriminalization_reduces_overdose_mortality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and fund harm reduction programs (needle exchange, supervised consumption, opioid agonist therapy). Gain institutional legitimacy, budget authority, and data infrastructure from the constraint. Can pivot resources to other public health priorities if political winds shift, but lose the specific mandate and funding stream.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, public_health_agencies, beneficiary).

% Operate syringe services, overdose prevention sites, low-threshold treatment. Receive state contracts and legal protection under the harm reduction framework. Dependent on continued political tolerance; vulnerable to funding cuts or zoning restrictions. Professional identity fused to the model — exit means leaving the field.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, constrained, regional).

% No longer face criminal penalties for possession; access sterile equipment and overdose reversal. Still bear health harms of drug use (infection, overdose risk, chronic conditions). Stigma persists in healthcare, housing, employment. Cannot 'exit' drug dependence easily; the constraint shapes their survival conditions but does not resolve the underlying dependence.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs_decriminalized, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs_decriminalized, payer).

% Experience discarded syringes in public spaces, property crime driven by untreated dependence, visible public intoxication, overdose deaths in neighborhoods. No meaningful political voice in policy design; bear externalities of both drug use and the service infrastructure. Cannot relocate easily; the constraint's benefits accrue elsewhere.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, third_party_communities_affected, payer,
    powerless, immediate, trapped, local).

% Subset of users for whom harm reduction services are insufficient — continuing injection-related infections, overdose events, untreated mental health comorbidities. The constraint reduces but does not eliminate their victimization; they remain in the health-harm victim set while exiting the criminal one. Exit from this position requires recovery pathways the constraint does not guarantee.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs_health_harms, payer,
    powerless, biographical, identity_locked, national).

% Respond to overdose calls, manage acute behavioral crises, transport to EDs. Burden reduced compared to prohibition era (fewer arrests, more naloxone in community) but still high — repeat calls, burnout, moral injury. Gain clearer protocols and community partnerships under harm reduction, but the operational load remains a structural cost.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, emergency_services_personnel, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, emergency_services_personnel, beneficiary).

% Shift from arrest-first to referral-first posture under decriminalization directives. Lose a traditional enforcement tool (possession charges) and associated clearance metrics; gain mandated diversion pathways. Institutional identity contested — some units resist, others adapt. Exit means reassignment or retirement; the constraint reshapes daily practice.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement, payer).

% Evaluate outcomes: overdose mortality, treatment engagement, crime trends, service utilization. Produce evidence that feeds back into policy contests. No direct material stake; career incentives align with measurable reductions in harms. See the full structure — coordination gains, extraction residues, and the unmoved victims.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, policy_analysts_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public health response to drug use that replaces criminal sanctions with service access: sterile equipment, overdose prevention, low-threshold treatment, and decriminalized possession reduce population-level mortality and disease transmission while maintaining state authority over the drug problem.
% TRANSFER_FUNCTION: Moves enforcement resources (police time, court dockets, incarceration costs) toward health services (needle exchange, OAT, supervised consumption, naloxone distribution). Transfers criminal victimization off people who use drugs (possession arrests, records) onto third parties who bear localized externalities (discarded equipment, public disorder, property crime) and onto users who continue to absorb health harms. The state retains regulatory authority without full market legalization.
% ABSENT_VOICES: Neighborhood associations in high-impact zones, families of people who use drugs, recovery community organizations advocating abstinence-based pathways, and people who use drugs who do not engage with services (the 'hidden population') are structurally excluded from policy design tables. Their objections — to visible disorder, to inadequate recovery options, to services that don't reach them — are mediated through complaint systems rather than governance.
% DISAPPEARANCE_RATIONALE: If the harm reduction framework vanished overnight, decriminalization would likely revert to de facto prohibition (arrest-driven), supervised consumption sites would close, syringe access would contract, and overdose mortality would spike — as observed in jurisdictions that defunded or banned these services. The world rearranges: people who use drugs re-enter the criminal victim set; public health agencies lose mandate and funding; third-party externalities shift from service-adjacent to enforcement-adjacent.
% FOUNDING_PROBLEM: Prohibition-era drug control produced mass incarceration, HIV/HCV epidemics among people who inject drugs, overdose mortality crises, and erosion of trust between marginalized communities and state institutions — without reducing drug availability or use prevalence.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and harm reduction advocates attest the founding problem remains live (overdose crisis persists, new synthetic drugs emerge). Law enforcement and some elected officials attest the founding problem is substantially solved (HIV transmission dropped, incarceration for possession declined) and the arrangement now serves institutional maintenance. Independent researchers (e.g., NASEM 2023, Lancet Commission 2022) corroborate that the original epidemic drivers are reduced but new harms (fentanyl mortality, xylazine wounds, stimulant psychosis) constitute a shifted problem the constraint was not built for.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).
:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the constraint transfers resources from criminal enforcement to health services while leaving health harms on users and externalities on third parties — neither pure coordination nor pure extraction. Suppression (0.38) is moderate: the constraint requires active enforcement against supply-side actors and maintains zoning/regulatory control over service sites, but does not suppress the target population (users) — it includes them. Theater ratio (0.25) captures the gap between 'health-first' rhetoric and the reality that service coverage is uneven, recovery pathways are underfunded, and the constraint stabilizes a managed crisis rather than resolving dependence. Accessibility collapse (0.55) reflects that alternatives (full legalization, abolition of drug laws, compulsory treatment) are politically collapsed but conceptually available. Resistance (0.52) captures ongoing political opposition from prohibitionist factions and community backlash against service siting.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (third-party communities, users with health harms) experience the constraint as ongoing extraction with limited recourse — their effective χ is high due to trapped/identity-locked exit. The agenda-setter/beneficiary seats (public health agencies, service providers) experience it as genuine coordination — their effective χ is low or negative (subsidy). Law enforcement sits in genuine contention: some units experience it as extraction (lost tools, mandate creep), others as coordination (clearer protocols, reduced futile arrests). The engine computes this divergence from the structural data; the claimed type (tangled_rope) asserts the hybrid is structural, not perceptual.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and harm reduction providers are structural beneficiaries (d near 0.0-0.2) — they gain mandate, funding, and professional legitimacy. People who use drugs (decriminalized) are partial beneficiaries with identity-locked exit (d ~0.35) — they gain legal protection and service access but remain in the health-harm victim set. Third-party communities are trapped payers (d ~0.9) — they bear externalities with no voice or exit. Users with ongoing health harms are identity-locked payers (d ~0.8) — the constraint reduces but does not eliminate their victimization. Emergency services and law enforcement are dual-positioned: agenda-setters who also bear operational costs (d ~0.45-0.55). Analytical observers sit at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (HIV/overdose/incarceration crisis) is contested: partially solved (HIV transmission, possession arrests down) but transformed (fentanyl mortality, stimulant harms, xylazine wounds). The constraint persists because it coordinates a real public health response AND extracts political legitimacy for the state ('we are doing something') while offloading externalities onto powerless third parties. If the founding problem were declared dead, the mandate would face legitimization pressure — but 'contested' status allows both continuation and expansion. The theater ratio rise (0.12→0.25) suggests growing performative maintenance: new service modalities (drug checking, safer supply) are piloted at the margins while core coverage gaps persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the harm reduction reading a stable, distinct constraint from its siblings, or a transitional compromise that collapses into prohibition or legalization under political pressure?',
    'Longitudinal policy trajectory analysis: if jurisdictions adopting harm reduction reliably progress to legalization (or revert to prohibition), the reading is transitional; if it persists as a stable equilibrium across political cycles, it is a distinct constraint.',
    'If transitional, the constraint''s ε and type are time-dependent — the engine would need to model reading drift. If stable, the current classification holds as a structural fact about this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading is a stable constraint or a transient policy compromise.').

omega_variable(
    service_coverage_gap_as_extraction,
    'Does the gap between harm reduction service coverage and the population needing it constitute structural extraction (the constraint legitimizes itself on coverage it does not deliver), or is partial coverage an inherent feature of any public health intervention?',
    'Compare service utilization rates to estimated need across jurisdictions with similar frameworks; assess whether coverage gaps correlate with political resistance vs. resource constraints.',
    'If gaps are politically produced (zoning bans, funding caps), the constraint extracts legitimacy from a coverage promise it does not fulfill — raising effective ε. If gaps are resource-bound, they are coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_coverage_gap_as_extraction, empirical, 'Whether unmet need under harm reduction is structural extraction or coordination cost.').

omega_variable(
    third_party_externality_internalization,
    'Can the externalities borne by third-party communities (discarded equipment, public disorder, property crime) be internalized into the constraint''s coordination function (e.g., via neighborhood-level service integration, community advisory boards, restitution mechanisms), or are they structurally necessary for the constraint''s political viability?',
    'Case study of jurisdictions with strong community governance of harm reduction services (e.g., Vancouver''s Downtown Eastside models, Lisbon''s dissuasion commissions) vs. top-down implementations.',
    'If internalizable, the constraint could reduce its victim set without losing coordination function — moving toward rope. If structurally necessary, the third-party victim set is a fixed feature of this reading''s ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_externality_internalization, conceptual, 'Whether third-party victimization is a removable bug or structural feature of harm reduction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.38) primarily structural (zoning restrictions on service sites, supply-side enforcement, regulatory barriers to prescribing) or internalized (stigma preventing service engagement, self-exclusion from care, normalized risk acceptance among users)?',
    'Post-policy-change observation: if suppression persists after structural barriers are removed (e.g., after zoning reform or prescribing liberalization), the residual is internalized. Qualitative studies of non-engaging users.',
    'If substantially internalized, effective suppression is higher than the structural measure — the constraint''s reach extends beyond state action into the subjectivity of the target population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in harm reduction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_control_hr_tr_t2000, substance_control_authority__harm_reduction_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(substance_control_hr_tr_t2005, substance_control_authority__harm_reduction_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(substance_control_hr_tr_t2010, substance_control_authority__harm_reduction_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(substance_control_hr_tr_t2015, substance_control_authority__harm_reduction_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(substance_control_hr_tr_t2020, substance_control_authority__harm_reduction_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(substance_control_hr_tr_t2025, substance_control_authority__harm_reduction_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(substance_control_hr_be_t2000, substance_control_authority__harm_reduction_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(substance_control_hr_be_t2005, substance_control_authority__harm_reduction_reading, base_extractiveness, 2005, 0.31).
narrative_ontology:measurement(substance_control_hr_be_t2010, substance_control_authority__harm_reduction_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(substance_control_hr_be_t2015, substance_control_authority__harm_reduction_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(substance_control_hr_be_t2020, substance_control_authority__harm_reduction_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(substance_control_hr_be_t2025, substance_control_authority__harm_reduction_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(substance_control_hr_su_t2000, substance_control_authority__harm_reduction_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(substance_control_hr_su_t2005, substance_control_authority__harm_reduction_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(substance_control_hr_su_t2010, substance_control_authority__harm_reduction_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(substance_control_hr_su_t2015, substance_control_authority__harm_reduction_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(substance_control_hr_su_t2020, substance_control_authority__harm_reduction_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(substance_control_hr_su_t2025, substance_control_authority__harm_reduction_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__harm_reduction_reading, 0.15).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, public_health_emergency_authority).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, criminal_justice_resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, housing_first_policy).

% DUAL FORMULATION NOTE:
% This constraint is one member of the substance_control_authority constraint family. The prohibition_reading (ε ≈ 0.75, snare) extracts via criminal enforcement; the legalization_reading (ε ≈ 0.30, rope) coordinates via regulated markets. This harm_reduction_reading (ε ≈ 0.42, tangled_rope) sits between: it removes the criminal victim set but retains health-harm victim set and third-party externalities, using services as the coordination mechanism. The three readings share the kernel's authority structure but instantiate different ε, different victim/beneficiary sets, and different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, organized, 0.45).
constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
