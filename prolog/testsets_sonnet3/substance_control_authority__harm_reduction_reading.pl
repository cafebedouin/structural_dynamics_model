% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Authority to Accept Drug Use While Minimizing Harm (Harm Reduction Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   A state or municipal government adopts a public-health posture toward
 *   drug use: rather than treating possession and use as the primary target
 *   of criminal law, it funds needle exchanges, naloxone distribution,
 *   drug-checking services, and supervised consumption sites, and directs
 *   low-level enforcement discretion toward diversion rather than
 *   prosecution. Supply-side criminal law (distribution, trafficking) remains
 *   largely intact. The arrangement reduces mortality and transmission
 *   relative to strict prohibition but leaves the underlying health harms of
 *   continued use unaddressed, leaves funding uneven across jurisdictions,
 *   and leaves host neighborhoods bearing concentrated site effects without a
 *   matched increase in local say. Law enforcement retains discretion over
 *   the boundary between decriminalized and prosecutable conduct, which
 *   structurally preserves its institutional footprint even as it appears to
 *   recede.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda_setter (institutional/analytical) — designs and funds the service architecture, sets decriminalization thresholds
 *   - people_who_use_drugs: beneficiary and payer (powerless/trapped) — gains reduced criminal exposure and access to services, still bears health harm and residual criminal risk
 *   - harm_reduction_service_providers: agenda_setter and beneficiary (organized/constrained) — operational implementers whose institutional survival depends on the framing
 *   - law_enforcement_agencies: agenda_setter and beneficiary (institutional/mobile) — retains discretion over the prosecutable boundary, preserving institutional footprint
 *   - neighborhoods_hosting_services: payer (moderate/constrained) — bears concentrated site effects
 *   - drug_market_organizers: excluded (organized/arbitrage) — outside the conversation, drives contamination-related mortality the services respond to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.38).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Authority to Accept Drug Use While Minimizing Harm (Harm Reduction Reading)").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55').
narrative_ontology:cs_kernel_codification('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', distributed).
narrative_ontology:cs_authority_grounding('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', practice).
narrative_ontology:cs_interpretation_layer_present('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55').
narrative_ontology:cs_reading_relation('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', substance_control_authority__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', foundational, continued_use_is_a_fact_to_manage_not_a_crime_to_suppress).
narrative_ontology:cs_axiom_status(continued_use_is_a_fact_to_manage_not_a_crime_to_suppress, holdable).
narrative_ontology:cs_axiom_grounding('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', continued_use_is_a_fact_to_manage_not_a_crime_to_suppress, empirically_contingent).
narrative_ontology:cs_axiom('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', secondary, decriminalization_of_use_without_regulation_of_supply_is_a_stable_equilibrium).
narrative_ontology:cs_axiom_status(decriminalization_of_use_without_regulation_of_supply_is_a_stable_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', decriminalization_of_use_without_regulation_of_supply_is_a_stable_equilibrium, instrumental).
narrative_ontology:cs_reference_frame('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', public_health_reframing_of_addiction_post_1980s).
narrative_ontology:cs_drift_state('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', contemporary_opioid_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a19d5fb-3fae-4406-a5d3-dc0cfa05ff55', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, adjacent_communities).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, uninsured_chronic_users).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, neighborhoods_hosting_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, harm_reduction_efficacy_doctrine).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, public_health_framing_of_addiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers needle exchanges, supervised consumption sites, naloxone distribution, and diversion-to-treatment programs. Sets eligibility rules, funding levels, and the boundary between decriminalized possession and prosecutable distribution. Retains the authority to criminalize supply-side conduct even while accepting use-side conduct.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Gain access to sterile equipment, overdose reversal, testing strips, and low-barrier services without automatic arrest for possession. Still bear the underlying health harms of use — overdose risk, infection, organ damage — and remain subject to arrest for possession above threshold amounts, for use in public, or for any supply-adjacent conduct. Exit from drug dependency itself is not offered by this arrangement; exit from criminal liability is only partial.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer).

% Operate exchange sites, mobile outreach, and consumption rooms under grants and state authorization. Their institutional survival depends on the harm-reduction framing remaining politically viable; a swing back toward strict prohibition or a leap to full legalization both threaten their funding model and mandate.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers, beneficiary).

% Retain and exercise discretion over which possession, distribution, and public-use conduct gets prosecuted, using the health framing to redirect low-level cases into services while preserving the underlying criminal statute for cases they choose to pursue. Their institutional footprint and budget are preserved because criminal authority is narrowed, not abolished.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, beneficiary).

% Live near consumption sites and exchanges and bear concentrated visible drug activity, discarded equipment, and localized disorder that the citywide harm-reduction framing does not resource them to manage. Cannot easily relocate service siting decisions made at a higher level of government.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, neighborhoods_hosting_services, payer,
    moderate, biographical, constrained, local).

% Benefit from reduced disease transmission (HIV, hepatitis C) and reduced overdose mortality at a population level, and from lower policing costs redirected from mass low-level arrests. Do not directly operate or fund the services and have limited say in siting or program design.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, adjacent_communities, beneficiary,
    moderate, generational, constrained, regional).

% Fall outside insured treatment pathways and depend entirely on underfunded, geographically sparse harm-reduction infrastructure; where services are absent or oversubscribed, they absorb the full health harm with none of the promised mitigation, while still facing residual criminal exposure for possession quantities or conduct outside decriminalized categories.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, uninsured_chronic_users, payer,
    powerless, biographical, trapped, local).

% Argue the harm-reduction framing normalizes continued use rather than pursuing recovery, and compete for the same public funding streams. Their objection is rarely incorporated into program design, which is dominated by public-health rather than recovery-oriented professional norms.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, abstinence_only_treatment_providers, excluded,
    organized, biographical, constrained, regional).

% Continue operating an illegal supply chain that harm reduction does not disrupt and arguably stabilizes by keeping demand alive without regulating supply quality; they are structurally outside the policy conversation but are the actors whose product quality (contamination, fentanyl adulteration) drives much of the mortality the services exist to mitigate.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, drug_market_organizers, excluded,
    organized, biographical, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shift of low-level drug-use conduct out of the criminal-legal system and into public-health infrastructure — needle exchange, naloxone, testing, and voluntary treatment referral — reducing overdose deaths and disease transmission without requiring supply-side legalization.
% TRANSFER_FUNCTION: Moves resources from general tax revenue and law-enforcement discretion toward health-service infrastructure, and moves a portion of criminal-legal risk off individual users onto the state's administrative capacity to run and fund services; unfunded or unresourced portions of that risk remain with users and host neighborhoods.
% ABSENT_VOICES: Drug market organizers, whose product quality drives much of the mortality the services respond to, are not party to the arrangement at all. Abstinence-oriented treatment providers who dispute the framing compete for funding but rarely shape program design. Host neighborhoods bear concentrated site effects with limited input into siting decisions made at city or state level.
% DISAPPEARANCE_RATIONALE: If this authority reverted to strict prohibition or leapt to full legalization overnight, service funding streams, professional harm-reduction workforce, prosecutorial diversion pipelines, and the partial decriminalization thresholds people currently rely on to avoid arrest would all be reorganized — either toward criminal-legal reassertion or toward regulated commerce, each with a different victim set.
% FOUNDING_PROBLEM: Rising overdose deaths and HIV/hepatitis transmission among drug users under strict prohibition regimes, where fear of arrest deterred people from seeking sterile equipment, testing, or emergency medical help, worsened rather than reduced the harms prohibition was meant to control.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological surveillance (CDC/state health department overdose and infectious-disease surveillance data) and academic public-health research outside the harm-reduction provider community corroborate continuing overdose mortality and transmission rates; this is not solely attested by the agencies and providers who administer or benefit from the programs.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) and rising slowly: the arrangement is genuinely coordinative — it reduces overdose deaths and disease transmission relative to strict prohibition — but it also extracts in a specific sense: it asks users to accept continued unaddressed health harm and unresourced neighborhoods to accept concentrated site effects, in exchange for partial (not full) exit from criminal liability, while law enforcement and service-provider institutions preserve their funding and mandate. Suppression is moderate and declining over the interval (0.55 to 0.38) — reflecting a real, if incomplete, retreat of coercive enforcement against use-side conduct as diversion programs mature; it does not reach zero because possession thresholds, public-use restrictions, and supply-side law remain actively enforced. Theater ratio is low-to-moderate and rising slightly (0.18 to 0.28), reflecting some drift toward programs that exist more for political cover ('doing something about the opioid crisis') than for measured harm reduction, without dominating the arrangement's real function. Accessibility collapse (0.35) and resistance (0.55) are both mid-range, appropriate to a genuinely contested tangled rope rather than a settled mountain or a naked snare — real alternatives (full prohibition, full legalization) remain politically live, and active resistance comes from both abstinence-oriented providers and prohibition-oriented law enforcement factions who see the framing as insufficiently punitive, as well as from harm-reduction advocates who see it as insufficiently decriminalizing.
 *
 * PERSPECTIVAL GAP:
 *   From the public-health-authority and law-enforcement seats, this looks like humane, evidence-based reform — a genuine improvement in coordination. From the seat of an uninsured chronic user in a jurisdiction with sparse services, or a neighborhood absorbing concentrated site effects without say in siting, the same structure looks like extraction dressed in health language: the health harm persists, the criminal exposure is only partially removed, and the institutions administering the arrangement retain their funding and authority either way. The engine computing divergent per-seat classifications from the same structural data is the intended behavior here, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs sit near the center but skewed toward target: they gain real services (subsidy) but the underlying extraction — continued health harm, residual criminal exposure for anything outside the decriminalized threshold, and dependence on unevenly funded infrastructure — keeps their directionality high relative to a genuine full-beneficiary position. Public health authorities and law enforcement agencies are structural agenda-setters whose institutional mandates are preserved or enhanced by administering the boundary between decriminalized and prosecutable conduct — low d, near-beneficiary. Neighborhoods hosting services and uninsured chronic users are near-full targets: trapped exit options, concentrated or unaddressed cost, no seat in program design. Adjacent communities are diffuse beneficiaries at low individual cost, appropriately placed near symmetric-to-beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — overdose deaths and disease transmission worsened by prohibition's deterrence of help-seeking — remains empirically live (status: live), corroborated by health-surveillance data independent of the provider and agency seats that administer the response. This blocks a mandatrophy read: the arrangement is not a zombie mandate persisting past its function, because the underlying epidemiological problem has not resolved. It is, however, a tangled rope rather than a clean rope precisely because coordination (real harm reduction) and extraction (unaddressed health harm, unfunded neighborhood cost, preserved police and provider mandates) are both genuinely present and mutually dependent on the same active-enforcement apparatus. Reading this as pure coordination would erase the users and neighborhoods who still pay; reading it as pure extraction would erase the measurable mortality and transmission reductions relative to strict prohibition — the tangled_rope classification is what prevents either erasure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_reduction_vs_prohibition_kernel_boundary,
    'Is the harm_reduction_reading a genuinely distinct constraint from the prohibition_reading, or is it prohibition with a health-service veneer layered on top — i.e., does law enforcement''s retained discretion over the prosecutable boundary mean the underlying criminal-authority structure of prohibition never actually left?',
    'Track prosecution rates, threshold enforcement, and diversion-program actual usage over multiple jurisdictions and years: if prosecutable-boundary enforcement declines in step with service expansion, the readings are structurally distinct; if enforcement discretion is exercised to functionally preserve prohibition-era arrest rates regardless of the health framing, the harm_reduction_reading may be better read as a piton or theater layer on an intact prohibition_reading.',
    'If enforcement data shows the prosecutable boundary is exercised as functionally equivalent to full prohibition, this story''s classification should shift toward a lower coordination function and higher theater_ratio, potentially collapsing into the prohibition_reading rather than remaining a distinct kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_vs_prohibition_kernel_boundary, empirical, 'Whether harm reduction is structurally distinct from prohibition or a rebranding of the same enforcement apparatus.').

omega_variable(
    service_funding_adequacy,
    'Is the health-harm and neighborhood-cost extraction this constraint carries an irreducible feature of the harm_reduction_reading, or is it an artifact of chronic underfunding that a fully resourced version of the same reading would eliminate?',
    'Compare outcomes across jurisdictions with markedly different per-capita harm-reduction funding levels: if health harm and neighborhood cost fall toward zero as funding approaches adequacy, the extraction is contingent, not structural to the reading itself.',
    'If extraction is a funding artifact, the ε value authored here (0.42) is a function of typical real-world underfunding rather than of the reading''s structure — a fully funded implementation might classify closer to rope. This story authors ε for the arrangement as it actually operates, per the ε-referent rule, so this omega documents the counterfactual sensitivity without changing the authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_funding_adequacy, conceptual, 'Whether extraction is intrinsic to the reading or an artifact of implementation funding levels.').

omega_variable(
    supply_side_exclusion_consequence,
    'Because this reading leaves supply-side criminalization intact, does the persistence of an unregulated, contamination-prone illegal market (fentanyl adulteration, etc.) undermine the mortality-reduction function the reading claims, such that third-party and user health harms attributable to supply-chain risk should be weighted more heavily against the coordination claim?',
    'Compare overdose mortality trends in this reading''s jurisdictions against jurisdictions that additionally pursue drug-checking-plus-supply-regulation (partial movement toward the legalization_reading) to isolate the marginal contribution of supply-side status.',
    'If mortality is driven predominantly by supply contamination rather than by criminal exposure to users, the harm_reduction_reading''s coordination claim is weaker than authored and the extractiveness borne by users may be undercounted in this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_side_exclusion_consequence, empirical, 'Whether retained supply-side prohibition limits this reading''s actual harm-reduction efficacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__harm_reduction_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__harm_reduction_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__harm_reduction_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__harm_reduction_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__harm_reduction_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__harm_reduction_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__harm_reduction_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__harm_reduction_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__harm_reduction_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__harm_reduction_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__harm_reduction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__harm_reduction_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__harm_reduction_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__harm_reduction_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__harm_reduction_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the substance_control_authority kernel. prohibition_reading criminalizes use/possession to protect third parties from crime and disorder, keeping users in the criminal-victim set entirely. legalization_reading regulates drug markets as licit commerce, removing both the criminal-victim set and much of the supply-contamination health harm via quality control, at the cost of different externalities (commercial promotion, market-scale health harm). This harm_reduction_reading occupies a distinct middle structural position: it removes users from the criminal-victim set only partially (decriminalization of use/possession up to a threshold, not legalization of supply) while keeping them in a partial health-harm victim set, and it does not resolve supply-side contamination risk the way legalization's quality-control mechanism would. Each reading has a distinct epsilon and distinct beneficiary/victim structure; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
