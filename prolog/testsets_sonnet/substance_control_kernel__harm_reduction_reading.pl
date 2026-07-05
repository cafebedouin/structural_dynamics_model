% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Reading of the Substance Control Kernel
 *   domain: public health / criminal justice / political economy
 *
 * SUMMARY:
 *   This story instantiates the harm-reduction reading of the substance
 *   control kernel: substance use is framed as a health condition warranting
 *   pragmatic intervention to reduce harm, independent of whether use stops.
 *   Under this reading, users exit the criminal victim set that characterizes
 *   the prohibition reading, but they do not exit state control entirely —
 *   they are re-classified from criminal subjects into health subjects,
 *   brought under case management, registries, and mandatory service contact
 *   that they did not choose. The supply chain remains fully criminalized
 *   (the reading does not extend to legalization), so unlicensed suppliers
 *   absorb enforcement intensity that recedes from users. Overdose mortality
 *   and disease transmission become the primary observables legitimating the
 *   apparatus, replacing arrest counts. This is a distinct constraint from
 *   the prohibition_reading (which criminalizes users directly for
 *   moral-order reasons) and from the legalization_reading (which treats use
 *   as a liberty exercised within a regulated, taxed, legal market) — each
 *   carries a different epsilon, a different beneficiary/victim structure,
 *   and a different type. Do not average across them; they are linked only
 *   via network.affects_constraints and the shared kernel.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: primary beneficiary of decriminalization, but also payer of ongoing paternalistic surveillance and continued exposure to unregulated supply
 *   - public_health_agencies: agenda_setter administering the harm reduction apparatus, institutional beneficiary of expanded mandate
 *   - harm_reduction_service_providers: organized beneficiary whose funding and survival depend on this reading's persistence
 *   - unlicensed_supply_chain_participants: payer bearing the full enforcement weight that receded from users
 *   - prohibition_constituencies: excluded voice objecting that use itself, not just its harms, should be punished
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.38).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Reading of the Substance Control Kernel").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public health / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, 'b993f608-8f3e-4324-bb8f-7eb2b162fa15').
narrative_ontology:cs_kernel_codification('b993f608-8f3e-4324-bb8f-7eb2b162fa15', distributed).
narrative_ontology:cs_authority_grounding('b993f608-8f3e-4324-bb8f-7eb2b162fa15', expertise).
narrative_ontology:cs_interpretation_layer_present('b993f608-8f3e-4324-bb8f-7eb2b162fa15').
narrative_ontology:cs_reading_relation('b993f608-8f3e-4324-bb8f-7eb2b162fa15', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('b993f608-8f3e-4324-bb8f-7eb2b162fa15', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('b993f608-8f3e-4324-bb8f-7eb2b162fa15', foundational, use_is_medical_not_moral_condition).
narrative_ontology:cs_axiom_status(use_is_medical_not_moral_condition, holdable).
narrative_ontology:cs_axiom_grounding('b993f608-8f3e-4324-bb8f-7eb2b162fa15', use_is_medical_not_moral_condition, empirically_contingent).
narrative_ontology:cs_axiom('b993f608-8f3e-4324-bb8f-7eb2b162fa15', foundational, harm_reduction_valid_absent_cessation).
narrative_ontology:cs_axiom_status(harm_reduction_valid_absent_cessation, holdable).
narrative_ontology:cs_axiom_grounding('b993f608-8f3e-4324-bb8f-7eb2b162fa15', harm_reduction_valid_absent_cessation, instrumental).
narrative_ontology:cs_reference_frame('b993f608-8f3e-4324-bb8f-7eb2b162fa15', criminalized_use_as_default).
narrative_ontology:cs_drift_state('b993f608-8f3e-4324-bb8f-7eb2b162fa15', post_overdose_crisis_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b993f608-8f3e-4324-bb8f-7eb2b162fa15', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, unlicensed_supply_chain_participants).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, taxpaying_public).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, substance_use_disorder_is_medical_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exit the criminal victim set that prohibition creates — no longer arrested for possession or use in jurisdictions where diversion or decriminalization applies. Gain access to syringe exchanges, supervised consumption sites, naloxone, and opioid substitution therapy without needing to stop using. In exchange, they become subject to mandatory case management, court-monitored 'treatment tracks,' registries, and health-surveillance contact points they did not choose and cannot easily decline without risking re-criminalization or loss of services. Their supply remains illegal, so they still buy from an unregulated, contamination-prone market even while the state now manages their health outcomes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer).

% Administer the harm reduction apparatus: fund needle exchanges, overdose prevention sites, drug-checking services, and substitution programs. Set eligibility criteria, collect overdose and disease-transmission data, and report outcomes to legislators. They benefit from an expanded mandate and budget, and their institutional legitimacy is now tied to declining overdose and HIV/hepatitis transmission numbers rather than to arrest statistics.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Nonprofits and clinics that run the exchange sites, consumption rooms, and outreach programs. They receive grant funding contingent on this reading of the kernel remaining dominant policy; their organizational survival is now bound to the continuation and expansion of harm reduction infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, mobile, regional).

% Growers, couriers, and street-level dealers remain fully criminalized under this reading — the delta explicitly keeps supply illegal even as use is destigmatized. They bear the enforcement weight that has receded from users: raids, seizures, incarceration, and violence risk from operating an unregulated market that harm reduction policy does nothing to legitimize or regulate.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, unlicensed_supply_chain_participants, payer,
    powerless, immediate, trapped, regional).

% Redirected from prosecuting users to interdicting supply. Some officers and departments experience this as a diminished, ambiguous mandate (arrests down, budgets contested); others redeploy toward supply-side enforcement, which under this reading absorbs the coercive apparatus that used to fall on users.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, payer).

% Moral-order and abstinence-based advocacy groups who hold that use itself is the wrong to be corrected, not merely its harms. They are largely outside the room when harm reduction policy is designed and funded, and experience its expansion as an abandonment of the deterrence and moral-signaling function they believe the law should serve.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_constituencies, excluded,
    organized, generational, constrained, national).

% Funds the harm reduction infrastructure through general revenue. Benefits diffusely from reduced disease transmission and overdose deaths (public health externalities) but has limited direct voice in whether resources go to harm reduction, enforcement, or abstinence-based treatment.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, taxpaying_public, payer,
    moderate, biographical, constrained, national).

% Compare overdose mortality, disease incidence, and incarceration rates across jurisdictions adopting different kernel readings. Positioned to see the structural trade the harm reduction reading makes: user decriminalization purchased at the cost of continued supply-chain criminalization and new paternalistic surveillance.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public health response to substance use that reduces overdose deaths and disease transmission by keeping people who use drugs alive, in contact with services, and off unregulated dosing risk, without requiring cessation as a precondition for help.
% TRANSFER_FUNCTION: Moves resources from general public revenue to health agencies and service providers; moves risk and enforcement burden off individual users and onto unlicensed supply-chain participants who remain fully criminalized; moves a degree of autonomy from users to health-monitoring institutions in exchange for reduced criminal exposure.
% ABSENT_VOICES: Prohibition constituencies who believe use itself (not just its harms) warrants state punishment are structurally excluded from harm-reduction program design; supply-chain participants have no voice at all in a reading that decriminalizes their customers while intensifying enforcement against them.
% DISAPPEARANCE_RATIONALE: If this reading's institutional apparatus disappeared overnight, jurisdictions would revert to prohibition-style enforcement of users (arrests, incarceration resuming) or shift toward the legalization reading; the funded harm reduction infrastructure — exchanges, consumption sites, substitution programs — would collapse, and overdose/transmission rates would move sharply, showing the arrangement is doing real causal work, not merely describing a natural background condition.
% FOUNDING_PROBLEM: Overdose deaths and HIV/hepatitis transmission were rising under a purely punitive regime that treated use as a crime to be deterred; the founding problem was that criminalizing users produced worse health outcomes without reducing use, while driving people away from the health system that could reduce mortality.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological research (needle-exchange and supervised-consumption-site mortality/transmission studies conducted outside the funded provider organizations, e.g. academic public health literature and WHO/UNAIDS review bodies) corroborates that overdose and transmission remain live problems responsive to harm reduction infrastructure, distinct from the self-interested attestation of the funded providers themselves.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).
:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: real coordination value exists (reduced mortality, reduced transmission, reduced downstream health costs) but is bundled with continued extraction from criminalized supply-chain participants and non-consensual health surveillance of users. Suppression (0.38, declining over the interval) reflects reduced coercive force applied to users as enforcement recedes from possession/use, even as coercion against supply-chain participants and mandatory program compliance persists. Theater ratio (0.28, rising modestly) captures a growing share of program activity that is compliance-reporting and registry-maintenance rather than direct harm reduction service delivery, as agencies mature and bureaucratize. Accessibility collapse (0.45) is middling: alternatives to state-run harm reduction infrastructure (mutual aid, unsanctioned safe-supply networks) persist but are marginalized relative to the funded apparatus. Resistance (0.5) reflects genuine friction from both prohibition constituencies who want it rolled back and from users who resist the surveillance/case-management conditions attached to services.
 *
 * PERSPECTIVAL GAP:
 *   From the public-health-agency seat, this reading looks like humane, evidence-based coordination — a rope. From the unlicensed supply-chain participant's seat, nothing has changed except that enforcement pressure previously spread across users and suppliers now concentrates entirely on suppliers — the same coercive machinery, narrowed and intensified. The engine's per-seat computation should reflect this asymmetry rather than resolving it into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs sit near symmetric-to-target: they gain real benefit (decriminalization, health access, survival) but also bear a real cost (mandatory contact points, registries, continued reliance on an illegal, contamination-prone supply they have no legal alternative to). Public health agencies and service providers are clear structural beneficiaries — their mandate, budget, and legitimacy grow with the reading's adoption. Unlicensed supply-chain participants are unambiguous targets: enforcement intensity that recedes from users is redirected toward them, and they receive none of the health-framing benefit extended to users.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rising overdose and transmission under punitive enforcement) remains live and is corroborated by epidemiological evidence generated outside the funded provider organizations, which distinguishes this from a zombie mandate. However, the reading's persistence should be watched for drift: if overdose and transmission metrics improve substantially while the case-management and registry apparatus continues to expand, that would signal the mandate shifting from harm reduction to institutionalized paternalism — a pattern the founding_problem_status field is designed to flag on reassessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_coordination_boundary,
    'Is the mandatory case-management and registry apparatus attached to harm reduction services a necessary coordination mechanism (tracking outcomes, ensuring continuity of care) or an extractive surveillance layer riding on genuine health services?',
    'Compare outcomes and uptake rates between low-barrier, minimally-monitored harm reduction services (e.g., anonymous needle exchanges) and heavily case-managed programs with mandatory registry enrollment; if uptake and outcomes are equivalent or better under low-barrier models, the monitoring layer is extractive rather than functionally necessary.',
    'If the monitoring layer is separable from the health benefit, the constraint''s true extraction is lower than measured and closer to a rope; if inseparable, the tangled_rope classification with genuine coordination-extraction fusion is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_coordination_boundary, empirical, 'Whether mandatory surveillance is functionally necessary to harm reduction''s coordination goal or an extractive add-on.').

omega_variable(
    supply_chain_criminalization_persistence,
    'Does keeping the supply chain criminalized while decriminalizing users represent a stable structural equilibrium, or is it a transitional half-measure that generational drift will push toward full legalization or back toward re-criminalization of users?',
    'Longitudinal tracking of jurisdictions adopting this reading: does the split (decriminalized demand, criminalized supply) persist over multiple political cycles, or does contamination/violence in the unregulated supply push policy toward regulated legal supply (legalization_reading) or toward re-criminalizing users under political pressure (reverting to prohibition_reading)?',
    'If the split proves unstable, this reading functions as a scaffold toward one of the sibling readings rather than a durable tangled_rope in its own right — reclassification would follow evidence of a declared or de facto sunset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_criminalization_persistence, conceptual, 'Whether the harm-reduction reading''s supply/demand split is a stable equilibrium or a transitional configuration.').

omega_variable(
    reading_selection_evidentiary_basis,
    'Is the harm_reduction_reading''s dominance in a given jurisdiction driven primarily by epidemiological evidence, or by the organizational interests of the public health agencies and service providers who benefit from its adoption and funding?',
    'Examine whether policy adoption timing correlates more strongly with epidemiological crisis indicators (overdose spikes, outbreak data) or with funding-cycle and advocacy-organization lobbying activity.',
    'If evidence-driven, the reading''s legitimacy claim is well-founded; if interest-driven, the beneficiary structure (public_health_agencies, harm_reduction_service_providers) constitutes a self-interested constituency shaping which kernel reading dominates, independent of comparative outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, empirical, 'Whether epidemiological evidence or institutional self-interest drives adoption of this reading over its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__harm_reduction_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__harm_reduction_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__harm_reduction_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__harm_reduction_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__harm_reduction_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__harm_reduction_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__harm_reduction_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__harm_reduction_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__harm_reduction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__harm_reduction_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__harm_reduction_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__harm_reduction_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the substance_control_kernel, each a separate constraint with its own epsilon, beneficiary/victim structure, and classification per the ε-invariance principle. prohibition_reading criminalizes users directly as moral transgressors (expected: snare or tangled_rope with users as primary victims). legalization_reading treats use as individual liberty with state intervention limited to externality capture (expected: rope or tangled_rope with lower suppression and no criminalized supply chain). harm_reduction_reading (this story) occupies a structurally distinct middle position: it decriminalizes the user's health status while leaving supply criminalized and substituting health surveillance for criminal enforcement. These are not three measurements of the same constraint — they are three constitutionally different arrangements that happen to share a kernel object (how the state treats substance use) and a contested legitimacy claim over which reading should govern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
