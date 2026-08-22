% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: State Authority to Tolerate Drug Use via Harm Reduction Services (Decriminalization Without Legalization)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story authors the harm reduction reading of the substance control
 *   authority kernel: the state's claim of authority to tolerate personal
 *   drug use and possession below a threshold while directing enforcement
 *   resources toward public health services rather than prosecution, while
 *   leaving drug production and distribution criminalized. This is a distinct
 *   constraint from the prohibition reading (full criminalization to protect
 *   third parties) and the legalization reading (regulated legal commerce) —
 *   it occupies a structurally intermediate position where users partially
 *   exit the criminal victim set but remain in the health-harm victim set,
 *   and third parties (neighbors, low-level dealers) absorb costs the other
 *   two readings distribute differently. ε for this reading is assessed on
 *   its own terms: moderate extraction from users (unregulated supply harms
 *   persist), from unregulated market participants (concentrated criminal
 *   risk), and diffusely from neighboring communities (externalities), rather
 *   than the near-zero extraction a legalization reading would claim or the
 *   high extraction the prohibition reading's own kernel logic would assign
 *   to users.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.38).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Authority to Tolerate Drug Use via Harm Reduction Services (Decriminalization Without Legalization)").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '86c133d8-e410-4dc2-8b6a-f05b5303dd4d').
narrative_ontology:cs_kernel_codification('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', distributed).
narrative_ontology:cs_authority_grounding('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', distributed).
narrative_ontology:cs_reading_relation('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', substance_control_authority__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', foundational, addiction_is_a_health_condition_not_a_crime).
narrative_ontology:cs_axiom_status(addiction_is_a_health_condition_not_a_crime, holdable).
narrative_ontology:cs_axiom_grounding('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', addiction_is_a_health_condition_not_a_crime, empirically_contingent).
narrative_ontology:cs_axiom('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', foundational, state_may_tolerate_use_while_still_criminalizing_supply).
narrative_ontology:cs_axiom_status(state_may_tolerate_use_while_still_criminalizing_supply, holdable).
narrative_ontology:cs_axiom_grounding('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', state_may_tolerate_use_while_still_criminalizing_supply, instrumental).
narrative_ontology:cs_reference_frame('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', public_health_medicalization_of_addiction).
narrative_ontology:cs_drift_state('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', post_overdose_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86c133d8-e410-4dc2-8b6a-f05b5303dd4d', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, municipal_governments).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, neighboring_residents).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, unregulated_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, public_health_framing_of_addiction).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, state_legitimacy_to_condition_tolerance_on_service_uptake).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% No longer face arrest or prosecution for personal possession in jurisdictions applying this reading, and can access syringe exchanges, supervised consumption sites, and naloxone without identifying themselves to police. Still bear the underlying health harms of use itself — overdose risk, infection, dependency — since the arrangement tolerates use rather than eliminating its supply chain or its dangers. Access to services is conditioned on engagement (referral acceptance, program enrollment) that some avoid, and the drugs themselves remain illegally sourced, meaning purity and dosage are still unregulated.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer).

% Administer the decriminalization thresholds, fund and operate harm reduction services, and collect the data used to justify continued state tolerance. Their institutional survival and budget lines depend partly on framing addiction as a treatable health condition rather than a crime, which gives them an interest in the arrangement's continuation independent of outcomes.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, regional).

% Operate needle exchanges, overdose prevention sites, and outreach programs funded by the state's tolerance of their client base. Their organizational existence depends on drug use remaining tolerated-but-not-normalized; full legalization could route users toward regulated commercial retail, and renewed prohibition could criminalize their clients again, either of which threatens their funding model and mission.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, constrained, local).

% Live near supervised consumption sites, open-air use locations, or encampments that persist because use is tolerated but the illegal supply chain is not disrupted. Bear discarded needles, visible public use, and associated low-level disorder without having consented to host the arrangement's externalities; their exit option is largely limited to relocation or political pressure through local government.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, neighboring_residents, payer,
    moderate, biographical, constrained, local).

% Continue supplying drugs through illegal channels because decriminalization applies to possession and use, not to production or distribution. They face full criminal enforcement while their customers face none, and violence associated with market competition and enforcement pressure falls disproportionately on low-level dealers rather than institutional actors.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, unregulated_market_participants, payer,
    powerless, immediate, trapped, regional).

% Retain authority to enforce against supply-side actors and against possession above decriminalization thresholds, while being directed to deprioritize enforcement against users. This creates operational ambiguity and occasional resistance from officers and departments whose institutional identity and budget justification have historically rested on drug enforcement volume.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement_agencies, payer).

% Set and can revise the decriminalization thresholds and service funding levels in response to political pressure, overdose statistics, or visible disorder complaints. Bear electoral risk if either overdose deaths or visible public disorder rise, giving them incentive to maintain the arrangement's ambiguous middle position rather than resolve it toward full prohibition or full legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, elected_officials, agenda_setter,
    institutional, biographical, mobile, regional).

% Would argue that tolerating use without addressing supply increases visible disorder and normalizes use without controlling its social costs. Their view is represented in political debate but structurally outside the administrative apparatus that sets and runs the harm reduction framework once adopted.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibition_advocates, excluded,
    organized, biographical, analytical, regional).

% Would argue that tolerating use while criminalizing supply preserves the violence and quality-control harms of an illegal market without capturing the tax revenue or quality-control benefits of regulation. Their preferred framework is a structurally different reading of the same kernel and is not implemented under this arrangement.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    organized, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose deaths, disease transmission, and criminal-justice-system burden by routing people who use drugs toward health services instead of incarceration, while preserving a formal state posture that use itself is not endorsed.
% TRANSFER_FUNCTION: Moves the cost of addiction-related harm from the criminal justice system (arrest, prosecution, incarceration) onto the public health system (services, outreach, supervised sites) and onto neighboring communities (externalities of tolerated public use), while leaving supply-side criminalization and its violence intact.
% ABSENT_VOICES: Prohibition advocates argue tolerance without supply control increases visible disorder; legalization advocates argue partial decriminalization preserves the illegal market's violence and quality-control failures without capturing regulatory benefits. Both are represented in political debate but neither controls the administrative apparatus once the harm reduction reading is adopted; low-level dealers bearing continued criminal risk are essentially unrepresented in either debate.
% DISAPPEARANCE_RATIONALE: If this specific state posture disappeared overnight, jurisdictions would revert either to full criminalization (users re-enter the criminal victim set, arrests resume) or advance to full legalization (supply chain enters regulated commerce). Harm reduction service funding, currently justified by the state's tolerance stance, would lose its legal and political footing; supervised consumption sites in many jurisdictions depend on this exact reading's legal cover to operate at all.
% FOUNDING_PROBLEM: Overdose deaths, HIV/Hepatitis C transmission from needle sharing, and mass incarceration for simple possession were rising under strict prohibition, while political and cultural resistance blocked full legalization of drug commerce.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological research (overdose mortality data, needle-exchange disease transmission studies) conducted by academic and international public health bodies outside both the harm-reduction service sector and law enforcement corroborates that overdose and transmission remain live problems. Criminal justice reform researchers outside the harm reduction service industry corroborate continued incarceration harms from possession-threshold enforcement, supporting that the founding problem persists rather than having been resolved by this arrangement alone.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) reflects that the reading achieves real reduction in criminal-justice harm to users but does nothing to address the underlying harms of an unregulated supply chain — the extraction shifts location (from courts to health systems and neighborhoods) rather than disappearing. Suppression (0.38) is moderate and falling over the measured interval as enforcement against users is deprioritized, while theater ratio (0.3) is rising modestly as administrative reporting and program metrics increasingly substitute for harder-to-measure outcomes like overdose mortality trends. Accessibility collapse (0.4) is moderate: users retain some informal alternatives (self-managed harm reduction, peer networks) that the formal service apparatus does not fully displace. Resistance (0.55) is elevated because both prohibition and legalization advocates actively contest this middle position from opposite directions.
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs are declared as both beneficiary (exit from criminal prosecution, access to services) and payer (residual health harms, conditional service access) because the reading's structural delta is precisely this split — it does not fully resolve their victim status, it partially recomposes it. Public health agencies and service providers are beneficiaries because their institutional mandate and funding derive from the state's tolerance framing. Unregulated market participants remain full targets because the reading's decriminalization threshold applies to possession, not supply, leaving them exposed to the same or intensified enforcement pressure as demand shifts. Neighboring residents bear externalities without holding either agenda-setting power or exit options proportionate to the burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overdose deaths, disease transmission, incarceration harms under strict prohibition) remains live by outside corroboration, which supports the tangled_rope classification over piton — the arrangement has not become pure inertial theater, though the rising theater_ratio trend signals administrative metrics beginning to substitute for harm outcomes and warrants continued monitoring. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine (if partial) coordination achievement — reduced arrest and disease transmission — as pure extraction; classifying it as tangled_rope rather than rope prevents ignoring the real victims (residual health harms, unregulated market participants, externalized neighborhood costs) that a rope reading would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_stability_vs_reversion,
    'Is the harm reduction reading a stable equilibrium state, or an unstable political compromise that will drift toward either the prohibition_reading or the legalization_reading under electoral pressure?',
    'Track jurisdictions that have adopted this reading over a multi-decade interval and observe whether policy reverts toward criminalization (as some jurisdictions have done after visible disorder complaints) or advances toward legalization (as others have done via ballot initiative), versus jurisdictions where the reading persists unchanged.',
    'If the reading is structurally unstable and typically reverts or advances within a bounded political timeframe, its tangled_rope classification should be read as a transitional state rather than an equilibrium, closer to a scaffold in practice even though no formal sunset clause is declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability_vs_reversion, empirical, 'Whether this reading is a stable middle position or a transitional compromise between the sibling readings.').

omega_variable(
    supply_side_exclusion_coherence,
    'Is it coherent for the state to tolerate demand-side drug use while maintaining full criminalization of supply, or does this create an internally unstable enforcement contradiction that shifts harm onto unregulated market participants without net reduction?',
    'Compare violence and market-structure outcomes (dealer-level violence, product adulteration rates) in jurisdictions with this reading against jurisdictions with full legalization and jurisdictions with full prohibition, controlling for baseline conditions.',
    'If supply-side criminalization under demand-side tolerance produces no meaningful reduction in market violence or adulteration compared to full prohibition, the reading''s claimed health-harm-reduction benefit is substantially offset by unaddressed supply-chain harms, weakening the coordination-function claim underlying its tangled_rope status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_exclusion_coherence, conceptual, 'Whether decriminalizing demand while criminalizing supply is a coherent harm-reduction strategy or an internally contradictory partial measure.').

omega_variable(
    service_conditionality_as_soft_coercion,
    'Does conditioning tolerance on service engagement (referral acceptance, program enrollment) constitute a form of suppression distinct from criminal enforcement, effectively replacing overt coercion with administrative coercion?',
    'Examine whether users who decline service engagement face de facto sanctions (loss of tolerance status, referral to law enforcement, exclusion from other benefits) versus genuinely voluntary service access with no consequence for non-engagement.',
    'If declining services carries functional sanctions, the suppression metric understates the true coercive character of the arrangement for users who prefer autonomy over program enrollment, and the reading''s distance from the prohibition_reading is smaller than the formal decriminalization threshold suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(service_conditionality_as_soft_coercion, empirical, 'Whether service conditionality functions as a softer but still coercive suppression mechanism for users who decline engagement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__harm_reduction_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__harm_reduction_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__harm_reduction_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__harm_reduction_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__harm_reduction_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__harm_reduction_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__harm_reduction_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__harm_reduction_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__harm_reduction_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__harm_reduction_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__harm_reduction_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__harm_reduction_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraints decomposed from the substance_control_authority kernel per the ε-invariance principle. prohibition_reading assigns users to the primary criminal victim set with high suppression; legalization_reading treats supply chain regulation as the primary mechanism with near-zero criminal extraction; this harm_reduction_reading occupies a structurally intermediate position with partial victim-set exit for users and continued full criminalization of supply. Each reading has its own ε, beneficiary/victim structure, and stakeholder set; they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
