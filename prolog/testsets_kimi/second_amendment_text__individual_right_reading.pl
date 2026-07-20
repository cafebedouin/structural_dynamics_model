% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Self-Defense Core)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story models the Second Amendment's
 *   individual_right_reading as instantiated in federal constitutional
 *   doctrine. The reading treats the operative clause as guaranteeing a
 *   personal right to possess firearms for self-defense, independent of
 *   militia service. It coordinates law around armed self-defense while
 *   extracting liberty and security from prohibited persons (felons, domestic
 *   abusers) and regulatory capacity from state and local governments. The
 *   authored metrics and claimed_type are independent: the reading is claimed
 *   as tangled_rope because it retains a genuine coordination function
 *   alongside asymmetric extraction, while metrics track rising
 *   extractiveness and suppression as the doctrine hardens.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (organized/national) â receives constitutional protection and judicial invalidation of regulations
 *   - disarmed_prohibited_persons: Primary target (powerless/trapped) â bears extraction through criminal prohibition and enforcement
 *   - federal_judiciary: Agenda setter (institutional/analytical) â enforces the reading through judicial review and historical-tradition tests
 *   - state_local_governments: Secondary payer (institutional/constrained) â loses regulatory autonomy and bears compliance costs
 *   - gun_control_advocates: Excluded voice (organized/constrained) â structurally absent from the interpretive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.6).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment Individual Right Reading (Self-Defense Core)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, 'aad16e90-e6c9-4aa2-a874-832adc5f5fcb').
narrative_ontology:cs_kernel_codification('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', fixed_text).
narrative_ontology:cs_authority_grounding('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', lineage).
narrative_ontology:cs_interpretation_layer_present('aad16e90-e6c9-4aa2-a874-832adc5f5fcb').
narrative_ontology:cs_reading_relation('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', second_amendment_text__originalist_civic_virtue_reading, influences).
narrative_ontology:cs_axiom('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', foundational, operative_clause_independent_right).
narrative_ontology:cs_axiom_status(operative_clause_independent_right, holdable).
narrative_ontology:cs_axiom_grounding('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', operative_clause_independent_right, conventional).
narrative_ontology:cs_axiom('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', secondary, personal_self_defense_core_activity).
narrative_ontology:cs_axiom_status(personal_self_defense_core_activity, holdable).
narrative_ontology:cs_axiom_grounding('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', personal_self_defense_core_activity, deontological).
narrative_ontology:cs_reference_frame('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', individual_self_defense_tradition).
narrative_ontology:cs_drift_state('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', post_bruen_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aad16e90-e6c9-4aa2-a874-832adc5f5fcb', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_prohibited_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, state_local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and acquire firearms for personal self-defense under constitutional protection; benefit from judicial invalidation of permit requirements, acquisition restrictions, and sensitive-place bans; their legal status shifts from regulated privilege to protected right as this reading hardens.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, generational, constrained, national).

% Subject to federal and state firearm prohibitions as persons deemed not among 'the people' protected by this reading; face criminal prosecution, incarceration, and lifetime disability for possession; their exclusion is the boundary condition that secures the right for lawful owners.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_prohibited_persons, payer,
    powerless, biographical, trapped, national).

% Exercises judicial review to enforce the individual-right reading, striking down laws that burden self-defense; administers the historical-tradition test to determine which modern regulations are consistent with the founding-era right and which are constitutionally void.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Lose regulatory autonomy over firearm permitting, registration, waiting periods, and sensitive-place designations; must redesign statutes to survive constitutional challenge; bear compliance and litigation costs of adapting to judicially enforced boundaries.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_local_governments, payer,
    institutional, generational, constrained, national).

% Advance public-health and collective-security framings that this reading structurally excludes from constitutional consideration; their preferred policy instrumentsâpermit regimes, categorical bans, acquisition restrictionsâare treated as presumptively invalid under the self-defense framing.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_control_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legal system around individual armed self-defense as a constitutionally protected activity, providing a uniform national floor against government disarmament and creating legal certainty for lawful ownership across jurisdictions.
% TRANSFER_FUNCTION: Transfers constitutional protection and a liberty interest in firearm possession to individual gun owners while transferring the costs of prohibition, criminal enforcement, and regulatory preemption onto disarmed prohibited persons and state and local governments.
% ABSENT_VOICES: Domestic violence survivors and urban communities experiencing high rates of firearm violence, who would prioritize disarmament of dangerous persons over unrestricted individual access; their security interests are subordinated to the self-defense framing and largely absent from the interpretive test.
% DISAPPEARANCE_RATIONALE: If this reading vanished, federal and state governments would regain broad authority to regulate firearm possession through permitting, registration, waiting periods, and categorical restrictions; the legal status of millions of gun owners would revert to legislative discretion; the category of disarmed prohibited persons would be redefined by statute rather than constitutional boundary.
% FOUNDING_PROBLEM: Tyrannical governments disarming political opponents; individual vulnerability to interpersonal violence in the absence of reliable state protection; the need for a politically empowered citizenry capable of resisting oppression.
% FOUNDING_PROBLEM_CORROBORATION: Originalist historians and some libertarian legal scholars corroborate the tyranny-resistance framing from outside the immediate beneficiary set. Public health researchers, criminologists, and survivors' groups contest that the founding problem persists in modern form; they attest that state capacity and democratic stability have obviated the need for armed individual self-help, and no neutral interdisciplinary consensus corroborates the unmodified founding narrative.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the reading funds individual gun owners' liberty by imposing categorical criminal disability on prohibited persons and preempting state regulatory capacity. Suppression (0.60) reflects the active judicial suppression of permit regimes and collective-security framings. Theater ratio (0.42) captures the growing performative burden of the historical-tradition test, which requires elaborate originalist pageantry to justify modern boundaries. Accessibility collapse (0.50) registers that alternative readings (collective security, civic virtue) are partially collapsed in federal courts but persist in state forums and scholarship. Resistance (0.58) reflects sustained opposition from gun-control advocates and resistant state legislatures.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is stark: from the individual gun owner's position the constraint is protective coordination that secures a fundamental liberty; from the prohibited person's position the same structure is an extractive criminalization regime that permanently strips a liberty interest; from the state government's position it is a federal preemption that disables democratic policymaking. The engine computes this divergence from the structural dataâbeneficiary, payer, and excluded rolesâwithout requiring a reconciled type.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners are declared beneficiaries with constrained exit (the right is national, so no internal exit), placing their directionality near the beneficiary pole. Disarmed prohibited persons are declared victims with trapped exit (criminal disability follows them nationwide), placing their directionality near the full-target pole. State and local governments are payers with constrained exit (they cannot exit the federal constitutional order), but they are not declared victims because their cost is loss of regulatory autonomy rather than personal extraction. The federal judiciary is agenda_setter with analytical exit; structurally it administers rather than pays or receives, so it sits near the low-extraction end.
 *
 * MANDATROPHY ANALYSIS:
 *   The individual right reading prevents mandatrophy mislabeling by preserving a genuine coordination function: it solves a real problem of disarmed vulnerability and provides legal certainty for ownership. Without that coordination component, the constitutional doctrine would compute as snare (pure extraction through interpretive capture). The declared victim set of prohibited persons and the active enforcement requirement (federal judicial review) keep the classification in tangled_rope rather than rope or mountain. Should the coordination function atrophyâif self-defense were no longer the operative justification and the reading served only to block all regulationâtheater_ratio would rise and the constraint would drift toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Does the Second Amendment''s operative clause logically sever from the militia preamble, or is the individual right reading an interpretive construction that rewrites the kernel?',
    'Historical-linguistic corpus analysis of 18th-century ''bearing arms'' usage; systematic review of state ratifying convention debates to determine whether the preamble was understood as operative or hortatory.',
    'If the preamble is inseparable, the individual right reading becomes a snare (pure extraction through interpretive coercion); if severable, the constraint may function as rope or mountain within constitutional law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the individual right reading is textually grounded or constructively imposed on the kernel').

omega_variable(
    sibling_reading_structural_delta,
    'Would adopting the collective_security_reading reclassify the constraint from tangled_rope to rope or scaffold by eliminating the victim set of disarmed prohibited persons?',
    'Comparative analysis of pre-Heller state regulatory regimes under collectivist readings versus post-Heller individual-right regimes; measurement of prohibited-person disability rates and regulatory autonomy under each reading.',
    'Under collective_security_reading, prohibited persons might remain regulated but not as a constitutional victim set of extraction; the constraint''s epsilon would drop significantly and its type could shift toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'How sibling reading choice alters the constraint''s beneficiary-victim structure').

omega_variable(
    prohibited_persons_victim_status,
    'Are firearm prohibitions on felons and domestic abusers a necessary boundary condition of the individual right, or an extractive collateral burden that exceeds the coordination function?',
    'Empirical analysis of violent recidivism rates among prohibited categories versus enforcement costs and procedural-due-process limitations; comparison with narrower regulatory regimes that use risk-based adjudication rather than categorical exclusion.',
    'If the prohibitions are overbroad, the victim set expands and extractiveness increases; if precisely tailored to demonstrated dangerousness, the constraint edges toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibited_persons_victim_status, empirical, 'Whether the disarmed populations category is a necessary boundary or extractive overreach').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(seco_tr_t6, second_amendment_text__individual_right_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(seco_tr_t12, second_amendment_text__individual_right_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(seco_tr_t18, second_amendment_text__individual_right_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(seco_tr_t24, second_amendment_text__individual_right_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__individual_right_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__individual_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(seco_be_t6, second_amendment_text__individual_right_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(seco_be_t12, second_amendment_text__individual_right_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(seco_be_t18, second_amendment_text__individual_right_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(seco_be_t24, second_amendment_text__individual_right_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__individual_right_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__individual_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(seco_su_t6, second_amendment_text__individual_right_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(seco_su_t12, second_amendment_text__individual_right_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(seco_su_t18, second_amendment_text__individual_right_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(seco_su_t24, second_amendment_text__individual_right_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__individual_right_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel second_amendment_text. The individual_right_reading instantiates a constraint with high extractiveness toward disarmed prohibited persons and strong beneficiary status for individual gun owners, diverging from the collective_security_reading (which locates the beneficiary in state militias and collective security) and the originalist_civic_virtue_reading (which locates the right in citizen-soldier capacity rather than personal self-defense).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
