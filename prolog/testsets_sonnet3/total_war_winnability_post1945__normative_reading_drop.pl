% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Post-1945 Normative Delegitimation of Total War (UN Charter Art. 2(4) / IHL Reading)
 *   domain: international_relations/law/security
 *
 * SUMMARY:
 *   This story instantiates the normative_reading_drop reading of the
 *   total_war_winnability_post1945 kernel: total war between major powers
 *   remains physically achievable (industrial and military capacity for it
 *   persists), but the UN Charter's Article 2(4) prohibition on the use of
 *   force and the postwar development of international humanitarian law
 *   (Geneva Conventions, Additional Protocols, the prohibition of aggression,
 *   and eventually the Rome Statute) constructed a normative architecture
 *   that made waging total war and annexing territory by force illegitimate
 *   rather than merely inadvisable. On this reading the mechanism of
 *   restraint is legal-normative coordination among states, not a physical or
 *   ideational-cultural mechanism (those are the sibling readings,
 *   structural_contraction_reading and strategic_culture_drift, generated as
 *   separate constraints). The coordination function is real: a
 *   Schelling-point norm against total war and civilian targeting solves a
 *   genuine collective-action problem states could not solve unilaterally.
 *   The extraction is real but comparatively modest and asymmetric:
 *   revisionist and annexationist powers pay reputational, institutional, and
 *   coalition costs for violating a norm they did not always consent to in
 *   practice, while global civilian populations and weaker states collect the
 *   benefit of a lowered baseline risk of total war.
 *
 * KEY AGENTS:
 *   - global_civilian_populations: diffuse beneficiary, powerless, trapped exit — collects reduced total-war risk but cannot enforce the norm directly
 *   - un_charter_signatory_states: institutional agenda-setter — administers Article 2(4) and IHL enforcement machinery through UN organs
 *   - revisionist_powers: powerful payer — bears isolation and sanction costs for pursuing territorial conquest despite retained physical capacity
 *   - great_power_permanent_members: institutional agenda-setter with arbitrage exit — drafted the framework, retains veto shield from its own binding force
 *   - international_law_scholars: analytical observer — traces compliance-pull and enforcement gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.28).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.42).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Post-1945 Normative Delegitimation of Total War (UN Charter Art. 2(4) / IHL Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/law/security").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, 'd1c9bc6c-2379-4c24-8e41-dad11af18465').
narrative_ontology:cs_kernel_codification('d1c9bc6c-2379-4c24-8e41-dad11af18465', fixed_text).
narrative_ontology:cs_authority_grounding('d1c9bc6c-2379-4c24-8e41-dad11af18465', lineage).
narrative_ontology:cs_interpretation_layer_present('d1c9bc6c-2379-4c24-8e41-dad11af18465').
narrative_ontology:cs_reading_relation('d1c9bc6c-2379-4c24-8e41-dad11af18465', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1c9bc6c-2379-4c24-8e41-dad11af18465', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('d1c9bc6c-2379-4c24-8e41-dad11af18465', foundational, restraint_is_legally_constituted_not_merely_structural).
narrative_ontology:cs_axiom_status(restraint_is_legally_constituted_not_merely_structural, holdable).
narrative_ontology:cs_axiom_grounding('d1c9bc6c-2379-4c24-8e41-dad11af18465', restraint_is_legally_constituted_not_merely_structural, conventional).
narrative_ontology:cs_axiom('d1c9bc6c-2379-4c24-8e41-dad11af18465', foundational, aggression_prohibition_binds_independent_of_capability).
narrative_ontology:cs_axiom_status(aggression_prohibition_binds_independent_of_capability, holdable).
narrative_ontology:cs_axiom_grounding('d1c9bc6c-2379-4c24-8e41-dad11af18465', aggression_prohibition_binds_independent_of_capability, deontological).
narrative_ontology:cs_reference_frame('d1c9bc6c-2379-4c24-8e41-dad11af18465', un_charter_founding_prohibition).
narrative_ontology:cs_drift_state('d1c9bc6c-2379-4c24-8e41-dad11af18465', post_cold_war_unipolar_and_multipolar_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d1c9bc6c-2379-4c24-8e41-dad11af18465', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, un_charter_signatory_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, territorial_annexationist_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, great_power_permanent_members).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, prohibition_of_aggressive_war_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, distinction_principle_in_ihl).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the physical costs of any war fought near or through them; the norm against total war and the humanitarian-law rules on distinction and proportionality reduce (without eliminating) the degree to which they are treated as legitimate military objects. They have no direct enforcement power and depend entirely on states and international bodies to uphold the norm on their behalf.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Lack the military capacity to deter conquest through force alone and rely on the Article 2(4) prohibition on the use of force and territorial annexation as a substitute shield. Their sovereignty and territorial integrity are protected on paper by a norm they did not have the power to write but now depend on for survival against more powerful neighbors.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers, beneficiary,
    moderate, generational, constrained, global).

% Collectively administer and invoke the Article 2(4) prohibition and the broader IHL architecture (Geneva Conventions, Additional Protocols, customary law) through the UN Security Council, ICJ, ICC, and diplomatic condemnation. They set the terms of what counts as a violation and can authorize collective responses, though enforcement is uneven and dependent on great-power consent.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, un_charter_signatory_states, agenda_setter,
    institutional, generational, constrained, global).

% States seeking to alter territorial boundaries or subjugate populations by force find their preferred strategy branded illegitimate, sanctioned, and diplomatically isolated even when they retain the physical capacity to wage total war. They bear reputational, economic, and coalition-forming costs for violating the norm, and must resort to legal fictions (self-defense claims, proxy warfare, salami-slicing below the threshold of full annexation) to pursue territorial aims without triggering the norm's full weight.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, global).

% Regimes that have already attempted or executed forcible annexation face sustained non-recognition of territorial gains, sanctions regimes, and exclusion from international institutions. The norm imposes durable costs on them even where military victory was physically achieved, because normative delegitimation persists independently of the battlefield outcome.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, territorial_annexationist_regimes, payer,
    powerful, biographical, constrained, regional).

% The P5 states drafted and administer the Charter framework that constrains others' use of force while retaining veto power that shields their own actions from binding enforcement. They benefit from the general stability the norm provides while retaining practical exit options (veto, selective compliance) unavailable to other signatories.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, great_power_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, great_power_permanent_members, beneficiary).

% Study the gap between the norm's formal prohibition and its selective enforcement, documenting instances of both compliance-pull and violation-without-consequence. Their analysis shapes doctrine but does not directly control state behavior.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: absent a shared, costly-to-violate norm against total war and unrestrained targeting of civilians, every state faces a security dilemma in which restraint by one party is exploitable by others. Article 2(4) and IHL create a Schelling point around which states can coordinate mutual restraint, reducing the probability and destructiveness of great-power war below what unconstrained capability alone would predict.
% TRANSFER_FUNCTION: Moves diplomatic legitimacy, access to international institutions, and freedom from coordinated sanction away from states that pursue territorial conquest by force, and toward states and civilian populations that benefit from the resulting reduction in permissible war aims. Revisionist powers pay in isolation and constrained strategic options; the diffuse global public collects the benefit of a lowered baseline of total-war risk.
% ABSENT_VOICES: Populations under regimes that have annexed territory in defiance of the norm (whose new rulers are not recognized, prolonging their uncertain status) are rarely heard directly; their interests are asserted by proxy through UN resolutions and third-state advocacy rather than through their own participation. Revisionist-power elites who view the norm as encoding a status quo favorable to the powers that won World War II are heard mainly through their own state's diplomatic channels, not as a legitimated grievance within the framework itself.
% DISAPPEARANCE_RATIONALE: If Article 2(4) and the accompanying humanitarian-law architecture vanished as normative constraints overnight, wars of conquest and unrestrained targeting of civilian populations would lose their reputational and institutional cost — sanctions regimes, non-recognition doctrine, and war-crimes prosecution would lose their legal anchor. States currently deterred from annexation by isolation costs (not by physical incapacity) would face a materially different calculus; the frequency and character of interstate war would very likely shift toward more total-war postures among powerful states still physically capable of them.
% FOUNDING_PROBLEM: The founding problem was the demonstrated failure of pre-1945 international order to prevent industrialized total war and mass civilian destruction (World War II, the Holocaust, strategic bombing campaigns) — a felt need to construct an enforceable norm against total war and civilian targeting where none had reliably existed before.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians and international-relations scholars outside the UN system (e.g., studies of the long peace and interstate war frequency) corroborate that great-power total war has been historically rare since 1945 relative to prior centuries, though they dispute how much credit belongs to the normative architecture versus nuclear deterrence or other structural factors. Human rights monitoring organizations independent of state parties corroborate that IHL compliance, while imperfect, has altered targeting practices and prosecution patterns in ways traceable to the treaty regime. No corroboration exists that is fully independent of the beneficiary states themselves for claims about the norm's causal weight relative to sibling explanations — this is exactly the contested boundary the kernel captures.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28 at 2025) because the normative architecture genuinely solves a coordination problem for the vast majority of states rather than functioning primarily as a rent-extraction device; the cost imposed on revisionist powers is a designed feature of the coordination mechanism (a norm that imposed no cost on violators would not function as a norm). Suppression sits at a moderate 0.42 because the norm's persistence depends on active enforcement infrastructure (Security Council referral, sanctions regimes, tribunals) that must be maintained and periodically wielded against non-compliant states; it is not self-enforcing. Theater ratio is kept low (0.2) reflecting that despite well-known enforcement gaps (veto-shielded great powers, selective prosecution), the underlying function — reducing the incidence and destructiveness of interstate war relative to unconstrained capability — remains substantially real and is not merely performed. The measurement series shows slow upward drift in all three metrics across the interval, consistent with a maturing but not yet saturated enforcement architecture (Nuremberg-era norms diffusing into Rome Statute-era institutionalization) rather than either sudden capture or decay.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of global civilian populations and small/middle powers, the arrangement reads as genuine, if imperfectly enforced, coordination — a rope. From the seat of a revisionist power contemplating territorial revision that it retains the physical capacity to execute, the same structure reads as an imposed constraint whose primary function is to foreclose a strategy that would otherwise be available to it — closer to a tangled arrangement from that vantage, though it lacks a concentrated beneficiary collecting rents, which is why the story-level claim remains rope rather than tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations and small/middle powers are structural beneficiaries: the norm was substantively built to reduce their exposure to total war and territorial conquest, and they have essentially no capacity to produce that protection unilaterally, so directionality sits near the beneficiary end despite their powerlessness. Revisionist powers and territorial-annexationist regimes are the structural targets: the norm imposes costs specifically and predictably on their preferred strategy (forcible territorial revision), so directionality sits near the target end even though they retain substantial physical power — this is a case where power and directionality diverge, since a powerful state can still be a structural target of a coordination norm built partly to constrain it. The P5 great powers occupy an asymmetric middle position: agenda-setters who also retain a partial beneficiary/arbitrage position via veto power, which is why they are marked with a secondary beneficiary role and arbitrage exit rather than the constrained exit of ordinary signatory states.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing recurrence of industrialized total war and mass civilian destruction) is contested rather than cleanly live or dead: nuclear deterrence and shifting strategic culture (the sibling readings) may now be doing much of the causal work historically attributed to the normative architecture, which raises the question of whether Article 2(4) and IHL persist partly as legitimating discourse for an outcome substantially secured by other mechanisms. This story deliberately does NOT resolve that question — it is exactly the decomposition boundary the kernel exists to mark. Classifying this reading as tangled_rope or piton would require establishing that the normative mechanism itself does no independent work, which is precisely the omega below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_norm_vs_nuclear_deterrence_causal_weight,
    'How much of the observed post-1945 absence of great-power total war is attributable to the Article 2(4)/IHL normative architecture specifically, as opposed to nuclear deterrence (the structural_contraction_reading) or an independent shift in elite strategic culture (the strategic_culture_drift reading)?',
    'Comparative case analysis of near-total-war crises where nuclear-armed and non-nuclear-armed states behaved differently under the same normative constraints, controlling for capability; counterfactual analysis of conflicts where legal norms were invoked in the absence of nuclear deterrence (e.g., conventional-only regional conflicts) to isolate the norm''s independent effect.',
    'If the normative mechanism is shown to do little independent causal work once nuclear deterrence and strategic-culture shift are controlled for, this reading''s coordination-function claim weakens substantially, and the constraint would drift toward a scaffold or theatrical-legitimation reading (the underlying restraint being produced elsewhere while the legal architecture provides post-hoc justification) rather than genuine rope-class coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_norm_vs_nuclear_deterrence_causal_weight, empirical, 'Whether the legal-normative mechanism has independent causal weight relative to the two sibling mechanisms (nuclear structure, strategic culture).').

omega_variable(
    great_power_veto_exemption_undermines_norm,
    'Does the P5''s veto-shielded exemption from binding enforcement of the very norm they authored make this a genuine universal coordination mechanism, or a norm whose binding force is calibrated to fall almost entirely on non-great-power states?',
    'Systematic review of Security Council referral and veto patterns in cases involving alleged Article 2(4) or IHL violations by P5 states versus non-P5 states, quantifying enforcement asymmetry.',
    'If enforcement asymmetry is severe and structural rather than incidental, the great-power seat''s classification would shift from agenda_setter/beneficiary toward a more extractive framing, and the story-level claim of rope (versus tangled_rope) becomes harder to sustain at the level of the full state system, though it may remain accurate for the non-great-power subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_veto_exemption_undermines_norm, conceptual, 'Whether P5 veto exemption structurally converts universal coordination into asymmetric constraint.').

omega_variable(
    kernel_framing_choice_and_alternative_decomposition,
    'Is the three-way decomposition (normative, structural/nuclear, strategic-culture) the correct carving of the kernel, or does the normative mechanism actually presuppose and depend on the structural (nuclear) mechanism in a way that makes them not fully independent siblings?',
    'Historical analysis of whether Article 2(4) and IHL commitments would have held the same normative force absent the nuclear backdrop — e.g., examining pre-nuclear interwar attempts at similar legal restraint (Kellogg-Briand Pact) and their comparative failure.',
    'If the normative mechanism is shown to be causally downstream of or co-dependent with the nuclear structural mechanism rather than a fully independent coordination device, this reading''s ε and coordination-function claims should be revisited as partially derivative rather than freestanding — though per the ε-invariance principle, this would still be documented as a relationship between two constraints via network links rather than collapsed into a single story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_and_alternative_decomposition, conceptual, 'Whether the normative reading is truly independent of the structural (nuclear) sibling reading or partially derivative of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(tota_tr_t1990, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1990, 0.16).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.2).
narrative_ontology:measurement(tota_be_t1990, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1960, 0.34).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.36).
narrative_ontology:measurement(tota_su_t1990, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the kernel total_war_winnability_post1945. structural_contraction_reading holds that nuclear weapons physically removed total war from the reachable strategic space (a mountain-flavored claim about structural impossibility). strategic_culture_drift holds that total war remained reachable but dropped from elite discourse via an ideational shift in strategic culture (a rope/scaffold-flavored claim about norm internalization without binding legal machinery). This story (normative_reading_drop) holds that total war remains physically possible but became normatively illegitimate through binding legal-institutional mechanisms (Article 2(4), IHL) — a rope-class claim resting on treaty-based coordination with identifiable enforcement machinery, beneficiaries, and victims. All three readings agree on the observed outcome (rarity of post-1945 great-power total war) and disagree on mechanism; each is authored as an independent, ε-invariant constraint per the decomposition principle, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
