% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment as Insurrectionist Guarantee (Armed Resistance Capacity Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates one reading of the Second Amendment kernel: the
 *   insurrectionist reading, which holds that the right to keep and bear arms
 *   exists fundamentally to preserve citizen capacity for armed resistance
 *   against a tyrannical government, and that individual possession is
 *   instrumental toward that potential overthrow. Under this premise's
 *   logical endpoint, military-grade arms enter the protected domain (a
 *   militia deterrent against a professional army needs comparable weaponry),
 *   and state efforts at disarmament or registration are treated as
 *   precursors to tyranny rather than ordinary public-safety regulation. This
 *   is a distinct constraint from the individual_right_reading (which grounds
 *   the right in personal self-defense/autonomy without the
 *   resistance-to-tyranny theory) and the militia_conditioned_reading (which
 *   bounds the right entirely to organized militia service and permits
 *   comprehensive regulation). The three readings share a text but not a
 *   referent for epsilon: this story's extraction is authored for the
 *   insurrectionist reading's own operation, not averaged with the siblings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.61).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.35).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment as Insurrectionist Guarantee (Armed Resistance Capacity Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'c052d821-26d1-4ac4-9cfc-12baa18d391c').
narrative_ontology:cs_kernel_codification('c052d821-26d1-4ac4-9cfc-12baa18d391c', fixed_text).
narrative_ontology:cs_authority_grounding('c052d821-26d1-4ac4-9cfc-12baa18d391c', lineage).
narrative_ontology:cs_interpretation_layer_present('c052d821-26d1-4ac4-9cfc-12baa18d391c').
narrative_ontology:cs_reading_relation('c052d821-26d1-4ac4-9cfc-12baa18d391c', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('c052d821-26d1-4ac4-9cfc-12baa18d391c', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('c052d821-26d1-4ac4-9cfc-12baa18d391c', foundational, armed_populace_as_ultimate_check_on_state_power).
narrative_ontology:cs_axiom_status(armed_populace_as_ultimate_check_on_state_power, holdable).
narrative_ontology:cs_axiom_grounding('c052d821-26d1-4ac4-9cfc-12baa18d391c', armed_populace_as_ultimate_check_on_state_power, deontological).
narrative_ontology:cs_axiom('c052d821-26d1-4ac4-9cfc-12baa18d391c', secondary, disarmament_measures_as_tyranny_precursor_evidence).
narrative_ontology:cs_axiom_status(disarmament_measures_as_tyranny_precursor_evidence, holdable).
narrative_ontology:cs_axiom_grounding('c052d821-26d1-4ac4-9cfc-12baa18d391c', disarmament_measures_as_tyranny_precursor_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('c052d821-26d1-4ac4-9cfc-12baa18d391c', founding_era_standing_army_distrust).
narrative_ontology:cs_drift_state('c052d821-26d1-4ac4-9cfc-12baa18d391c', post_professionalized_military_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c052d821-26d1-4ac4-9cfc-12baa18d391c', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizen_militia_movement).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_and_ammunition_manufacturers).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, gun_violence_survivors_and_families).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, popular_sovereignty_as_ultimate_check).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, distrust_of_standing_government_as_founding_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes around the claim that private ownership of military-pattern rifles and, in the logical extension of the premise, more capable arms is constitutionally protected specifically because it preserves the capacity to resist a future tyrannical government. Actively lobbies against registries, bans, and confiscation measures by framing them as tyranny precursors. Gains political legitimacy, legal cover, and a recruiting narrative from the reading; faces no meaningful cost from maintaining it.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizen_militia_movement, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, armed_citizen_militia_movement, agenda_setter).

% Benefits commercially from a legal and rhetorical environment in which the insurrectionist reading pushes courts and legislatures toward treating high-capacity, military-derived weapons as presumptively protected. Funds litigation and advocacy sustaining the reading; can relocate production or lobbying investment across states with minimal friction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_and_ammunition_manufacturers, beneficiary,
    organized, generational, arbitrage, national).

% Comprises the law enforcement and military personnel whom the insurrectionist reading structurally casts as the anticipated targets of citizen armed resistance and whom the reading treats as an entity whose disarmament initiatives (buybacks, registries, enhanced background checks) are themselves evidence of encroaching tyranny. Cannot exit the role the reading assigns it; must operate under a legal framework that treats its own regulatory tools as suspect.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, trapped, national).

% Represents bystanders, family members, and community residents who would bear the physical and social costs of any scenario in which the reading's logical endpoint is exercised — organized armed resistance against state authority. They have no voice in whether the doctrine is adopted and no capacity to exit the geography where such conflict would occur.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict, payer,
    powerless, biographical, trapped, regional).

% Bears the diffuse but continuous cost of a legal-political environment in which the insurrectionist reading is used to resist regulation of weapon lethality and availability generally, on the theory that regulation is a step toward disarming the populace ahead of tyranny. Cannot litigate against a constitutional interpretation; can only lobby for narrower alternative readings.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_violence_survivors_and_families, payer,
    powerless, biographical, trapped, national).

% Would regulate weapon categories, magazine capacity, and registration requirements based on ordinary public-safety calculus, but under the insurrectionist reading any such measure is recast as an attack on the deterrent function itself, foreclosing normal legislative bargaining before it starts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, federal_and_state_legislatures, excluded,
    institutional, biographical, constrained, national).

% Assess the historical plausibility of the insurrectionist premise against founding-era militia practice, subsequent incorporation doctrine, and comparative constitutional design. Their analysis feeds litigation and public debate but does not itself resolve which reading prevails.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_historians_and_courts, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates a distributed deterrent against state overreach: if enough citizens retain arms capable of organized resistance, the theory holds, no government will risk attempting tyranny, benefiting everyone who prefers a check on state power to exist even if never used.
% TRANSFER_FUNCTION: Moves political and legal insulation from regulation toward armed citizen organizations and arms manufacturers, and moves risk — of both hypothetical armed conflict and routine gun violence enabled by permissive doctrine — onto civilians, security personnel, and violence survivors who did not choose the arrangement.
% ABSENT_VOICES: Civilians who would be caught in any actual exercise of armed resistance, and security personnel cast as the reading's designated targets, have no seat in constitutional interpretation; their exposure is a downstream consequence of a doctrine adopted without their consent.
% DISAPPEARANCE_RATIONALE: Proponents would say the deterrent value against tyranny would vanish, altering the fundamental power balance between citizens and state — a civilizational-scale rearrangement. Critics would say ordinary regulatory capacity would simply resume, and little in daily civilian safety would change, since the doctrine's practical effect is mainly to block regulation rather than to have prevented any actual instance of state tyranny. The parties dispute which world we are actually in.
% FOUNDING_PROBLEM: The founding-era anxiety that a professional standing army under federal control, absent a citizenry capable of armed resistance, could enable the new federal government to become as tyrannical as the crown it replaced.
% FOUNDING_PROBLEM_CORROBORATION: Militia movement advocates and some originalist legal scholars attest the problem remains live, citing federal power expansion generally. Constitutional historians outside the movement, and most sitting appellate courts applying incorporation doctrine, attest that the standing-army-versus-citizen-militia structural problem the framers faced has been superseded by the professionalized, civilian-controlled military and layered checks (elections, courts, federalism, free press) that did not exist in 1791 — no institutional body outside the movement corroborates that armed civilian resistance capacity remains the operative check.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects that the coordination story — collective deterrence against tyranny — functions as cover for a real transfer: political insulation and market protection for arms manufacturers and organized militia movements, financed by diffuse costs borne by security personnel, violence survivors, and civilians with no say in the doctrine's adoption. Suppression is moderate (0.35) rather than high because the reading operates mainly through legal argument and political mobilization rather than direct coercion — its force is rhetorical and juridical, foreclosing regulatory debate rather than physically compelling anyone. Accessibility collapse is low (0.3): alternative readings remain fully articulable and litigated; this is a contested doctrine, not a settled fact. Resistance is high (0.78) because constitutional historians, gun violence prevention advocates, and much of the judiciary actively contest the premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizen militia organizations and firearms manufacturers sit near the full-beneficiary end: they gain legitimacy, market protection, and political leverage while bearing none of the downstream physical risk. State security personnel and civilians in any hypothetical conflict zone sit near the full-target end: the doctrine structurally designates them as the resistance's object or as collateral exposure, and they cannot exit that designation. Gun violence survivors bear a diffuse but real cost from the doctrine's chilling effect on ordinary regulation, trapped by a national-scope legal environment they cannot individually escape.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fear of federal standing-army tyranny absent citizen check — was live in 1791 and is contested as either live or dead today; corroboration from outside the movement (courts, historians) tends toward dead, given elections, an apolitical professional military, and federalism as the operative checks now. The doctrine's persistence despite contested founding-problem status, combined with rising measured extraction and suppression over 1990-2024, is the signature this framework is built to catch: coordination language ('deterrent against tyranny') sustaining a transfer (manufacturer profit, movement legitimacy) that outlived the scenario it was designed for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insurrectionist_premise_historical_grounding,
    'Does founding-era militia practice and Convention-era debate actually support an individual-resistance-capacity theory, or is this a twentieth-century reconstruction read backward onto the text?',
    'Systematic historical review of state ratification debates, contemporaneous militia statutes, and founding-era usage of ''bear arms'' in non-military contexts, weighed against the reading''s own claimed lineage.',
    'If the historical grounding is weak, the reading''s authority_grounding shifts further toward extraction/movement-serving reconstruction rather than lineage-based interpretation, strengthening the mandatrophy finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurrectionist_premise_historical_grounding, empirical, 'Whether the insurrectionist premise has genuine founding-era grounding or is a later reconstruction.').

omega_variable(
    military_grade_arms_logical_endpoint,
    'Does the insurrectionist premise necessarily entail protection for military-grade weaponry, or can the reading be held while still permitting some regulation of weapon lethality?',
    'Doctrinal analysis of whether courts adopting insurrectionist reasoning have in practice extended protection to fully automatic weapons, or have carved exceptions (NFA-type restrictions) while retaining the theory.',
    'If courts consistently carve exceptions, the ''military-grade arms enter protected domain'' delta is weaker than assumed and epsilon should be revised downward in a future version of this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_grade_arms_logical_endpoint, conceptual, 'Whether the military-grade-arms delta is a necessary or contingent feature of the insurrectionist reading.').

omega_variable(
    kernel_reading_relative_authority,
    'Which of the three sibling readings currently commands majority judicial authority, and how does that allocation affect the practical (as opposed to doctrinal) extraction this reading generates?',
    'Track circuit court and Supreme Court opinions explicitly invoking resistance-to-tyranny reasoning versus individual-right or militia-conditioned reasoning over the measurement interval.',
    'If insurrectionist reasoning is invoked rarely relative to individual_right_reading in binding precedent, this constraint''s practical suppression and extraction operate more through political rhetoric than court-enforced doctrine, which would refine but not eliminate the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relative_authority, empirical, 'Relative judicial uptake of the insurrectionist reading versus its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1990, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(seco_tr_t1998, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1998, 0.3).
narrative_ontology:measurement(seco_tr_t2006, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2006, 0.34).
narrative_ontology:measurement(seco_tr_t2012, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2012, 0.37).
narrative_ontology:measurement(seco_tr_t2018, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(seco_be_t1990, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(seco_be_t1998, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1998, 0.44).
narrative_ontology:measurement(seco_be_t2006, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2006, 0.5).
narrative_ontology:measurement(seco_be_t2012, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(seco_be_t2018, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1990, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(seco_su_t1998, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1998, 0.24).
narrative_ontology:measurement(seco_su_t2006, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2006, 0.28).
narrative_ontology:measurement(seco_su_t2012, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2012, 0.3).
narrative_ontology:measurement(seco_su_t2018, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2018, 0.33).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__insurrectionist_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% This constraint, individual_right_reading, and militia_conditioned_reading are three readings of the single second_amendment_boundary kernel text. Each authors its own epsilon, beneficiary/victim structure, and classification rather than sharing a measurement. This story's epsilon (0.61, tangled_rope) reflects the insurrectionist premise's extension to military-grade arms and treatment of disarmament as tyranny evidence; individual_right_reading's epsilon reflects a narrower self-defense grounding with a different victim set; militia_conditioned_reading's epsilon reflects a regulation-permissive reading with minimal victim structure. The forecloses relation to militia_conditioned_reading is declared because the insurrectionist premise requires the right to extend beyond organized militia service to individual resistance capacity, directly contradicting the militia-bounded premise within any single interpretive framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
