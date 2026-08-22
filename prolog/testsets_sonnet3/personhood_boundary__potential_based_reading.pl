% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Potential-Based Personhood Boundary (Rational-Agency-Potential Reading)
 *   domain: moral_philosophy/bioethics/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the potential_based_reading of the contested
 *   personhood_boundary kernel: moral standing is grounded not in birth (the
 *   birth_threshold_reading) nor in demonstrated post-natal fitness (the
 *   fitness_contingent_reading), but in a clinical prognosis about an
 *   infant's future potential for rational agency. Under this reading,
 *   severely disabled infants whose prognosis forecloses any plausible future
 *   capacity for agency-relevant functioning may be judged to lack full moral
 *   standing, licensing withdrawal-of-care decisions that would be
 *   impermissible if personhood attached unconditionally at birth. The
 *   reading gives parents and physicians decisional authority structured
 *   around a potential-threshold rather than an actuality-threshold. ε is
 *   authored for the standing arrangement as this reading's own lights see
 *   it: a real, functioning clinical/ethical practice that nonetheless
 *   transfers standing away from a powerless class based on contested and
 *   historically unreliable predictions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.61).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.52).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Potential-Based Personhood Boundary (Rational-Agency-Potential Reading)").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/bioethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'ce00b4ce-3a22-4d48-9861-569ce7f04cc1').
narrative_ontology:cs_kernel_codification('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', distributed).
narrative_ontology:cs_authority_grounding('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', practice).
narrative_ontology:cs_interpretation_layer_present('ce00b4ce-3a22-4d48-9861-569ce7f04cc1').
narrative_ontology:cs_reading_relation('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', foundational, prospective_capacity_grounds_standing).
narrative_ontology:cs_axiom_status(prospective_capacity_grounds_standing, holdable).
narrative_ontology:cs_axiom_grounding('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', prospective_capacity_grounds_standing, instrumental).
narrative_ontology:cs_axiom('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', secondary, birth_alone_insufficient_for_full_standing).
narrative_ontology:cs_axiom_status(birth_alone_insufficient_for_full_standing, holdable).
narrative_ontology:cs_axiom_grounding('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', birth_alone_insufficient_for_full_standing, deontological).
narrative_ontology:cs_reference_frame('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', capacity_based_moral_status_tradition).
narrative_ontology:cs_drift_state('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', post_disability_rights_movement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce00b4ce-3a22-4d48-9861-569ce7f04cc1', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, attending_physicians).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, hospital_ethics_committees).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, healthcare_systems).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, disability_rights_advocates).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, surviving_disabled_adults).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_agency_potential_as_ground_of_standing).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, capacity_based_moral_status_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot advocate, testify, or exit. Their moral standing under this reading is assessed by others based on a clinical prediction about future capacity for rational agency. If judged to lack sufficient potential, decisions about withholding treatment or life support proceed with reduced moral weight attached to their interests.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Hold legal and practical authority to consent to withdrawal of treatment based on prognosis of the infant's future capacities. Bear the emotional, financial, and caregiving burden of the alternative (continued life with severe disability) and benefit from a framework that treats non-treatment as morally permissible rather than as ending a full person's life.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants, beneficiary).

% Apply clinical judgment to assess prognosis for future rational-agency-relevant capacities and translate that assessment into a personhood determination that shapes what care is offered. Benefit from a doctrine that supplies a professionally defensible boundary for withdrawing intensive intervention, reducing liability and resource strain.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, attending_physicians, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, attending_physicians, beneficiary).

% Adjudicate contested cases by applying the potential-based standard, issuing institutional determinations that structure what counts as permissible withdrawal of care. Administer the boundary rather than bear its costs directly.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, hospital_ethics_committees, agenda_setter,
    institutional, generational, constrained, regional).

% Benefit financially and operationally from a doctrine that permits reduced allocation of intensive-care resources to infants judged to lack potential for rational agency, easing capacity pressure in neonatal intensive care.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, healthcare_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Argue the potential standard imports an ableist metric of worth that devalues disabled lives generally, since capacity for rational agency is not binary and disabled adults who defy early prognosis are living counterevidence. Their objections are heard in policy fora but do not control bedside determinations, which remain physician- and parent-driven.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded).

% Living evidence that early clinical potential-prognoses are frequently wrong. Bear the reputational and dignitary cost of a doctrine that would, applied consistently at their own birth, have licensed treating them as non-persons; some testify in policy debates but were not parties to the original decisions made about infants like them.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, surviving_disabled_adults, payer,
    moderate, biographical, mobile, national).

% Analyze the coherence of grounding moral status in potential rather than actual capacity, comparing this reading to birth-threshold and fitness-contingent alternatives without being bound by clinical outcomes.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, philosophers_of_personhood, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, healthcare_systems).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies clinicians, parents, and ethics committees a workable decision procedure for a genuinely hard class of cases — profound congenital impairment with poor prognosis for any future capacity resembling agency — where some shared standard is needed to avoid ad hoc, inconsistent bedside decisions under extreme time pressure.
% TRANSFER_FUNCTION: Moves moral standing (and consequently resource allocation, legal protection, and claim-weight in life-and-death decisions) away from infants whose predicted future capacities fall below the threshold, and toward the decisional authority and resource discretion of parents, physicians, and healthcare systems.
% ABSENT_VOICES: The infants themselves have no voice by definition. Disability rights advocates are present in academic and policy debate but structurally absent from the bedside determination itself, where the standard is actually applied. Adults who were once such infants and survived against prognosis are almost never consulted retrospectively about the doctrine that nearly excluded them.
% DISAPPEARANCE_RATIONALE: If potential-based standing vanished and personhood attached unconditionally at birth (the sibling birth_threshold_reading), a substantial category of currently-permitted non-treatment decisions would require reclassification as ending a full person's life, triggering different legal, insurance, and clinical consent requirements across neonatal intensive care.
% FOUNDING_PROBLEM: Clinicians and families facing extreme congenital impairment needed a principled way to distinguish cases where aggressive intervention serves a future person's interests from cases where it prolongs dying without a plausible future agent to benefit — avoiding both reflexive over-treatment and reflexive under-treatment.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists working from capacity-based theories of moral status (e.g., in the tradition associated with debates following Singer and Kuhse) attest the problem remains live: prognosis for some conditions genuinely cannot support any account of future rational agency. Disability studies scholars and longitudinal outcome researchers — outside the class that administers or benefits from the doctrine — attest that clinical potential-prognosis in infancy is measured with poor reliability and that the doctrine's operative boundary has shifted opportunistically with resource pressure rather than with improved prognostic accuracy; no source entirely outside both the philosophical and clinical communities corroborates the doctrine's stability.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects that a real coordination problem exists (clinicians need SOME standard for these cases) but the standard's operation transfers moral and resource weight away from the least powerful party in the story based on predictions with a documented error rate, and the class bearing the cost (the infants) has zero capacity to contest the classification. Suppression (0.52) is moderate rather than severe — the doctrine operates through institutional procedure (ethics committees, clinical consensus) rather than brute coercion, but dissenting views (disability rights framings) are structurally excluded from the actual point of decision. Accessibility collapse is moderate (0.42): alternative frameworks (birth threshold, fitness-contingent) remain visible and contested in law and philosophy, they have not collapsed the way a mountain's alternatives would. Resistance is comparatively high (0.68) because disability rights advocacy against capacity-based standards is organized, sustained, and growing.
 *
 * DIRECTIONALITY LOGIC:
 *   Parents, physicians, ethics committees, and healthcare systems occupy the agenda-setting/beneficiary end: they administer the standard, and the standard reduces their liability and resource burden by supplying a morally permissive category for non-treatment. Severely disabled infants are the pure target class — trapped, powerless, no voice, and the classification is applied TO them, not negotiated with them, which pushes their directionality close to full-target regardless of scope. Surviving disabled adults and disability rights advocates are payers in a diffuse sense: they carry the dignitary and reputational cost of a standard that, applied at their own birth, might have excluded them, even though they were never direct subjects of a determination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuinely hard triage decisions under uncertainty) remains partly live — some prognoses are genuinely unambiguous — but founding_problem_status is authored as contested because outcome research increasingly shows infancy-stage capacity predictions are unreliable for a meaningful fraction of borderline cases, meaning the doctrine may now be extracting standing from infants whose prognosis was simply wrong, not just hard. This is precisely the mandatrophy signature the R5 interview is designed to surface: a mandate whose original justification (irreducible uncertainty requiring some threshold) persists institutionally even where the specific predictive tool underlying it has been shown less reliable than the doctrine assumes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prognostic_reliability_of_potential_judgments,
    'How reliably can clinicians actually predict, at or shortly after birth, an infant''s future capacity for rational-agency-relevant functioning?',
    'Longitudinal outcome studies tracking infants given severe prognoses against their actual developmental trajectories, compared across diagnostic categories and time periods.',
    'Low reliability would mean the doctrine extracts standing from infants based on predictions no better than chance for a significant subset of cases, sharpening the tangled_rope reading toward snare; high reliability for at least a well-defined subset of conditions would support the coordination function for that subset specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prognostic_reliability_of_potential_judgments, empirical, 'Empirical reliability of infant-stage capacity prognosis.').

omega_variable(
    potential_vs_actuality_grounding,
    'Is grounding moral status in POTENTIAL for a capacity philosophically coherent, or does it collapse into either the birth_threshold_reading (if potential is imputed to nearly all born humans) or the fitness_contingent_reading (if potential is cashed out as a disguised actuality requirement)?',
    'Philosophical analysis of whether ''potential for X'' can be a stable, non-arbitrary threshold distinct from both ''born'' and ''currently demonstrates X'' — this is the crux distinguishing the three sibling readings and is unresolvable by empirical data alone.',
    'If potential-based grounding is philosophically unstable, this reading is not a genuinely distinct third position but an unstable hybrid that authority-holders can slide toward either sibling reading depending on convenience — increasing suppression and reducing the reading''s claim to independent coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(potential_vs_actuality_grounding, conceptual, 'Whether the potential-based reading is a stable third position or collapses into a sibling reading.').

omega_variable(
    disability_devaluation_generalization,
    'Does applying a capacity-based threshold to severely disabled infants generalize into devaluation of disabled persons'' lives and interests more broadly, or is the boundary contained to a narrow class of prognostically hopeless cases?',
    'Track whether legal and clinical reasoning developed for the infant-personhood boundary is subsequently cited or extended in cases involving disabled adults, guardianship, or resource rationing.',
    'Evidence of generalization would support disability rights advocates'' claim that the doctrine is not narrowly contained and its extraction reaches well beyond the infant class named as victims here; containment would narrow the story''s scope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_devaluation_generalization, empirical, 'Whether the doctrine''s reasoning generalizes beyond the infant case.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__potential_based_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__potential_based_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__potential_based_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__potential_based_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__potential_based_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__potential_based_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__potential_based_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__potential_based_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__potential_based_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__potential_based_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__potential_based_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__potential_based_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(personhood_boundary__potential_based_reading, 0.1).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'personhood boundary' kernel per the ε-invariance principle: birth_threshold_reading, fitness_contingent_reading, and this potential_based_reading. Each has a distinct victim set, distinct authority structure, and — critically — a distinct ε, because each reading's own lights assess a different standing arrangement. Do not average across them; consult each file independently and use affects_constraints edges for propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
