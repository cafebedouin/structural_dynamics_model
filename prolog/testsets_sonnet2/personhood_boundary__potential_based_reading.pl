% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Potential-Based Personhood Boundary (Rational Agency Potential Reading)
 *   domain: moral_philosophy/bioethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the potential-based reading of the contested
 *   personhood-boundary kernel: moral standing is grounded not in birth per
 *   se, nor in demonstrated fitness, but in an entity's potential to develop
 *   rational-agency-relevant capacities. Under this reading, a severely
 *   disabled infant whose prognosis rules out ever developing such capacities
 *   may lack full standing, which licenses parents and clinical ethics
 *   committees to treat withdrawal of aggressive intervention as morally
 *   permissible in a way an unqualified birth-threshold reading would not.
 *   This is a single reading generated as its own clean, epsilon-invariant
 *   constraint; the sibling readings (birth-threshold, fitness-contingent)
 *   are separate constraint stories linked via network.affects_constraints,
 *   not alternative measurements folded into this one.
 *
 * KEY AGENTS:
 *   - severely_disabled_infants_deemed_without_potential: primary payer (powerless/trapped) — bears the standing exclusion
 *   - parents_of_severely_disabled_infants: agenda_setter/beneficiary (moderate/constrained) — gains decisional discretion
 *   - clinical_ethics_committees: agenda_setter/beneficiary (institutional/arbitrage) — administers and holds discretion over the standard
 *   - healthcare_systems_managing_resource_allocation: beneficiary (institutional/arbitrage) — resource relief from withdrawal decisions
 *   - disability_rights_advocates: excluded (organized/constrained) — contests the standard's historical misapplication
 *   - moral_philosophers_of_capacity_theory: analytical observer — theorizes the potential criterion's boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.58).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.62).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Potential-Based Personhood Boundary (Rational Agency Potential Reading)").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/bioethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '0bf4e9bd-da42-4559-9c09-4031ea5d5b3b').
narrative_ontology:cs_kernel_codification('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', distributed).
narrative_ontology:cs_authority_grounding('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', practice).
narrative_ontology:cs_interpretation_layer_present('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b').
narrative_ontology:cs_reading_relation('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', personhood_boundary__fitness_contingent_reading, influences).
narrative_ontology:cs_axiom('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', foundational, standing_grounded_in_capacity_for_rational_agency).
narrative_ontology:cs_axiom_status(standing_grounded_in_capacity_for_rational_agency, holdable).
narrative_ontology:cs_axiom_grounding('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', standing_grounded_in_capacity_for_rational_agency, deontological).
narrative_ontology:cs_axiom('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', secondary, prognosis_of_permanent_incapacity_defeats_presumptive_standing).
narrative_ontology:cs_axiom_status(prognosis_of_permanent_incapacity_defeats_presumptive_standing, holdable).
narrative_ontology:cs_axiom_grounding('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', prognosis_of_permanent_incapacity_defeats_presumptive_standing, empirically_contingent).
narrative_ontology:cs_reference_frame('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', clinical_bioethics_capacity_consensus).
narrative_ontology:cs_drift_state('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', post_disability_rights_movement_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0bf4e9bd-da42-4559-9c09-4031ea5d5b3b', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, clinical_ethics_committees).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, healthcare_systems_managing_resource_allocation).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, philosophical_traditions_grounding_standing_in_capacity).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants_deemed_without_potential).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, disability_rights_advocates).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, families_seeking_full_standing_recognition).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, capacity_grounded_moral_status_theory).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, gradualist_personhood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot self-advocate, communicate preferences, or contest the clinical/parental determination that they lack the relevant potential for rational agency. Their moral standing under this reading is contingent on a prognosis rendered by others; if judged to lack potential, decisions about withholding treatment, resource allocation, or life-sustaining care are made without their standing counting as a constraint on those decisions in the way a full person's would.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants_deemed_without_potential, payer,
    powerless, biographical, trapped, national).

% Empowered under this reading to participate in, and often decisively influence, determinations of whether their infant possesses the relevant potential. This authority relieves them of an otherwise absolute duty of care obligation and gives them legitimate grounds to consent to withholding treatment — a real benefit to a family facing catastrophic caregiving burden, but one that depends on the infant's standing being unsettled rather than fixed.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants, beneficiary).

% Administer the potential-based standard in practice: convene to assess prognosis, likelihood of ever developing rational-agency-relevant capacities, and advise on treatment withdrawal. Their institutional function and continued relevance depend on personhood remaining a matter of clinical judgment rather than a bright-line birth threshold; they operationalize the boundary and hold discretion in applying it.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, clinical_ethics_committees, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, clinical_ethics_committees, beneficiary).

% Benefit from a standard that permits withdrawal of extraordinary intensive-care resources from infants judged to lack relevant potential, easing NICU capacity and cost pressures. The system does not present this as its rationale, but the potential-based standard's practical effect is to make some resource-conserving decisions morally permissible that a strict birth-threshold standard would forbid.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, healthcare_systems_managing_resource_allocation, beneficiary,
    institutional, generational, arbitrage, national).

% Argue that grounding standing in potential for rational agency systematically devalues disabled lives and encodes an ableist standard into moral status itself — that the same reasoning, applied consistently, would have stripped standing from people now living full lives who were once judged unlikely to develop expected capacities. They are rarely represented on the clinical ethics committees that apply the standard in individual cases.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Parents who reject the potential-based framing and want their infant treated as a full person regardless of prognosis find themselves contesting a committee's authority to make the determination at all, often under time pressure and without equivalent institutional standing to the clinicians and ethicists they are arguing against.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, families_seeking_full_standing_recognition, payer,
    powerless, biographical, constrained, local).

% Develop and defend the theoretical architecture — potential for rational agency as the relevant threshold property — and debate where the line between 'has potential but not yet manifest' and 'lacks potential entirely' can be drawn without collapsing into either the birth-threshold or fitness-contingent readings.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, moral_philosophers_of_capacity_theory, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled basis for distinguishing cases where withholding aggressive intervention is morally permissible from cases where it is not, allowing clinicians and families facing genuinely hopeless prognoses (e.g., anencephaly, certain severe trisomies) to make care decisions without being bound by an absolute birth-threshold duty that some regard as clinically and morally unworkable at the margins.
% TRANSFER_FUNCTION: Moves decisional authority over an infant's treatment, and ultimately over whether the infant's interests count as constraints on that decision, from the infant (who cannot exercise it) to parents and clinical committees; moves resource burden away from intensive-care systems in cases where potential is judged absent.
% ABSENT_VOICES: The infant itself has no voice under any reading. Disability rights advocates who would contest the potential criterion's history of misapplication (historically used to deny standing to people with Down syndrome, spina bifida, and other conditions now understood to be compatible with rich human lives) are largely outside the clinical ethics committee room where determinations are actually made.
% DISAPPEARANCE_RATIONALE: If the potential-based standard vanished and were replaced by an unqualified birth-threshold standard, clinical ethics committees would lose their adjudicative role in withdrawal-of-care cases, parents would lose the discretion the standard currently grants them to consent to non-treatment on standing grounds, and NICU resource-allocation decisions in extreme-prognosis cases would have to be justified on grounds other than personhood (e.g., futility or proportionality doctrines), reorganizing a substantial area of neonatal ethics practice.
% FOUNDING_PROBLEM: Clinicians and families facing extreme neonatal prognoses (total absence of higher brain function, imminent and certain death regardless of intervention) needed a framework that did not require treating every born human as an absolute claim-holder regardless of capacity, in order to avoid what they judged to be futile or even cruel prolongation of dying.
% FOUNDING_PROBLEM_CORROBORATION: Clinical bioethicists and some parents in extreme-prognosis cases attest the problem remains live and the standard serves a genuine function. Disability rights organizations and several bioethicists writing from outside the clinical committee structure (e.g., critics of the 'quality of life' calculus in disability studies literature) attest that the same reasoning has historically been misapplied well beyond its founding cases, and that its persistence now serves administrative and resource-allocation convenience as much as the narrow founding problem.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.58: substantial but below the level of a pure snare, because the standard does perform genuine coordination work in a narrow class of truly extreme-prognosis cases (anencephaly, near-certain imminent death) while extending, in practice, to a much broader and contested set of disability presentations where the 'lacks potential' judgment is itself the site of dispute. Suppression (0.62) reflects that the infant subject to the determination has no mechanism to contest it and no voice in the committee process; this is a raw structural fact about who can object, not scaled by scope. Accessibility collapse is moderate (0.5) — the standard has not fully displaced the birth-threshold alternative in law or practice, and legal challenges and disability-rights advocacy keep the boundary contested rather than settled. Resistance is high (0.7): disability rights movements have actively and effectively challenged specific applications of capacity-based standing determinations, most visibly in cases now recognized as having wrongly denied treatment to infants with conditions (e.g., Down syndrome, spina bifida) later shown compatible with flourishing lives — this resistance history is exactly why extractiveness is authored as declining over the measured interval rather than flat or rising.
 *
 * DIRECTIONALITY LOGIC:
 *   Severely disabled infants judged to lack potential sit at the full-target end: they cannot exit, cannot contest the determination, and the entire apparatus of decision-making operates on them rather than through them. Parents and clinical ethics committees sit toward the beneficiary end — they gain legitimate discretion and relief from an absolute duty-of-care standard, and their exit options (constrained/arbitrage respectively) reflect that they retain agency the infant lacks. Disability rights advocates are declared as excluded rather than payer because the direct extraction does not fall on them personally, but their structural exclusion from the determining committees is itself a site of contest documented under absent_voices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuinely hopeless neonatal prognoses requiring an ethical framework beyond strict rescue obligation — remains partially live (anencephaly and comparable conditions still occur and still present the same dilemma the standard was built to address), which blocks a clean mandatrophy verdict. But the founding_problem_status is authored as contested rather than dead precisely because the standard's scope of application has historically outrun its narrow founding cases (misapplication to Down syndrome and spina bifida infants in the mid-20th century being the clearest documented instances), and disability rights corroboration from outside the benefiting parties (clinicians, parents making withdrawal decisions) supports treating the standard as at least partly captured by administrative and resource-allocation convenience beyond its original justification. Classifying this as tangled_rope rather than snare preserves the genuine coordination function in the narrowest cases while still registering the asymmetric extraction on the excluded infant population as real, rather than either fully vindicating or fully condemning the standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_criterion_reliability,
    'How reliably can clinical prognosis distinguish ''genuinely lacks potential for rational agency'' from ''currently lacks demonstrated capacity but will develop it,'' given the documented historical record of misapplication to conditions now known to be compatible with rich human lives?',
    'Longitudinal outcome data comparing infants judged to lack potential against actual developmental trajectories where treatment was nonetheless provided (e.g., natural experiments from jurisdictions with mandatory-treatment rules for comparable conditions), cross-checked against disability-rights-documented cases of historical misapplication.',
    'If the criterion is shown to be unreliable at the margins historically applied, the standard''s coordination function narrows sharply to only the most extreme, unambiguous cases (e.g., anencephaly), and its extension to a broader disabled-infant population becomes harder to distinguish from pure extraction dressed as principled ethics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_criterion_reliability, empirical, 'Whether the potential criterion reliably tracks what it claims to track, or has a documented history of false negatives.').

omega_variable(
    kernel_framing_which_reading_governs,
    'Is the potential-based reading the correct lens for the personhood-boundary kernel, or do the birth-threshold and fitness-contingent readings better capture what different legal and moral traditions actually enforce?',
    'Comparative analysis of which reading actually governs decision-making in specific jurisdictions and specific case types — the potential-based reading may dominate in neonatal ICU ethics committees while birth-threshold dominates in criminal and civil law generally.',
    'If jurisdictions apply different readings to different legal contexts simultaneously, the personhood-boundary kernel is not resolved by any single reading but persists as an active site of multi-reading contest — which is the structural situation this story assumes and the sibling stories (fitness_contingent_reading, birth_threshold_reading) are written to capture independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_which_reading_governs, conceptual, 'Alternative framing: which reading of the personhood kernel actually governs practice in a given institutional context, and what would change under an alternative framing.').

omega_variable(
    committee_capture_by_resource_pressure,
    'To what extent do clinical ethics committee determinations of ''lacks potential'' track genuine prognosis versus institutional resource-allocation pressure (bed availability, cost containment)?',
    'Comparison of determination rates and outcomes across institutions with differing resource constraints; audit of committee composition and incentive structures.',
    'If determinations correlate with resource pressure independent of prognosis, the beneficiary declaration for healthcare_systems_managing_resource_allocation moves from incidental to load-bearing, and the classification shifts further toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committee_capture_by_resource_pressure, empirical, 'Whether resource pressure, rather than prognosis alone, drives potential determinations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__potential_based_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__potential_based_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__potential_based_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__potential_based_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__potential_based_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__potential_based_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__potential_based_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__potential_based_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__potential_based_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__potential_based_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__potential_based_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__potential_based_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(personhood_boundary__potential_based_reading, 0.1).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the personhood_boundary kernel. birth_threshold_reading treats all born humans as full standing-holders and would classify this constraint's exclusion mechanism as illegitimate; fitness_contingent_reading extends the exclusion logic to a broader set of entities based on demonstrated rather than potential capacity, of which this reading is a narrower, more restrictive case. Each reading carries its own epsilon, beneficiary/victim structure, and classification — this file does not average across readings, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
